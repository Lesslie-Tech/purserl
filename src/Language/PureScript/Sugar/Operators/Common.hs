module Language.PureScript.Sugar.Operators.Common where

import Prelude

import Control.Monad (guard, join)
import Control.Monad.Except (throwError)

import Data.Either (rights)
import Data.Functor.Identity (Identity)
import Data.List (sortOn)
import Data.Maybe (mapMaybe, fromJust)
import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Text.Parsec qualified as P
import Text.Parsec.Pos qualified as P
import Text.Parsec.Expr qualified as P

import Language.PureScript.AST (Associativity(..), ErrorMessageHint(..), SourceSpan)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (ErrorMessage(..), MultipleErrors(..), SimpleErrorMessage(..))
import Language.PureScript.Names (OpName, Qualified, eraseOpName)

type Chain a = [Either a a]

type FromOp nameType a = a -> Maybe (SourceSpan, Qualified (OpName nameType))
type Reapply nameType a = SourceSpan -> Qualified (OpName nameType) -> a -> a -> a

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token (const "") (const (P.initialPos ""))

parseValue :: P.Parsec (Chain a) () a
parseValue = token (either Just (const Nothing)) P.<?> "expression"

parseOp
  :: FromOp nameType a
  -> P.Parsec (Chain a) () (SourceSpan, Qualified (OpName nameType))
parseOp fromOp = token (either (const Nothing) fromOp) P.<?> "operator"

matchOp
  :: FromOp nameType a
  -> Qualified (OpName nameType)
  -> P.Parsec (Chain a) () SourceSpan
matchOp fromOp op = do
  (ss, ident) <- parseOp fromOp
  guard $ ident == op
  pure ss

opTable
  :: [[(Qualified (OpName nameType), Associativity)]]
  -> FromOp nameType a
  -> Reapply nameType a
  -> [[P.Operator (Chain a) () Identity a]]
opTable ops fromOp reapply =
  map (map (\(name, a) -> P.Infix (P.try (matchOp fromOp name) >>= \ss -> return (reapply ss name)) (toAssoc a))) ops

-- | The precedence level of every operator: 0 binds tightest.
type OpLevels nameType = M.Map (Qualified (OpName nameType)) (Int, Associativity)

opLevels :: [[(Qualified (OpName nameType), Associativity)]] -> OpLevels nameType
opLevels ops = M.fromList [(name, (lvl, assoc)) | (lvl, grp) <- zip [0 ..] ops, (name, assoc) <- grp]

-- | An operator with its level, associativity, how to apply it and its right operand.
data Item a = Item !Int !Associativity (a -> a -> a) a

-- | Resolves precedence by precedence climbing. It only answers when the parser
-- below would answer with the same tree: on an unknown operator or a clash
-- between operators of one level (mixed or repeated non-associative) it gives
-- 'Nothing', and the parser reports the error. Backtick operators come from
-- @fromTick@ and bind tighter than all the others (level -1).
reorderChain
  :: forall a nameType
   . FromOp nameType a
  -> (a -> Maybe (a -> a -> a))
  -> Reapply nameType a
  -> OpLevels nameType
  -> Chain a
  -> Maybe a
reorderChain fromOp fromTick reapply levels chain = do
  (lhs, items) <- toItems chain
  (result, []) <- climb maxBound lhs items
  pure result
  where
  toItems :: Chain a -> Maybe (a, [Item a])
  toItems (Left lhs : rest) = (lhs,) <$> itemsOf rest
  toItems _ = Nothing

  itemsOf :: Chain a -> Maybe [Item a]
  itemsOf [] = Just []
  itemsOf (Right op : Left r : rest) = do
    (lvl, assoc, f) <- classify op
    (Item lvl assoc f r :) <$> itemsOf rest
  itemsOf _ = Nothing

  classify :: a -> Maybe (Int, Associativity, a -> a -> a)
  classify op = case fromOp op of
    Just (ss, name) -> (\(lvl, assoc) -> (lvl, assoc, reapply ss name)) <$> M.lookup name levels
    Nothing -> (-1, Infixl,) <$> fromTick op

  -- Takes the operators of level @k@ or tighter that follow @lhs@.
  climb :: Int -> a -> [Item a] -> Maybe (a, [Item a])
  climb _ lhs [] = Just (lhs, [])
  climb k lhs items@(Item lvl assoc f r : more)
    | lvl > k = Just (lhs, items)
    | otherwise = case assoc of
        Infixr -> do
          (r', rest) <- rightOperand lvl r more
          climb k (f lhs r') rest
        _ -> do
          (r', rest) <- climb (lvl - 1) r more
          guard (not (clashes lvl assoc rest))
          climb k (f lhs r') rest

  -- The right operand of a right associative operator of level @lvl@.
  rightOperand :: Int -> a -> [Item a] -> Maybe (a, [Item a])
  rightOperand lvl r more = do
    (z, rest) <- climb (lvl - 1) r more
    case rest of
      Item lvl' assoc' f' r' : more' | lvl' == lvl -> do
        guard (assoc' == Infixr)
        (y, rest') <- rightOperand lvl r' more'
        pure (f' z y, rest')
      _ -> pure (z, rest)

  -- Whether the next operator is of the same level but can't continue a chain of @assoc@.
  clashes :: Int -> Associativity -> [Item a] -> Bool
  clashes lvl assoc (Item lvl' assoc' _ _ : _) = lvl' == lvl && (assoc' /= assoc || assoc == Infix)
  clashes _ _ [] = False

matchOperators
  :: forall a nameType
   . Show a
  => (a -> Bool)
  -> (a -> Maybe (a, a, a))
  -> FromOp nameType a
  -> (a -> Maybe (a -> a -> a))
  -> Reapply nameType a
  -> ([[P.Operator (Chain a) () Identity a]] -> P.OperatorTable (Chain a) () Identity a)
  -> [[(Qualified (OpName nameType), Associativity)]]
  -> a
  -> Either MultipleErrors a
matchOperators = matchOperatorsWith True

-- | With 'False' the precedence climbing is skipped and the parser decides everything.
matchOperatorsWith
  :: forall a nameType
   . Show a
  => Bool
  -> (a -> Bool)
  -> (a -> Maybe (a, a, a))
  -> FromOp nameType a
  -> (a -> Maybe (a -> a -> a))
  -> Reapply nameType a
  -> ([[P.Operator (Chain a) () Identity a]] -> P.OperatorTable (Chain a) () Identity a)
  -> [[(Qualified (OpName nameType), Associativity)]]
  -> a
  -> Either MultipleErrors a
matchOperatorsWith climbFirst isBinOp extractOp fromOp fromTick reapply modOpTable ops = parseChains
  where
  levels :: OpLevels nameType
  levels = opLevels ops
  parseChains :: a -> Either MultipleErrors a
  parseChains ty
    | True <- isBinOp ty = bracketChain (extendChain ty)
    | otherwise = pure ty
  extendChain :: a -> Chain a
  extendChain ty
    | Just (op, l, r) <- extractOp ty = Left l : Right op : extendChain r
    | otherwise = [Left ty]
  bracketChain :: Chain a -> Either MultipleErrors a
  -- A lone operator needs no precedence resolution.
  bracketChain [Left l, Right op, Left r]
    | Just (ss, name) <- fromOp op = pure (reapply ss name l r)
  bracketChain chain
    | climbFirst, Just a <- reorderChain fromOp fromTick reapply levels chain = pure a
    | otherwise =
        case P.parse opParser "operator expression" chain of
          Right a -> pure a
          Left _ -> throwError . MultipleErrors $ mkErrors chain
  opParser :: P.Parsec (Chain a) () a
  opParser = P.buildExpressionParser (modOpTable (opTable ops fromOp reapply)) parseValue <* P.eof

  -- Generating a good error message involves a bit of work here, as the parser
  -- can't provide one for us.
  --
  -- We examine the expression chain, plucking out the operators and then
  -- grouping them by shared precedence, then if any of the following conditions
  -- are met, we have something to report:
  --   1. any of the groups have mixed associativity
  --   2. there is more than one occurrence of a non-associative operator in a
  --      precedence group
  mkErrors :: Chain a -> [ErrorMessage]
  mkErrors chain =
    let
      opInfo :: M.Map (Qualified (OpName nameType)) (Integer, Associativity)
      opInfo = M.fromList $ concatMap (\(n, o) -> map (\(name, assoc) -> (name, (n, assoc))) o) (zip [0..] ops)
      opPrec :: Qualified (OpName nameType) -> Integer
      opPrec = fst . fromJust . flip M.lookup opInfo
      opAssoc :: Qualified (OpName nameType) -> Associativity
      opAssoc = snd . fromJust . flip M.lookup opInfo
      chainOpSpans :: M.Map (Qualified (OpName nameType)) (NEL.NonEmpty SourceSpan)
      chainOpSpans = foldr (\(ss, name) -> M.alter (Just . maybe (pure ss) (NEL.cons ss)) name) M.empty . mapMaybe fromOp $ rights chain
      opUsages :: Qualified (OpName nameType) -> Int
      opUsages = maybe 0 NEL.length . flip M.lookup chainOpSpans
      precGrouped :: [NEL.NonEmpty (Qualified (OpName nameType))]
      precGrouped = NEL.groupWith opPrec . sortOn opPrec $ M.keys chainOpSpans
      assocGrouped :: [NEL.NonEmpty (NEL.NonEmpty (Qualified (OpName nameType)))]
      assocGrouped = fmap (NEL.groupWith1 opAssoc . NEL.sortWith opAssoc) precGrouped
      mixedAssoc :: [NEL.NonEmpty (Qualified (OpName nameType))]
      mixedAssoc = fmap join . filter (\precGroup -> NEL.length precGroup > 1) $ assocGrouped
      nonAssoc :: [NEL.NonEmpty (Qualified (OpName nameType))]
      nonAssoc = NEL.filter (\assocGroup -> opAssoc (NEL.head assocGroup) == Infix && sum (fmap opUsages assocGroup) > 1) =<< assocGrouped
    in
      if null (nonAssoc ++ mixedAssoc)
        then internalError "matchOperators: cannot reorder operators"
        else
          map
            (\grp ->
              mkPositionedError chainOpSpans grp
                (MixedAssociativityError (fmap (\name -> (eraseOpName <$> name, opAssoc name)) grp)))
            mixedAssoc
          ++ map
            (\grp ->
              mkPositionedError chainOpSpans grp
                (NonAssociativeError (fmap (fmap eraseOpName) grp)))
            nonAssoc

  mkPositionedError
    :: M.Map (Qualified (OpName nameType)) (NEL.NonEmpty SourceSpan)
    -> NEL.NonEmpty (Qualified (OpName nameType))
    -> SimpleErrorMessage
    -> ErrorMessage
  mkPositionedError chainOpSpans grp =
    ErrorMessage
      [PositionedError (fromJust . flip M.lookup chainOpSpans =<< grp)]

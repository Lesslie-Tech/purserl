module TestOperatorChain (spec) where

import Prelude

import Data.Either (isLeft)
import Data.Text qualified as T
import Language.PureScript.AST (Associativity(..), SourceSpan, nullSourceSpan)
import Language.PureScript.Names (ModuleName(..), OpName(..), OpNameType(..), Qualified(..), QualifiedBy(..))
import Language.PureScript.Sugar.Operators.Common (matchOperatorsWith, token)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Gen, choose, elements, forAll, listOf1, property, vectorOf, (===))
import Text.Parsec qualified as P
import Text.Parsec.Expr qualified as PE

-- A stand-in for Expr: leaves, operators, backtick operators and binary applications.
data T = Leaf Int | Op (Qualified (OpName 'ValueOpName)) | Tick Int | Bin T T T
  deriving (Eq, Show)

opName :: Int -> Qualified (OpName 'ValueOpName)
opName i = Qualified (ByModuleName (ModuleName (T.pack "M"))) (OpName (T.pack ("o" ++ show i)))

matchT :: Bool -> [[(Qualified (OpName 'ValueOpName), Associativity)]] -> T -> Either () T
matchT climbFirst table = either (const (Left ())) Right . matchOperatorsWith climbFirst isBin extract fromOp fromTick reapply modTable table
  where
  isBin Bin{} = True
  isBin _ = False
  extract (Bin op l r) = Just (op, l, r)
  extract _ = Nothing
  fromOp :: T -> Maybe (SourceSpan, Qualified (OpName 'ValueOpName))
  fromOp (Op q) = Just (nullSourceSpan, q)
  fromOp _ = Nothing
  fromTick (Tick n) = Just (Bin (Tick n))
  fromTick _ = Nothing
  reapply _ q = Bin (Op q)
  modTable t = [PE.Infix (P.try (Bin <$> parseTick)) PE.AssocLeft] : t
  parseTick = token (\case Right (Tick n) -> Just (Tick n); _ -> Nothing)

spec :: Spec
spec = describe "operator chains" $ modifyMaxSuccess (const 20000) $
  it "precedence climbing agrees with the parser on random chains" $
    property $ forAll genCase $ \(table, tree) ->
      matchT True table tree === matchT False table tree

genCase :: Gen ([[(Qualified (OpName 'ValueOpName), Associativity)]], T)
genCase = do
  levels <- choose (1, 4)
  sizes <- vectorOf levels (choose (1, 3))
  assocs <- mapM (\n -> vectorOf n (elements [Infixl, Infixr, Infix])) sizes
  let names = zipWith (\i a -> (opName i, a)) [0 ..] (concat assocs)
      table = splitPlaces sizes names
  ops <- listOf1 (genOp (length names))
  leaves <- vectorOf (length ops + 1) (Leaf <$> choose (0, 99))
  pure (table, chain leaves ops)
  where
  genOp n = do
    tick <- choose (0, 4 :: Int)
    if tick == 0 then Tick <$> choose (0, 9) else Op . opName <$> choose (0, n - 1)
  chain (l : ls) (o : os) = Bin o l (chain ls os)
  chain [l] [] = l
  chain _ _ = error "chain"
  splitPlaces [] _ = []
  splitPlaces (n : ns) xs = let (a, b) = splitAt n xs in a : splitPlaces ns b

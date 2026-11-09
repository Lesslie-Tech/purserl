{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Data types for the intermediate simplified-Erlang AST
--
module Language.PureScript.Erl.CodeGen.AST where

import Prelude.Compat

import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad ((>=>))
import Control.Monad.Identity
import Data.Bifunctor (bimap, second)

import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
import Debug.Trace (traceM, trace)
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

-- |
-- Data type for simplified Erlang expressions
--
data Erl
  -- |
  -- A numeric literal
  --
  = ENumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | EStringLiteral PSString
  -- |
  -- A char literal
  --
  | ECharLiteral Char
  -- |
  -- An atom literal (possibly qualified a:b)
  --
  | EAtomLiteral Atom
  -- |
  -- A unary operator application
  --
  | EUnary UnaryOperator Erl
  -- |
  -- A binary operator application
  --
  | EBinary BinaryOperator Erl Erl
  -- |
  -- Top-level function definition (over-simplified)
  --
  | EFunctionDef (Maybe EType) (Maybe SourceSpan) Atom [Text] Erl
  -- TODO not really a separate form. and misused
  | EBind Erl Erl
  -- |
  -- A variable
  --
  | EVar Text
  -- |
  -- A function reference f/1
  --
  | EFunRef Atom Int
  -- |
  -- A fun definition
  --
  | EFunFull (Maybe Text) [(EFunBinder, Erl)]
  -- |
  -- Function application
  --
  | EApp AppAnnotation Erl [Erl]
  -- |
  -- Block
  --
  | EBlock [Erl] -- Array should be nonempty
  -- |
  -- An effectful variable bind, separate from EBlock to keep track of which function calls are effectful
  --
  | EAndThen Erl Erl
  -- |
  -- One line in an EBlock, separate from EBlock to avoid having to flatten blocks
  --
  | ELet Erl Erl

  -- |
  -- Tuple literal {a, 1, "C"}
  --
  | ETupleLiteral [Erl]

  | EComment Text

  | EMapLiteral [(Atom, Erl)]

  | EArrayLiteral [Erl]

  | EMapPattern [(Atom, Erl)]

  | EMapUpdate Erl [(Atom,Erl)]

  | EListLiteral [Erl]

  | EListCons [Erl] Erl

  | ECaseOf Erl [(EBinder, Erl)]
  -- |
  -- Attribute including raw text between the parens
  --
  | EAttribute PSString PSString
  -- Spec attribute
  | ESpec Atom EType
  | EType Atom [Text] EType
  -- [drathier]: Try
  | ETryAnyAny Erl Erl
  -- [drathier]: raw erlang code
  | ERawErlangSource T.Text [(Atom, Erl)]
  -- | A `maybe ... end` block: each step is `Pattern ?= Expr` or, with no pattern,
  -- a plain expression, followed by the final expression. A pattern that
  -- doesn't match ends the block with the value that failed to match.
  | EMaybe MaybeKind [(Maybe Erl, Erl)] Erl

  deriving (Show, Eq)

-- | What the value that ends a `maybe` block early is.
data MaybeKind
  = -- | A chain of binds: the steps match the success case, so the value that
    -- ends the block is a failure and the block is what a failing bind returns.
    -- Such a block that is matched by a bind can become steps of that chain.
    ExitsOnFailure
  | -- | An alt chain: the steps match the failure case, so the value that ends
    -- the block is the first success. It still has to be matched by what uses it.
    ExitsOnSuccess
  deriving (Show, Eq)

-- | [drathier]: I think this is annotating if a particular App is applying something compiler-generated like a type class dict (SyntheticApp), or is just a normal function call (RegularApp).
data AppAnnotation
  = RegularApp
  | SyntheticApp
  deriving (Show, Eq)

-- | EVarBind as defined before drathier expanded it to allow more than simple var lhs's
pattern EVarBind :: Text -> Erl -> Erl
pattern EVarBind name e = EBind (EVar name) e

-- | Aliases of EApp with specific args exact-called out and others exact-called after, to width-align pattern matches
-- pattern EApp1 :: Maybe Text -> Erl -> Erl
-- pattern EApp1 appKind modu f instModu instPrefix = EApp

-- pattern EApp3 :: _

pattern EApp1 modu1 f instModu instF a b <- EApp _ (EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a]) [b]
pattern EApp2 modu1 f instModu instF a b <- EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a,b]
-- pattern EApp1 modu1 f instModu instF a b <- EApp _ (EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) []]) [a, b]
pattern EApp3 modu1 f instModu instF a b <- EApp _ (EAtomLiteral (Atom (Just modu1) f)) [EApp _ (EAtomLiteral (Atom (Just instModu) instF)) [], a, b]

pattern ETrue = EAtomLiteral (Atom Nothing "true")
pattern EFalse = EAtomLiteral (Atom Nothing "false")

pattern EIntLit a = ENumericLiteral (Left a)
pattern ENumLit a = ENumericLiteral (Right a)

pattern ENegate a = EUnary Negate a

-- | Simple 0-arity version of EFun1
pattern EFun0 :: Maybe Text -> Erl -> Erl
pattern EFun0 name e = EFunFull name [(EFunBinder [], e)]

-- | Simple fun definition fun f(X) -> e end (arity 1 with single head with simple variable pattern, name optional)
pattern EFun1 :: Maybe Text -> Text -> Erl -> Erl
pattern EFun1 name var e = EFunFull name [(EFunBinder [EVar var], e)]

extractVars :: [Erl] -> Maybe [Text]
extractVars = traverse var
  where var (EVar x) = Just x
        var _ = Nothing

-- | Simple arity-N version of EFun1
pattern EFunN :: Maybe Text -> [Text] -> Erl -> Erl
pattern EFunN name vars e <- EFunFull name [(EFunBinder (extractVars -> Just vars), e)] where
  EFunN name vars e = EFunFull name [(EFunBinder (map EVar vars), e)]


curriedLambda :: Erl -> [Text] -> Erl
curriedLambda = foldr (EFun1 Nothing)

curriedApp :: [Erl] -> Erl -> Erl
curriedApp = flip (foldl (\fn a -> EApp RegularApp fn [a]))



newtype EFunBinder
 = EFunBinder [Erl]
 -- [drathier]: using ECaseOf instead of EFunBinder _ (Just _)
 -- = EFunBinder [Erl] (Maybe Guard)

   deriving (Show, Eq)

newtype EBinder
  = EBinder Erl -- TODO split out literals?
  -- [drathier]: using ECaseOf instead of EGuardedBinder
  -- | EGuardedBinder Erl Guard

  deriving (Show, Eq)

newtype Guard
  = Guard Erl
  deriving (Show, Eq)

-- | Possibly qualified atom
-- TODO : This is not really an atom, each part is an atom.
data Atom
  = Atom (Maybe Text) Text
  | AtomPS (Maybe Text) PSString
  deriving (Show, Eq, Ord)
-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide

  -- [drathier]: Purescript euclidian integer division and remainder is not the same as erlang division, so we can't inline it as `div` or `rem` here. See data_euclideanRing@foreign:intDiv and https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf for the various kinds of division.
  -- -- |
  -- -- Numeric division (integer)
  -- --
  -- -- | IDivide
  -- -- |
  -- -- Integer Remainder
  -- --
  -- | IRemainder
  -- |
  -- Float Remainder
  --
  | FRemainder
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight

  -- |
  -- List concatenation (++)
  --
  | ListConcat
  -- |
  -- Binary concatenation (<<A/binary,B/binary>>)
  --
  | BinaryConcat
  -- |
  -- Array concatenation (data_semigroup@foreign:concatArray(A,B))
  --
  | ArrayConcat
  -- |
  -- List subtraction (--)
  --
  | ListSubtract

  deriving (Show, Eq)

-- Simplified Erlang types
data EType
  = TAny

  | TNone
  | TPid
  | TPort
  | TReference
  | TNil
  | TAtom (Maybe Atom)
  -- bitstring
  | TFloat
  -- | TFunAny
  | TVar Text
  | TFun [EType] EType
  | TInteger -- no ranges
  | TList EType -- no improper lists
  -- maps
  | TMap (Maybe [(EType, EType)])
  | TTuple [EType]
  | TUnion [EType]
  | TRemote Text Text [EType]
  | TAlias Atom [EType]
  deriving (Show, Eq)

-- | Pointer equality: a false negative only costs a rebuild.
same :: a -> a -> Bool
same a b = isTrue# (reallyUnsafePtrEquality# a b)
{-# INLINE same #-}

-- | Maps a list, returning the list itself when no element changed.
mapShared :: (a -> a) -> [a] -> [a]
mapShared _ [] = []
mapShared g l@(x : xs) =
  let !x' = g x
      !xs' = mapShared g xs
  in if same x x' && same xs xs' then l else x' : xs'

-- | Applies 'f' bottom-up and rebuilds a node only when a child changed.
everywhereOnErl :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErl f = go
  where
  go :: Erl -> Erl
  go erl =
    case erl of
      EUnary op e -> let !e' = go e in f (if same e e' then erl else EUnary op e')
      EBinary op e1 e2 ->
        let !e1' = go e1; !e2' = go e2
        in f (if same e1 e1' && same e2 e2' then erl else EBinary op e1' e2')
      EFunctionDef t ssann a vs e -> let !e' = go e in f (if same e e' then erl else EFunctionDef t ssann a vs e')
      EBind x e ->
        let !x' = go x; !e' = go e
        in f (if same x x' && same e e' then erl else EBind x' e')
      EFunFull fname args -> let !a' = binds args in f (if same args a' then erl else EFunFull fname a')
      EApp meta e es ->
        let !e' = go e; !es' = mapShared go es
        in f (if same e e' && same es es' then erl else EApp meta e' es')
      EBlock es -> let !es' = mapShared go es in f (if same es es' then erl else EBlock es')
      ETupleLiteral es -> let !es' = mapShared go es in f (if same es es' then erl else ETupleLiteral es')
      EArrayLiteral es -> let !es' = mapShared go es in f (if same es es' then erl else EArrayLiteral es')
      EMapLiteral bs -> let !b' = binds bs in f (if same bs b' then erl else EMapLiteral b')
      EMapPattern bs -> let !b' = binds bs in f (if same bs b' then erl else EMapPattern b')
      EMapUpdate e bs ->
        let !e' = go e; !b' = binds bs
        in f (if same e e' && same bs b' then erl else EMapUpdate e' b')
      ECaseOf e bs ->
        let !e' = go e; !b' = binds bs
        in f (if same e e' && same bs b' then erl else ECaseOf e' b')
      EListLiteral es -> let !es' = mapShared go es in f (if same es es' then erl else EListLiteral es')
      EListCons es e ->
        let !es' = mapShared go es; !e' = go e
        in f (if same es es' && same e e' then erl else EListCons es' e')
      ETryAnyAny e1 e2 ->
        let !e1' = go e1; !e2' = go e2
        in f (if same e1 e1' && same e2 e2' then erl else ETryAnyAny e1' e2')
      EAndThen a b ->
        let !a' = go a; !b' = go b
        in f (if same a a' && same b b' then erl else EAndThen a' b')
      ELet a b ->
        let !a' = go a; !b' = go b
        in f (if same a a' && same b b' then erl else ELet a' b')
      ERawErlangSource fmt bs -> let !b' = binds bs in f (if same bs b' then erl else ERawErlangSource fmt b')
      EMaybe kind steps final ->
        let !st' = mapShared step steps; !fi' = go final
        in f (if same steps st' && same final fi' then erl else EMaybe kind st' fi')
      _ -> f erl

  binds :: [(x, Erl)] -> [(x, Erl)]
  binds = mapShared (\p@(x, e) -> let !e' = go e in if same e e' then p else (x, e'))

  step p@(pat, e) =
    let !pat' = case pat of
          Nothing -> pat
          Just q -> let !q' = go q in if same q q' then pat else Just q'
        !e' = go e
    in if same pat pat' && same e e' then p else (pat', e')

everywhereOnErlTopDown :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErlTopDown f = runIdentity . everywhereOnErlTopDownM (Identity . f)

{-# INLINABLE everywhereOnErlTopDownM #-}
everywhereOnErlTopDownM :: forall m. (Monad m) => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownM f = f >=> go
  where
  f' = f >=> go

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef t ssann a ss e) = EFunctionDef t ssann a ss <$> f' e
  go (EBind x e) = EBind <$> f' x <*> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp meta e es) = EApp meta <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EArrayLiteral es) = EArrayLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EListLiteral es) = EListLiteral <$> traverse f' es
  go (EListCons es e) = EListCons <$> traverse f' es <*> f' e
  go (ETryAnyAny e1 e2) = ETryAnyAny <$> f' e1 <*> f' e2
  go (EAndThen a b) = EAndThen <$> f' a <*> f' b
  go (ELet a b) = ELet <$> f' a <*> f' b
  go (ERawErlangSource fmt binds) = ERawErlangSource fmt <$> fargs binds
  go (EMaybe kind steps final) = EMaybe kind <$> traverse (\(p, e) -> (,) <$> traverse f' p <*> f' e) steps <*> f' final

  go other = f other

everything :: forall r. (r -> r -> r) -> (Erl -> r) -> Erl -> r
everything (<>.) f = go
  where
  go :: Erl -> r
  go e0@(EUnary _ e) = f e0 <>. go e
  go e0@(EBinary _ e1 e2) = f e0 <>. go e1 <>. go e2
  go e0@(EFunctionDef _ _ _ _ e) = f e0 <>. go e
  go e0@(EBind x e) = f e0 <>. go x <>. go e
  go e0@(EFunFull _ args) = foldl (<>.) (f e0) (map (go . snd) args)
  go e0@(EApp _ e es) = foldl (<>.) (f e0 <>. go e) (map go es)
  go e0@(EBlock es) = foldl (<>.) (f e0) (map go es)
  go e0@(ETupleLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EArrayLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EMapLiteral binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapPattern binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapUpdate e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(ECaseOf e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(EListLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EListCons es e) = foldl (<>.) (f e0) (map go $ es <> [e])
  go e0@(ETryAnyAny e1 e2) = f e0 <>. go e1 <>. go e2
  go e0@(EAndThen a b) = f e0 <>. go a <>. go b
  go e0@(ELet a b) = f e0 <>. go a <>. go b
  go e0@(ERawErlangSource _ binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMaybe _ steps final) = foldl (<>.) (f e0) (concatMap (\(p, e) -> maybe [] (pure . go) p <> [go e]) steps <> [go final])
  -- go other = f other

  go e0@(EVar {}) = f e0
  go e0@(EAtomLiteral {}) = f e0
  go e0@(ENumericLiteral {}) = f e0
  go e0@(EStringLiteral {}) = f e0
  go e0@(ECharLiteral {}) = f e0
  go e0@(EFunRef {}) = f e0
  go e0@(EComment {}) = f e0
  go e0@(EAttribute {}) = f e0
  go e0@(ESpec {}) = f e0
  go e0@(EType {}) = f e0


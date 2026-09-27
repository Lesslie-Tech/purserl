{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Erl.Pretty (
  prettyPrintErl
) where

import Prelude ()
import Prelude.Compat

import Language.PureScript.Crash
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common

import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.Maybe (fromMaybe)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intersperse)

import Language.PureScript.PSString (PSString, decodeString)
import Language.PureScript.PSString qualified as PS

-- |
-- A direct recursive-descent pretty printer for the Erlang AST.
--
-- This deliberately avoids the generic PatternArrows/OperatorTable combinator
-- machinery this printer used to share with the (since-removed) JS backend's
-- printer: with only one Erlang target and one string representation left,
-- that indirection bought nothing but overhead and profiler noise.
-- Parenthesization instead follows an explicit
-- precedence rank per node (see 'rank'/'binOpRank'), hand-verified against
-- the previous implementation's output on an exhaustive cross-product of
-- operator nestings (including its "same specific operator chains without
-- parens, anything else falls back to a fully parenthesized render" quirk).
prettyPrintErl :: (String -> String) -> [Erl] -> Text
prettyPrintErl _transformFilename es = TL.toStrict $ B.toLazyText $ statements 0 es

statements :: Int -> [Erl] -> Builder
statements ind es = mintercalate "\n" [ sp ind <> render ind e <> ".\n" | e <- es ]

sp :: Int -> Builder
sp n = B.fromText (T.replicate n " ")

mintercalate :: Monoid m => m -> [m] -> m
mintercalate x xs = mconcat (intersperse x xs)

commaSep :: Int -> [Erl] -> Builder
commaSep ind = mintercalate ", " . map (render ind)

fromChar :: Char -> Word16
fromChar = toEnum . fromEnum

escapeQuotedVar :: Text -> Text
escapeQuotedVar x =
  case "\"" `T.isInfixOf` x of
    False -> x
    True -> "'" <> x <> "'"

prettyPrintStringErl :: PSString -> Text
prettyPrintStringErl s = "~\"" <> utf8BinaryContent s <> "\""

-- | Peel a right-leaning chain of EAndThen\/ELet into a flat statement list,
-- exactly as the two constructors are meant to be read (they're both just
-- "this, then this").
flattenSeq :: Erl -> Erl -> [Erl]
flattenSeq arg1 arg2 = go (EAndThen arg1 arg2) []
  where
    go (EAndThen a b) acc = go b (a : acc)
    go (ELet a b) acc = go b (a : acc)
    go e acc = reverse (e : acc)

-- | The body of a `begin ... end`\/case-branch\/function clause: each
-- statement on its own line, all sharing one indent, joined by ",\n".
blockBody :: Int -> [Erl] -> Builder
blockBody ind es = sp ind <> mintercalate (",\n" <> sp ind) (map (render ind) es)

-- | Render `e` as a function\/case\/fun clause body, handling the implicit
-- flattening of EAndThen\/ELet chains into a plain statement list.
clauseBody :: Int -> Erl -> Builder
clauseBody ind (EAndThen a b) = blockBody ind (flattenSeq a b)
clauseBody ind (ELet a b) = blockBody ind (flattenSeq a b)
clauseBody ind (EBlock es) = blockBody ind es
clauseBody ind e = sp ind <> render ind e

--------------------------------------------------------------------------------
-- Operator precedence / parenthesization
--
-- Lower rank binds tighter. A node used as an operand renders bare (no extra
-- parens beyond whatever its own render produces) when its rank is strictly
-- tighter than its parent's, or when it's the exact same operator used in
-- the one chainable slot (left operand for a binary op; the sole operand for
-- a unary op; the callee for an application). Anything else falls back to a
-- full render wrapped in one extra pair of parens -- this includes different
-- operators that happen to share a rank, which is why e.g. `(a + b) - c`
-- still gets explicit parens around `a + b` despite `+`/`-` "looking" like
-- the same precedence level.
--------------------------------------------------------------------------------

rank :: Erl -> Int
rank (EApp {}) = 1
rank (EUnary {}) = 2
rank (EBinary op _ _) = binOpRank op
rank _ = 0

binOpRank :: BinaryOperator -> Int
binOpRank op = case op of
  FDivide -> 3
  Multiply -> 3
  BitwiseAnd -> 3
  And -> 3

  Add -> 4
  Subtract -> 4
  BitwiseOr -> 4
  BitwiseXor -> 4
  ShiftLeft -> 4
  ShiftRight -> 4
  Or -> 4
  XOr -> 4

  ListConcat -> 5
  ListSubtract -> 5

  EqualTo -> 6
  NotEqualTo -> 6
  IdenticalTo -> 6
  NotIdenticalTo -> 6
  LessThan -> 6
  LessThanOrEqualTo -> 6
  GreaterThan -> 6
  GreaterThanOrEqualTo -> 6

  AndAlso -> 7

  OrElse -> 8

  BinaryConcat -> 9
  ArrayConcat -> 9
  FRemainder -> 9

unaryText :: UnaryOperator -> Builder
unaryText Negate = "-"
unaryText Not = "not"
unaryText BitwiseNot = "bnot"
unaryText Positive = "+"

binOpText :: BinaryOperator -> Builder
binOpText op = case op of
  Add -> "+"
  Subtract -> "-"
  Multiply -> "*"
  FDivide -> "/"
  EqualTo -> "=="
  NotEqualTo -> "/="
  IdenticalTo -> "=:="
  NotIdenticalTo -> "=/="
  LessThan -> "<"
  LessThanOrEqualTo -> "=<"
  GreaterThan -> ">"
  GreaterThanOrEqualTo -> ">="
  And -> "and"
  Or -> "or"
  AndAlso -> "andalso"
  OrElse -> "orelse"
  XOr -> "xor"
  BitwiseAnd -> "band"
  BitwiseOr -> "bor"
  BitwiseXor -> "bxor"
  ShiftLeft -> "bsl"
  ShiftRight -> "bsr"
  ListConcat -> "++"
  ListSubtract -> "--"
  -- BinaryConcat/ArrayConcat/FRemainder have their own templates in
  -- `render`'s EBinary case and never reach this function.
  BinaryConcat -> internalErrorText
  ArrayConcat -> internalErrorText
  FRemainder -> internalErrorText
  where
    internalErrorText = error "binOpText: operator has a bespoke template"

-- | Render `operand` in the sole chainable slot of unary operator `op`.
renderUnaryOperand :: Int -> UnaryOperator -> Erl -> Builder
renderUnaryOperand ind op operand = case operand of
  EUnary op' inner | op' == op -> renderUnary ind op' inner
  _
    | rank operand < 2 -> render ind operand
    | otherwise -> "(" <> render ind operand <> ")"

renderUnary :: Int -> UnaryOperator -> Erl -> Builder
renderUnary ind op operand = unaryText op <> renderUnaryOperand ind op operand

-- | Render `l` in the left (chainable) operand slot of binary operator `op`.
renderBinLeft :: Int -> BinaryOperator -> Erl -> Builder
renderBinLeft ind op l = case l of
  EBinary op' l2 r2 | op' == op -> renderBinary ind op' l2 r2
  _
    | rank l < binOpRank op -> render ind l
    | otherwise -> "(" <> render ind l <> ")"

-- | Render `r` in the right (never-chainable) operand slot of binary
-- operator `op`.
renderBinRight :: Int -> BinaryOperator -> Erl -> Builder
renderBinRight ind op r
  | rank r < binOpRank op = render ind r
  | otherwise = "(" <> render ind r <> ")"

renderBinary :: Int -> BinaryOperator -> Erl -> Erl -> Builder
renderBinary ind op l r =
  let lft = renderBinLeft ind op l
      rgt = renderBinRight ind op r
  in case op of
       BinaryConcat -> "<< <<" <> lft <> "/binary>>/binary, <<" <> rgt <> "/binary>> /binary>>"
       ArrayConcat -> "(data_semigroup@foreign:concatArray(" <> lft <> "," <> rgt <> "))"
       FRemainder -> "(math@foreign:remainder(" <> lft <> "," <> rgt <> "))"
       _ -> "(" <> lft <> " " <> binOpText op <> " " <> rgt <> ")"

-- | Render `val` in the callee (chainable) slot of a function application.
--
-- The application operator is listed twice in the original operator table
-- (once at the very front, once again after `orelse`), and since later table
-- entries are tried first, every EApp is actually dispatched through the
-- *second* copy. That copy's "fits without an extra paren" fallback range
-- covers everything in front of it in the table, i.e. every real operator
-- rank (1-8) except the trailing BinaryConcat\/ArrayConcat\/FRemainder group
-- (rank 9) -- not just application/literals as the first copy alone would
-- suggest. Hand-verified against the previous implementation's output.
renderCallee :: Int -> Erl -> Builder
renderCallee ind val
  | rank val < 9 = render ind val
  | otherwise = "(" <> render ind val <> ")"

renderApp :: Int -> Erl -> [Erl] -> Builder
renderApp ind val args = "(" <> renderCallee ind val <> "(" <> commaSep ind args <> "))"

--------------------------------------------------------------------------------
-- Main render
--------------------------------------------------------------------------------

render :: Int -> Erl -> Builder
render ind = go
  where
  go :: Erl -> Builder
  go (ENumericLiteral n) = B.fromText $ T.pack $ either show show n
  go (EStringLiteral s) = B.fromText $ prettyPrintStringErl s
  go (ECharLiteral c) = "$" <> B.fromText (encodeChar (fromChar c))
  go (EAtomLiteral a) = B.fromText $ runAtom a
  go (ETupleLiteral es) = "{ " <> commaSep ind es <> " }"

  go (EBind x e) = go x <> " = " <> go e

  go (EFunctionDef _t _ss x xs e) =
    B.fromText (runAtom x) <> "(" <> mintercalate "," (map (B.fromText . escapeQuotedVar) xs) <> ") -> " <> body
    where
      body = case e of
        EAndThen a b -> blockWithSurroundingNewlines (flattenSeq a b)
        ELet a b -> blockWithSurroundingNewlines (flattenSeq a b)
        EBlock es -> blockWithSurroundingNewlines es
        _ -> "\n" <> sp (ind + 2) <> render (ind + 2) e
      blockWithSurroundingNewlines es = "\n" <> blockBody (ind + 2) es <> "\n"

  go (EVar x) = B.fromText (escapeQuotedVar x)

  go (EMapLiteral elts) =
    let ci = sp (ind + 2)
    in "#{" <> mintercalate (",\n" <> ci) (map (\(k, e) -> B.fromText (runAtom k) <> "=>" <> go e) elts) <> "}"

  go (EMapPattern elts) =
    "#{" <> mintercalate ", " (map (\(k, e) -> B.fromText (runAtom k) <> ":=" <> go e) elts) <> "}"

  go (EMapUpdate e elts) =
    "(" <> go e <> ")" <> "#{" <> mintercalate ", " (map (\(k, ee) -> B.fromText (runAtom k) <> "=>" <> go ee) elts) <> "}"

  go (EArrayLiteral es) = "(array:from_list([" <> commaSep ind es <> "]))"

  go (EListLiteral es) = "[" <> commaSep ind es <> "]"

  go (EListCons es e) = "[" <> commaSep ind es <> " | " <> go e <> "]"

  go (ETryAnyAny e1 e2) =
    let base = ind + 2
    in "\n" <> sp base <> "try\n"
       <> sp (base + 2) <> render (base + 2) e1
       <> "\n" <> sp base <> "catch\n"
       <> sp (base + 2) <> "_:_ ->\n"
       <> sp (base + 4) <> render (base + 4) e2
       <> "\n" <> sp base <> "end"

  go (EAndThen a b) = renderBlock (flattenSeq a b)
  go (ELet a b) = renderBlock (flattenSeq a b)
  go (EBlock es) = renderBlock es

  go (ERawErlangSource fmt binds) =
    let (p1 : parts) = T.splitOn "$" fmt
        piece p =
          let needle = T.takeWhile (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_') p
          in case lookup (AtomPS Nothing (PS.fromText needle)) binds of
               Nothing -> error (show ("[drathier]: CodeGen.erlang got key", "$", needle, "not found in", map fst binds))
               Just v -> render ind v <> B.fromText (T.drop (T.length needle) p)
    in mconcat (B.fromText p1 : map piece parts)

  go (EComment s) = "\n" <> sp ind <> "% " <> B.fromText s <> "\n"

  go (EFunRef x n) = "fun " <> B.fromText (runAtom x) <> "/" <> B.fromText (T.pack (show n))

  -- EFun1/EFun0 are pattern synonyms over EFunFull; GHC tries this
  -- single-clause single-var-pattern case before the general EFunFull case
  -- below, so e.g. a zero-arity fun (EFun0, an *empty* binder list) does
  -- NOT match here and falls through to the multi-clause rendering.
  go (EFun1 name x e) =
    "fun " <> B.fromText (fromMaybe "" name) <> "(" <> B.fromText (escapeQuotedVar x) <> ") -> \n"
    <> clauseBody (ind + 2) e
    <> "\n" <> sp ind <> "end"

  go (EFunFull name binders) =
    "fun\n" <> mintercalate ";\n" (map prettyPrintBinder binders) <> "\n" <> sp ind <> "end"
    where
      prettyPrintBinder (EFunBinder binds, e') =
        sp (ind + 2) <> B.fromText (fromMaybe "" name) <> "(" <> commaSep (ind + 2) binds <> ") -> \n"
        <> clauseBody (ind + 2) e'

  go (ECaseOf e binders) =
    "case " <> go e <> " of\n" <> mintercalate ";\n" (map prettyPrintBinder binders) <> "\n" <> sp ind <> "end"
    where
      prettyPrintBinder (EBinder eb, e') =
        sp (ind + 2) <> "(" <> render (ind + 2) eb <> ") ->\n"
        <> sp (ind + 4) <> render (ind + 4) e'

  go (EAttribute name text) = case (decodeString name, decodeString text) of
    (Just name', Just text') -> "-" <> B.fromText name' <> "(" <> B.fromText text' <> ")"
    _ -> internalError "Did not expect non UTF8 safe attribute text"

  go (ESpec name ty) = B.fromText $ printFunTy name (Just ty)
  go (EType name args ty) = B.fromText $ printTypeDef name args (Just ty)

  go (EUnary op operand) = renderUnary ind op operand
  go (EBinary op l r) = renderBinary ind op l r
  go (EApp _ val args) = renderApp ind val args

  renderBlock es = "begin\n" <> blockBody (ind + 2) es <> "\n" <> sp ind <> "end"

  printFunTy name (Just (TFun ts ty)) =
    "%-spec " <> runAtom name <> "(" <> T.intercalate "," (printTy <$> ts) <> ") -> " <> printTy ty
  printFunTy _ _ = internalError "Can't print spec for function with unknown arg length"

  printTypeDef name args (Just ty) =
    "%-type " <> runAtom name <> "(" <> T.intercalate "," args <> ") :: " <> printTy ty
  printTypeDef _ _ _ = internalError "Empty typedef"

  printTy TAny = "any()"
  printTy TNone = "none()"
  printTy TPid = "pid()"
  printTy TPort = "port()"
  printTy TReference = "reference()"
  printTy TNil = "[]"
  printTy TInteger = "integer()"
  printTy (TVar var) = var
  printTy (TFun ts tf) = "fun((" <> T.intercalate "," (printTy <$> ts) <> ") -> " <> printTy tf <> ")"
  printTy TFloat = "float()"
  printTy (TAlias alias ts) = runAtom alias <> "(" <> T.intercalate "," (printTy <$> ts) <> ")"
  printTy (TAtom Nothing) = "atom()"
  printTy (TAtom (Just a)) = runAtom a
  printTy (TList ts) = "list(" <> printTy ts <> ")"
  printTy (TMap Nothing) = "map()"
  printTy (TMap (Just ts)) = "#{" <> T.intercalate "," ((\(t1, t2) -> printTy t1 <> " => " <> printTy t2) <$> ts) <> "}"
  printTy (TTuple ts) = "{" <> T.intercalate "," (printTy <$> ts) <> "}"
  printTy (TUnion ts) = T.intercalate " | " $ printTy <$> ts
  printTy (TRemote tymod tyname tys) = tymod <> ":" <> tyname <> "(" <> T.intercalate "," (printTy <$> tys) <> ")"

-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( PrettyPrintType(..)
  , PrettyPrintConstraint
  , convertPrettyPrintType
  , typeAsBox
  , typeDiffAsBox
  , prettyPrintType
  , prettyPrintTypeWithUnicode
  , prettyPrintSuggestedType
  , typeAtomAsBox
  , prettyPrintTypeAtom
  , prettyPrintLabel
  , prettyPrintObjectKey
  ) where

import Prelude hiding ((<>))

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Names (OpName(..), OpNameType(..), ProperName(..), ProperNameType(..), Qualified, coerceProperName, disqualify, showQualified)
import Language.PureScript.Pretty.Common (before, objectKeyRequiresQuoting)
import Language.PureScript.Types (Constraint(..), pattern REmptyKinded, RowListItem(..), Type(..), TypeVarVisibility(..), WildcardData(..), eqType, rowToSortedList, typeVarVisibilityPrefix)
import Language.PureScript.PSString (PSString, prettyPrintString, decodeString)
import Language.PureScript.Label (Label(..))

import Text.PrettyPrint.Boxes (Box(..), hcat, hsep, left, moveRight, nullBox, render, text, top, vcat, (<>))

data PrettyPrintType
  = PPTUnknown Int
  | PPTypeVar Text (Maybe Text)
  | PPTypeLevelString PSString
  | PPTypeLevelInt Integer
  | PPTypeWildcard (Maybe Text)
  | PPTypeConstructor (Qualified (ProperName 'TypeName))
  | PPTypeOp (Qualified (OpName 'TypeOpName))
  | PPSkolem Text Int
  | PPTypeApp PrettyPrintType PrettyPrintType
  | PPKindArg PrettyPrintType
  | PPConstrainedType PrettyPrintConstraint PrettyPrintType
  | PPKindedType PrettyPrintType PrettyPrintType
  | PPBinaryNoParensType PrettyPrintType PrettyPrintType PrettyPrintType
  | PPParensInType PrettyPrintType
  | PPForAll [(TypeVarVisibility, Text, Maybe PrettyPrintType)] PrettyPrintType
  | PPFunction PrettyPrintType PrettyPrintType
  | PPRecord [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPRow [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPTruncated

type PrettyPrintConstraint = (Qualified (ProperName 'ClassName), [PrettyPrintType], [PrettyPrintType])

convertPrettyPrintType :: Int -> Type a -> PrettyPrintType
convertPrettyPrintType = go
  where
  go _ (TUnknown _ n) = PPTUnknown n
  go _ (TypeVar _ t) = PPTypeVar t Nothing
  go _ (TypeLevelString _ s) = PPTypeLevelString s
  go _ (TypeLevelInt _ n) = PPTypeLevelInt n
  go _ (TypeWildcard _ (HoleWildcard n)) = PPTypeWildcard (Just n)
  go _ (TypeWildcard _ _) = PPTypeWildcard Nothing
  go _ (TypeConstructor _ c) = PPTypeConstructor c
  go _ (TypeOp _ o) = PPTypeOp o
  go _ (Skolem _ t _ n _) = PPSkolem t n
  go _ (REmpty _) = PPRow [] Nothing
  -- Guard the remaining "complex" type atoms on the current depth value. The
  -- prior  constructors can all be printed simply so it's not really helpful to
  -- truncate them.
  go d _ | d < 0 = PPTruncated
  go d (ConstrainedType _ (Constraint _ cls kargs args _) ty) = PPConstrainedType (cls, go (d-1) <$> kargs, go (d-1) <$> args) (go d ty)
  go d (KindedType _ ty k) = PPKindedType (go (d-1) ty) (go (d-1) k)
  go d (BinaryNoParensType _ ty1 ty2 ty3) = PPBinaryNoParensType (go (d-1) ty1) (go (d-1) ty2) (go (d-1) ty3)
  go d (ParensInType _ ty) = PPParensInType (go (d-1) ty)
  go d ty@RCons{} = uncurry PPRow (goRow d ty)
  go d (ForAll _ vis v mbK ty _) = goForAll d [(vis, v, fmap (go (d-1)) mbK)] ty
  go d (TypeApp _ a b) = goTypeApp d a b
  go d (KindApp _ a b) = PPTypeApp (go (d-1) a) (PPKindArg (go (d-1) b))

  goForAll d vs (ForAll _ vis v mbK ty _) = goForAll d ((vis, v, fmap (go (d-1)) mbK) : vs) ty
  goForAll d vs ty = PPForAll (reverse vs) (go (d-1) ty)

  goRow d ty =
    let (items, tail_) = rowToSortedList ty
    in ( map (\item -> (rowListLabel item, go (d-1) (rowListType item))) items
       , case tail_ of
           REmptyKinded _ _ -> Nothing
           _ -> Just (go (d-1) tail_)
       )

  goTypeApp d (TypeApp _ f a) b
    | eqType f tyFunction = PPFunction (go (d-1) a) (go (d-1) b)
    | otherwise = PPTypeApp (goTypeApp d f a) (go (d-1) b)
  goTypeApp d o ty@RCons{}
    | eqType o tyRecord = uncurry PPRecord (goRow d ty)
  goTypeApp d a b = PPTypeApp (go (d-1) a) (go (d-1) b)

-- TODO(Christoph): get rid of T.unpack s

constraintsAsBox :: TypeRenderOptions -> PrettyPrintConstraint -> Box -> Box
constraintsAsBox tro con ty =
    constraintAsBox con `before` (" " <> text doubleRightArrow <> " " <> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: PrettyPrintConstraint -> Box
constraintAsBox (pn, ks, tys) = typeAsBox' (foldl PPTypeApp (foldl (\a b -> PPTypeApp a (PPKindArg b)) (PPTypeConstructor (fmap coerceProperName pn)) ks) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> [(Label, PrettyPrintType)] -> Maybe PrettyPrintType -> Box
prettyPrintRowWith tro open close labels rest =
  case (labels, rest) of
    ([], Nothing) ->
      if troRowAsDiff tro then text [ open, ' ' ] <> text "..." <> text [ ' ', close ] else text [ open, close ]
    ([], Just _) ->
      text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
    _ ->
      vcat left $
        zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) labels [0 :: Int ..] ++
        catMaybes [ rowDiff, pure $ tailToPs rest, pure $ text [close] ]

  where
  nameAndTypeToPs :: Char -> Label -> PrettyPrintType -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : T.unpack (prettyPrintLabel name) ++ " " ++ doubleColon ++ " ") <> typeAsBox' ty

  doubleColon = if troUnicode tro then "∷" else "::"

  rowDiff = if troRowAsDiff tro then Just (text "...") else Nothing

  tailToPs :: Maybe PrettyPrintType -> Box
  tailToPs Nothing = nullBox
  tailToPs (Just other) = text "| " <> typeAsBox' other

-- |
-- Precedence rank of each "operator" shape, tightest-binding first. This
-- mirrors the old operator-table's level order and drives parenthesization:
-- an operand renders bare when it's strictly tighter than its parent, or
-- (`operand`'s `allowChain` flag) when it's the exact same shape recursing
-- into its one chainable slot (e.g. `@\@k`, `f x y`, `a -> b -> c`, nested
-- foralls/parens...); anything else falls back to a fully parenthesized
-- render. Every rank below corresponds to exactly one constructor, so rank
-- equality already implies "same shape" with no further check needed.
-- Everything not listed (the plain "atoms": vars, constructors, records,
-- rows, wildcards, etc.) is rank 0, tighter than every real operator.
rank :: PrettyPrintType -> Int
rank (PPKindArg _) = 1
rank (PPTypeApp _ _) = 2
rank (PPFunction _ _) = 3
rank (PPConstrainedType _ _) = 4
rank (PPForAll _ _) = 5
rank (PPKindedType _ _) = 6
rank (PPParensInType _) = 7
rank _ = 0

-- | Wrap a box in literal parens, matching the original's placement of the
-- closing paren on its own line when the wrapped content is multi-row.
wrapParens :: Box -> Box
wrapParens inner = (text "(" <> inner) `before` text ")"

typeAtomAsBox' :: PrettyPrintType -> Box
typeAtomAsBox' ty
  | rank ty == 0 = typeAsBox' ty
  | otherwise = wrapParens (typeAsBox' ty)

typeAtomAsBox :: Int -> Type a -> Box
typeAtomAsBox maxDepth = typeAtomAsBox' . convertPrettyPrintType maxDepth

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Int -> Type a -> String
prettyPrintTypeAtom maxDepth = render . typeAtomAsBox maxDepth

typeAsBox' :: PrettyPrintType -> Box
typeAsBox' = typeAsBoxImpl defaultOptions

typeAsBox :: Int -> Type a -> Box
typeAsBox maxDepth = typeAsBox' . convertPrettyPrintType maxDepth

typeDiffAsBox' :: PrettyPrintType -> Box
typeDiffAsBox' = typeAsBoxImpl diffOptions

typeDiffAsBox :: Int -> Type a -> Box
typeDiffAsBox maxDepth = typeDiffAsBox' . convertPrettyPrintType maxDepth

data TypeRenderOptions = TypeRenderOptions
  { troSuggesting :: Bool
  , troUnicode :: Bool
  , troRowAsDiff :: Bool
  }

suggestingOptions :: TypeRenderOptions
suggestingOptions = TypeRenderOptions True False False

defaultOptions :: TypeRenderOptions
defaultOptions = TypeRenderOptions False False False

diffOptions :: TypeRenderOptions
diffOptions = TypeRenderOptions False False True

unicodeOptions :: TypeRenderOptions
unicodeOptions = TypeRenderOptions False True False

typeAsBoxImpl :: TypeRenderOptions -> PrettyPrintType -> Box
typeAsBoxImpl = renderType

-- | Render an operand of a "rank `selfRank`" construct: bare if it's
-- strictly tighter, bare (recursing straight back into `renderType`) if
-- `allowChain` and it's the exact same shape, otherwise a fully
-- parenthesized render.
operand :: TypeRenderOptions -> Int -> Bool -> PrettyPrintType -> Box
operand tro selfRank allowChain x
  | rank x < selfRank = renderType tro x
  | allowChain && rank x == selfRank = renderType tro x
  | otherwise = wrapParens (renderType tro x)

-- If both boxes span a single line, keep them on the same line, or else
-- use the specified function to modify the second box, then combine vertically.
keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
keepSingleLinesOr f b1 b2
  | rows b1 > 1 || rows b2 > 1 = vcat left [ b1, f b2 ]
  | otherwise = hcat top [ b1, text " ", b2]

printMbKindedType :: TypeRenderOptions -> (TypeVarVisibility, Text, Maybe PrettyPrintType) -> Box
printMbKindedType tro (vis, v, mbK) = case mbK of
  Nothing -> text (T.unpack (typeVarVisibilityPrefix vis) ++ T.unpack v)
  Just k -> text ("(" ++ T.unpack (typeVarVisibilityPrefix vis) ++ T.unpack v ++ " " ++ doubleColon tro ++ " ") <> typeAsBox' k <> text ")"

doubleColon :: TypeRenderOptions -> String
doubleColon tro = if troUnicode tro then "∷" else "::"

renderType :: TypeRenderOptions -> PrettyPrintType -> Box
renderType tro = go
  where
  rightArrow = if troUnicode tro then "→" else "->"
  forall' = if troUnicode tro then "∀" else "forall"

  go :: PrettyPrintType -> Box
  go (PPTypeWildcard name) = text $ maybe "_" (('?' :) . T.unpack) name
  go (PPTypeVar var _) = text $ T.unpack var
  go (PPTypeLevelString s) = text $ T.unpack $ prettyPrintString s
  go (PPTypeLevelInt n) = text $ show n
  go (PPTypeConstructor ctor) = text $ T.unpack $ runProperName $ disqualify ctor
  go (PPTUnknown u)
    | troSuggesting tro = text "_"
    | otherwise = text $ 't' : show u
  go (PPSkolem name s)
    | troSuggesting tro = text $ T.unpack name
    | otherwise = text $ T.unpack name ++ show s
  go (PPRecord labels tail_) = prettyPrintRowWith tro '{' '}' labels tail_
  go (PPRow labels tail_) = prettyPrintRowWith tro '(' ')' labels tail_
  go (PPBinaryNoParensType op l r) =
    typeAsBox' l <> text " " <> typeAsBox' op <> text " " <> typeAsBox' r
  go (PPTypeOp op) = text $ T.unpack $ showQualified runOpName op
  go PPTruncated = text "..."

  go (PPKindArg ty) = text "@" <> operand tro 1 True ty
  go (PPTypeApp f x) = keepSingleLinesOr (moveRight 2) (operand tro 2 True f) (operand tro 2 False x)
  go (PPFunction argT ret) = keepSingleLinesOr id (operand tro 3 False argT) (text rightArrow <> " " <> operand tro 3 True ret)
  go (PPConstrainedType deps ty) = constraintsAsBox tro deps (operand tro 4 True ty)
  go (PPForAll idents ty) =
    keepSingleLinesOr (moveRight 2) (hsep 1 top (text forall' : fmap (printMbKindedType tro) idents) <> text ".") (operand tro 5 True ty)
  go (PPKindedType t k) = keepSingleLinesOr (moveRight 2) (typeAsBox' t) (text (doubleColon tro ++ " ") <> operand tro 6 True k)
  go (PPParensInType ty) = operand tro 7 True ty

-- | Generate a pretty-printed string representing a 'Type'
prettyPrintType :: Int -> Type a -> String
prettyPrintType = flip prettyPrintType' defaultOptions

-- | Generate a pretty-printed string representing a 'Type' using unicode
-- symbols where applicable
prettyPrintTypeWithUnicode :: Int -> Type a -> String
prettyPrintTypeWithUnicode = flip prettyPrintType' unicodeOptions

-- | Generate a pretty-printed string representing a suggested 'Type'
prettyPrintSuggestedType :: Type a -> String
prettyPrintSuggestedType = prettyPrintType' maxBound suggestingOptions

prettyPrintType' :: Int -> TypeRenderOptions -> Type a -> String
prettyPrintType' maxDepth tro = render . typeAsBoxImpl tro . convertPrettyPrintType maxDepth

prettyPrintLabel :: Label -> Text
prettyPrintLabel (Label s) =
  case decodeString s of
    Just s' | not (objectKeyRequiresQuoting s') ->
      s'
    _ ->
      prettyPrintString s

prettyPrintObjectKey :: PSString -> Text
prettyPrintObjectKey = prettyPrintLabel . Label

module TestListLiterals (spec) where

import Prelude

import Data.Map qualified as M
import Data.Text qualified as T

import Language.PureScript qualified as P
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), Expr(..), Module(..))
import Language.PureScript.Names (runIdent)

import Test.Hspec (Spec, context, expectationFailure, shouldSatisfy, specify)

-- | Compile a module named @List@, so that it defines the list type
-- an array literal may denote, and return its renamed CoreFn.
compileToCoreFn :: [T.Text] -> IO (Either P.MultipleErrors (Module Ann))
compileToCoreFn body =
  case sequence $ CST.parseFromFile "Test.purs" (T.unlines (header <> body)) of
    Left parseError ->
      pure $ Left (CST.toMultipleErrors "Test.purs" parseError)
    Right (_warnings, m) -> do
      let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
          makeActions = P.buildMakeActions ".test_modules/list-literals" filePathMap M.empty False Nothing
      (result, _) <- P.runMake P.defaultOptions $
        P.rebuildModuleTypecheck makeActions P.primEnv [] m Nothing P.UnknownRecompileReason
      pure $ P.mcrRenamed <$> result
  where
  header =
    [ "module List where"
    , "foreign import data List :: Type -> Type"
    , "foreign import data Box :: Type -> Type"
    , "idList :: forall a. List a -> List a"
    , "idList x = x"
    , "same :: forall f a. f a -> f a"
    , "same x = x"
    ]

binding :: T.Text -> Module Ann -> Maybe (Expr Ann)
binding name m = lookup name
  [ (runIdent ident, expr)
  | b <- moduleDecls m
  , (ident, expr) <- case b of
      NonRec _ i e -> [(i, e)]
      Rec bs -> [(i, e) | ((_, i), e) <- bs]
  ]

-- | What the literal at the heart of a binding turned out to be.
data Kind = IsList | IsArray | IsOther
  deriving (Eq, Show)

kindOf :: Expr Ann -> Kind
kindOf = \case
  Literal _ (ListLiteral _) -> IsList
  Literal _ (ArrayLiteral _) -> IsArray
  App _ _ arg -> kindOf arg
  _ -> IsOther

expectKinds :: [T.Text] -> [(T.Text, Kind)] -> IO ()
expectKinds body expected =
  compileToCoreFn body >>= \case
    Left errs -> expectationFailure $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right m -> mapM_ (check m) expected
  where
  check m (name, kind) =
    case binding name m of
      Nothing -> expectationFailure $ "No binding " <> T.unpack name
      Just expr -> kindOf expr `shouldSatisfy` (== kind)

-- | The kind of the first pattern of each alternative of the first case in a binding.
patternKinds :: Expr Ann -> [Kind]
patternKinds = \case
  Abs _ _ e -> patternKinds e
  Case _ _ alts -> [ binderKind b | CaseAlternative (b : _) _ <- alts ]
  _ -> []
  where
  binderKind = \case
    LiteralBinder _ (ListLiteral _) -> IsList
    LiteralBinder _ (ArrayLiteral _) -> IsArray
    _ -> IsOther

expectPatterns :: [T.Text] -> [(T.Text, [Kind])] -> IO ()
expectPatterns body expected =
  compileToCoreFn body >>= \case
    Left errs -> expectationFailure $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right m -> mapM_ (check m) expected
  where
  check m (name, kinds) =
    case binding name m of
      Nothing -> expectationFailure $ "No binding " <> T.unpack name
      Just expr -> patternKinds expr `shouldSatisfy` (== kinds)

spec :: Spec
spec = context "Array literals denoting lists" $ do
  specify "the expected type decides, and an unconstrained literal stays an array" $
    expectKinds
      [ "pinned :: List Int"
      , "pinned = [1, 2]"
      , "arg :: List Int"
      , "arg = idList [1, 2]"
      , "viaResult :: List Int"
      , "viaResult = same [1, 2]"
      , "viaPolymorphicParam :: List Int"
      , "viaPolymorphicParam = same (same [1])"
      , "plain :: Array Int"
      , "plain = [1, 2]"
      , "plainSame :: Array Int"
      , "plainSame = same [1]"
      , "defaulted = [1, 2]"
      , "defaultedSame = same [1]"
      , "fn x = [x]"
      ]
      [ ("pinned", IsList), ("arg", IsList), ("viaResult", IsList), ("viaPolymorphicParam", IsList)
      , ("plain", IsArray), ("plainSame", IsArray), ("defaulted", IsArray), ("defaultedSame", IsArray)
      ]

  specify "nested literals are decided independently" $
    expectKinds
      [ "listOfArrays :: List (Array Int)"
      , "listOfArrays = [[1], [2]]"
      , "arrayOfLists :: Array (List Int)"
      , "arrayOfLists = [[1], []]"
      ]
      [ ("listOfArrays", IsList), ("arrayOfLists", IsArray) ]

  specify "a container that is neither array nor list is rejected at the literal" $
    compileToCoreFn ["bad :: Box Int", "bad = same [1]"] >>= \case
      Left errs -> do
        let rendered = T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
        rendered `shouldSatisfy` T.isInfixOf "Could not match type"
        rendered `shouldSatisfy` T.isInfixOf "Test.purs:"
      Right _ -> expectationFailure "Expected `same [1] :: Box Int` to be rejected"

  specify "array patterns follow the scrutinee type the same way" $
    expectPatterns
      [ "onList :: List Int -> Int"
      , "onList [a] = a"
      , "onList _ = 0"
      , "onArray :: Array Int -> Int"
      , "onArray [a] = a"
      , "onArray _ = 0"
      , "onUnknown xs = case xs of"
      , "  [a] -> a"
      , "  _ -> 0"
      ]
      [ ("onList", [IsList, IsOther]), ("onArray", [IsArray, IsOther]), ("onUnknown", [IsArray, IsOther]) ]

  specify "a pattern on a container that is neither array nor list is rejected" $
    compileToCoreFn ["bad :: Box Int -> Int", "bad [a] = a", "bad _ = 0"] >>= \case
      Left errs ->
        T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
          `shouldSatisfy` T.isInfixOf "Could not match type"
      Right _ -> expectationFailure "Expected an array pattern on `Box Int` to be rejected"

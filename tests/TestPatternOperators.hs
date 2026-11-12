module TestPatternOperators (spec) where

import Prelude

import Data.Map qualified as M
import Data.Text qualified as T

import Language.PureScript qualified as P
import Language.PureScript.CST qualified as CST
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), Expr(..), Module(..))
import Language.PureScript.Names (runIdent)

import Test.Hspec (Spec, context, expectationFailure, shouldSatisfy, specify)

-- | Compile a module, with the given name and header, and return its renamed CoreFn.
compileToCoreFn :: [T.Text] -> [T.Text] -> IO (Either P.MultipleErrors (Module Ann))
compileToCoreFn header body =
  case sequence $ CST.parseFromFile "Test.purs" (T.unlines (header <> body)) of
    Left parseError ->
      pure $ Left (CST.toMultipleErrors "Test.purs" parseError)
    Right (_warnings, m) -> do
      let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
          makeActions = P.buildMakeActions ".test_modules/pattern-operators" filePathMap M.empty False Nothing
      (result, _) <- P.runMake P.defaultOptions $
        P.rebuildModuleTypecheck makeActions P.primEnv [] m Nothing P.UnknownRecompileReason
      pure $ P.mcrRenamed <$> result

-- | Modules with the list and the map types, but no operators: the pattern
-- operators are built into the compiler.
header, mapHeader :: [T.Text]
header =
  [ "module List where"
  , "foreign import data List :: Type -> Type"
  ]
mapHeader =
  [ "module Map where"
  , "foreign import data Map :: Type -> Type -> Type"
  ]

binding :: T.Text -> Module Ann -> Maybe (Expr Ann)
binding name m = lookup name
  [ (runIdent ident, expr)
  | b <- moduleDecls m
  , (ident, expr) <- case b of
      NonRec _ i e -> [(i, e)]
      Rec bs -> [(i, e) | ((_, i), e) <- bs]
  ]

data Shape = Cons Shape | Prefix Shape | Entry | And Shape Shape | Other
  deriving (Eq, Show)

shapeOf :: Binder Ann -> Shape
shapeOf = \case
  ConsBinder _ _ t -> Cons (shapeOf t)
  StringPrefixBinder _ _ b -> Prefix (shapeOf b)
  MapEntryBinder{} -> Entry
  AndBinder _ l r -> And (shapeOf l) (shapeOf r)
  NamedBinder _ _ b -> shapeOf b
  _ -> Other

-- | The shape of the first pattern of each alternative of the first case in a binding.
patternShapes :: Expr Ann -> [Shape]
patternShapes = \case
  Abs _ _ e -> patternShapes e
  Case _ _ alts -> [ shapeOf b | CaseAlternative (b : _) _ <- alts ]
  _ -> []

expectShapes :: [T.Text] -> [T.Text] -> [(T.Text, [Shape])] -> IO ()
expectShapes header body expected =
  compileToCoreFn header body >>= \case
    Left errs -> expectationFailure $ P.prettyPrintMultipleErrors P.defaultPPEOptions errs
    Right m -> mapM_ (check m) expected
  where
  check m (name, shapes) =
    case binding name m of
      Nothing -> expectationFailure $ "No binding " <> T.unpack name
      Just expr -> patternShapes expr `shouldSatisfy` (== shapes)

expectError :: [T.Text] -> [T.Text] -> T.Text -> IO ()
expectError header body message =
  compileToCoreFn header body >>= \case
    Left errs -> T.pack (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) `shouldSatisfy` T.isInfixOf message
    Right _ -> expectationFailure $ "Expected an error mentioning: " <> T.unpack message

-- | The warnings of a module that compiles.
warningsOf :: [T.Text] -> [T.Text] -> IO T.Text
warningsOf header body =
  case sequence $ CST.parseFromFile "Test.purs" (T.unlines (header <> body)) of
    Left parseError ->
      expectationFailure (show parseError) >> pure ""
    Right (_warnings, m) -> do
      let filePathMap = M.singleton (P.getModuleName m) (Left P.RebuildAlways)
          makeActions = P.buildMakeActions ".test_modules/pattern-operators" filePathMap M.empty False Nothing
      (result, warnings) <- P.runMake P.defaultOptions $
        P.rebuildModuleTypecheck makeActions P.primEnv [] m Nothing P.UnknownRecompileReason
      case result of
        Left errs -> expectationFailure (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> pure ""
        Right _ -> pure . T.pack $ P.prettyPrintMultipleErrors P.defaultPPEOptions warnings

expectUnreachable :: [T.Text] -> [T.Text] -> [T.Text] -> IO ()
expectUnreachable header body patterns = do
  warnings <- warningsOf header body
  warnings `shouldSatisfy` T.isInfixOf "unreachable cases"
  mapM_ (\p -> warnings `shouldSatisfy` T.isInfixOf p) patterns

expectNoUnreachable :: [T.Text] -> [T.Text] -> IO ()
expectNoUnreachable header body = do
  warnings <- warningsOf header body
  warnings `shouldSatisfy` (not . T.isInfixOf "unreachable cases")

spec :: Spec
spec = context "Built-in pattern operators" $ do
  specify "`:` matches a list head and tail, and nests to the right" $
    expectShapes header
      [ "hd :: List Int -> Int"
      , "hd (x : _) = x"
      , "hd _ = 0"
      , "two :: List Int -> Int"
      , "two (_ : y : _) = y"
      , "two _ = 0"
      , "named :: List Int -> List Int"
      , "named all@(_ : _) = all"
      , "named l = l"
      ]
      [ ("hd", [Cons Other, Other]), ("two", [Cons (Cons Other), Other]), ("named", [Cons Other, Other]) ]

  specify "`<>` matches a string prefix, and nests to the right" $
    expectShapes header
      [ "greet :: String -> String"
      , "greet (\"hello \" <> name) = name"
      , "greet s = s"
      , "multi :: String -> Int"
      , "multi (\"a\" <> \"b\" <> _) = 1"
      , "multi (\"c\" <> \"\") = 2"
      , "multi _ = 0"
      ]
      [ ("greet", [Prefix Other, Other]), ("multi", [Prefix (Prefix Other), Prefix Other, Other]) ]

  specify "`:=` matches a map entry, and `&` combines entries" $
    expectShapes mapHeader
      [ "one :: Map String Int -> Int"
      , "one (\"a\" := x) = x"
      , "one _ = 0"
      , "two :: Map String Int -> Int"
      , "two (\"a\" := _ & \"b\" := y & \"c\" := _) = y"
      , "two _ = 0"
      ]
      [ ("one", [Entry, Other])
      , ("two", [And Entry (And Entry Entry), Other])
      ]

  specify "the operators bind as : over <> over := over &" $
    expectShapes mapHeader
      [ "f :: Map String String -> String"
      , "f (\"p\" := \"q\" <> r & \"z\" := _) = r"
      , "f _ = \"\""
      ]
      [ ("f", [And Entry Entry, Other]) ]

  specify "`:` only matches lists" $
    expectError header ["f :: Int -> Int", "f (_ : _) = 1", "f _ = 0"] "Could not match type"

  specify "`<>` only matches strings" $
    expectError header ["f :: Array Int -> Int", "f (\"a\" <> _) = 1", "f _ = 0"] "Could not match type"

  specify "`:=` only matches maps, and types the key and value" $ do
    expectError mapHeader ["f :: String -> Int", "f (\"a\" := _) = 1", "f _ = 0"] "Could not match type"
    expectError mapHeader ["f :: Map String Int -> Int", "f (\"a\" := \"x\") = 1", "f _ = 0"] "Could not match type"

  specify "`<>` needs a string literal on the left" $
    expectError header ["f :: String -> String", "f (x <> _) = x"] "must be a string literal followed by"

  specify "`<>` needs a variable, wildcard or string on the right" $
    expectError header ["f :: String -> String", "f (\"a\" <> b@_) = b", "f _ = \"\""] "must be a string literal followed by"

  specify "`:=` needs a literal key" $
    expectError mapHeader ["f :: Map String Int -> Int", "f (k := _) = 1"] "must be a literal"

  specify "`&` only combines map entries" $ do
    expectError header ["f :: List Int -> Int", "f (_ : _ & _ : _) = 1", "f _ = 0"] "can only combine map entries"
    expectError header ["f :: { a :: Int, b :: Int } -> Int", "f ({ a: _ } & { b: _ }) = 1", "f _ = 0"] "can only combine map entries"

  specify "no other operator can be used in a pattern, whether it aliases a function or a constructor" $ do
    expectError
      [ "module Test where"
      , "foo :: Int -> Int -> Int"
      , "foo a _ = a"
      , "infixr 5 foo as +++"
      ]
      ["f :: Int -> Int", "f (1 +++ _) = 1", "f _ = 0"]
      "cannot be used in a pattern"
    expectError
      [ "module Test where"
      , "data Pair a b = Pair a b"
      , "infixr 5 Pair as :*"
      ]
      ["f :: Pair Int Int -> Int", "f (a :* _) = a"]
      "cannot be used in a pattern"

  context "exhaustiveness" $ do
    specify "a list is covered by the empty list and a cons, in any order" $
      expectShapes header
        [ "a :: List Int -> Int"
        , "a [] = 0"
        , "a (x : _) = x"
        , "b :: List Int -> Int"
        , "b (x : _) = x"
        , "b [] = 0"
        , "c :: List Int -> Int"
        , "c [] = 0"
        , "c [x] = x"
        , "c (_ : y : _) = y"
        , "d :: List Int -> List Int -> Int"
        , "d [] _ = 0"
        , "d (x : _) [] = x"
        , "d (_ : _) (y : _) = y"
        , "e :: List Int -> Int"
        , "e l = case l of"
        , "  [] -> 0"
        , "  [_, _] -> 2"
        , "  _ : _ -> 1"
        ]
        []

    specify "a list inside a constructor is checked as well" $
      expectShapes header
        [ "data Maybe a = Nothing | Just a"
        , "f :: Maybe (List Int) -> Int"
        , "f Nothing = 0"
        , "f (Just []) = 1"
        , "f (Just (x : _)) = x"
        ]
        []

    specify "a missing list case is reported" $ do
      expectError header ["f :: List Int -> Int", "f (x : _) = x"] "cover all inputs"
      expectError header ["f :: List Int -> Int", "f [] = 0", "f (x : []) = x"] "(_ : _ : _)"
      expectError header ["f :: List Int -> List Int -> Int", "f [] _ = 0", "f (x : _) (y : _) = x"] "(_ : _)"

    specify "a string needs a catch-all, unless the prefix is empty" $ do
      expectShapes header
        [ "a :: String -> String"
        , "a (\"p\" <> r) = r"
        , "a s = s"
        , "b :: String -> String"
        , "b (\"\" <> r) = r"
        ]
        []
      expectError header ["f :: String -> String", "f (\"p\" <> r) = r"] "cover all inputs"

    specify "a map needs a catch-all" $ do
      expectShapes mapHeader
        [ "f :: Map String Int -> Int"
        , "f (\"a\" := x) = x"
        , "f _ = 0"
        ]
        []
      expectError mapHeader ["f :: Map String Int -> Int", "f (\"a\" := x & \"b\" := _) = x"] "cover all inputs"

  context "redundancy" $ do
    specify "a list pattern that an earlier one covers is unreachable" $ do
      expectUnreachable header
        [ "f :: List Int -> Int", "f [] = 0", "f [] = 1", "f (x : _) = x" ]
        [ "[]" ]
      expectUnreachable header
        [ "f :: List Int -> Int", "f (x : _) = x", "f [_] = 1", "f _ = 0" ]
        [ "[ _ ]" ]

    specify "a string pattern that an earlier prefix covers is unreachable" $ do
      expectUnreachable header
        [ "f :: String -> Int"
        , "f (\"ab\" <> _) = 1"
        , "f (\"abc\" <> _) = 2"
        , "f \"abcd\" = 3"
        , "f (\"x\" <> _) = 4"
        , "f _ = 0"
        ]
        [ "(\"abc\" <> _)", "\"abcd\"" ]
      expectUnreachable header
        [ "f :: String -> Int", "f (\"a\" <> _) = 1", "f (\"a\" <> _) = 2", "f _ = 0" ]
        [ "(\"a\" <> _)" ]

    specify "a more general string pattern after a specific one is reachable" $
      expectNoUnreachable header
        [ "f :: String -> Int"
        , "f (\"abc\" <> _) = 1"
        , "f (\"ab\" <> _) = 2"
        , "f \"a\" = 3"
        , "f _ = 0"
        ]

    specify "a map pattern that an earlier one covers is unreachable" $
      expectUnreachable mapHeader
        [ "f :: Map String Int -> Int"
        , "f (\"a\" := _) = 1"
        , "f (\"a\" := 1 & \"b\" := _) = 2"
        , "f _ = 0"
        ]
        [ "((\"a\" := 1) & (\"b\" := _))" ]

    specify "a more general map pattern after a specific one is reachable" $
      expectNoUnreachable mapHeader
        [ "f :: Map String Int -> Int"
        , "f (\"a\" := _ & \"b\" := _) = 1"
        , "f (\"a\" := _) = 2"
        , "f (\"c\" := 1) = 3"
        , "f (\"c\" := 2) = 4"
        , "f _ = 0"
        ]

    specify "an earlier alternative with a guard that can fail covers nothing" $
      expectNoUnreachable header
        [ "f :: String -> Boolean -> Int"
        , "f (\"a\" <> _) b | b = 1"
        , "f (\"a\" <> _) _ = 2"
        , "f _ _ = 0"
        ]

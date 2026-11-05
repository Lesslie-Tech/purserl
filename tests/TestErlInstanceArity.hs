{-# LANGUAGE OverloadedStrings #-}

module TestErlInstanceArity (spec) where

import Prelude

import Data.List.NonEmpty qualified as NEL
import Data.Map qualified as M

import Language.PureScript.Environment (Environment (..), initEnvironment, makeTypeClassData, tyInt)
import Language.PureScript.Erl.CodeGen (FnArity (..), instanceArities)
import Language.PureScript.Names (Ident (..), ModuleName (..), ProperName (..), ProperNameType (..), Qualified (..), QualifiedBy (..))
import Language.PureScript.TypeClassDictionaries (NamedDict, TypeClassDictionaryInScope (..))
import Language.PureScript.Types (srcConstraint)
import Test.Hspec (Spec, describe, it, shouldBe)

modName :: ModuleName
modName = ModuleName "M"

cls :: ProperName 'ClassName -> Qualified (ProperName 'ClassName)
cls = Qualified (ByModuleName modName)

-- A class with one method, so its dictionary is a real value.
fullClass, emptyClass :: ProperName 'ClassName
fullClass = ProperName "Full"
emptyClass = ProperName "Empty"

unknownClass :: ProperName 'ClassName
unknownClass = ProperName "Hidden"

-- `instance <deps> => <klass> t`, named `ident`.
instanceOf :: Ident -> ProperName 'ClassName -> [ProperName 'ClassName] -> NamedDict
instanceOf ident klass deps =
  TypeClassDictionaryInScope
    { tcdChain = Nothing
    , tcdIndex = 0
    , tcdValue = Qualified (ByModuleName modName) ident
    , tcdPath = []
    , tcdClassName = cls klass
    , tcdForAll = []
    , tcdInstanceKinds = []
    , tcdInstanceTypes = []
    , tcdDependencies = Just [srcConstraint (cls d) [] [] Nothing | d <- deps]
    , tcdDescription = Nothing
    }

arities :: [NamedDict] -> M.Map (Qualified Ident) FnArity
arities dicts =
  instanceArities
    initEnvironment
      { typeClasses =
          M.fromList
            [ (cls fullClass, makeTypeClassData [] [(Ident "m", tyInt)] [] [] False)
            , (cls emptyClass, makeTypeClassData [] [] [] [] True)
            ]
      , typeClassDictionaries =
          M.singleton (ByModuleName modName) $
            M.fromListWith (<>) [(tcdClassName d, M.singleton (tcdValue d) (NEL.singleton d)) | d <- dicts]
      }

arityOf :: Ident -> M.Map (Qualified Ident) FnArity -> Maybe FnArity
arityOf ident = M.lookup (Qualified (ByModuleName modName) ident)

spec :: Spec
spec = describe "instanceArities" $ do
  it "counts one dictionary per non-empty-class constraint" $
    arityOf (Ident "i") (arities [instanceOf (Ident "i") fullClass [fullClass, fullClass, emptyClass]])
      `shouldBe` Just (Arity (2, 0))

  it "gives no arity to an instance whose constraints are all empty classes" $
    arityOf (Ident "i") (arities [instanceOf (Ident "i") fullClass [emptyClass]]) `shouldBe` Nothing

  it "gives no arity to an instance without constraints" $
    arityOf (Ident "i") (arities [instanceOf (Ident "i") fullClass []]) `shouldBe` Nothing

  -- Its dictionary is an empty map that takes no arguments, however many constraints it has.
  it "gives no arity to an instance of an empty class" $
    arityOf (Ident "i") (arities [instanceOf (Ident "i") emptyClass [fullClass]]) `shouldBe` Nothing

  -- Unexported classes aren't in other modules' environments, so no one can agree on a count.
  it "gives no arity to an instance with a constraint on an unknown class" $
    arityOf (Ident "i") (arities [instanceOf (Ident "i") fullClass [fullClass, unknownClass]]) `shouldBe` Nothing

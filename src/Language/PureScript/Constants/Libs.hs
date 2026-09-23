-- | Various constants which refer to things in the Prelude and other core libraries
module Language.PureScript.Constants.Libs where

import Protolude (Eq, IsString)
import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..))

-- purescript-prelude

-- Control.Apply
pattern S_apply :: forall a. (Eq a, IsString a) => a
pattern S_apply = "apply"

pattern I_apply :: Qualified Ident
pattern I_apply = Qualified (ByModuleName (ModuleName "Control.Apply")) (Ident "apply")

-- Control.Applicative
pattern I_applicativeArray :: Qualified Ident
pattern I_applicativeArray = Qualified (ByModuleName (ModuleName "Control.Applicative")) (Ident "applicativeArray")

pattern S_pure :: forall a. (Eq a, IsString a) => a
pattern S_pure = "pure"

pattern I_pure :: Qualified Ident
pattern I_pure = Qualified (ByModuleName (ModuleName "Control.Applicative")) (Ident "pure")

-- Control.Bind
pattern I_bindArray :: Qualified Ident
pattern I_bindArray = Qualified (ByModuleName (ModuleName "Control.Bind")) (Ident "bindArray")

pattern I_bind :: Qualified Ident
pattern I_bind = Qualified (ByModuleName (ModuleName "Control.Bind")) (Ident "bind")

pattern I_discard :: Qualified Ident
pattern I_discard = Qualified (ByModuleName (ModuleName "Control.Bind")) (Ident "discard")

pattern S_bind :: forall a. (Eq a, IsString a) => a
pattern S_bind = "bind"

pattern Discard :: Qualified (ProperName 'ClassName)
pattern Discard = Qualified (ByModuleName (ModuleName "Control.Bind")) (ProperName "Discard")

pattern S_discard :: forall a. (Eq a, IsString a) => a
pattern S_discard = "discard"

pattern P_discard :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_discard = (ModuleName "Control.Bind", "discard")

-- Control.Category
pattern I_identity :: Qualified Ident
pattern I_identity = Qualified (ByModuleName (ModuleName "Control.Category")) (Ident "identity")

-- Control.Semigroupoid
pattern P_semigroupoidFn :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_semigroupoidFn = (ModuleName "Control.Semigroupoid", "semigroupoidFn")

-- Data.Bounded
pattern P_bottom :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_bottom = (ModuleName "Data.Bounded", "bottom")

pattern P_top :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_top = (ModuleName "Data.Bounded", "top")

pattern P_boundedBoolean :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_boundedBoolean = (ModuleName "Data.Bounded", "boundedBoolean")

-- Data.Eq
pattern Eq :: Qualified (ProperName 'ClassName)
pattern Eq = Qualified (ByModuleName (ModuleName "Data.Eq")) (ProperName "Eq")

pattern S_eq :: forall a. (Eq a, IsString a) => a
pattern S_eq = "eq"

pattern P_eq :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eq = (ModuleName "Data.Eq", "eq")

pattern I_eq :: Qualified Ident
pattern I_eq = Qualified (ByModuleName (ModuleName "Data.Eq")) (Ident "eq")

pattern Eq1 :: Qualified (ProperName 'ClassName)
pattern Eq1 = Qualified (ByModuleName (ModuleName "Data.Eq")) (ProperName "Eq1")

pattern S_eq1 :: forall a. (Eq a, IsString a) => a
pattern S_eq1 = "eq1"

pattern I_eq1 :: Qualified Ident
pattern I_eq1 = Qualified (ByModuleName (ModuleName "Data.Eq")) (Ident "eq1")

pattern P_notEq :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_notEq = (ModuleName "Data.Eq", "notEq")

pattern P_eqBoolean :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eqBoolean = (ModuleName "Data.Eq", "eqBoolean")

pattern P_eqChar :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eqChar = (ModuleName "Data.Eq", "eqChar")

pattern P_eqInt :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eqInt = (ModuleName "Data.Eq", "eqInt")

pattern P_eqNumber :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eqNumber = (ModuleName "Data.Eq", "eqNumber")

pattern P_eqString :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_eqString = (ModuleName "Data.Eq", "eqString")

-- Data.EuclideanRing
pattern P_div :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_div = (ModuleName "Data.EuclideanRing", "div")

pattern P_euclideanRingNumber :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_euclideanRingNumber = (ModuleName "Data.EuclideanRing", "euclideanRingNumber")

-- Data.Function
pattern I_functionApply :: Qualified Ident
pattern I_functionApply = Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "apply")

pattern I_functionApplyFlipped :: Qualified Ident
pattern I_functionApplyFlipped = Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "applyFlipped")

pattern I_const :: Qualified Ident
pattern I_const = Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "const")

pattern I_flip :: Qualified Ident
pattern I_flip = Qualified (ByModuleName (ModuleName "Data.Function")) (Ident "flip")

-- Data.Functor
pattern Functor :: Qualified (ProperName 'ClassName)
pattern Functor = Qualified (ByModuleName (ModuleName "Data.Functor")) (ProperName "Functor")

pattern S_map :: forall a. (Eq a, IsString a) => a
pattern S_map = "map"

pattern I_map :: Qualified Ident
pattern I_map = Qualified (ByModuleName (ModuleName "Data.Functor")) (Ident "map")

pattern I_functorArray :: Qualified Ident
pattern I_functorArray = Qualified (ByModuleName (ModuleName "Data.Functor")) (Ident "functorArray")

-- Data.Generic.Rep
pattern Generic :: Qualified (ProperName 'ClassName)
pattern Generic = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Generic")

pattern I_from :: Qualified Ident
pattern I_from = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (Ident "from")

pattern I_to :: Qualified Ident
pattern I_to = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (Ident "to")

pattern Argument :: Qualified (ProperName 'TypeName)
pattern Argument = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Argument")

pattern C_Argument :: Qualified (ProperName 'ConstructorName)
pattern C_Argument = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Argument")

pattern Constructor :: Qualified (ProperName 'TypeName)
pattern Constructor = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Constructor")

pattern C_Constructor :: Qualified (ProperName 'ConstructorName)
pattern C_Constructor = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Constructor")

pattern NoArguments :: Qualified (ProperName 'TypeName)
pattern NoArguments = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "NoArguments")

pattern C_NoArguments :: Qualified (ProperName 'ConstructorName)
pattern C_NoArguments = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "NoArguments")

pattern NoConstructors :: Qualified (ProperName 'TypeName)
pattern NoConstructors = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "NoConstructors")

pattern Product :: Qualified (ProperName 'TypeName)
pattern Product = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Product")

pattern C_Product :: Qualified (ProperName 'ConstructorName)
pattern C_Product = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Product")

pattern Sum :: Qualified (ProperName 'TypeName)
pattern Sum = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Sum")

pattern C_Inl :: Qualified (ProperName 'ConstructorName)
pattern C_Inl = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Inl")

pattern C_Inr :: Qualified (ProperName 'ConstructorName)
pattern C_Inr = Qualified (ByModuleName (ModuleName "Data.Generic.Rep")) (ProperName "Inr")

-- Data.HeytingAlgebra
pattern I_conj :: Qualified Ident
pattern I_conj = Qualified (ByModuleName (ModuleName "Data.HeytingAlgebra")) (Ident "conj")

pattern P_conj :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_conj = (ModuleName "Data.HeytingAlgebra", "conj")

pattern P_disj :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_disj = (ModuleName "Data.HeytingAlgebra", "disj")

pattern P_not :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_not = (ModuleName "Data.HeytingAlgebra", "not")

pattern P_heytingAlgebraBoolean :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_heytingAlgebraBoolean = (ModuleName "Data.HeytingAlgebra", "heytingAlgebraBoolean")

-- Data.Monoid
pattern I_mempty :: Qualified Ident
pattern I_mempty = Qualified (ByModuleName (ModuleName "Data.Monoid")) (Ident "mempty")

-- Data.Ord
pattern Ord :: Qualified (ProperName 'ClassName)
pattern Ord = Qualified (ByModuleName (ModuleName "Data.Ord")) (ProperName "Ord")

pattern S_compare :: forall a. (Eq a, IsString a) => a
pattern S_compare = "compare"

pattern I_compare :: Qualified Ident
pattern I_compare = Qualified (ByModuleName (ModuleName "Data.Ord")) (Ident "compare")

pattern Ord1 :: Qualified (ProperName 'ClassName)
pattern Ord1 = Qualified (ByModuleName (ModuleName "Data.Ord")) (ProperName "Ord1")

pattern S_compare1 :: forall a. (Eq a, IsString a) => a
pattern S_compare1 = "compare1"

pattern I_compare1 :: Qualified Ident
pattern I_compare1 = Qualified (ByModuleName (ModuleName "Data.Ord")) (Ident "compare1")

pattern P_greaterThan :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_greaterThan = (ModuleName "Data.Ord", "greaterThan")

pattern P_greaterThanOrEq :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_greaterThanOrEq = (ModuleName "Data.Ord", "greaterThanOrEq")

pattern P_lessThan :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_lessThan = (ModuleName "Data.Ord", "lessThan")

pattern P_lessThanOrEq :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_lessThanOrEq = (ModuleName "Data.Ord", "lessThanOrEq")

pattern P_ordBoolean :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ordBoolean = (ModuleName "Data.Ord", "ordBoolean")

pattern P_ordChar :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ordChar = (ModuleName "Data.Ord", "ordChar")

pattern P_ordInt :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ordInt = (ModuleName "Data.Ord", "ordInt")

pattern P_ordNumber :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ordNumber = (ModuleName "Data.Ord", "ordNumber")

pattern P_ordString :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ordString = (ModuleName "Data.Ord", "ordString")

-- Data.Ordering
pattern Ordering :: Qualified (ProperName 'TypeName)
pattern Ordering = Qualified (ByModuleName (ModuleName "Data.Ordering")) (ProperName "Ordering")

pattern C_EQ :: Qualified (ProperName 'ConstructorName)
pattern C_EQ = Qualified (ByModuleName (ModuleName "Data.Ordering")) (ProperName "EQ")

pattern C_GT :: Qualified (ProperName 'ConstructorName)
pattern C_GT = Qualified (ByModuleName (ModuleName "Data.Ordering")) (ProperName "GT")

pattern C_LT :: Qualified (ProperName 'ConstructorName)
pattern C_LT = Qualified (ByModuleName (ModuleName "Data.Ordering")) (ProperName "LT")

-- Data.Reflectable
pattern Reflectable :: Qualified (ProperName 'ClassName)
pattern Reflectable = Qualified (ByModuleName (ModuleName "Data.Reflectable")) (ProperName "Reflectable")

-- Data.Ring
pattern S_negate :: forall a. (Eq a, IsString a) => a
pattern S_negate = "negate"

pattern P_negate :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_negate = (ModuleName "Data.Ring", "negate")

pattern P_sub :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_sub = (ModuleName "Data.Ring", "sub")

pattern P_ringInt :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ringInt = (ModuleName "Data.Ring", "ringInt")

pattern P_ringNumber :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_ringNumber = (ModuleName "Data.Ring", "ringNumber")

-- Data.Semigroup
pattern I_append :: Qualified Ident
pattern I_append = Qualified (ByModuleName (ModuleName "Data.Semigroup")) (Ident "append")

pattern P_append :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_append = (ModuleName "Data.Semigroup", "append")

pattern P_semigroupString :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_semigroupString = (ModuleName "Data.Semigroup", "semigroupString")

-- Data.Semiring
pattern P_add :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_add = (ModuleName "Data.Semiring", "add")

pattern P_mul :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_mul = (ModuleName "Data.Semiring", "mul")

pattern P_one :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_one = (ModuleName "Data.Semiring", "one")

pattern P_zero :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_zero = (ModuleName "Data.Semiring", "zero")

pattern P_semiringInt :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_semiringInt = (ModuleName "Data.Semiring", "semiringInt")

pattern P_semiringNumber :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_semiringNumber = (ModuleName "Data.Semiring", "semiringNumber")

-- Data.Symbol
pattern IsSymbol :: Qualified (ProperName 'ClassName)
pattern IsSymbol = Qualified (ByModuleName (ModuleName "Data.Symbol")) (ProperName "IsSymbol")

-- purescript-bifunctors

-- Data.Bifunctor
pattern Bifunctor :: Qualified (ProperName 'ClassName)
pattern Bifunctor = Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (ProperName "Bifunctor")

pattern S_bimap :: forall a. (Eq a, IsString a) => a
pattern S_bimap = "bimap"

pattern I_bimap :: Qualified Ident
pattern I_bimap = Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "bimap")

pattern I_lmap :: Qualified Ident
pattern I_lmap = Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "lmap")

pattern I_rmap :: Qualified Ident
pattern I_rmap = Qualified (ByModuleName (ModuleName "Data.Bifunctor")) (Ident "rmap")

-- purescript-contravariant

-- Data.Functor.Contravariant
pattern Contravariant :: Qualified (ProperName 'ClassName)
pattern Contravariant = Qualified (ByModuleName (ModuleName "Data.Functor.Contravariant")) (ProperName "Contravariant")

pattern S_cmap :: forall a. (Eq a, IsString a) => a
pattern S_cmap = "cmap"

pattern I_cmap :: Qualified Ident
pattern I_cmap = Qualified (ByModuleName (ModuleName "Data.Functor.Contravariant")) (Ident "cmap")

-- purescript-effect

-- Effect
pattern I_bindEffect :: Qualified Ident
pattern I_bindEffect = Qualified (ByModuleName (ModuleName "Effect")) (Ident "bindEffect")

pattern I_applicativeEffect :: Qualified Ident
pattern I_applicativeEffect = Qualified (ByModuleName (ModuleName "Effect")) (Ident "applicativeEffect")

pattern I_effectBindE :: Qualified Ident
pattern I_effectBindE = Qualified (ByModuleName (ModuleName "Effect")) (Ident "bindE")

pattern P_effectBindE :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_effectBindE = (ModuleName "Effect", "bindE")

pattern I_effectPureE :: Qualified Ident
pattern I_effectPureE = Qualified (ByModuleName (ModuleName "Effect")) (Ident "pureE")

pattern P_effectPureE :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_effectPureE = (ModuleName "Effect", "pureE")

-- Effect.Uncurried
pattern P_mkEffectFn :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_mkEffectFn = (ModuleName "Effect.Uncurried", "mkEffectFn")

pattern P_runEffectFn :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_runEffectFn = (ModuleName "Effect.Uncurried", "runEffectFn")

-- purescript-foldable-traversable

-- Data.Bifoldable
pattern Bifoldable :: Qualified (ProperName 'ClassName)
pattern Bifoldable = Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (ProperName "Bifoldable")

pattern S_bifoldMap :: forall a. (Eq a, IsString a) => a
pattern S_bifoldMap = "bifoldMap"

pattern I_bifoldMap :: Qualified Ident
pattern I_bifoldMap = Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldMap")

pattern S_bifoldl :: forall a. (Eq a, IsString a) => a
pattern S_bifoldl = "bifoldl"

pattern I_bifoldl :: Qualified Ident
pattern I_bifoldl = Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldl")

pattern S_bifoldr :: forall a. (Eq a, IsString a) => a
pattern S_bifoldr = "bifoldr"

pattern I_bifoldr :: Qualified Ident
pattern I_bifoldr = Qualified (ByModuleName (ModuleName "Data.Bifoldable")) (Ident "bifoldr")

-- Data.Bitraversable
pattern Bitraversable :: Qualified (ProperName 'ClassName)
pattern Bitraversable = Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (ProperName "Bitraversable")

pattern I_bitraverse :: Qualified Ident
pattern I_bitraverse = Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "bitraverse")

pattern S_bitraverse :: forall a. (Eq a, IsString a) => a
pattern S_bitraverse = "bitraverse"

pattern S_bisequence :: forall a. (Eq a, IsString a) => a
pattern S_bisequence = "bisequence"

pattern I_ltraverse :: Qualified Ident
pattern I_ltraverse = Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "ltraverse")

pattern I_rtraverse :: Qualified Ident
pattern I_rtraverse = Qualified (ByModuleName (ModuleName "Data.Bitraversable")) (Ident "rtraverse")

-- Data.Foldable
pattern Foldable :: Qualified (ProperName 'ClassName)
pattern Foldable = Qualified (ByModuleName (ModuleName "Data.Foldable")) (ProperName "Foldable")

pattern S_foldMap :: forall a. (Eq a, IsString a) => a
pattern S_foldMap = "foldMap"

pattern I_foldMap :: Qualified Ident
pattern I_foldMap = Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldMap")

pattern S_foldl :: forall a. (Eq a, IsString a) => a
pattern S_foldl = "foldl"

pattern I_foldl :: Qualified Ident
pattern I_foldl = Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldl")

pattern S_foldr :: forall a. (Eq a, IsString a) => a
pattern S_foldr = "foldr"

pattern I_foldr :: Qualified Ident
pattern I_foldr = Qualified (ByModuleName (ModuleName "Data.Foldable")) (Ident "foldr")

-- Data.Traversable
pattern Traversable :: Qualified (ProperName 'ClassName)
pattern Traversable = Qualified (ByModuleName (ModuleName "Data.Traversable")) (ProperName "Traversable")

pattern I_traverse :: Qualified Ident
pattern I_traverse = Qualified (ByModuleName (ModuleName "Data.Traversable")) (Ident "traverse")

pattern S_traverse :: forall a. (Eq a, IsString a) => a
pattern S_traverse = "traverse"

pattern S_sequence :: forall a. (Eq a, IsString a) => a
pattern S_sequence = "sequence"

-- purescript-functions

-- Data.Function.Uncurried
pattern M_Data_Function_Uncurried :: ModuleName
pattern M_Data_Function_Uncurried = ModuleName "Data.Function.Uncurried"

pattern S_mkFn :: forall a. (Eq a, IsString a) => a
pattern S_mkFn = "mkFn"

pattern S_runFn :: forall a. (Eq a, IsString a) => a
pattern S_runFn = "runFn"

pattern P_runFn :: forall a. (Eq a, IsString a) => (ModuleName, a)
pattern P_runFn = (ModuleName "Data.Function.Uncurried", "runFn")

-- purescript-newtype

-- Data.Newtype
pattern Newtype :: Qualified (ProperName 'ClassName)
pattern Newtype = Qualified (ByModuleName (ModuleName "Data.Newtype")) (ProperName "Newtype")

-- purescript-partial

-- Partial.Unsafe
pattern I_unsafePartial :: Qualified Ident
pattern I_unsafePartial = Qualified (ByModuleName (ModuleName "Partial.Unsafe")) (Ident "unsafePartial")

-- purescript-profunctor

-- Data.Profunctor
pattern Profunctor :: Qualified (ProperName 'ClassName)
pattern Profunctor = Qualified (ByModuleName (ModuleName "Data.Profunctor")) (ProperName "Profunctor")

pattern S_dimap :: forall a. (Eq a, IsString a) => a
pattern S_dimap = "dimap"

pattern I_dimap :: Qualified Ident
pattern I_dimap = Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "dimap")

pattern I_lcmap :: Qualified Ident
pattern I_lcmap = Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "lcmap")

pattern I_profunctorRmap :: Qualified Ident
pattern I_profunctorRmap = Qualified (ByModuleName (ModuleName "Data.Profunctor")) (Ident "rmap")

-- [drathier]: purserl-specific modules

-- Array
pattern I_arrayMap :: Qualified Ident
pattern I_arrayMap = Qualified (ByModuleName (ModuleName "Array")) (Ident "map")

pattern I_arrayBind :: Qualified Ident
pattern I_arrayBind = Qualified (ByModuleName (ModuleName "Array")) (Ident "bind")

pattern I_arrayPure :: Qualified Ident
pattern I_arrayPure = Qualified (ByModuleName (ModuleName "Array")) (Ident "pure")

-- E
pattern I_eMap :: Qualified Ident
pattern I_eMap = Qualified (ByModuleName (ModuleName "E")) (Ident "map")

pattern I_eBind :: Qualified Ident
pattern I_eBind = Qualified (ByModuleName (ModuleName "E")) (Ident "bind")

pattern I_eDiscard :: Qualified Ident
pattern I_eDiscard = Qualified (ByModuleName (ModuleName "E")) (Ident "discard")

-- Either
pattern I_functorEither :: Qualified Ident
pattern I_functorEither = Qualified (ByModuleName (ModuleName "Either")) (Ident "functorEither")

pattern I_bindEither :: Qualified Ident
pattern I_bindEither = Qualified (ByModuleName (ModuleName "Either")) (Ident "bindEither")

pattern I_applicativeEither :: Qualified Ident
pattern I_applicativeEither = Qualified (ByModuleName (ModuleName "Either")) (Ident "applicativeEither")

pattern I_eitherMap :: Qualified Ident
pattern I_eitherMap = Qualified (ByModuleName (ModuleName "Either")) (Ident "map")

pattern I_eitherBind :: Qualified Ident
pattern I_eitherBind = Qualified (ByModuleName (ModuleName "Either")) (Ident "bind")

pattern I_eitherPure :: Qualified Ident
pattern I_eitherPure = Qualified (ByModuleName (ModuleName "Either")) (Ident "pure")

-- Erl.Data.List.Types
pattern I_functorList :: Qualified Ident
pattern I_functorList = Qualified (ByModuleName (ModuleName "Erl.Data.List.Types")) (Ident "functorList")

-- Maybe
pattern I_functorMaybe :: Qualified Ident
pattern I_functorMaybe = Qualified (ByModuleName (ModuleName "Maybe")) (Ident "functorMaybe")

pattern I_bindMaybe :: Qualified Ident
pattern I_bindMaybe = Qualified (ByModuleName (ModuleName "Maybe")) (Ident "bindMaybe")

pattern I_applicativeMaybe :: Qualified Ident
pattern I_applicativeMaybe = Qualified (ByModuleName (ModuleName "Maybe")) (Ident "applicativeMaybe")

pattern I_maybeMap :: Qualified Ident
pattern I_maybeMap = Qualified (ByModuleName (ModuleName "Maybe")) (Ident "map")

pattern I_maybePure :: Qualified Ident
pattern I_maybePure = Qualified (ByModuleName (ModuleName "Maybe")) (Ident "pure")

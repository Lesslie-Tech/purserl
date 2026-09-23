-- | Various constants which refer to things in Prim
module Language.PureScript.Constants.Prim where

import Protolude (Eq, IsString)
import Language.PureScript.Names (Ident(..), ModuleName(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..))

-- Prim
pattern M_Prim :: ModuleName
pattern M_Prim = ModuleName "Prim"

pattern Partial :: Qualified (ProperName 'ClassName)
pattern Partial = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Partial")

pattern Array :: Qualified (ProperName 'TypeName)
pattern Array = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Array")

pattern Boolean :: Qualified (ProperName 'TypeName)
pattern Boolean = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Boolean")

pattern Char :: Qualified (ProperName 'TypeName)
pattern Char = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Char")

pattern Constraint :: Qualified (ProperName 'TypeName)
pattern Constraint = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Constraint")

pattern Function :: Qualified (ProperName 'TypeName)
pattern Function = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Function")

pattern Int :: Qualified (ProperName 'TypeName)
pattern Int = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Int")

pattern Number :: Qualified (ProperName 'TypeName)
pattern Number = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Number")

pattern Record :: Qualified (ProperName 'TypeName)
pattern Record = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Record")

pattern Row :: Qualified (ProperName 'TypeName)
pattern Row = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Row")

pattern String :: Qualified (ProperName 'TypeName)
pattern String = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "String")

pattern Symbol :: Qualified (ProperName 'TypeName)
pattern Symbol = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Symbol")

pattern Type :: Qualified (ProperName 'TypeName)
pattern Type = Qualified (ByModuleName (ModuleName "Prim")) (ProperName "Type")

pattern S_undefined :: forall a. (Eq a, IsString a) => a
pattern S_undefined = "undefined"

pattern I_undefined :: Qualified Ident
pattern I_undefined = Qualified (ByModuleName (ModuleName "Prim")) (Ident "undefined")

-- Prim.Boolean
pattern M_Prim_Boolean :: ModuleName
pattern M_Prim_Boolean = ModuleName "Prim.Boolean"

pattern False :: Qualified (ProperName 'TypeName)
pattern False = Qualified (ByModuleName (ModuleName "Prim.Boolean")) (ProperName "False")

pattern True :: Qualified (ProperName 'TypeName)
pattern True = Qualified (ByModuleName (ModuleName "Prim.Boolean")) (ProperName "True")

-- Prim.Coerce
pattern M_Prim_Coerce :: ModuleName
pattern M_Prim_Coerce = ModuleName "Prim.Coerce"

pattern Coercible :: Qualified (ProperName 'ClassName)
pattern Coercible = Qualified (ByModuleName (ModuleName "Prim.Coerce")) (ProperName "Coercible")

-- Prim.Int
pattern M_Prim_Int :: ModuleName
pattern M_Prim_Int = ModuleName "Prim.Int"

pattern IntAdd :: Qualified (ProperName 'ClassName)
pattern IntAdd = Qualified (ByModuleName (ModuleName "Prim.Int")) (ProperName "Add")

pattern IntCompare :: Qualified (ProperName 'ClassName)
pattern IntCompare = Qualified (ByModuleName (ModuleName "Prim.Int")) (ProperName "Compare")

pattern IntMul :: Qualified (ProperName 'ClassName)
pattern IntMul = Qualified (ByModuleName (ModuleName "Prim.Int")) (ProperName "Mul")

pattern IntToString :: Qualified (ProperName 'ClassName)
pattern IntToString = Qualified (ByModuleName (ModuleName "Prim.Int")) (ProperName "ToString")

-- Prim.Ordering
pattern M_Prim_Ordering :: ModuleName
pattern M_Prim_Ordering = ModuleName "Prim.Ordering"

pattern TypeOrdering :: Qualified (ProperName 'TypeName)
pattern TypeOrdering = Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "Ordering")

pattern EQ :: Qualified (ProperName 'TypeName)
pattern EQ = Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "EQ")

pattern GT :: Qualified (ProperName 'TypeName)
pattern GT = Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "GT")

pattern LT :: Qualified (ProperName 'TypeName)
pattern LT = Qualified (ByModuleName (ModuleName "Prim.Ordering")) (ProperName "LT")

-- Prim.Row
pattern M_Prim_Row :: ModuleName
pattern M_Prim_Row = ModuleName "Prim.Row"

pattern RowCons :: Qualified (ProperName 'ClassName)
pattern RowCons = Qualified (ByModuleName (ModuleName "Prim.Row")) (ProperName "Cons")

pattern RowLacks :: Qualified (ProperName 'ClassName)
pattern RowLacks = Qualified (ByModuleName (ModuleName "Prim.Row")) (ProperName "Lacks")

pattern RowNub :: Qualified (ProperName 'ClassName)
pattern RowNub = Qualified (ByModuleName (ModuleName "Prim.Row")) (ProperName "Nub")

pattern RowUnion :: Qualified (ProperName 'ClassName)
pattern RowUnion = Qualified (ByModuleName (ModuleName "Prim.Row")) (ProperName "Union")

-- Prim.RowList
pattern M_Prim_RowList :: ModuleName
pattern M_Prim_RowList = ModuleName "Prim.RowList"

pattern RowList :: Qualified (ProperName 'TypeName)
pattern RowList = Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName "RowList")

pattern RowToList :: Qualified (ProperName 'ClassName)
pattern RowToList = Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName "RowToList")

pattern RowListCons :: Qualified (ProperName 'TypeName)
pattern RowListCons = Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName "Cons")

pattern RowListNil :: Qualified (ProperName 'TypeName)
pattern RowListNil = Qualified (ByModuleName (ModuleName "Prim.RowList")) (ProperName "Nil")

-- Prim.Symbol
pattern M_Prim_Symbol :: ModuleName
pattern M_Prim_Symbol = ModuleName "Prim.Symbol"

pattern SymbolAppend :: Qualified (ProperName 'ClassName)
pattern SymbolAppend = Qualified (ByModuleName (ModuleName "Prim.Symbol")) (ProperName "Append")

pattern SymbolCompare :: Qualified (ProperName 'ClassName)
pattern SymbolCompare = Qualified (ByModuleName (ModuleName "Prim.Symbol")) (ProperName "Compare")

pattern SymbolCons :: Qualified (ProperName 'ClassName)
pattern SymbolCons = Qualified (ByModuleName (ModuleName "Prim.Symbol")) (ProperName "Cons")

-- Prim.TypeError
pattern M_Prim_TypeError :: ModuleName
pattern M_Prim_TypeError = ModuleName "Prim.TypeError"

pattern Fail :: Qualified (ProperName 'ClassName)
pattern Fail = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Fail")

pattern Warn :: Qualified (ProperName 'ClassName)
pattern Warn = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Warn")

pattern Above :: Qualified (ProperName 'TypeName)
pattern Above = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Above")

pattern Beside :: Qualified (ProperName 'TypeName)
pattern Beside = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Beside")

pattern Doc :: Qualified (ProperName 'TypeName)
pattern Doc = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Doc")

pattern Quote :: Qualified (ProperName 'TypeName)
pattern Quote = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Quote")

pattern QuoteLabel :: Qualified (ProperName 'TypeName)
pattern QuoteLabel = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "QuoteLabel")

pattern Text :: Qualified (ProperName 'TypeName)
pattern Text = Qualified (ByModuleName (ModuleName "Prim.TypeError")) (ProperName "Text")

-- Backtrace
pattern M_Backtrace :: ModuleName
pattern M_Backtrace = ModuleName "Backtrace"

primModules :: [ModuleName]
primModules = [M_Prim, M_Prim_Boolean, M_Prim_Coerce, M_Prim_Ordering, M_Prim_Row, M_Prim_RowList, M_Prim_Symbol, M_Prim_Int, M_Prim_TypeError]

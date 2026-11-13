{-# LANGUAGE DeriveAnyClass #-}
-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.Names (Ident, OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified)
import Language.PureScript.Comments (Comment)
import Language.PureScript.Types (SourceType)
import Language.PureScript.PSString (PSString)

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder !SourceSpan !(Literal Binder)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder !SourceSpan !Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder !SourceSpan !(Qualified (ProperName 'ConstructorName)) [Binder]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder !SourceSpan !(Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder !Binder !Binder !Binder
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder !Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder !SourceSpan !Ident !Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder !SourceSpan ![Comment] !Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder !SourceType !Binder
  -- |
  -- Wildcard binder for a discarded do-notation statement. Matches anything,
  -- but the typechecker requires the matched value to be of type `Unit`.
  --
  | DiscardBinder !SourceSpan
  -- |
  -- Wildcard binder for an explicit @_ <- x@ do-notation statement. The
  -- typechecker rejects it when the matched value is of type `Unit`.
  --
  | UnitDiscardBinder !SourceSpan
  -- |
  -- A native list cons pattern, @head : tail@. Introduced when the built-in
  -- operator is resolved.
  --
  | ConsBinder !SourceSpan !Binder !Binder
  -- |
  -- A string prefix pattern, @"prefix" <> rest@. Introduced when the built-in
  -- operator is resolved.
  --
  | StringPrefixBinder !SourceSpan !PSString !Binder
  -- |
  -- A map entry pattern, @key := value@. The key is a literal.
  --
  | MapEntryBinder !SourceSpan !Binder !Binder
  -- |
  -- A pattern that has to match along with another one, @left & right@.
  --
  | AndBinder !SourceSpan !Binder !Binder
  deriving (Show, Generic, NFData)

-- Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- was expensive. This made exhaustiveness checking observably slow for code
-- such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- Custom instances were written to skip comparing the `SourceSpan` values. Only
-- the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- to have mismatched behavior.
instance Eq Binder where
  NullBinder == NullBinder =
    True
  (LiteralBinder _ lb) == (LiteralBinder _ lb') =
    lb == lb'
  (VarBinder _ ident) == (VarBinder _ ident') =
    ident == ident'
  (ConstructorBinder _ qpc bs) == (ConstructorBinder _ qpc' bs') =
    qpc == qpc' && bs == bs'
  (OpBinder _ qov) == (OpBinder _ qov') =
    qov == qov'
  (BinaryNoParensBinder b1 b2 b3) == (BinaryNoParensBinder b1' b2' b3') =
    b1 == b1' && b2 == b2' && b3 == b3'
  (ParensInBinder b) == (ParensInBinder b') =
    b == b'
  (NamedBinder _ ident b) == (NamedBinder _ ident' b') =
    ident == ident' && b == b'
  (PositionedBinder _ comments b) == (PositionedBinder _ comments' b') =
    comments == comments' && b == b'
  (TypedBinder ty b) == (TypedBinder ty' b') =
    ty == ty' && b == b'
  DiscardBinder _ == DiscardBinder _ =
    True
  UnitDiscardBinder _ == UnitDiscardBinder _ =
    True
  (ConsBinder _ h t) == (ConsBinder _ h' t') =
    h == h' && t == t'
  (StringPrefixBinder _ s b) == (StringPrefixBinder _ s' b') =
    s == s' && b == b'
  (MapEntryBinder _ k v) == (MapEntryBinder _ k' v') =
    k == k' && v == v'
  (AndBinder _ l r) == (AndBinder _ l' r') =
    l == l' && r == r'
  _ == _ = False

instance Ord Binder where
  compare NullBinder NullBinder = EQ
  compare (LiteralBinder _ lb) (LiteralBinder _ lb') =
    compare lb lb'
  compare (VarBinder _ ident) (VarBinder _ ident') =
    compare ident ident'
  compare (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    compare qpc qpc' <> compare bs bs'
  compare (OpBinder _ qov) (OpBinder _ qov') =
    compare qov qov'
  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare (ParensInBinder b) (ParensInBinder b') =
    compare b b'
  compare (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    compare ident ident' <> compare b b'
  compare (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    compare comments comments' <> compare b b'
  compare (TypedBinder ty b) (TypedBinder ty' b') =
    compare ty ty' <> compare b b'
  compare (ConsBinder _ h t) (ConsBinder _ h' t') =
    compare h h' <> compare t t'
  compare (StringPrefixBinder _ s b) (StringPrefixBinder _ s' b') =
    compare s s' <> compare b b'
  compare (MapEntryBinder _ k v) (MapEntryBinder _ k' v') =
    compare k k' <> compare v v'
  compare (AndBinder _ l r) (AndBinder _ l' r') =
    compare l l' <> compare r r'
  compare binder binder' =
    compare (orderOf binder) (orderOf binder')
      where
        orderOf :: Binder -> Int
        orderOf NullBinder = 0
        orderOf LiteralBinder{} = 1
        orderOf VarBinder{} = 2
        orderOf ConstructorBinder{} = 3
        orderOf OpBinder{} = 4
        orderOf BinaryNoParensBinder{} = 5
        orderOf ParensInBinder{} = 6
        orderOf NamedBinder{} = 7
        orderOf PositionedBinder{} = 8
        orderOf TypedBinder{} = 9
        orderOf DiscardBinder{} = 10
        orderOf ConsBinder{} = 11
        orderOf StringPrefixBinder{} = 12
        orderOf MapEntryBinder{} = 13
        orderOf UnitDiscardBinder{} = 15
        orderOf AndBinder{} = 14

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = map snd . binderNamesWithSpans

binderNamesWithSpans :: Binder -> [(SourceSpan, Ident)]
binderNamesWithSpans = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder ss name) = (ss, name) : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b) = go ns b
  go ns (NamedBinder ss name b) = go ((ss, name) : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns (ConsBinder _ h t) = foldl go ns [h, t]
  go ns (StringPrefixBinder _ _ b) = go ns b
  go ns (MapEntryBinder _ k v) = foldl go ns [k, v]
  go ns (AndBinder _ l r) = foldl go ns [l, r]
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns (ListLiteral bs) = foldl go ns bs
  lit ns _ = ns


isIrrefutable :: Binder -> Bool
isIrrefutable NullBinder = True
isIrrefutable (DiscardBinder _) = True
isIrrefutable (UnitDiscardBinder _) = True
isIrrefutable (VarBinder _ _) = True
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b) = isIrrefutable b
isIrrefutable _ = False

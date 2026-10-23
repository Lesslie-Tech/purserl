-- |
-- This module provides basic inlining capabilities
module Language.PureScript.Erl.CodeGen.Optimizer.Inliner
  ( inlineCommonValuesTopDown,
    inlineCommonValuesBottomUp,
    specialize,
    inlineCommonOperators,
    inlineCommonFnsM,
    evaluateIifes,
    etaConvert,
    singleBegin,
    beginBinds,
    collectLists,
    replaceAppliedFunRefs,
  )
where

import Control.Monad.Supply (Supply)
import Control.Monad.Supply.Class (MonadSupply (fresh))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript.Constants.Prim as C
import qualified Language.PureScript.Constants.Libs as C
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common (atomPS, runAtom)
import qualified Language.PureScript.Erl.CodeGen.Constants as EC
import Language.PureScript.Erl.CodeGen.Optimizer.Common
import Language.PureScript.PSString (PSString, mkString)
import Prelude.Compat
import Debug.Trace (trace, traceM)
import Language.PureScript.PSString qualified as PS
import Data.Function ((&))
import Debug.Trace qualified as Debug

-- | The source text of a `CodeGen.erlang` template: the string literal's
-- contents as they are, so the template can contain any escaped atom.
rawTemplate :: PSString -> Text
rawTemplate fmt = fromMaybe (PS.prettyPrintString fmt & T.drop 1 & T.dropEnd 1) (PS.decodeString fmt)

isEVar :: Erl -> Bool
isEVar (EVar _) = True
isEVar _ = False

unEVar :: Erl -> Maybe Text
unEVar (EVar x) = Just x
unEVar _ = Nothing

-- inline as we generate this for FFI calls
-- begin X = E, X(A)(B)... end
singleBegin :: Erl -> Erl
singleBegin = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EBlock [EVarBind x e, e2])
      | happy x e2 =
        replaceIdents [(x, e)] e2
    convert e = e

    happy x (EVar x') | x == x' = True
    happy x (EApp _ e [EVar y]) | x /= y = happy x e
    happy _ _ = False

beginBinds :: Erl -> Erl
beginBinds = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EApp meta (EBlock es) args)
      | e : evars <- reverse es,
        all (okBind args) evars =
        EBlock (reverse evars ++ [EApp meta e args])
    convert other = other

    okBind args ebind =
      all isEVar args
        && ( case ebind of
               (EVarBind x e) ->
                 not (x `Set.member` argVars)
                   && all (\y -> not $ occurs y e) argVars
               _ -> False
           )
      where
        argVars = Set.fromList $ mapMaybe unEVar args

-- (begin X1 = E1, ... Xn = En, F end)(X)
-- to
-- begin X1 = E1, ... Xn = En, F(X) end
-- X /= Xi, X \notin FV(Ei)

etaConvert :: Erl -> Supply Erl
etaConvert = everywhereOnErlTopDownM convert
  where
    convert :: Erl -> Supply Erl
    -- TODO ported from JS, but this seems to be beta-reduction and the iife below is eta...?
    convert (EApp _ (EFunN _ xs e) args)
      | all isEVar args,
        xs `disjoint` mapMaybe unEVar args,
        not (any (`isRebound` e) xs),
        not (any (`isReboundE` e) args) =
        renameBoundVars $ replaceIdents (zip xs args) e
    convert e = pure e

    disjoint l1 l2 =
      Set.null $ s1 `Set.intersection` s2
      where
        s1 = Set.fromList l1
        s2 = Set.fromList l2

    isReboundE (EVar x) e = isRebound x e
    isReboundE _ _ = False

-- TODO: That's not iifes
-- \x. (f x) --eta--> f  (x \notelem FV(f))
-- -- fun (X) -> fun {body} end(X) end  --> fun {body} end
evaluateIifes :: Erl -> Erl
evaluateIifes = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert (EFun1 Nothing x (EApp _ fun@EFunFull {} [EVar x'])) | x == x', not (occurs x fun) = fun
    convert e = e

collectLists :: Erl -> Erl
collectLists = everywhereOnErl go
  where
    go (EListCons xs (EListLiteral ys)) = EListLiteral (xs <> ys)
    go (EListCons xs (EListCons ys z)) = EListCons (xs <> ys) z
    go (EBinary ListConcat (EListLiteral xs) (EListLiteral ys)) = EListLiteral (xs <> ys)
    go (EBinary ListConcat (EListLiteral xs) (EListCons ys z)) = EListCons (xs <> ys) z
    go other = other

replaceAppliedFunRefs :: Erl -> Erl
replaceAppliedFunRefs = everywhereOnErl go
  where
    go (EApp meta (EFunRef name arity) args)
      | length args == arity =
        EApp meta (EAtomLiteral name) args
    go other = other


inlineCommonValuesBottomUp :: (Erl -> Erl) -> Erl -> Erl
inlineCommonValuesBottomUp expander = everywhereOnErl convert
  where
    convert :: Erl -> Erl
    convert expr =
      case expander expr of
        EApp _ fn [dict]
          | isDict semiringInt dict && isUncurriedFn fnZero fn -> EIntLit 0
          | isDict semiringNumber dict && isUncurriedFn fnZero fn -> ENumericLiteral (Right 0.0)
          | isDict semiringInt dict && isUncurriedFn fnOne fn -> EIntLit 1
          | isDict semiringNumber dict && isUncurriedFn fnOne fn -> ENumericLiteral (Right 1.0)
          | isDict boundedBoolean dict && isUncurriedFn fnBottom fn -> EAtomLiteral $ Atom Nothing "false"
          | isDict boundedBoolean dict && isUncurriedFn fnTop fn -> EAtomLiteral $ Atom Nothing "true"

        -- drathier added, functions rather than dicts
        EApp _ fn [a]
          | isFnName (EC.effect, snd C.P_effectPureE) fn -> EFun0 Nothing a

        EApp _ fn [a, f]
          | isFnName (EC.effect, snd C.P_effectBindE) fn ->
            EFun0 Nothing
              (EApp RegularApp
                (EApp RegularApp f
                  [ EApp RegularApp a []
                  ]
                )
                []
              )

        EApp app (EFunRef atom i) args | i == length args ->
          EApp app (EAtomLiteral atom) args

        EFunFull Nothing [(EFunBinder vars, EApp RegularApp (EAtomLiteral atom) args)] | vars == args ->
          EFunRef atom (length vars)

        EListCons xs (EListLiteral ys) -> EListLiteral (xs <> ys)
        EListCons xs (EListCons ys z) -> EListCons (xs <> ys) z
        EBinary ListConcat (EListLiteral xs) (EListLiteral ys) -> EListLiteral (xs <> ys)
        EBinary ListConcat (EListLiteral xs) (EListCons ys z) -> EListCons (xs <> ys) z

        ELet (EBind a b) c | a == c -> b

        EApp {} | Just e <- applySpine expr -> convert e

        ECaseOf cond [(EBinder pat, rhs)] | pat == rhs ->
          cond

        ECaseOf cond [(EBinder pat, rhs)] | pat == cond ->
          rhs

        ECaseOf cond [(EBinder pat, rhs)] | [] <- varsInExpr pat ->
          rhs

        ECaseOf cond [(EBinder pat, rhs)] ->
          convert $ ELet (EBind pat cond) rhs

        ELet (EBind (ETupleLiteral pats) (ETupleLiteral rhs)) body ->
          letBindPats pats rhs body

        fn
          | isFn (EC.dataUnit, EC.unit) fn -> EAtomLiteral $ Atom Nothing "unit"
          | isFn ("map@ps", "empty") fn -> EMapLiteral []
          | isFn ("list@ps", "empty") fn -> EListLiteral []

        -- [drathier]: constant folding
        EBinary ArrayConcat (EArrayLiteral a) (EArrayLiteral b) -> EArrayLiteral (a <> b)
        EBinary BinaryConcat (EStringLiteral a) (EStringLiteral b) -> EStringLiteral (a <> b)

        EBinary Add (EIntLit a) (EIntLit b) -> EIntLit (a+b)
        EBinary Subtract (EIntLit a) (EIntLit b) -> EIntLit (a-b)
        EBinary Multiply (EIntLit a) (EIntLit b) -> EIntLit (a*b)
        -- [drathier]: don't want to get divides or remainders wrong, so skipping them here for now

        EBinary Add (ENumericLiteral (Right a)) (ENumericLiteral (Right b)) -> ENumericLiteral (Right (a+b))
        EBinary Subtract (ENumericLiteral (Right a)) (ENumericLiteral (Right b)) -> ENumericLiteral (Right (a-b))
        EBinary Multiply (ENumericLiteral (Right a)) (ENumericLiteral (Right b)) -> ENumericLiteral (Right (a*b))
        -- [drathier]: don't want to get divides or remainders wrong, so skipping them here for now

        -- float constants left in associative binops. Cases where both sides are constants have already been handled by previous constant folding cases.
        ENegate (EIntLit a) -> EIntLit (-a)
        ENegate (ENumLit a) -> ENumLit (-a)
        EBinary Subtract a (EIntLit b) -> convert $ EBinary Add a (EIntLit (-b))
        EBinary Subtract a b -> convert $ EBinary Add a (convert (ENegate b))
        EBinary Add (EBinary Add a b) c -> floatAssocInt Add a b c
        EBinary Multiply (EBinary Multiply a b) c -> floatAssocInt Multiply a b c

        EBinary AndAlso ETrue b -> b
        EBinary AndAlso a ETrue -> a
        EBinary AndAlso EFalse _ -> EFalse
        EBinary AndAlso _ EFalse -> EFalse

        EBinary OrElse EFalse b -> b
        EBinary OrElse a EFalse -> a
        EBinary OrElse ETrue _ -> ETrue
        EBinary OrElse _ ETrue -> ETrue

        other -> other

    floatAssocInt op a b c =
      let noop = EBinary op (EBinary op a b) c
          inner x y = convert (EBinary op x y)
      in
      case (a,b,c) of
        (EIntLit _, EIntLit _, EIntLit _) -> noop
        (EIntLit _, EIntLit _, _) -> noop
        (EIntLit _, _, EIntLit _) -> EBinary op (inner a c) b
        (_, EIntLit _, EIntLit _) -> EBinary op (inner b c) a
        (EIntLit _, _, _) -> noop
        (_, EIntLit _, _) -> EBinary op (inner b a) c
        (_, _, EIntLit _) -> EBinary op (inner c a) b
        (_, _, _) -> noop


    fnZero = (EC.dataSemiring, snd C.P_zero)
    fnOne = (EC.dataSemiring, snd C.P_one)
    fnBottom = (EC.dataBounded, snd C.P_bottom)
    fnTop = (EC.dataBounded, snd C.P_top)

inlineCommonValuesTopDown :: (Erl -> Erl) -> Erl -> Erl
inlineCommonValuesTopDown expander = everywhereOnErlTopDown convert
  where
    convert :: Erl -> Erl
    convert expr =
      case expander expr of

--        EApp _ (EFun1 Nothing var1 (EApp _ (EFun1 Nothing var2 (EApp _ (EFun1 Nothing var3 (EApp _ (EFun1 Nothing var4 (EApp _ (EFun1 Nothing var5 body) [arg5])) [arg4])) [arg3])) [arg2])) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, EVarBind var4 arg4, EVarBind var5 arg5, convert body]
--        EApp _ (EFun1 Nothing var1 (EApp _ (EFun1 Nothing var2 (EApp _ (EFun1 Nothing var3 (EApp _ (EFun1 Nothing var4 body) [arg4])) [arg3])) [arg2])) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, EVarBind var4 arg4, convert body]
--        EApp _ (EFun1 Nothing var1 (EApp _ (EFun1 Nothing var2 (EApp _ (EFun1 Nothing var3 body) [arg3])) [arg2])) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, convert body]
--        EApp _ (EFun1 Nothing var1 (EApp _ (EFun1 Nothing var2 body) [arg2])) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, convert body]
--        EApp _ (EFun1 Nothing var1 body) [arg1] -> EBlock [EVarBind var1 arg1, convert body]

--        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 (EFun1 Nothing var5 body))))) [arg5]) [arg4]) [arg3]) [arg2]) [arg1] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5)] (convert body)
--        EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 body)))) [arg4]) [arg3]) [arg2]) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, EVarBind var4 arg4, convert body]
--        EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 body))) [arg3]) [arg2]) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, convert body]
--        EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 body)) [arg2]) [arg1] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, convert body]
--        EApp _ (EFun1 Nothing var1 body) [arg1] -> EBlock [EVarBind var1 arg1, convert body]

        EApp _ (EFun0 _ body) [] -> body

{-
        -- [drathier]: needlessly _@123-wrapped top-level functions, take the inner var and use that instead of the _@123 var.
        -- TODO[drathier]: perf: these only ever apply at top-level, so no need to check for them while recursing. They won't apply for let-bound functions either, as EFunctionDef is only valid at top-level. Otherwise it's an EFunFull.
        EFunctionDef mType mSS name [var1a, var2a, var3a, var4a, var5a] (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1i (EFun1 Nothing var2i (EFun1 Nothing var3i (EFun1 Nothing var4i (EFun1 Nothing var5i body))))) [EVar var1b]) [EVar var2b]) [EVar var3b]) [EVar var4b]) [EVar var5b]) | var1a == var1b, var2a == var2b, var3a == var3b, var4a == var4b, var5a == var5b -> EFunctionDef mType mSS name [var1i, var2i, var3i, var4i, var5i] (letBindIdents [(var1a, EVar var1i), (var1b, EVar var1i), (var2a, EVar var2i), (var2b, EVar var2i), (var3a, EVar var3i), (var3b, EVar var3i), (var4a, EVar var4i), (var4b, EVar var4i), (var5a, EVar var5i), (var5b, EVar var5i)] body) -- TODO[drathier]: this clause is untested, only tested 1-3 args
        EFunctionDef mType mSS name [var1a, var2a, var3a, var4a] (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1i (EFun1 Nothing var2i (EFun1 Nothing var3i (EFun1 Nothing var4i body)))) [EVar var1b]) [EVar var2b]) [EVar var3b]) [EVar var4b]) | var1a == var1b, var2a == var2b, var3a == var3b, var4a == var4b -> EFunctionDef mType mSS name [var1i, var2i, var3i, var4i] (letBindIdents [(var1a, EVar var1i), (var1b, EVar var1i), (var2a, EVar var2i), (var2b, EVar var2i), (var3a, EVar var3i), (var3b, EVar var3i), (var4a, EVar var4i), (var4b, EVar var4i)] body) -- TODO[drathier]: this clause is untested, only tested 1-3 args
        EFunctionDef mType mSS name [var1a, var2a, var3a] (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1i (EFun1 Nothing var2i (EFun1 Nothing var3i body))) [EVar var1b]) [EVar var2b]) [EVar var3b]) | var1a == var1b, var2a == var2b, var3a == var3b -> EFunctionDef mType mSS name [var1i, var2i, var3i] (letBindIdents [(var1a, EVar var1i), (var1b, EVar var1i), (var2a, EVar var2i), (var2b, EVar var2i), (var3a, EVar var3i), (var3b, EVar var3i)] body)
        EFunctionDef mType mSS name [var1a, var2a] (EApp _ (EApp _ (EFun1 Nothing var1i (EFun1 Nothing var2i body)) [EVar var1b]) [EVar var2b]) | var1a == var1b, var2a == var2b -> EFunctionDef mType mSS name [var1i, var2i] (letBindIdents [(var1a, EVar var1i), (var1b, EVar var1i), (var2a, EVar var2i), (var2b, EVar var2i)] body)
        EFunctionDef mType mSS name [var1a] (EApp _ (EFun1 Nothing var1i body) [EVar var1b]) | var1a == var1b -> EFunctionDef mType mSS name [var1i] (letBindIdents [(var1a, EVar var1i), (var1b, EVar var1i)] body)
-}

        -- [drathier]: immediately called funs, let-bind their vars
        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 (EFun1 _ var5 (EFun1 _ var6 (EFun1 _ var7 (EFun1 _ var8 body)))))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5]) [arg6]) [arg7] ) [arg8] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5), (var6, arg6), (var7, arg7), (var8, arg8)] body
        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 (EFun1 _ var5 (EFun1 _ var6 (EFun1 _ var7 body))))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5]) [arg6]) [arg7] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5), (var6, arg6), (var7, arg7)] body
        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 (EFun1 _ var5 (EFun1 _ var6 body)))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5]) [arg6] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5), (var6, arg6)] body
        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 (EFun1 _ var5 body))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5)] body
        EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 body)))) [arg1]) [arg2]) [arg3]) [arg4] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4)] body
        EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 body))) [arg1]) [arg2]) [arg3] -> letBindIdents [(var1, arg1), (var2, arg2), (var3, arg3)] body
        EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 body)) [arg1]) [arg2] -> letBindIdents [(var1, arg1), (var2, arg2)] body
        EApp _ (EFun1 _ var1 body) [arg1] -> letBindIdents [(var1, arg1)] body

--        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 (EFun1 _ var5 body))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5)] body
--        EApp _ (EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 (EFun1 _ var4 body)))) [arg1]) [arg2]) [arg3]) [arg4] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4)] body
--        EApp _ (EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 (EFun1 _ var3 body))) [arg1]) [arg2]) [arg3] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3)] body
--        EApp _ (EApp _ (EFun1 _ var1 (EFun1 _ var2 body)) [arg1]) [arg2] -> replaceIdents [(var1, arg1), (var2, arg2)] body
--        EApp _ (EFun1 _ var1 body) [arg1] -> replaceIdents [(var1, arg1)] body

        -- [drathier]: technically the `Just _` version might not be safe, but at time of writing that the function name is never used. TODO[drathier]: change guard code gen to generate anonymous functions instead of functions named `GUARD`.
--        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 (EFun1 Nothing var5 body))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4), (var5, arg5)] body
--        EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 body)))) [arg1]) [arg2]) [arg3]) [arg4] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3), (var4, arg4)] body
--        EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 body))) [arg1]) [arg2]) [arg3] -> replaceIdents [(var1, arg1), (var2, arg2), (var3, arg3)] body
--        EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 body)) [arg1]) [arg2] -> replaceIdents [(var1, arg1), (var2, arg2)] body
--        EApp _ (EFun1 Nothing var1 body) [arg1] -> replaceIdents [(var1, arg1)] body


        -- TODO[drathier]: occours check needed here or not? yes
        -- EFunctionDef _ _ _ vars (EApp _ body args) | map EVar vars == args -> body
        -- [drathier]: Skipping because it traverses body: -- EFunFull _ [(EFunBinder vars, (EApp _ body args))] | not (isEAtomLiteral body), vars == args, isAnyMentioned (concatMap varsInExpr vars) (varsInExpr body) == False -> body

        EApp _ (EAtomLiteral (Atom (Just "maps") "get")) [EAtomLiteral key, EMapLiteral fields] | Just v <- findKey (runAtom key) fields -> v
        -- EMapLiteral fields | Just rhs <- allFieldsAreMapGetSame Nothing fields -> rhs -- [drathier]: this optimization broke prod

        EApp {} | Just e <- applySpine expr -> e

        -- EFunFull Nothing [(EFunBinder [EVar "_@251"],EApp RegularApp (EAtomLiteral (Atom Nothing "eqNewtypeRep_156")) [EVar "_@251"])]
        -- (EFunFull Nothing [(EFunBinder [EVar "_@251"],EVar "_@251")])

        -- EFunctionDef Nothing (Just ss) (Atom Nothing "eqNewtypeRep_156") [] (EFunFull Nothing [(EFunBinder [EVar "_@251"],EVar "_@251")]),
        -- EFunctionDef Nothing (Just ss) (Atom Nothing "eqNewtypeRep_156") [] (EAtomLiteral (Atom Nothing "eqNewtypeRep_156")),

-- TODO[drathier]: replace this pattern match with pattern unwrapping? i.e. replace EFun1 EVar with FunDef EBind?
--        EApp _ (EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 (EFun1 Nothing var5 body))))) [arg1]) [arg2]) [arg3]) [arg4]) [arg5] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, EVarBind var4 arg4, EVarBind var5 arg5, body]
--        EApp _ (EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 (EFun1 Nothing var4 body)))) [arg1]) [arg2]) [arg3]) [arg4] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, EVarBind var4 arg4, body]
--        EApp _ (EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 (EFun1 Nothing var3 body))) [arg1]) [arg2]) [arg3] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, EVarBind var3 arg3, body]
--        EApp _ (EApp _ (EFun1 Nothing var1 (EFun1 Nothing var2 body)) [arg1]) [arg2] -> EBlock [EVarBind var1 arg1, EVarBind var2 arg2, body]
--        EApp _ (EFun1 Nothing var1 body) [arg1] -> EBlock [EVarBind var1 arg1, body]


        -- EApp _ (EFunFull Nothing [(EFunBinder [pat1], body)]) [arg1] -> EBlock [EBind pat1 arg1, convert body]

        other -> other

specialize :: Erl -> Erl
specialize = everywhereOnErl onErl
  where
    onErl :: Erl -> Erl
    onErl expr =
      -- Debug.trace (show ("specialize.any", expr)) $
      case expr of
        -- NOTE[drathier]: type class instance apply is sometimes RegularApp and sometimes SyntheticApp for whatever currently unknown reason, so we're matching both here
        -- INVARIANT[drathier]: CSE has to let these through for us to get the chance to specialize them here. See purescript/src/Language/PureScript/CoreFn/CSE.hs:optimizeCommonSubexpressions.shouldFloatExpr

        -- CodeGen.erlang magic
        EApp RegularApp (EAtomLiteral (Atom (Just "codeGen@ps") "erlang")) [EStringLiteral fmt,EMapLiteral binds] ->
          -- [drathier]: trimming surrounding quotes. We don't worry about inline quotes, as we only support A-Za-z0-9_.
          ERawErlangSource (rawTemplate fmt) binds
        EApp RegularApp (EApp RegularApp (EApp RegularApp (EAtomLiteral (Atom (Just "codeGen@ps") "erlang")) []) [EStringLiteral fmt]) [EMapLiteral binds] ->
          -- [drathier]: trimming surrounding quotes. We don't worry about inline quotes, as we only support A-Za-z0-9_.
          ERawErlangSource (rawTemplate fmt) binds

        -- Int
        -- EApp3 _ (EAtomLiteral (Atom (Just "data_semiring@ps") "add")) (EApp _ (EAtomLiteral (Atom (Just "data_semiring@ps") inst)) []) a b | isInst inst "semiringInt" -> EBinary Add a b
        EApp1 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Add a b
        EApp2 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Add a b
        EApp3 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Add a b
        EApp1 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Multiply a b
        EApp2 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Multiply a b
        EApp3 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringInt" -> EBinary Multiply a b
        EApp1 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringInt" -> EBinary Subtract a b
        EApp2 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringInt" -> EBinary Subtract a b
        EApp3 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringInt" -> EBinary Subtract a b

        -- NOTE[drathier]: euclidian int div is not the same as erlang int div. The translation between them is quite complex, see data_euclideanRing@foreign.
        -- EApp1 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingInt" -> EBinary IDivide a b
        -- EApp2 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingInt" -> EBinary IDivide a b
        -- EApp3 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingInt" -> EBinary IDivide a b
        EApp1 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThan a b
        EApp2 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThan a b
        EApp3 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThan a b
        EApp1 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThanOrEqualTo a b
        EApp2 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThanOrEqualTo a b
        EApp3 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordInt" -> EBinary LessThanOrEqualTo a b
        EApp1 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqInt" -> EBinary EqualTo a b
        EApp2 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqInt" -> EBinary EqualTo a b
        EApp3 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqInt" -> EBinary EqualTo a b
        -- EApp _ (EAtomLiteral (Atom (Just "math@ps") "intRemainder")) [a,b] -> EBinary IRemainder a b

        -- Number
        EApp1 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Add a b
        EApp2 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Add a b
        EApp3 "data_semiring@ps" "add" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Add a b
        EApp1 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Multiply a b
        EApp2 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Multiply a b
        EApp3 "data_semiring@ps" "mul" "data_semiring@ps" inst a b | isInst inst "semiringNumber" -> EBinary Multiply a b
        EApp1 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringNumber" -> EBinary Subtract a b
        EApp2 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringNumber" -> EBinary Subtract a b
        EApp3 "data_ring@ps" "sub" "data_ring@ps" inst a b | isInst inst "ringNumber" -> EBinary Subtract a b
        EApp1 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingNumber" -> EBinary FDivide a b
        EApp2 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingNumber" -> EBinary FDivide a b
        EApp3 "data_euclideanRing@ps" "div" "data_euclideanRing@ps" inst a b | isInst inst "euclideanRingNumber" -> EBinary FDivide a b
        EApp1 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThan a b
        EApp2 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThan a b
        EApp3 "data_ord@ps" "lessThan" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThan a b
        EApp1 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThanOrEqualTo a b
        EApp2 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThanOrEqualTo a b
        EApp3 "data_ord@ps" "lessThanOrEq" "data_ord@ps" inst a b | isInst inst "ordNumber" -> EBinary LessThanOrEqualTo a b
        EApp1 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqNumber" -> EBinary EqualTo a b
        EApp2 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqNumber" -> EBinary EqualTo a b
        EApp3 "data_eq@ps" "eq" "data_eq@ps" inst a b | isInst inst "eqNumber" -> EBinary EqualTo a b
        EApp _ (EAtomLiteral (Atom (Just "math@ps") "remainder")) [a,b] -> EBinary FRemainder a b

        -- Boolean
        EApp1 "data_heytingAlgebra@ps" "conj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary AndAlso a b
        EApp2 "data_heytingAlgebra@ps" "conj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary AndAlso a b
        EApp3 "data_heytingAlgebra@ps" "conj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary AndAlso a b
        EApp1 "data_heytingAlgebra@ps" "disj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary OrElse a b
        EApp2 "data_heytingAlgebra@ps" "disj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary OrElse a b
        EApp3 "data_heytingAlgebra@ps" "disj" "data_heytingAlgebra@ps" inst a b | isInst inst "heytingAlgebraBoolean" -> EBinary OrElse a b

        -- String
        EApp1 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupString" -> EBinary BinaryConcat a b
        EApp2 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupString" -> EBinary BinaryConcat a b
        EApp3 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupString" -> EBinary BinaryConcat a b

        -- Array
        EApp1 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupArray" -> EBinary ArrayConcat a b
        EApp2 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupArray" -> EBinary ArrayConcat a b
        EApp3 "data_semigroup@ps" "append" "data_semigroup@ps" inst a b | isInst inst "semigroupArray" -> EBinary ArrayConcat a b

        -- List
        EApp1 "data_semigroup@ps" "append" "erl_data_list_types@ps" inst a b | isInst inst "semigroupList" -> EBinary ListConcat a b
        EApp2 "data_semigroup@ps" "append" "erl_data_list_types@ps" inst a b | isInst inst "semigroupList" -> EBinary ListConcat a b
        EApp3 "data_semigroup@ps" "append" "erl_data_list_types@ps" inst a b | isInst inst "semigroupList" -> EBinary ListConcat a b

        other -> other

    fnZero = (EC.dataSemiring, snd C.P_zero)
    fnOne = (EC.dataSemiring, snd C.P_one)
    fnBottom = (EC.dataBounded, snd C.P_bottom)
    fnTop = (EC.dataBounded, snd C.P_top)

isInst inst prefix = T.isPrefixOf prefix inst


letBindIdents :: [(Text, Erl)] -> Erl -> Erl
letBindIdents vars body =
  case vars of
    ("_",EVar _):rest -> letBindIdents rest body
    (_,EVar "_"):rest -> letBindIdents rest body
    (a,b):rest -> bindLet (EVar a) b (letBindIdents rest body)
    [] -> body

-- | @let a = b in a@ is just @b@. Aliasing a variable only to rename it is
-- dropped, for the body variables made by `wrapped` and for `_`.
bindLet :: Erl -> Erl -> Erl -> Erl
bindLet a b body
  | a == body = b
  | EVar "_" <- a, EVar _ <- b = body
  | EVar x <- a, EVar _ <- b, "Body@" `T.isPrefixOf` x = replaceIdents [(x, b)] body
  | otherwise = ELet (EBind a b) body

letBindPats :: [Erl] -> [Erl] -> Erl -> Erl
letBindPats pats rhs body =
  case (pats, rhs) of
    (EVar "_":prest, EVar _:brest) -> letBindPats prest brest body
    (EVar _:prest, EVar "_":brest) -> letBindPats prest brest body
    (a:prest,b:brest) -> bindLet a b (letBindPats prest brest body)
    ([],[]) -> body

findKey k fields =
  case fields of
    [] -> Nothing
    (k2,v):_ | k == runAtom k2 -> Just v
    _:rest -> findKey k rest

varsInExpr e =
  everything (<>)
    (\case
        EVar var -> [var]
        _ -> []
    ) e


data Binary
  = Binary (Text, PSString) (Text, PSString) BinaryOperator
  | BinaryFn (Text, PSString) (Text, PSString) (Erl -> Erl -> Erl)

data Unary = Unary (Text, PSString) (Text, PSString) UnaryOperator

inlineCommonFnsM :: (Erl -> Maybe PSString) -> Erl -> Supply Erl
inlineCommonFnsM symbolOf = everywhereOnErlTopDownM (inlineLibFns symbolOf libRules)

inlineCommonOperators :: Text -> EC.EffectDictionaries -> (Erl -> Erl) -> Erl -> Erl
inlineCommonOperators effectModule EC.EffectDictionaries {..} expander =
  everywhereOnErlTopDown $
    applyAll
      [ binaryOps expander,
        unaryOps expander,
        inlineNonClassFunction ("list@ps", "cons") $ \x xs -> EListCons [x] xs,
        inlineNonClassUnaryFunction ("list@ps", "isEmpty") $ \x -> EBinary EqualTo (EListLiteral []) x,
        inlineNonClassUnaryFunction ("list@ps", "isNotEmpty") $ \x -> EBinary NotEqualTo (EListLiteral []) x,
        inlineNonClassUnaryFunction ("list@ps", "singleton") $ \x -> EListLiteral [x],
        inlineNonClassFunction ("list@ps", "append") $ \xs ys -> EBinary ListConcat xs ys,
        inlineNonClassUnaryFunction ("list@ps", "length") $ \x -> EApp RegularApp erlangLength [x],
        inlineNonClassUnaryFunction ("list@ps", "reverse") $ \x -> EApp RegularApp erlangListsReverse [x],
        inlineNonClassUnaryFunction ("list@ps", "concat") $ \x -> EApp RegularApp erlangListsAppend1 [x],
        inlineNonClassUnaryFunction ("list@ps", "unsafeHead") $ \x -> EApp RegularApp erlangHd [x],
        inlineNonClassFunction ("list@ps", "map") $ \f xs -> EApp RegularApp erlangListsMap [f, xs],
        -- Local `E` wrappers around Effect's primitives: an Effect is a nullary fun.
        inlineNonClassUnaryFunction ("e@ps", "pure") $ \x -> EFun0 Nothing x,
        inlineNonClassFunction ("e@ps", "bind") $ \a f -> effectBind a f,
        inlineNonClassFunction ("e@ps", "andThen") $ \f a -> effectBind a f,
        inlineNonClassFunction ("e@ps", "map") $ \f a -> EFun0 Nothing (EApp RegularApp f [runEffect a]),
        -- Same shape as Effect.applyE, so evaluation order is unchanged.
        inlineNonClassFunction ("e@ps", "apply") $ \f a -> EFun0 Nothing (EApp RegularApp (runEffect f) [runEffect a]),
        inlineNonClassUnaryFunction ("e@ps", "join") $ \a -> EFun0 Nothing (EApp RegularApp (runEffect a) []),
        inlineArrayFromList,
        inlineNonClassUnaryFunction ("array@ps", "size") $ \x -> EApp RegularApp erlangArraySize [x],
        -- `Array.map`/`Array.mapWithIndex`'s own FFI already wraps `F` in an
        -- adapter closure to match `array:map/2`'s (Index, Elem) -> Elem shape;
        -- build that same adapter inline instead of calling through the FFI hop.
        inlineNonClassFunction ("array@ps", "map") $ \f arr ->
          EApp RegularApp erlangArrayMap [EFunN Nothing ["_", "_X@arrMap"] (EApp RegularApp f [EVar "_X@arrMap"]), arr],
        inlineNonClassFunction ("array@ps", "mapWithIndex") $ \f arr ->
          EApp RegularApp erlangArrayMap [EFunN Nothing ["_I@arrMap", "_X@arrMap"] (EApp RegularApp (EApp RegularApp f [EVar "_I@arrMap"]) [EVar "_X@arrMap"]), arr],
        inlineNonClassUnaryFunction ("map@ps", "isEmpty") $ \x -> EBinary EqualTo (EMapLiteral []) x,
        inlineNonClassUnaryFunction ("map@ps", "isNotEmpty") $ \x -> EBinary NotEqualTo (EMapLiteral []) x,
        inlineNonClassFunction ("map@ps", "member") $ \k m -> EApp RegularApp erlangMapsIsKey [k, m],
        inlineNonClassFunction ("map@ps", "delete") $ \k m -> EApp RegularApp erlangMapsRemove [k, m],
        inlineNonClassUnaryFunction ("map@ps", "keys") $ \m -> EApp RegularApp erlangMapsKeys [m],
        inlineNonClassUnaryFunction ("map@ps", "values") $ \m -> EApp RegularApp erlangMapsValues [m],
        inlineNonClassUnaryFunction ("map@ps", "size") $ \m -> EApp RegularApp erlangMapsSize [m],
        inlineNonClassFunction ("map@ps", "mapWithIndex") $ \f m ->
          EApp RegularApp erlangMapsMap [EFunN Nothing ["_K@mapMap", "_V@mapMap"] (EApp RegularApp (EApp RegularApp f [EVar "_K@mapMap"]) [EVar "_V@mapMap"]), m],
        inlineNonClassFunction3 ("map@ps", "insert") $ \k v m -> EApp RegularApp erlangMapsPut [k, v, m],
        inlineNonClassFunction3 ("map@ps", "unsafeUpdate") $ \k v m -> EApp RegularApp erlangMapsUpdate [k, v, m],
        -- Note: argument order swap preserved from Map's own FFI -- erlang's
        -- maps:merge/2 is right-biased, but Map.union is left-biased.
        inlineNonClassFunction ("map@ps", "union") $ \m1 m2 -> EApp RegularApp erlangMapsMerge [m2, m1],
        inlineArrayToList,
        inlineErlAtom,
        unaryFn (effectModule, edFunctor) functorVoid id,
        inlineNonClassUnaryFunction (EC.unsafeCoerceMod, EC.unsafeCoerce) id,
        inlineNonClassUnaryFunction (EC.unsafeMod, EC.cast) id,
        inlineNonClassUnaryFunction (EC.foreignMod, EC.cast) id,
        inlineNonClassUnaryFunction (EC.foreignMod, EC.unsafeToForeign) id,
        inlineNonClassUnaryFunction (EC.foreignMod, EC.unsafeFromForeign) id,
        inlineNonClassUnaryFunction (EC.effectUnsafe, EC.unsafePerformEffect) $ \x -> EApp RegularApp x [],
        inlineNonClassUnaryFunction ("int@ps", "toNumber") $ \x -> EApp RegularApp erlangFloat [x],
        unaryUndefTCFn (EC.safeCoerceMod, EC.coerce) id,
        -- The Newtype dictionary is erased, so these are plain calls.
        inlineNonClassUnaryFunction (EC.dataNewtype, EC.unwrap) id,
        inlineNonClassUnaryFunction (EC.dataNewtype, EC.wrap) id,
        inlineNonClassFunction (EC.dataNewtype, "un") $ \_ x -> x,
        -- `over c f` is `f` at runtime; extra saturated args are applied directly.
        inlineNonClassFunction (EC.dataNewtype, EC.over) $ \_ f -> f,
        inlineNonClassFunction (EC.dataNewtype, EC.over2) $ \_ f -> f,
        overSaturated (EC.dataNewtype, EC.over) 1,
        overSaturated (EC.dataNewtype, EC.over2) 2,
        onNFn expander,
        onTupleN
      ]
  where
    unaryFn :: (Text, PSString) -> (Text, PSString) -> (Erl -> Erl) -> Erl -> Erl
    unaryFn dicts fns f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ (EApp _ fn []) [dict]) [x]) | isDict dicts dict && isFnName fns fn = f x
        convert (EApp _ (EApp _ fn [dict]) [x]) | isDict dicts dict && isFnName fns fn = f x
        convert (EApp _ fn [dict, x]) | isFnName dicts dict && isFnName fns fn = f x
        convert other = other

    unaryUndefTCFn :: (Text, PSString) -> (Erl -> Erl) -> Erl -> Erl
    unaryUndefTCFn fns f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (expander -> EApp _ fn [undef]) [x]) | isUndef undef && isFnName fns fn = f x
        convert (EApp _ fn [undef, x]) | isUndef undef && isFnName fns fn = f x
        convert other = other

        isUndef (EAtomLiteral atom) | runAtom atom == C.S_undefined = True
        isUndef _ = False

    overSaturated :: (Text, Text) -> Int -> Erl -> Erl
    overSaturated modFn n = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' (_ : f : args)) | length args == n, isUncurriedFn' modFn op' = foldl (\g a -> EApp RegularApp g [a]) f args
        convert other = other

    inlineNonClassFunction :: (Text, Text) -> (Erl -> Erl -> Erl) -> Erl -> Erl
    inlineNonClassFunction modFn f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ op' [x]) [y]) | isModFn modFn op' = f x y
        convert (EApp _ op' [x, y]) | isUncurriedFn' modFn op' = f x y
        convert other = other

    inlineNonClassFunction3 :: (Text, Text) -> (Erl -> Erl -> Erl -> Erl) -> Erl -> Erl
    inlineNonClassFunction3 modFn f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ (EApp _ (EApp _ op' [x]) [y]) [z]) | isModFn modFn op' = f x y z
        convert (EApp _ op' [x, y, z]) | isUncurriedFn' modFn op' = f x y z
        convert other = other

    inlineNonClassUnaryFunction :: (Text, Text) -> (Erl -> Erl) -> Erl -> Erl
    inlineNonClassUnaryFunction modFn f = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [x]) | isModFn modFn op' = f x
        convert (EApp _ op' [x]) | isUncurriedFn' modFn op' = f x
        convert other = other

    -- Local Array/List modules wrap array:to_list/1 in an FFI hop purely for
    -- the array<->list conversion; skip straight to the OTP call, and skip
    -- the conversion entirely when the source is already a literal array.
    isArrayToListCall :: Erl -> Bool
    isArrayToListCall op' =
      isModFn ("array@ps", "toList") op'
        || isUncurriedFn' ("array@ps", "toList") op'
        || isModFn ("list@ps", "fromArray") op'
        || isUncurriedFn' ("list@ps", "fromArray") op'

    -- Note: `f $ x` / `x # f` (Data.Function.apply/applyFlipped) are already
    -- beta-reduced to a plain call at the CoreFn level (see
    -- optimizeDataFunctionApply in Language.PureScript.CoreFn.Optimizer),
    -- long before this pass runs, so no special-casing is needed for them here.
    inlineArrayToList :: Erl -> Erl
    inlineArrayToList = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [EArrayLiteral es]) | isArrayToListCall op' = EListLiteral es
        convert (EApp _ op' [x]) | isArrayToListCall op' = EApp RegularApp erlangArrayToList [x]
        convert other = other

    -- The reverse conversion (list -> array) has no equivalent literal fold:
    -- EArrayLiteral es already pretty-prints to exactly array:from_list([es...])
    -- (Pretty.hs), so there's no second operation to cancel out the way
    -- toList/fromArray on a literal array does -- just skip the FFI hop.
    isArrayFromListCall :: Erl -> Bool
    isArrayFromListCall op' =
      isModFn ("array@ps", "fromList") op'
        || isUncurriedFn' ("array@ps", "fromList") op'
        || isModFn ("list@ps", "toArray") op'
        || isUncurriedFn' ("list@ps", "toArray") op'

    inlineArrayFromList :: Erl -> Erl
    inlineArrayFromList = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [x]) | isArrayFromListCall op' = EApp RegularApp erlangArrayFromList [x]
        convert other = other

    inlineErlAtom :: Erl -> Erl
    inlineErlAtom = convert
      where
        convert :: Erl -> Erl
        convert (EApp _ op' [EStringLiteral s])
          | isModFn (EC.erlAtom, EC.atom) op'
              || isUncurriedFn' (EC.erlAtom, EC.atom) op' =
            EAtomLiteral (AtomPS Nothing s)
        convert other = other

    isModFn :: (Text, Text) -> Erl -> Bool
    isModFn = isFn

    runEffect :: Erl -> Erl
    runEffect e = EApp RegularApp e []

    effectBind :: Erl -> Erl -> Erl
    effectBind a f = EFun0 Nothing (EApp RegularApp (EApp RegularApp f [runEffect a]) [])

binaryOps :: (Erl -> Erl) -> Erl -> Erl
binaryOps expander = \case
  eapp@(EApp _ fn [dict, opArg1, opArg2])
    | Just op <- getOp fn dict ->
      res op opArg1 opArg2
    | otherwise ->
      eapp
  EApp _ (EApp _ (expander -> EApp _ fn [dict]) [opArg1]) [opArg2]
    | Just op <- getOp fn dict ->
      res op opArg1 opArg2
  other -> other
  where
    res :: Either BinaryOperator (Erl -> Erl -> Erl) -> Erl -> Erl -> Erl
    res (Left op) = EBinary op
    res (Right f) = f

    getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp _ (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
      Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) binaryOperators
    getOp _ _ = Nothing

unaryOps :: (Erl -> Erl) -> Erl -> Erl
unaryOps expander = \case
  eapp@(EApp _ fn [dict, opArg])
    | Just op <- getOp fn dict ->
      EUnary op opArg
    | otherwise -> eapp
  EApp _ (expander -> EApp _ fn [dict]) [opArg]
    | Just op <- getOp fn dict ->
      EUnary op opArg
  other -> other
  where
    getOp (EAtomLiteral (Atom (Just moduleName) fnName)) (EApp _ (EAtomLiteral (Atom (Just dictModuleName) dictName)) []) =
      Map.lookup ((dictModuleName, dictName), (moduleName, fnName)) unaryOperators
    getOp _ _ = Nothing

onNFn :: (Erl -> Erl) -> Erl -> Erl
onNFn expander = convert
  where
    convert (EApp _ mkFnN [EFun1 Nothing _ e])
      | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN,
        Just (MkFnN 0 res) <- Map.lookup (mkMod, mkFun) fnNs =
        res [] e
    convert (EApp _ mkFnN [fn])
      | (EAtomLiteral (Atom (Just mkMod) mkFun)) <- mkFnN,
        Just (MkFnN n res) <- Map.lookup (mkMod, mkFun) fnNs,
        Just (args, e) <- collectArgs n n [] fn =
        res args e
    convert (expander -> EApp _ runFnN (fn : args))
      | (EAtomLiteral (Atom (Just runMod) runFun)) <- runFnN,
        Just (RunFnN n res) <- Map.lookup (runMod, runFun) fnNs,
        length args == n =
        res fn args
    convert other = other

    collectArgs :: Int -> Int -> [Text] -> Erl -> Maybe ([Text], Erl)
    collectArgs n 1 acc (EFun1 Nothing arg e) | length acc == n - 1 = Just (reverse (arg : acc), e)
    collectArgs n m acc (EFun1 Nothing arg e) = collectArgs n (m - 1) (arg : acc) e
    collectArgs _ _ _ _ = Nothing

data FnNRes = MkFnN Int ([Text] -> Erl -> Erl) | RunFnN Int (Erl -> [Erl] -> Erl)

fnNs :: Map.Map (Text, Text) FnNRes
fnNs =
  Map.fromList $
    [ fn | i <- [0 .. 10], fn <-
                             [ ((EC.dataFunctionUncurried, name C.S_mkFn i), MkFnN i $ \args e -> EFunN Nothing args e),
                               ((EC.effectUncurried, name (snd C.P_mkEffectFn) i), MkFnN i $ \args e -> EFunN Nothing args (EApp RegularApp e []))
                             ]
    ]
      ++ [ fn | i <- [1 .. 10], fn <-
                                  [ ((EC.dataFunctionUncurried, name C.S_runFn i), RunFnN i (EApp RegularApp)),
                                    ((EC.effectUncurried, name (snd C.P_runEffectFn) i), RunFnN i $ \fn acc -> EFun0 Nothing (EApp RegularApp fn acc))
                                  ]
         ]
  where
    name prefix n = atomPS $ mkString $ prefix <> T.pack (show n)

-- | Push a curried application chain into the @let@s and single-argument
-- functions at its head in one go, instead of one argument per optimizer round.
applySpine :: Erl -> Maybe Erl
applySpine = go []
  where
    go as = \case
      EApp k f args -> go ((k, args) : as) f
      h | not (null as), canStep h as -> Just (step h as)
      _ -> Nothing

    canStep (ELet _ _) _ = True
    canStep (EFun1 _ _ _) ((_, [_]) : _) = True
    canStep _ _ = False

    step (ELet b body) as = ELet b (step body as)
    step (EFun1 _ v body) ((_, [arg]) : as) = bindLet (EVar v) arg (step body as)
    step h as = foldl (\g (k, args) -> EApp k g args) h as

onTupleN :: Erl -> Erl
onTupleN = \case
  EApp _ tupleN args
    | (EAtomLiteral (Atom (Just tupleMod) tupleFn)) <- tupleN,
      Just i <- Map.lookup (tupleMod, tupleFn) tupleNs,
      length args == i ->
      ETupleLiteral args
  other -> other

tupleNs :: Map.Map (Text, Text) Int
tupleNs =
  Map.fromList $
    [ fn i | i <- [0 .. 10]
    ]
  where
    fn i = ((EC.erlDataTuple, name EC.tuple i), i)
    name prefix n = atomPS $ mkString $ prefix <> T.pack (show n)

binaryOperators :: Map.Map ((Text, Text), (Text, Text)) (Either BinaryOperator (Erl -> Erl -> Erl))
binaryOperators =
  Map.fromList $
    conv
      <$> ( [ Binary euclideanRingNumber opDiv FDivide,
              -- [drathier]: Purescript euclidian integer division is not the same division as erlang division, so we can't inline it as `div` here. See data_euclideanRing@foreign:intDiv and https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf for the various kinds of division.
              Binary heytingAlgebraBoolean opConj AndAlso,
              Binary heytingAlgebraBoolean opDisj OrElse,
              Binary semigroupList opAppend ListConcat
            ]
              ++ concatMap
                ( \(semi, ring) ->
                    [ Binary semi opAdd Add,
                      Binary semi opMul Multiply,
                      Binary ring opSub Subtract
                    ]
                )
                [(semiringNumber, ringNumber), (semiringInt, ringInt)]
              ++ concatMap
                ( \eq ->
                    [ Binary eq opEq IdenticalTo,
                      Binary eq opNotEq NotIdenticalTo
                    ]
                )
                [eqNumber, eqInt, eqString, eqChar, eqBoolean]
              ++ concatMap
                ( \ord ->
                    [ Binary ord opLessThan LessThan,
                      Binary ord opLessThanOrEq LessThanOrEqualTo,
                      Binary ord opGreaterThan GreaterThan,
                      Binary ord opGreaterThanOrEq GreaterThanOrEqualTo,
                      BinaryFn ord opMin (\x y -> EApp RegularApp erlangMin [x, y]),
                      BinaryFn ord opMax (\x y -> EApp RegularApp erlangMax [x, y])
                    ]
                )
                [ordBoolean, ordChar, ordInt, ordNumber, ordString]
          )
  where
    conv (Binary (dmod, dfn) (omod, ofn) result) = (((dmod, atomPS dfn), (omod, atomPS ofn)), Left result)
    conv (BinaryFn (dmod, dfn) (omod, ofn) result) = (((dmod, atomPS dfn), (omod, atomPS ofn)), Right result)

unaryOperators :: Map.Map ((Text, Text), (Text, Text)) UnaryOperator
unaryOperators =
  Map.fromList $
    (\(Unary (dmod, dfn) (omod, ofn) result) -> (((dmod, atomPS dfn), (omod, atomPS ofn)), result))
      <$> [ Unary ringNumber opNegate Negate,
            Unary ringInt opNegate Negate,
            Unary heytingAlgebraBoolean opNot Not
          ]

semiringNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
semiringNumber = (EC.dataSemiring, snd C.P_semiringNumber)

semiringInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
semiringInt = (EC.dataSemiring, snd C.P_semiringInt)

ringNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ringNumber = (EC.dataRing, snd C.P_ringNumber)

ringInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ringInt = (EC.dataRing, snd C.P_ringInt)

euclideanRingNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
euclideanRingNumber = (EC.dataEuclideanRing, snd C.P_euclideanRingNumber)

eqNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqNumber = (EC.dataEq, snd C.P_eqNumber)

eqInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqInt = (EC.dataEq, snd C.P_eqInt)

eqString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqString = (EC.dataEq, snd C.P_eqString)

eqChar :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqChar = (EC.dataEq, snd C.P_eqChar)

eqBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
eqBoolean = (EC.dataEq, snd C.P_eqBoolean)

ordBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordBoolean = (EC.dataOrd, snd C.P_ordBoolean)

ordNumber :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordNumber = (EC.dataOrd, snd C.P_ordNumber)

ordInt :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordInt = (EC.dataOrd, snd C.P_ordInt)

ordString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordString = (EC.dataOrd, snd C.P_ordString)

ordChar :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
ordChar = (EC.dataOrd, snd C.P_ordChar)

-- semigroupString :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
-- semigroupString = (EC.dataSemigroup, snd $ C.P_semigroupString)

boundedBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
boundedBoolean = (EC.dataBounded, snd C.P_boundedBoolean)

heytingAlgebraBoolean :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
heytingAlgebraBoolean = (EC.dataHeytingAlgebra, snd C.P_heytingAlgebraBoolean)

semigroupList :: forall a b. (IsString a, IsString b) => (a, b)
semigroupList = (EC.erlDataListTypes, EC.semigroupList)

-- semigroupoidFn :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
-- semigroupoidFn = (EC.controlSemigroupoid, snd $ C.P_semigroupoidFn)

opAdd :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opAdd = (EC.dataSemiring, snd C.P_add)

opMul :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opMul = (EC.dataSemiring, snd C.P_mul)

opEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opEq = (EC.dataEq, snd C.P_eq)

opNotEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNotEq = (EC.dataEq, snd C.P_notEq)

opLessThan :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opLessThan = (EC.dataOrd, snd C.P_lessThan)

opLessThanOrEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opLessThanOrEq = (EC.dataOrd, snd C.P_lessThanOrEq)

opGreaterThan :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opGreaterThan = (EC.dataOrd, snd C.P_greaterThan)

opGreaterThanOrEq :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opGreaterThanOrEq = (EC.dataOrd, snd C.P_greaterThanOrEq)

opMin :: forall a b. (IsString a, IsString b) => (a, b)
opMin = (EC.dataOrd, EC.min)

opMax :: forall a b. (IsString a, IsString b) => (a, b)
opMax = (EC.dataOrd, EC.max)

opAppend :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opAppend = (EC.dataSemigroup, snd C.P_append)

opSub :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opSub = (EC.dataRing, snd C.P_sub)

opNegate :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNegate = (EC.dataRing, snd C.P_negate)

opDiv :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opDiv = (EC.dataEuclideanRing, snd C.P_div)

opConj :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opConj = (EC.dataHeytingAlgebra, snd C.P_conj)

opDisj :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opDisj = (EC.dataHeytingAlgebra, snd C.P_disj)

opNot :: forall a b. (IsString a, IsString b, Eq b) => (a, b)
opNot = (EC.dataHeytingAlgebra, snd C.P_not)

functorVoid :: forall a b. (IsString a, IsString b) => (a, b)
functorVoid = (EC.dataFunctor, EC.void)

erlangMin :: Erl
erlangMin = EAtomLiteral (Atom (Just "erlang") "min")

erlangMax :: Erl
erlangMax = EAtomLiteral (Atom (Just "erlang") "max")

erlangFloat :: Erl
erlangFloat = EAtomLiteral (Atom (Just "erlang") "float")

erlangArrayToList :: Erl
erlangArrayToList = EAtomLiteral (Atom (Just "array") "to_list")

erlangArrayFromList :: Erl
erlangArrayFromList = EAtomLiteral (Atom (Just "array") "from_list")

erlangArraySize :: Erl
erlangArraySize = EAtomLiteral (Atom (Just "array") "size")

erlangLength :: Erl
erlangLength = EAtomLiteral (Atom (Just "erlang") "length")

erlangHd :: Erl
erlangHd = EAtomLiteral (Atom (Just "erlang") "hd")

erlangListsReverse :: Erl
erlangListsReverse = EAtomLiteral (Atom (Just "lists") "reverse")

-- | Flattens a list of lists (arity 1) -- distinct from the binary ListConcat
-- operator (arity 2, used for List.append) despite the shared "append" name.
erlangListsAppend1 :: Erl
erlangListsAppend1 = EAtomLiteral (Atom (Just "lists") "append")

erlangMapsIsKey :: Erl
erlangMapsIsKey = EAtomLiteral (Atom (Just "maps") "is_key")

erlangMapsRemove :: Erl
erlangMapsRemove = EAtomLiteral (Atom (Just "maps") "remove")

erlangMapsKeys :: Erl
erlangMapsKeys = EAtomLiteral (Atom (Just "maps") "keys")

erlangMapsValues :: Erl
erlangMapsValues = EAtomLiteral (Atom (Just "maps") "values")

erlangMapsSize :: Erl
erlangMapsSize = EAtomLiteral (Atom (Just "maps") "size")

erlangMapsPut :: Erl
erlangMapsPut = EAtomLiteral (Atom (Just "maps") "put")

erlangMapsUpdate :: Erl
erlangMapsUpdate = EAtomLiteral (Atom (Just "maps") "update")

erlangMapsMerge :: Erl
erlangMapsMerge = EAtomLiteral (Atom (Just "maps") "merge")

erlangMapsMap :: Erl
erlangMapsMap = EAtomLiteral (Atom (Just "maps") "map")

erlangListsMap :: Erl
erlangListsMap = EAtomLiteral (Atom (Just "lists") "map")

erlangArrayMap :: Erl
erlangArrayMap = EAtomLiteral (Atom (Just "array") "map")

erlangMapsFind :: Erl
erlangMapsFind = EAtomLiteral (Atom (Just "maps") "find")

-- | The arguments of a call to a module function, whether it was emitted as
-- `m:f(a, b)`, `m:f()(a)(b)` or a mix of the two.
flattenCall :: Erl -> Maybe ((Text, Text), [Erl])
flattenCall = go []
  where
    go acc = \case
      EApp _ (EAtomLiteral (Atom (Just m) f)) args -> Just ((m, f), args <> acc)
      -- `fun m:f/N` applied to N arguments is a direct call.
      EApp _ (EFunRef (Atom (Just m) f) n) args | n == length args -> Just ((m, f), args <> acc)
      -- Only the `m:f()` that starts a curried call has no arguments; running a result, `m:f(a)()`, is not one.
      EApp _ _ [] -> Nothing
      EApp _ fn args -> go (args <> acc) fn
      _ -> Nothing

-- | What a monad wraps around its base type, outermost first.
data Wrapper = Reader | Thunk

-- | The base type of a monad, which decides how a bind short-circuits.
data Base = OkVeither | JustMaybe | RightEither

-- | An inlining rule for a function of the local Maybe/Either/Veither/Map modules.
data LibRule = LibRule
  { -- | Number of arguments, not counting the dictionary.
    ruleArity :: Int,
    -- | The function takes an `IsSymbol` dictionary first; the builder gets its label.
    ruleLabelled :: Bool,
    ruleBuild :: Maybe PSString -> [Erl] -> Supply Erl
  }

-- | Inline a call whose arguments are all present. A partial application is
-- eta-expanded over the missing arguments, so it becomes a closure with the
-- inlined body instead of a call into the generic function.
inlineLibFns :: (Erl -> Maybe PSString) -> Map.Map (Text, Text) LibRule -> Erl -> Supply Erl
inlineLibFns symbolOf table expr@EApp {}
  | Just (key, allArgs) <- flattenCall expr,
    Just rule <- Map.lookup key table,
    Just (label, args) <- splitLabel rule allArgs =
    case compare (length args) (ruleArity rule) of
      EQ -> ruleBuild rule label args
      LT | isJust label || not (null args) -> etaExpand rule label args
      _ -> pure expr
  where
    splitLabel rule as
      | not (ruleLabelled rule) = Just (Nothing, as)
      | dict : rest <- as, Just s <- symbolOf dict = Just (Just s, rest)
      | otherwise = Nothing

    etaExpand rule label args = do
      missing <- traverse (const (freshName "_A")) [1 .. ruleArity rule - length args]
      -- Supplied arguments are evaluated once, outside the closure, unless they are values already.
      supplied <- traverse (\a -> if isValue a then pure (a, Nothing) else (\v -> (EVar v, Just (v, a))) <$> freshName "_B") args
      body <- ruleBuild rule label (map fst supplied <> map EVar missing)
      pure (foldr (\(v, a) b -> ELet (EBind (EVar v) a) b) (foldr (EFun1 Nothing) body missing) (mapMaybe snd supplied))

    isValue = \case
      EVar _ -> True
      EAtomLiteral _ -> True
      ENumericLiteral _ -> True
      EStringLiteral _ -> True
      EFunRef {} -> True
      EFunFull {} -> True
      _ -> False
inlineLibFns _ _ expr = pure expr

freshName :: Text -> Supply Text
freshName prefix = do
  n <- fresh
  pure (prefix <> "@" <> T.pack (show n))

-- | Arguments that are only needed on one branch end up inside that branch, so
-- they are evaluated lazily. The names here must include the Veither functions
-- that CoreFn's `shouldFloatExpr` keeps un-floated.
libRules :: Map.Map (Text, Text) LibRule
libRules =
  Map.fromList $
    stackRules
    ++ [ (("map@ps", "lookupOr"), r3 $ \d k m -> do
        v <- freshVar "_V"
        pure $ ECaseOf (app erlangMapsFind [k, m]) [(EBinder (tagged "ok" [v]), v), (EBinder (atom "error"), d)]),
      (("maybe@ps", "maybe"), r3 $ \x f m -> maybeCase m x (\z -> app f [z])),
      (("maybe@ps", "withDefault"), r2 $ \x m -> maybeCase m x id),
      (("maybe@ps", "iff"), r2 $ \b v -> pure (boolCase b (just v) nothing)),
      (("maybe@ps", "keepIf"), r2 $ \p m -> maybeCase m nothing (\a -> boolCase (app p [a]) (just a) nothing)),
      (("maybe@ps", "dropIf"), r2 $ \p m -> maybeCase m nothing (\a -> boolCase (app p [a]) nothing (just a))),
      (("either@ps", "either"), r3 $ \l r e -> eitherCase e (\z -> app l [z]) (\z -> app r [z])),
      (("either@ps", "fromLeft"), r2 $ \r e -> eitherCase e id (const r)),
      (("either@ps", "fromRight"), r2 $ \l e -> eitherCase e (const l) id),
      (("either@ps", "withDefault"), r2 $ \l e -> eitherCase e (const l) id),
      (("either@ps", "fromMaybe"), r2 $ \e m -> maybeCase m (left e) right),
      (("either@ps", "assert"), r2 $ \e b -> pure (boolCase b (right unit) (left e))),
      (("veither@ps", "expand"), r1 pure),
      (("veither@ps", "withDefault"), r2 $ \d v -> okCase v id (const d)),
      (("veither@ps", "isOk"), r1 $ \v -> okCase v (const ETrue) (const EFalse)),
      (("veither@ps", "isErr"), r1 $ \v -> okCase v (const EFalse) (const ETrue)),
      (("veither@ps", "toMaybe"), r1 $ \v -> okCase v just (const nothing)),
      (("veither@ps", "onOtherErr"), r2 $ \f v -> okCase v id (\x -> app f [x])),
      (("veither@ps", "veither"), r3 $ \he hs v -> okCase v (\a -> app hs [a]) (\x -> app he [x])),
      (("veither@ps", "toEither"), r2 $ \f v -> okCase v right (\x -> left (app f [x]))),
      (("veither@ps", "err"), l1 $ \s a -> pure (veitherVal s a)),
      (("veither@ps", "fromMaybe"), l2 $ \s a m -> maybeCase m (veitherVal s a) (veitherVal okLabel)),
      (("veither@ps", "assert"), l2 $ \s a b -> pure (boolCase b (veitherVal okLabel unit) (veitherVal s a))),
      (("veither@ps", "fromEither"), l1 $ \s e -> eitherCase e (veitherVal s) (veitherVal okLabel)),
      (("veither@ps", "is"), l1 $ \s v -> labelCase s v (const ETrue) (const EFalse)),
      (("veither@ps", "on"), l2 $ \s f v -> labelCase s v (\a -> veitherVal okLabel (app f [a])) id),
      -- A Veff is `fun() -> Veither end` and a Ctx is `fun(Body) -> Veff end`; the newtype wrappers are erased.
      (("veff@ps", "catch"), r1 pure),
      (("veff@ps", "fromVeither"), r1 $ \v -> wrapped [Thunk] (\_ -> pure v)),
      (("veff@ps", "lift"), r1 $ \m -> wrapped [Thunk] (\_ -> pure (veitherVal okLabel (app m [])))),
      (("ctx@ps", "fromBodyFn"), r1 pure),
      (("ctx@ps", "toVeff"), r2 $ \b c -> pure (app c [b])),
      (("ctx@ps", "catch"), r2 $ \b c -> pure (app c [b])),
      (("ctx@ps", "fromVeither"), r1 $ \v -> wrapped [Reader, Thunk] (\_ -> pure v))
    ]
  where
    -- A monad is some wrappers around a base type. `Reader` takes the body, `Thunk` is an Effect.
    stackRules = concat
      [ stack "maybe@ps" [] JustMaybe,
        stack "either@ps" [] RightEither,
        stack "veither@ps" [] OkVeither,
        stack "veff@ps" [Thunk] OkVeither,
        stack "ctx@ps" [Reader, Thunk] OkVeither
      ]

    stack m ws base =
      [ ((m, "pure"), r1 $ \x -> wrapped ws (\_ -> pure (basePure base x))),
        ((m, "map"), r2 $ \f v -> wrapped ws (\run -> baseMap base (run v) (\a -> app f [a]))),
        ((m, "bind"), r2 $ \v f -> wrapped ws (\run -> baseBind base (run v) (\a -> run (app f [a])))),
        ((m, "andThen"), r2 $ \f v -> wrapped ws (\run -> baseBind base (run v) (\a -> run (app f [a])))),
        ((m, "join"), r1 $ \v -> wrapped ws (\run -> baseBind base (run v) run))
      ]

    -- Build the wrappers around a body that is given `run`, which unwraps a value down to the base.
    wrapped :: [Wrapper] -> ((Erl -> Erl) -> Supply Erl) -> Supply Erl
    wrapped [] k = k id
    wrapped (Reader : ws) k = do
      b <- freshName "Body"
      inner <- wrapped ws (\run -> k (run . \e -> app e [EVar b]))
      pure (EFun1 Nothing (if b `elem` varsInExpr inner then b else "_") inner)
    wrapped (Thunk : ws) k = EFun0 Nothing <$> wrapped ws (\run -> k (run . \e -> app e []))

    basePure :: Base -> Erl -> Erl
    basePure = \case
      OkVeither -> veitherVal okLabel
      JustMaybe -> just
      RightEither -> right

    -- Continue with the wrapped value, or keep the failure as it is.
    baseBind :: Base -> Erl -> (Erl -> Erl) -> Supply Erl
    baseBind = \case
      OkVeither -> \v k -> okCase v k id
      JustMaybe -> \m k -> maybeCase m nothing k
      RightEither -> \e k -> eitherCase e left k

    baseMap :: Base -> Erl -> (Erl -> Erl) -> Supply Erl
    baseMap base v f = baseBind base v (basePure base . f)

    -- Rules by number of arguments; the dispatcher only calls a builder with that many.
    r1 f = LibRule 1 False (\_ -> \case [a] -> f a; _ -> unreachable)
    r2 f = LibRule 2 False (\_ -> \case [a, b] -> f a b; _ -> unreachable)
    r3 f = LibRule 3 False (\_ -> \case [a, b, c] -> f a b c; _ -> unreachable)
    l1 f = LibRule 1 True (\l -> \case [a] -> f (label l) a; _ -> unreachable)
    l2 f = LibRule 2 True (\l -> \case [a, b] -> f (label l) a b; _ -> unreachable)
    label = fromMaybe unreachable
    unreachable = error "libRules: arity mismatch"

    app = EApp RegularApp
    atom = EAtomLiteral . Atom Nothing
    tagged tag xs = ETupleLiteral (atom tag : xs)
    nothing = tagged "nothing" []
    just x = tagged "just" [x]
    left x = tagged "left" [x]
    right x = tagged "right" [x]
    unit = atom "unit"

    freshVar prefix = EVar <$> freshName prefix

    boolCase b onTrue onFalse = ECaseOf b [(EBinder ETrue, onTrue), (EBinder EFalse, onFalse)]

    maybeCase m onNothing onJust = do
      v <- freshVar "_J"
      pure $ ECaseOf m [(EBinder nothing, onNothing), (EBinder (just v), onJust v)]

    eitherCase e onLeft onRight = do
      v <- freshVar "_E"
      pure $ ECaseOf e [(EBinder (left v), onLeft v), (EBinder (right v), onRight v)]

    -- A Veither is `#{type := ~"_", value := V}` when ok and `#{type := Label, value := E}` for an error.
    okLabel = mkString "_"
    veitherVal label v = EMapLiteral [(Atom Nothing "type", EStringLiteral label), (Atom Nothing "value", v)]
    veitherPat label v = EMapPattern [(Atom Nothing "type", EStringLiteral label), (Atom Nothing "value", v)]

    -- Branch on the label; the other branch gets the whole value.
    labelCase label v onMatch onOther = do
      a <- freshVar "_V"
      x <- freshVar "_X"
      pure $ ECaseOf v [(EBinder (veitherPat label a), onMatch a), (EBinder x, onOther x)]

    okCase = labelCase okLabel

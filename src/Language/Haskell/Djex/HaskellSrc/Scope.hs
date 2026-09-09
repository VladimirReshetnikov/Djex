-- | Presentation scope for implicitly quantified source signatures. The
-- source parser and candidate graph remain the typing authorities; this
-- module only supplies names for GHC's visible type-binding patterns.
module Language.Haskell.Djex.HaskellSrc.Scope
  ( hasExplicitOuterForall
  , scopedTypeBinderNames
  , scopedTypePatterns
  , scopedSourceDefinition
  , scopedSourceExpression
  , standaloneSourceExpression
  ) where

import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Parser as HSE
import qualified Language.Haskell.Exts.Pretty as HSE
import qualified Language.Haskell.Exts.Syntax as HSE
import Language.Haskell.Exference.HaskellSrcUtils (contextConstraints)
import Language.Haskell.Exference.TypeFromHaskellSrc (haskellSrcExtsParseMode)

hasExplicitOuterForall :: String -> Bool
hasExplicitOuterForall source = case HSE.parseTypeWithMode
    (haskellSrcExtsParseMode "synthesis-root-forall") source of
  HSE.ParseOk parsed -> explicit parsed
  HSE.ParseFailed{} -> False
 where
  explicit (HSE.TyParen _ body) = explicit body
  explicit (HSE.TyForall _ (Just _) _ _) = True
  explicit _ = False

-- | A leading explicit forall after an implicit context must also be opened
-- at the binding site. No patterns are added to already explicitly scoped
-- signatures, whose established rendering path remains unchanged.
scopedTypeBinderNames :: String -> [String]
scopedTypeBinderNames = snd . bindingScope

scopedTypePatterns :: String -> String
scopedTypePatterns = concatMap (" @" ++) . scopedTypeBinderNames

scopedSourceDefinition :: String -> String -> String -> String
scopedSourceDefinition source name term =
  name ++ scopedTypePatterns source ++ " = " ++ term

-- | An expression has its own polymorphic type annotation. Unlike a named
-- equation, its type abstraction needs an explicitly polymorphic expected
-- type. Applying this expression to a type preserves the selected context;
-- assigning it implicitly to an ambiguous monomorphic RHS would lose it.
scopedSourceExpression :: String -> String -> String
scopedSourceExpression source term = case bindingScope source of
  ([], _) -> term
  (implicit, binders) -> "((\\" ++ concatMap (" @" ++) binders ++ " -> " ++ term
    ++ ") :: forall " ++ unwords implicit ++ ". " ++ annotation ++ ")"
 where
  annotation = case HSE.parseTypeWithMode
      (haskellSrcExtsParseMode "synthesis-expression-signature") source of
    HSE.ParseOk parsed -> unwords $ lines $ HSE.prettyPrint parsed
    HSE.ParseFailed{} -> source

-- | One-shot expression output must carry its own binding scope, including
-- explicit outer quantifiers. The REPL's RHS form can instead use binders from
-- the surrounding requested signature, so that existing form stays separate.
standaloneSourceExpression :: String -> String -> Either String String
standaloneSourceExpression source term = case bindingScope source of
  (_ : _, _) -> Right $ scopedSourceExpression source term
  _ -> case HSE.parseTypeWithMode
      (haskellSrcExtsParseMode "synthesis-standalone-signature") source of
    HSE.ParseFailed{} -> Left "cannot parse the standalone expression's source signature"
    HSE.ParseOk parsed -> do
      binders <- leading parsed
      if length binders /= Set.size (Set.fromList binders)
        then Left "standalone expression has repeated leading type binder names"
        else let annotation = unwords $ lines $ HSE.prettyPrint parsed
             in Right $ if null binders then "(" ++ term ++ " :: " ++ annotation ++ ")"
                  else "((\\" ++ concatMap (" @" ++) binders ++ " -> " ++ term
                    ++ ") :: " ++ annotation ++ ")"
 where
  leading (HSE.TyParen _ body) = leading body
  leading (HSE.TyForall _ variables _ body) =
    (++) <$> maybe (Right []) (traverse binder) variables <*> leading body
  leading _ = Right []
  binder (HSE.UnkindedVar _ name) = Right $ HSE.prettyPrint name
  binder HSE.KindedVar{} = Left "standalone expression does not yet support kinded source binders"

bindingScope :: String -> ([String], [String])
bindingScope source = case HSE.parseTypeWithMode
    (haskellSrcExtsParseMode "synthesis-binding-scope") source of
  HSE.ParseOk parsed -> case outer parsed of
    -- Respect GHC's forall-or-nothing rule. Free variables in an explicit
    -- outer forall are not silently promoted to implicit root quantifiers.
    HSE.TyForall _ (Just _) _ _ -> ([], [])
    ty -> case free Set.empty ty of
      Just variables ->
        let implicit = distinct variables
            binders = implicit ++ leading ty
        in if null implicit || length binders /= Set.size (Set.fromList binders)
             then ([], []) else (implicit, binders)
      Nothing -> ([], [])
  HSE.ParseFailed{} -> ([], [])
 where
  outer (HSE.TyParen _ body) = outer body
  outer ty = ty
  binderName (HSE.UnkindedVar _ name) = Just $ HSE.prettyPrint name
  binderName HSE.KindedVar{} = Nothing
  leading ty = case outer ty of
    HSE.TyForall _ binders _ body ->
      maybe [] (maybe [] id . traverse binderName) binders ++ leading body
    _ -> []
  distinct = go Set.empty
   where
    go _ [] = []
    go seen (name : rest)
      | Set.member name seen = go seen rest
      | otherwise = name : go (Set.insert name seen) rest
  free bound ty = case ty of
    HSE.TyVar _ name -> pure [spelling | not $ Set.member spelling bound]
     where spelling = HSE.prettyPrint name
    HSE.TyCon{} -> pure []
    HSE.TyFun _ a b -> (++) <$> free bound a <*> free bound b
    HSE.TyApp _ a b -> (++) <$> free bound a <*> free bound b
    HSE.TyTuple _ _ fields -> concat <$> traverse (free bound) fields
    HSE.TyList _ element -> free bound element
    HSE.TyParen _ body -> free bound body
    HSE.TyInfix _ a HSE.UnpromotedName{} b -> (++) <$> free bound a <*> free bound b
    HSE.TyForall _ variables context body -> do
      names <- maybe (pure []) (traverse binderName) variables
      let nested = Set.union bound $ Set.fromList names
      constraints <- concat <$> traverse (constraintFree nested) (contextConstraints context)
      (constraints ++) <$> free nested body
    _ -> Nothing
  constraintFree bound (HSE.TypeA _ ty) = free bound ty
  constraintFree _ _ = Nothing

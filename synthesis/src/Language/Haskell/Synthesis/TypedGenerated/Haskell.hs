-- | Haskell elaboration hints from one retained graph. This module renders
-- evidence; it neither creates a checked candidate association nor replaces
-- independent GHC checking of the complete requested type.
module Language.Haskell.Synthesis.TypedGenerated.Haskell
  ( HaskellGraphRenderError (..)
  , renderHaskellTermGraph
  ) where

import Control.Monad (unless)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Qualification (renderNamePrefix)
import qualified Language.Haskell.Synthesis.Type as T
import qualified Language.Haskell.Synthesis.TypeAtom as A
import qualified Language.Haskell.Synthesis.TypeRender as R
import qualified Language.Haskell.Synthesis.TypedGenerated as Q

data HaskellGraphRenderError
  = HaskellGraphSyntaxError G.RenderError
  | HaskellGraphOpenRoot
  | HaskellGraphUnboundTypeVariable
  | HaskellGraphMissingNode
  | HaskellGraphUnsupportedForall
  | HaskellGraphUnsupportedContextEvidence
  | HaskellGraphHole
  deriving (Eq, Show)

-- | Render a closed graph with explicit scoped forall introductions, typed
-- patterns, application domains, and the graph's exact type selections.
--
-- Requires RankNTypes, ImpredicativeTypes, ScopedTypeVariables and
-- TypeApplications, plus the extensions required by the retained class
-- contexts (for example FlexibleContexts). Dictionary parameters remain
-- implicit: qualified annotations introduce the checked givens, and exact
-- result annotations discharge them at their witnessed applications. A use
-- with multiple alpha-equal lexical givens is unsupported because ordinary
-- Haskell cannot select its exact dictionary slot. Unsupported evidence is
-- reported, never replaced by an
-- inferred type or an annotation borrowed from another candidate. Type names
-- are allocated lexically; sibling and nested forall scopes cannot donate a
-- name to an escaped skolem. Compatibility syntax is validated independently.
renderHaskellTermGraph
  :: (Ord variable, Ord local)
  => G.RenderOptions local
  -> Q.TermGraph (T.Type variable) local
  -> Either HaskellGraphRenderError String
renderHaskellTermGraph options graph = do
  _ <- syntax $ G.renderExpression options erased
  root <- node $ Q.termGraphRoot graph
  unless (Set.null $ T.freeVariables $ Q.termNodeType root) $
    Left HaskellGraphOpenRoot
  names <- syntax $ G.allocateLocalNames
    options { G.reservedLocalNames = helpers ++ G.reservedLocalNames options } erased
  render names Map.empty Map.empty $ Q.termGraphRoot graph
 where
  erased = Q.eraseTermGraph graph
  qualification = G.renderQualification options
  globals = map (renderNamePrefix qualification) $ G.expressionGlobals erased
  helper key = choose ("djexPoly" ++ show (Q.termNodeIdValue key))
   where
    choose name | name `elem` globals = choose $ name ++ "_"
                | otherwise = name
  helpers = [helper key | (key, _) <- Q.termGraphNodes graph]
  syntax = either (Left . HaskellGraphSyntaxError) Right
  node key = maybe (Left HaskellGraphMissingNode) Right $ Q.lookupTermNode key graph
  local names key = maybe (Left $ HaskellGraphSyntaxError G.UnboundLocalIdentity)
    Right $ Map.lookup key names
  parens text = "(" ++ text ++ ")"
  annotate text ty = parens $ text ++ " :: " ++ ty

  -- Each bound type gets a name disjoint from every opened skolem name.
  -- Repeated source identities in nested foralls are handled by lexical maps.
  typeText scope ty = R.renderTypeWithQualification qualification id
    <$> rename (0 :: Int) scope ty
  rename depth scope ty = case ty of
    T.TypeVariable variable -> maybe (Left HaskellGraphUnboundTypeVariable)
      (Right . T.TypeVariable) $ Map.lookup variable scope
    T.TypeConstructor name -> Right $ T.TypeConstructor name
    T.TypeApplication f a -> T.TypeApplication <$> rename depth scope f <*> rename depth scope a
    T.FunctionType a b -> T.FunctionType <$> rename depth scope a <*> rename depth scope b
    T.TupleType boxity fields -> T.TupleType boxity <$> traverse (rename depth scope) fields
    T.ForallType variables constraints body -> do
      let spellings = ["djexBound" ++ show depth ++ "_" ++ show index
                      | index <- [0 :: Int .. length variables - 1]]
          nested = Map.union (Map.fromList $ zip variables spellings) scope
      T.ForallType spellings
        <$> traverse (traverse $ rename (depth + 1) nested) constraints
        <*> rename (depth + 1) nested body

  patternText names scope pattern = do
    ty <- typeText scope $ Q.typedPatternType pattern
    body <- case Q.typedPatternNode pattern of
      Q.TypedBind variable -> local names variable
      Q.TypedWildcard -> Right "_"
      Q.TypedConstructor name fields -> do
        children <- traverse (patternText names scope) fields
        pure $ unwords $ renderNamePrefix qualification name : children
      Q.TypedTuplePattern fields -> do
        children <- traverse (patternText names scope) fields
        pure $ parens $ intercalate ", " children
      Q.TypedAs variable child -> do
        name <- local names variable
        childText <- patternText names scope child
        pure $ name ++ "@" ++ childText
    pure $ annotate body ty

  render names scope givens key = do
    current <- node key
    case Q.termNodeForm current of
      Q.TypedLocal _ variable -> local names variable
      Q.TypedGlobal _ name -> Right $ renderNamePrefix qualification name
      Q.TypedHole{} -> Left HaskellGraphHole
      Q.TypedLambda patterns body -> do
        parameters <- traverse (patternText names scope) patterns
        result <- render names scope givens body
        pure $ parens $ "\\" ++ unwords parameters ++ " -> " ++ result
      Q.TypedApply function argument witness -> do
        f <- render names scope givens function
        a <- render names scope givens argument
        domain <- typeText scope $ Q.applicationDomain witness
        pure $ parens $ parens f ++ " " ++ annotate a domain
      Q.TypedVisibleTypeApplication _ function _ witness ->
        application names scope givens function (Q.typeApplicationSource witness)
          (Q.typeApplicationSelected witness)
      Q.TypedImplicitTypeApplication _ function witness ->
        application names scope givens function (Q.implicitTypeApplicationSource witness)
          (Q.implicitTypeApplicationSelected witness)
      Q.TypedForallIntroduction _ body witness ->
        case (Q.forallIntroductionSource witness, Q.forallIntroductionVariable witness) of
          (T.ForallType (_ : _) _ _, T.TypeVariable variable) -> do
            let spelling = "djexSkolem" ++ show (Q.termNodeIdValue key)
                nested = Map.insert variable spelling scope
                name = helper key
            opened <- typeText nested $ Q.forallIntroductionBody witness
            result <- render names nested givens body
            pure $ parens $ "let { " ++ name ++ " :: forall " ++ spelling ++ ". "
              ++ opened ++ "; " ++ name ++ " = " ++ result ++ " } in " ++ name
          _ -> Left HaskellGraphUnsupportedForall
      Q.TypedContextIntroduction occurrence body witness -> do
        let source = Q.contextIntroductionSource witness
            constraints = Q.contextIntroductionConstraints witness
            introduced = Map.fromList
              [ (Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence slot, constraint)
              | (slot, constraint) <- zip [0 ..] constraints ]
        checkContext source constraints $ Q.contextIntroductionBody witness
        signature <- typeText scope source
        result <- render names scope (Map.union introduced givens) body
        pure $ annotate result signature
      Q.TypedContextApplication _ function witness -> do
        let source = Q.contextApplicationSource witness
            constraints = Q.contextApplicationConstraints witness
            evidence = Q.contextApplicationEvidence witness
        checkContext source constraints $ Q.contextApplicationResult witness
        unless (length constraints == length evidence) $
          Left HaskellGraphUnsupportedContextEvidence
        mapM_ (checkGiven givens) $ zip constraints evidence
        f <- render names scope givens function
        signature <- typeText scope source
        result <- typeText scope $ Q.contextApplicationResult witness
        pure $ annotate (annotate f signature) result
      Q.TypedTuple elements -> do
        fields <- traverse (render names scope givens) elements
        pure $ parens $ intercalate ", " fields
      Q.TypedLet pattern value body -> do
        binder <- patternText names scope pattern
        rhs <- render names scope givens value
        result <- render names scope givens body
        pure $ parens $ "let { " ++ binder ++ " = " ++ rhs ++ " } in " ++ result
      Q.TypedCase scrutinee alternatives -> do
        value <- render names scope givens scrutinee
        cases <- traverse (\(pattern, body) -> do
          binder <- patternText names scope pattern
          result <- render names scope givens body
          pure $ binder ++ " -> " ++ result) alternatives
        pure $ parens $ "case " ++ value ++ " of { " ++ intercalate "; " cases ++ " }"

  -- Generic graphs may have been sealed with a different type observer.
  -- This renderer supports only a literal shared qualified layer and never
  -- invents Haskell syntax for another observer's claimed dictionary rules.
  checkContext source constraints result = case source of
    T.ForallType [] expected body
      | not (null expected)
      , length expected == length constraints
      , and (zipWith sameConstraint expected constraints)
      , A.alphaEquivalentTypes body result -> Right ()
    _ -> Left HaskellGraphUnsupportedContextEvidence

  sameConstraint (Constraint leftName left) (Constraint rightName right) =
    leftName == rightName && length left == length right
      && and (zipWith A.alphaEquivalentTypes left right)

  checkGiven givens (required, evidence) =
    case Map.lookup (Q.contextEvidenceBinder evidence) givens of
      Just actual
        | sameConstraint required actual
        , length (filter (sameConstraint required) $ Map.elems givens) == 1 -> Right ()
      _ -> Left HaskellGraphUnsupportedContextEvidence

  application names scope givens function source selected = do
    f <- render names scope givens function
    signature <- typeText scope source
    argument <- typeText scope selected
    pure $ parens $ annotate f signature ++ " @" ++ parens argument

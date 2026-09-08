-- | Haskell elaboration hints from one retained graph. This module renders
-- evidence; it neither creates a checked candidate association nor replaces
-- independent GHC checking of the complete requested type.
module Language.Haskell.Synthesis.TypedGenerated.Haskell
  ( HaskellGraphRenderError (..)
  , renderHaskellTermGraph
  , renderHaskellTermGraphAtSignature
  ) where

import Control.Monad (unless)
import Data.Char (isAlphaNum, isLower)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import qualified Language.Haskell.Synthesis.Generated as G
import Language.Haskell.Synthesis.Name (parseName)
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
  | HaskellGraphRootSignatureRequired
  | HaskellGraphRootSignatureMismatch
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
renderHaskellTermGraph = renderGraph Nothing

-- | Render the right-hand side of a binding with this exact, explicitly
-- quantified source signature. ScopedTypeVariables brings its leading binders
-- into scope. This is necessary when a root parameter occurs only in class
-- constraints: introducing a new polymorphic helper and leaving its final use
-- implicit would forget which outer dictionary the graph selected.
--
-- Source spelling supplies no typing authority. Before importing any binder
-- names, compare the complete closed signature with this candidate's own root.
-- Unrelated signatures, invalid names and incomplete opening chains fail.
renderHaskellTermGraphAtSignature
  :: (Ord variable, Ord local)
  => G.RenderOptions local -> T.Type String -> Q.TermGraph (T.Type variable) local
  -> Either HaskellGraphRenderError String
renderHaskellTermGraphAtSignature options signature = renderGraph (Just signature) options

renderGraph
  :: (Ord variable, Ord local)
  => Maybe (T.Type String) -> G.RenderOptions local -> Q.TermGraph (T.Type variable) local
  -> Either HaskellGraphRenderError String
renderGraph suppliedSignature options graph = do
  _ <- syntax $ G.renderExpression options erased
  root <- node $ Q.termGraphRoot graph
  unless (Set.null $ T.freeVariables $ Q.termNodeType root) $
    Left HaskellGraphOpenRoot
  names <- syntax $ G.allocateLocalNames
    options { G.reservedLocalNames = helpers ++ G.reservedLocalNames options } erased
  if constraintOnlyForall $ Q.termNodeType root then do
    signature <- maybe (Left HaskellGraphRootSignatureRequired) Right suppliedSignature
    unless (A.alphaEquivalentClosedTypes signature $ Q.termNodeType root) $
      Left HaskellGraphRootSignatureMismatch
    let spellings = T.leadingForallVariables signature
    unless (all validBinder spellings && length spellings == Set.size (Set.fromList spellings)) $
      Left HaskellGraphRootSignatureMismatch
    renderRoot names Map.empty Map.empty spellings $ Q.termGraphRoot graph
  else render names Map.empty Map.empty $ Q.termGraphRoot graph
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

  validBinder spelling = case spelling of
    first : rest | (isLower first || first == '_') && spelling /= "_" &&
        all (\c -> isAlphaNum c || c == '_' || c == '\'') rest ->
      either (const False)
        (either (const False) (const True) . G.mkDefinitionName) $ parseName spelling
    _ -> False

  freshSpelling used candidate
    | candidate `Set.member` used = freshSpelling used $ candidate ++ "'"
    | otherwise = candidate

  freshSpellings occupied = choose occupied
   where
    choose _ [] = []
    choose used (base : remaining) =
      let next = freshSpelling used base
      in next : choose (Set.insert next used) remaining

  renderRoot names scope givens remaining key = do
    current <- node key
    case Q.termNodeForm current of
      Q.TypedForallIntroduction _ body witness -> case
          (remaining, Q.forallIntroductionVariable witness) of
        (spelling : rest, T.TypeVariable variable) ->
          renderRoot names (Map.insert variable spelling scope) givens rest body
        _ -> Left HaskellGraphRootSignatureMismatch
      Q.TypedContextIntroduction occurrence body witness -> do
        let constraints = Q.contextIntroductionConstraints witness
            introduced = Map.fromList
              [(Q.contextEvidenceBinder $ Q.givenContextEvidence occurrence slot, constraint)
              | (slot, constraint) <- zip [0 ..] constraints]
        checkContext (Q.contextIntroductionSource witness) constraints $
          Q.contextIntroductionBody witness
        renderRoot names scope (Map.union introduced givens) remaining body
      _ | null remaining -> render names scope givens key
        | otherwise -> Left HaskellGraphRootSignatureMismatch

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
      let spellings = freshSpellings (Set.fromList $ Map.elems scope)
            ["djexBound" ++ show depth ++ "_" ++ show index
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
    -- Re-annotating an ambiguous rank-N parameter makes GHC subsume two
    -- independently quantified contexts. Its enclosing checked arrow already
    -- supplies this exact scheme, so keep the binder and its expected type.
    pure $ if constraintOnlyForall $ Q.typedPatternType pattern
      then body else annotate body ty

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
            let spelling = freshSpelling (Set.fromList $ Map.elems scope) $
                  "djexSkolem" ++ show (Q.termNodeIdValue key)
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
    -- The checked global/local declaration already supplies its source
    -- scheme. A forall annotation with a constraint-only parameter would
    -- instantiate it before the explicit type application can select that
    -- parameter, producing precisely the ambiguity this evidence resolves.
    pure $ parens $ (if constraintOnlyForall source then f else annotate f signature)
      ++ " @" ++ parens argument

-- Leading parameters which occur in qualification but nowhere in ordinary
-- term argument/result types cannot be recovered by Haskell subsumption.
-- Keep this test structural; do not infer a compiler diagnosis from a name.
constraintOnlyForall :: Ord variable => T.Type variable -> Bool
constraintOnlyForall source = case source of
  T.ForallType binders constraints body ->
    any (\binder -> binder `Set.member` T.freeVariables (T.ForallType [] constraints body)
          && binder `Set.notMember` T.freeVariables (withoutContexts body)) binders
      || constraintOnlyForall body
  _ -> False
 where
  withoutContexts ty = case ty of
    T.ForallType binders _ body -> T.ForallType binders [] $ withoutContexts body
    T.FunctionType domain result -> T.FunctionType (withoutContexts domain) $ withoutContexts result
    T.TypeApplication function argument -> T.TypeApplication (withoutContexts function) $ withoutContexts argument
    T.TupleType box fields -> T.TupleType box $ map withoutContexts fields
    other -> other

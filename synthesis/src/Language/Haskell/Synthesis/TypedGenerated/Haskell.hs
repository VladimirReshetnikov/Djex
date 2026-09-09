-- | Haskell elaboration hints from one retained graph. This module renders
-- evidence; it neither creates a checked candidate association nor replaces
-- independent GHC checking of the complete requested type.
module Language.Haskell.Synthesis.TypedGenerated.Haskell
  ( HaskellGraphRenderError (..)
  , renderHaskellTermGraph
  , renderHaskellTermGraphAtSignature
  , renderHaskellTermGraphWithMetavariables
  , renderHaskellTermGraphAtSignatureWithMetavariables
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
renderHaskellTermGraph = renderGraph (const False) Nothing

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
renderHaskellTermGraphAtSignature options signature = renderGraph (const False) (Just signature) options

-- | Render tagged synthesis variables, admitting local generalization of
-- unconstrained flexible variables in an internal expression. A fresh variable
-- must occur in a retained proper term type and escape into neither the result
-- nor captured local types or dictionaries. Rigid variables are never generalized. The helper's
-- unused identity-function arguments carry those complete term types, so GHC
-- infers its kinds without choosing an arbitrary default type. This is a
-- presentation of the same expression, not new candidate or graph evidence;
-- independent checking of the exact output remains required.
renderHaskellTermGraphWithMetavariables
  :: (Ord variable, Ord local)
  => G.RenderOptions local
  -> Q.TermGraph (T.Type (T.Variable variable)) local
  -> Either HaskellGraphRenderError String
renderHaskellTermGraphWithMetavariables = renderGraph flexible Nothing

-- | The source-signature counterpart of 'renderHaskellTermGraphWithMetavariables'.
renderHaskellTermGraphAtSignatureWithMetavariables
  :: (Ord variable, Ord local)
  => G.RenderOptions local -> T.Type String
  -> Q.TermGraph (T.Type (T.Variable variable)) local
  -> Either HaskellGraphRenderError String
renderHaskellTermGraphAtSignatureWithMetavariables options signature =
  renderGraph flexible (Just signature) options

flexible :: T.Variable variable -> Bool
flexible T.FlexibleVariable{} = True
flexible T.RigidVariable{} = False

renderGraph
  :: (Ord variable, Ord local)
  => (variable -> Bool) -> Maybe (T.Type String)
  -> G.RenderOptions local -> Q.TermGraph (T.Type variable) local
  -> Either HaskellGraphRenderError String
renderGraph canGeneralize suppliedSignature options graph = do
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
  else render names Map.empty Map.empty Map.empty $ Q.termGraphRoot graph
 where
  erased = Q.eraseTermGraph graph
  qualification = G.renderQualification options
  globals = map (renderNamePrefix qualification) $ G.expressionGlobals erased
  helper key = choose ("djexPoly" ++ show (Q.termNodeIdValue key))
   where
    choose name | name `elem` globals = choose $ name ++ "_"
                | otherwise = name
  metaHelper key = choose $ helper key ++ "_types"
   where
    choose name | name `elem` globals = choose $ name ++ "_"
                | otherwise = name
  helpers = concat [[helper key, metaHelper key] | (key, _) <- Q.termGraphNodes graph]
  flexibleVariables = Set.filter canGeneralize $ foldMap
    (T.freeVariables . Q.termNodeType . snd) $ Q.termGraphNodes graph
  capturedGlobalVariables = foldMap T.freeVariables
    [Q.termNodeType current | (_, current) <- Q.termGraphNodes graph,
      Q.TypedGlobal{} <- [Q.termNodeForm current]]

  -- Traverse this rooted subtree only. A sibling must not supply a type scope.
  subtreeTypes key = collect Set.empty [key]
   where
    collect _ [] = Right []
    collect visited (next : rest)
      | next `Set.member` visited = collect visited rest
      | otherwise = do
          current <- node next
          remaining <- collect (Set.insert next visited) $
            children (Q.termNodeForm current) ++ rest
          pure $ Q.termNodeType current : remaining
    children form = case form of
      Q.TypedLocal{} -> []
      Q.TypedGlobal{} -> []
      Q.TypedHole{} -> []
      Q.TypedLambda _ body -> [body]
      Q.TypedApply function argument _ -> [function, argument]
      Q.TypedVisibleTypeApplication _ function _ _ -> [function]
      Q.TypedImplicitTypeApplication _ function _ -> [function]
      Q.TypedForallIntroduction _ body _ -> [body]
      Q.TypedContextIntroduction _ body _ -> [body]
      Q.TypedContextApplication _ function _ -> [function]
      Q.TypedTuple elements -> elements
      Q.TypedLet _ value body -> [value, body]
      Q.TypedCase scrutinee alternatives -> scrutinee : map snd alternatives
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
      _ | null remaining -> render names scope givens Map.empty key
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

  bindPattern locals pattern = case Q.typedPatternNode pattern of
    Q.TypedBind variable -> Map.insert variable (Q.typedPatternType pattern) locals
    Q.TypedWildcard -> locals
    Q.TypedConstructor _ fields -> foldl bindPattern locals fields
    Q.TypedTuplePattern fields -> foldl bindPattern locals fields
    Q.TypedAs variable child -> bindPattern
      (Map.insert variable (Q.typedPatternType pattern) locals) child

  render names scope givens locals key = do
    current <- node key
    let protected = Map.keysSet scope `Set.union`
          capturedGlobalVariables `Set.union`
          T.freeVariables (Q.termNodeType current) `Set.union`
          foldMap T.freeVariables locals `Set.union`
          foldMap (foldMap T.freeVariables) givens
        available = flexibleVariables `Set.difference` protected
    carriers <- if Set.null available then pure [] else do
      types <- subtreeTypes key
      let eligible ty = let free = T.freeVariables ty in
            not (Set.null $ free `Set.intersection` available) &&
              free `Set.isSubsetOf` (Map.keysSet scope `Set.union` available)
          select _ [] = []
          select covered (ty : remaining)
            | T.freeVariables ty `Set.isSubsetOf` covered = select covered remaining
            | otherwise = ty : select (covered `Set.union` T.freeVariables ty) remaining
      pure $ select (Map.keysSet scope) $ filter eligible types
    if null carriers then renderNode names scope givens locals key current else do
      let fresh = Set.toAscList $ foldMap T.freeVariables carriers
            `Set.difference` Map.keysSet scope
          spellings = freshSpellings (Set.fromList $ Map.elems scope)
            ["djexMeta" ++ show (Q.termNodeIdValue key) ++ "_" ++ show index
            | index <- [0 :: Int .. length fresh - 1]]
          nested = Map.union (Map.fromList $ zip fresh spellings) scope
          name = metaHelper key
          helperType = foldr (\ty -> T.FunctionType $ T.FunctionType ty ty)
            (Q.termNodeType current) carriers
      signature <- typeText nested helperType
      result <- render names nested givens locals key
      pure $ parens $ "let { " ++ name ++ " :: forall " ++ unwords spellings
        ++ ". " ++ signature ++ "; " ++ name ++ concatMap (const " _") carriers
        ++ " = " ++ result ++ " } in " ++ name
        ++ concatMap (const " (\\djexIdentity -> djexIdentity)") carriers

  renderNode names scope givens locals key current =
    case Q.termNodeForm current of
      Q.TypedLocal _ variable -> local names variable
      Q.TypedGlobal _ name -> Right $ renderNamePrefix qualification name
      Q.TypedHole{} -> Left HaskellGraphHole
      Q.TypedLambda patterns body -> do
        parameters <- traverse (patternText names scope) patterns
        result <- render names scope givens (foldl bindPattern locals patterns) body
        pure $ parens $ "\\" ++ unwords parameters ++ " -> " ++ result
      Q.TypedApply function argument witness -> do
        f <- render names scope givens locals function
        a <- render names scope givens locals argument
        domain <- typeText scope $ Q.applicationDomain witness
        pure $ parens $ parens f ++ " " ++ annotate a domain
      Q.TypedVisibleTypeApplication _ function _ witness ->
        application names scope givens locals function (Q.typeApplicationSource witness)
          (Q.typeApplicationSelected witness)
      Q.TypedImplicitTypeApplication _ function witness ->
        application names scope givens locals function (Q.implicitTypeApplicationSource witness)
          (Q.implicitTypeApplicationSelected witness)
      Q.TypedForallIntroduction _ body witness ->
        case (Q.forallIntroductionSource witness, Q.forallIntroductionVariable witness) of
          (T.ForallType (_ : _) _ _, T.TypeVariable variable) -> do
            let spelling = freshSpelling (Set.fromList $ Map.elems scope) $
                  "djexSkolem" ++ show (Q.termNodeIdValue key)
                nested = Map.insert variable spelling scope
                name = helper key
            opened <- typeText nested $ Q.forallIntroductionBody witness
            result <- render names nested givens locals body
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
        result <- render names scope (Map.union introduced givens) locals body
        pure $ annotate result signature
      Q.TypedContextApplication _ function witness -> do
        let source = Q.contextApplicationSource witness
            constraints = Q.contextApplicationConstraints witness
            evidence = Q.contextApplicationEvidence witness
        checkContext source constraints $ Q.contextApplicationResult witness
        unless (length constraints == length evidence) $
          Left HaskellGraphUnsupportedContextEvidence
        mapM_ (checkGiven givens) $ zip constraints evidence
        f <- render names scope givens locals function
        signature <- typeText scope source
        result <- typeText scope $ Q.contextApplicationResult witness
        pure $ annotate (annotate f signature) result
      Q.TypedTuple elements -> do
        fields <- traverse (render names scope givens locals) elements
        pure $ parens $ intercalate ", " fields
      Q.TypedLet pattern value body -> do
        binder <- patternText names scope pattern
        rhs <- render names scope givens locals value
        result <- render names scope givens (bindPattern locals pattern) body
        pure $ parens $ "let { " ++ binder ++ " = " ++ rhs ++ " } in " ++ result
      Q.TypedCase scrutinee alternatives -> do
        value <- render names scope givens locals scrutinee
        cases <- traverse (\(pattern, body) -> do
          binder <- patternText names scope pattern
          result <- render names scope givens (bindPattern locals pattern) body
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

  application names scope givens locals function source selected = do
    f <- render names scope givens locals function
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

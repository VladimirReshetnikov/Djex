-- | Render a selected candidate in the binding scope of the requested source
-- signature. Shared by public frontends; source spelling supplies no graph
-- authority and never substitutes another candidate's type evidence.
module Language.Haskell.Djex.Command.Typed
  ( elaborateSourceCandidate
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.Djex
import Language.Haskell.Djex.HaskellSrc
  ( ParsedSourceType, parsedSourceType, parsedSourceTypeVariableNames )
import qualified Language.Haskell.Synthesis.Type as Type
import qualified Language.Haskell.Synthesis.TypedGenerated.Haskell as Haskell

elaborateSourceCandidate
  :: (Show failure, Ord variable, Ord local)
  => ParsedSourceType
  -> (Type.Type String -> Type.Type String)
  -> RenderOptions local
  -> TypedCandidate failure (Type.Type (Type.Variable variable)) local candidate
  -> (String, Either String String)
elaborateSourceCandidate parsed projectSignature options candidate =
  case typedCandidateTermGraph candidate of
    Left failure ->
      ("graph unavailable: " ++ show failure,
       Left $ "no source graph for this candidate: " ++ show failure)
    Right graph ->
      ( "graph present; root=" ++ show (termGraphRoot graph)
          ++ "; nodes=" ++ show (length $ termGraphNodes graph)
      , either (Left . show) Right $
          case traverse (`Map.lookup` sourceNames) $ parsedSourceType parsed of
            Nothing -> Haskell.renderHaskellTermGraphWithMetavariables options graph
            Just signature -> Haskell.renderHaskellTermGraphAtSignatureWithMetavariables options
              (projectSignature signature) graph
      )
 where
  hintedNames = Map.fromList
    [(FlexibleVariable identifier, spelling)
    | (spelling, identifier) <- Map.toList $ parsedSourceTypeVariableNames parsed]
  -- Alpha-normalization can give repeated nested binders fresh identities
  -- without source-spelling hints. Their names are local presentation choices;
  -- they must not make us discard the root signature and its dictionary scope.
  -- Root binders still require their actual source spelling. A fresh name for
  -- one of those would refer to a different scope at the RHS binding site.
  nestedWithoutHints = Set.toAscList $
    foldMap Set.singleton (parsedSourceType parsed)
      `Set.difference` Map.keysSet hintedNames
      `Set.difference` Set.fromList (Type.leadingForallVariables $ parsedSourceType parsed)
  freshNames = filter (`Set.notMember` Set.fromList (Map.elems hintedNames))
    ["djexSource" ++ show index | index <- [0 :: Integer ..]]
  sourceNames = Map.union hintedNames $ Map.fromList $ zip nestedWithoutHints freshNames

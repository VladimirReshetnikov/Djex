-- | Render a selected candidate in the binding scope of the requested source
-- signature. Shared by public frontends; source spelling supplies no graph
-- authority and never substitutes another candidate's type evidence.
module Language.Haskell.Djex.Command.Typed
  ( elaborateSourceCandidate
  ) where

import qualified Data.Map.Strict as Map
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
  sourceNames = Map.fromList
    [(FlexibleVariable identifier, spelling)
    | (spelling, identifier) <- Map.toList $ parsedSourceTypeVariableNames parsed]

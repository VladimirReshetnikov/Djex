-- | Checked Djinn sessions behind Djex's backend-neutral query envelope.
--
-- A session seals the exact Djinn environment once and retains the shared
-- inventory that justified it.  Queries still use Djinn's own type and option
-- vocabulary: those values carry proof-search semantics that a premature
-- lowest-common-denominator configuration would obscure.
module Language.Haskell.Djex.Djinn
  ( DjinnSession
  , DjinnCandidate
  , DjinnCandidateDetails (..)
  , DjinnQueryMetadata (..)
  , DjinnRequest
  , DjinnResult
  , mkDjinnSession
  , djinnSessionInventory
  , runDjinnQuery
  ) where

import Djinn.Core
  ( DjinnCandidate
  , DjinnCandidateDetails (..)
  , Environment
  , HSymbol
  , HType
  , QueryOptions
  , SynthesisInventory
  , generatedReportCandidates
  , generatedReportCompletion
  , generatedReportEvidence
  , generatedReportFormula
  , generatedReportProof
  , inhabitGenerated
  , toSynthesisInventory
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , diagnostic
  , withCode
  , withContext
  )
import Language.Haskell.Synthesis.Generated (validateDefinitionName)
import Language.Haskell.Synthesis.Name
  ( Name
  , nameSpelling
  , renderCanonical
  )
import Language.Haskell.Synthesis.Query
  ( QueryRequest (..)
  , QueryResult (..)
  )
import Language.Haskell.Synthesis.Search
  ( Progress (Completed)
  , SearchBatch (SearchBatch)
  )

-- | A validated Djinn environment paired with its exact shared inventory.
-- The constructor is private so the two views cannot drift apart.
data DjinnSession = DjinnSession Environment SynthesisInventory

-- | Djinn-specific explanatory data that does not belong in the common
-- operational search status.
data DjinnQueryMetadata = DjinnQueryMetadata
  { djinnTranslatedFormula :: String
  , djinnFirstExploredProof :: Maybe String
  }
  deriving (Eq, Show)

type DjinnRequest = QueryRequest HType QueryOptions

type DjinnResult = QueryResult DjinnQueryMetadata DjinnCandidate

-- | Seal an already checked Djinn environment into a reusable session.
mkDjinnSession :: Environment -> Either Diagnostic DjinnSession
mkDjinnSession environment = case toSynthesisInventory environment of
  Left failure -> Left $ withContext (show failure)
    $ withCode "DJEX_DJINN_ENV"
    $ diagnostic Error "cannot seal the Djinn session environment"
  Right inventory -> Right $ DjinnSession environment inventory

djinnSessionInventory :: DjinnSession -> SynthesisInventory
djinnSessionInventory (DjinnSession _ inventory) = inventory

-- | Run one complete configured Djinn search and project it into a single
-- terminal shared batch.  Logical evidence stays independent of operational
-- completion: a validated candidate found before a budget expires remains a
-- candidate, while an empty truncated search remains undecided.
runDjinnQuery
  :: DjinnSession
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQuery (DjinnSession environment _) request = do
  target <- targetSymbol $ requestTarget request
  report <- case inhabitGenerated
      (requestOptions request)
      environment
      (requestContexts request)
      target
      (requestGoal request) of
    Left failure -> Left $ withContext failure
      $ withCode "DJEX_DJINN_QUERY"
      $ diagnostic Error "Djinn rejected the query"
    Right value -> Right value
  let metadata = DjinnQueryMetadata
        { djinnTranslatedFormula = generatedReportFormula report
        , djinnFirstExploredProof = generatedReportProof report
        }
      batch = SearchBatch
        (Completed $ generatedReportCompletion report)
        metadata
        (generatedReportCandidates report)
  pure $ QueryResult (generatedReportEvidence report) batch

targetSymbol :: Name -> Either Diagnostic HSymbol
targetSymbol target
  | Right () <- validateDefinitionName target
  , Just spelling <- nameSpelling target
  = Right spelling
  | otherwise = Left $ withContext (renderCanonical target)
      $ withCode "DJEX_DJINN_TARGET"
      $ diagnostic Error
          "Djinn targets must be unqualified value identifiers or operators"

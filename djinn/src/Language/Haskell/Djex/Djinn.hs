-- | Checked Djinn sessions behind Djex's backend-neutral query envelope.
--
-- A session seals the exact Djinn environment once and retains the shared
-- inventory that justified it.  Queries still use Djinn's own type and option
-- vocabulary: those values carry proof-search semantics that a premature
-- lowest-common-denominator configuration would obscure.
module Language.Haskell.Djex.Djinn
  ( DjinnSession
  , HType
  , Context
  , QueryOptions (..)
  , defaultQueryOptions
  , DjinnCandidate
  , DjinnCandidateDetails (..)
  , Qualification (..)
  , DjinnCandidateRenderError (..)
  , DjinnQueryMetadata (..)
  , DjinnRequest
  , DjinnResult
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionInventory
  , parseDjinnRequest
  , runDjinnQuery
  , renderDjinnCandidateExpression
  , renderDjinnCandidateDefinition
  ) where

import Data.Bifunctor (first)
import Text.ParserCombinators.ReadP
  ( ReadP
  , eof
  , option
  , readP_to_S
  , skipSpaces
  )

import Djinn.Core
  ( Context
  , DjinnCandidate
  , DjinnCandidateDetails (..)
  , Environment
  , HSymbol
  , HType
  , PreparedEnvironment
  , QueryOptions (..)
  , SynthesisInventory
  , defaultQueryOptions
  , generatedReportCandidates
  , generatedReportCompletion
  , generatedReportEvidence
  , generatedReportFormula
  , generatedReportProof
  , inhabitGeneratedPrepared
  , prepareEnvironment
  , preparedEnvironmentInventory
  , standardEnvironment
  )
import Djinn.Internal.HTypes (pHContext, pHType)
import Language.Haskell.Synthesis.Candidate (candidateOutput)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , diagnostic
  , withCode
  , withContext
  , withSource
  )
import Language.Haskell.Synthesis.Generated
  ( Qualification (..)
  , RenderError
  , RenderOptions (renderQualification)
  , defaultRenderOptions
  , functionClauseExpression
  , renderExpression
  , renderFunctionClause
  , validateDefinitionName
  )
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
newtype DjinnSession = DjinnSession PreparedEnvironment

-- | Djinn-specific explanatory data that does not belong in the common
-- operational search status.
data DjinnQueryMetadata = DjinnQueryMetadata
  { djinnTranslatedFormula :: String
  , djinnFirstExploredProof :: Maybe String
  }
  deriving (Eq, Show)

type DjinnRequest = QueryRequest HType QueryOptions

type DjinnResult = QueryResult DjinnQueryMetadata DjinnCandidate

data DjinnCandidateRenderError
  = DjinnGeneratedRenderError RenderError
  deriving (Eq, Show)

-- | Seal an already checked Djinn environment into a reusable session.
mkDjinnSession :: Environment -> Either Diagnostic DjinnSession
mkDjinnSession environment = case prepareEnvironment environment of
  Left failure -> Left $ withContext (show failure)
    $ withCode "DJEX_DJINN_ENV"
    $ diagnostic Error "cannot seal the Djinn session environment"
  Right prepared -> Right $ DjinnSession prepared

-- | The historical checked Djinn prelude, sealed for facade-only clients.
-- Advanced clients can still build an editable 'Environment' through
-- "Djinn.Core" and pass it to 'mkDjinnSession'.
standardDjinnSession :: Either Diagnostic DjinnSession
standardDjinnSession = mkDjinnSession standardEnvironment

djinnSessionInventory :: DjinnSession -> SynthesisInventory
djinnSessionInventory (DjinnSession prepared) =
  preparedEnvironmentInventory prepared

-- | Parse the type portion of a Djinn query.  The accepted context grammar is
-- exactly the historical one: either one constraint or a comma-separated
-- parenthesized list, followed by @=>@ and the goal.  The session argument
-- keeps this boundary parallel with 'parseExferenceRequest'; Djinn's parser
-- itself is environment-independent, while kind and class lookup remain part
-- of 'runDjinnQuery'.
parseDjinnRequest
  :: DjinnSession
  -> QueryOptions
  -> Name
  -> FilePath
  -> String
  -> Either Diagnostic DjinnRequest
parseDjinnRequest _session options target sourceName source = do
  -- Preserve command-boundary precedence: an invalid output name is a usage
  -- error even when the source text is also malformed.
  _ <- targetSymbol target
  (contexts, goal) <- case parseContextualType source of
    Right parsed -> Right parsed
    Left failure -> Left $ withContext failure
      $ withSource sourceName
      $ withCode "DJEX_DJINN_PARSE"
      $ diagnostic Error "cannot parse the Djinn query type"
  pure QueryRequest
    { requestTarget = target
    , requestGoal = goal
    , requestContexts = contexts
    , requestOptions = options
    }

-- | Run one complete configured Djinn search and project it into a single
-- terminal shared batch.  Logical evidence stays independent of operational
-- completion: a validated candidate found before a budget expires remains a
-- candidate, while an empty truncated search remains undecided.
runDjinnQuery
  :: DjinnSession
  -> DjinnRequest
  -> Either Diagnostic DjinnResult
runDjinnQuery (DjinnSession prepared) request = do
  target <- targetSymbol $ requestTarget request
  report <- case inhabitGeneratedPrepared
      (requestOptions request)
      prepared
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

renderDjinnCandidateExpression
  :: Qualification
  -> DjinnCandidate
  -> Either DjinnCandidateRenderError String
renderDjinnCandidateExpression qualification candidate = first
  DjinnGeneratedRenderError
  $ renderExpression (candidateRenderOptions qualification)
  $ functionClauseExpression
  $ candidateOutput candidate

renderDjinnCandidateDefinition
  :: Qualification
  -> DjinnCandidate
  -> Either DjinnCandidateRenderError String
renderDjinnCandidateDefinition qualification = first
  DjinnGeneratedRenderError
  . renderFunctionClause (candidateRenderOptions qualification)
  . candidateOutput

candidateRenderOptions :: Qualification -> RenderOptions HSymbol
candidateRenderOptions qualification =
  (defaultRenderOptions id) {renderQualification = qualification}

parseContextualType :: String -> Either String ([Context], HType)
parseContextualType source = case
    [parsed | (parsed, "") <- readP_to_S parser source] of
  parsed : _ -> Right parsed
  [] -> Left $ "cannot parse contextual type: " ++ show source
 where
  parser :: ReadP ([Context], HType)
  parser = do
    contexts <- option [] pHContext
    goal <- pHType
    skipSpaces
    eof
    pure (contexts, goal)

targetSymbol :: Name -> Either Diagnostic HSymbol
targetSymbol target
  | Right () <- validateDefinitionName target
  , Just spelling <- nameSpelling target
  = Right spelling
  | otherwise = Left $ withContext (renderCanonical target)
      $ withCode "DJEX_DJINN_TARGET"
      $ diagnostic Error
          "Djinn targets must be unqualified value identifiers or operators"

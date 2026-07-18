-- | Parser-neutral compatibility facade over the Exference search core.
module Language.Haskell.Exference
  ( findExpressions
  , findOneExpression
  , ExferenceInput ( .. )
  , ExferenceHeuristicsConfig (..)
  , ExferenceOutputElement
  , ExferenceChunkElement (..)
  , ExferenceBatchMetadata (..)
  , ExferenceCandidateDetails (..)
  , ExferenceTypeVariableHints
  , ExferenceCandidate
  , ExferenceStats (..)
  , ExferenceInputError (..)
  , findExpressionsEither
  , findExpressionsWithStatsEither
  , SearchCompletion (..)
  , SearchStatus (..)
  , Penalty (..)
  , Priority (..)
  )
where



import Data.Maybe ( listToMaybe )

import Language.Haskell.Exference.Core hiding ( findExpressions )
import qualified Language.Haskell.Exference.Core as Core

import Language.Haskell.Exference.Core.ExferenceStats



-- These list-returning searches predate Djex's checked query envelope. Keep
-- them as small compatibility projections, but leave every presentation
-- policy to the shared Selection API.
{-# DEPRECATED findExpressions, findOneExpression
  "Use Language.Haskell.Djex.Exference.runExferenceQuery; apply Language.Haskell.Synthesis.Selection.selectQueryResults when selecting results." #-}

findExpressions :: ExferenceInput -> [ExferenceOutputElement]
findExpressions = either (const []) id . Core.findExpressionsEither

-- | Return the first raw search result, without applying a presentation
-- policy or comparing candidate ratings.
findOneExpression :: ExferenceInput -> Maybe ExferenceOutputElement
findOneExpression = listToMaybe . findExpressions

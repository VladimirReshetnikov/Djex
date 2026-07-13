{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-neutral synthesis candidates.
--
-- A candidate combines checked generated output with any residual constraints
-- and backend-specific evidence or statistics.  Keeping those concerns in one
-- value lets query adapters share a result shape without erasing information
-- that only the producing backend can interpret.
module Language.Haskell.Synthesis.Candidate
  ( Candidate (..)
  , renderCandidateExpression
  , renderCandidateDefinition
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Constraint (Constraint)
import Language.Haskell.Synthesis.Generated
  ( FunctionClause
  , RenderError
  , RenderOptions
  , functionClauseExpression
  , renderExpression
  , renderFunctionClause
  )

-- | One generated output and the obligations and details attached to it.
--
-- The output parameter is last so ordinary 'fmap', 'foldMap', and 'traverse'
-- transform generated output without disturbing residual constraints or
-- backend-specific details.
data Candidate ty details output = Candidate
  { candidateOutput :: output
  , candidateResidualConstraints :: [Constraint ty]
  , candidateDetails :: details
  }
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    )

instance (NFData ty, NFData details, NFData output) =>
    NFData (Candidate ty details output)

-- | Render the expression denoted by a candidate's top-level clause.
--
-- Clause patterns become leading lambda patterns, while a patternless value
-- clause renders as its body.  Backends remain responsible for choosing local
-- name preferences and qualification through 'RenderOptions'.
renderCandidateExpression
  :: Ord local
  => RenderOptions local
  -> Candidate ty details (FunctionClause local)
  -> Either RenderError String
renderCandidateExpression options =
  renderExpression options . functionClauseExpression . candidateOutput

-- | Render a candidate as its complete top-level function equation.
renderCandidateDefinition
  :: Ord local
  => RenderOptions local
  -> Candidate ty details (FunctionClause local)
  -> Either RenderError String
renderCandidateDefinition options =
  renderFunctionClause options . candidateOutput

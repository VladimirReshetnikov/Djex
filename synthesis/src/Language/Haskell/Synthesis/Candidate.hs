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
      ( DuplicateLocalBinderIdentity
      , UnboundLocalIdentity
      )
  , RenderOptions
  , ScopeError (..)
  , functionClauseExpression
  , renderExpression
  , renderFunctionClause
  , validateFunctionClauseScope
  )

-- | One generated output and the obligations and details attached to it.
--
-- The output parameter is last so ordinary 'fmap', 'foldMap', and 'traverse'
-- transform generated output without disturbing residual constraints or
-- backend-specific details.
data Candidate ty details output = Candidate
  { candidateOutput :: output
    -- ^ Checked generated syntax or another backend-neutral output view.
  , candidateResidualConstraints :: [Constraint ty]
    -- ^ Unresolved obligations required by this particular output.
  , candidateDetails :: details
    -- ^ Backend-owned evidence, ranking inputs, or search statistics.
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
-- name preferences and qualification through 'RenderOptions'. The exported
-- The t'Candidate' constructor remains available for source compatibility, so
-- this boundary rejects caller-built outputs with free local identities or
-- reused binder identities before they can be presented as checked source.
renderCandidateExpression
  :: Ord local
  => RenderOptions local
  -> Candidate ty details (FunctionClause local)
  -> Either RenderError String
renderCandidateExpression options candidate = do
  validateCandidateScope candidate
  renderExpression options
    $ functionClauseExpression $ candidateOutput candidate

-- | Render a candidate as its complete top-level function equation.
--
-- Scope is checked here as well as in 'renderCandidateExpression': candidates
-- are inspectable public records, not an opaque proof that their generated
-- output came from a backend.
renderCandidateDefinition
  :: Ord local
  => RenderOptions local
  -> Candidate ty details (FunctionClause local)
  -> Either RenderError String
renderCandidateDefinition options candidate = do
  validateCandidateScope candidate
  renderFunctionClause options $ candidateOutput candidate

validateCandidateScope
  :: Ord local
  => Candidate ty details (FunctionClause local)
  -> Either RenderError ()
validateCandidateScope = either (Left . renderScopeError) Right
  . validateFunctionClauseScope
  . candidateOutput

-- Render errors deliberately do not carry the backend-specific local identity
-- type. Clients that need the offending identity can inspect 'candidateOutput'
-- with 'validateFunctionClauseScope' before rendering.
renderScopeError :: ScopeError local -> RenderError
renderScopeError scopeError = case scopeError of
  UnboundLocal{} -> UnboundLocalIdentity
  DuplicatePatternBinder{} -> DuplicateLocalBinderIdentity

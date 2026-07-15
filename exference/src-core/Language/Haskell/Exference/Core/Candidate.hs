-- | Exference's information-preserving projection to shared candidates.
module Language.Haskell.Exference.Core.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceCandidateError (..)
  , ExferenceTypeVariableHints
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableHintError (..)
  , ExferenceGeneratedCandidate
  , mkExferenceGeneratedCandidate
  , emptyExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableHints
  , typeVariableHints
  ) where

import Language.Haskell.Exference.Core.Internal.Candidate

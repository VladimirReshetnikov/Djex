-- | Exference's information-preserving projection to shared candidates.
module Language.Haskell.Exference.Core.Candidate
  ( ExferenceCandidateDetails (..)
  , ExferenceTypeVariableHints
  , ExferenceSourceTypeVariableHints
  , ExferenceSourceTypeVariableHintError (..)
  , ExferenceCandidate
  , emptyExferenceSourceTypeVariableHints
  , mkExferenceSourceTypeVariableHints
  ) where

import Language.Haskell.Exference.Core.Internal.Candidate

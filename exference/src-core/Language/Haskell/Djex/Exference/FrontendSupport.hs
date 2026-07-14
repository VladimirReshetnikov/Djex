-- | Parser-neutral support for implementing Exference source frontends.
--
-- This module is an explicit service-provider interface: it exposes the
-- narrow operations needed to turn an already checked source projection into
-- a sealed session and to attach source provenance to a checked request. It
-- does not expose either opaque representation, and applications that already
-- have a neutral synthesis environment should use
-- "Language.Haskell.Djex.Exference" instead.
--
-- The module is intentionally not re-exported by the default @djex@ library
-- or by @exference-frontend@. A source adapter opts into this lower-level
-- contract by importing it directly from @djex:exference-core@.
module Language.Haskell.Djex.Exference.FrontendSupport
  ( sealPreparedExferenceSessionWithPolicy
  , mkExferenceRequestWithSourceInfo
  , validateExferenceTarget
  , allocateFreshTypeVariableId
  ) where

import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)

import Language.Haskell.Djex.Exference
  ( ExferenceLocal
  , ExferenceOptions
  , ExferenceRequest
  , ExferenceSession
  , ExferenceType
  , Penalty
  )
import qualified Language.Haskell.Djex.Exference.Internal.Request as Request
import qualified Language.Haskell.Djex.Exference.Internal.Session as Session
import Language.Haskell.Exference.Core.Declaration
  ( PreparedNeutralSynthesisInventory )
import qualified Language.Haskell.Exference.Core.Internal.FlexibleIds
  as FlexibleIds
import Language.Haskell.Exference.Core.Types (TVarId)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , SourceSpan
  )
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Query (QueryRequest)

-- | Seal a source frontend's checked, internally consistent projection while
-- applying the same exclusions and rating overrides as the neutral session
-- constructor. The prepared inventory's opaque representation prevents a
-- frontend from pairing declarations with an unrelated search dictionary.
sealPreparedExferenceSessionWithPolicy
  :: [Name]
  -> Map Name Penalty
  -> PreparedNeutralSynthesisInventory
  -> Either Diagnostic ExferenceSession
sealPreparedExferenceSessionWithPolicy exclusions overrides prepared =
  Session.sealPreparedExferenceSessionWithPolicy
    exclusions overrides prepared

-- | Construct a checked request and attach parser-neutral rendering hints and
-- source location information. Provenance cannot be replaced after the
-- request has been sealed, and it does not affect request equality or display.
mkExferenceRequestWithSourceInfo
  :: Map String ExferenceLocal
  -> Maybe (FilePath, SourceSpan)
  -> QueryRequest ExferenceType ExferenceOptions
  -> Either Diagnostic ExferenceRequest
mkExferenceRequestWithSourceInfo sourceVariables sourceLocation query =
  Request.mkExferenceRequestWithSourceInfo
    sourceVariables sourceLocation query

-- | Validate a raw source-level result name before parsing its type. Source
-- frontends use this preflight to preserve target-diagnostic precedence.
validateExferenceTarget :: Name -> Either Diagnostic DefinitionName
validateExferenceTarget = Request.validateExferenceTarget

-- | Allocate the first available type-variable identifier from an exact
-- parser-neutral namespace. Exhaustion returns 'Nothing'; sparse and boundary
-- identifiers never wrap or collide.
allocateFreshTypeVariableId :: IntSet.IntSet -> Maybe TVarId
allocateFreshTypeVariableId reserved = fst
  <$> FlexibleIds.allocateFreshIdentifier
    (FlexibleIds.supplyFromIdentifierSet reserved)

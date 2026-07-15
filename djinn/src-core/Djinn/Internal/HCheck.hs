--
-- Copyright (c) 2005 Lennart Augustsson
-- See LICENSE for licensing details.
--

-- | Pre-cache raw kind-checking compatibility surface.
--
-- Prepared-session construction belongs to Djinn's hidden implementation;
-- exposing it here would let compatibility clients depend on private indexes
-- that exist only to connect a sealed Djex inventory to proof search.
module Djinn.Internal.HCheck
    ( htCheckEnv
    , htCheckType
    , htCheckTypeKind
    , htCheckTypesKinds
    , htInferClassKinds
    ) where

import qualified Djinn.Internal.HCheck.Implementation as Implementation
import Djinn.Internal.HTypes (HKind, HSymbol, HType)

-- Explicit forwarders keep these compatibility names owned by this exposed
-- module. A bare re-export would leak the hidden implementation module through
-- Template Haskell names and interface documentation.
htCheckEnv
    :: [(HSymbol, ([HSymbol], HType, annotation))]
    -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv = Implementation.htCheckEnv

htCheckType
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> HType
    -> Either String ()
htCheckType = Implementation.htCheckType

htCheckTypeKind
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> HKind
    -> HType
    -> Either String ()
htCheckTypeKind = Implementation.htCheckTypeKind

htCheckTypesKinds
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> [(HKind, HType)]
    -> Either String ()
htCheckTypesKinds = Implementation.htCheckTypesKinds

htInferClassKinds
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> [HSymbol]
    -> [HType]
    -> Either String [(HSymbol, HKind)]
htInferClassKinds = Implementation.htInferClassKinds

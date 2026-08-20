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

-- | Kind-check a raw type environment, inferring and attaching the ground
-- kind of every declared type constructor.
--
-- Explicit forwarders keep these compatibility names owned by this exposed
-- module. A bare re-export would leak the hidden implementation module through
-- Template Haskell names and interface documentation.
htCheckEnv
    :: [(HSymbol, ([HSymbol], HType, annotation))]
    -> Either String [(HSymbol, ([HSymbol], HType, HKind))]
htCheckEnv = Implementation.htCheckEnv

-- | Check that a type has kind @*@ in the given checked environment
-- (equivalent to 'htCheckTypeKind' with @KStar@).
htCheckType
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> HType
    -> Either String ()
htCheckType = Implementation.htCheckType

-- | Check that a type is well-kinded and has the given ground kind.  Free
-- type variables receive fresh kinds, so a variable fits any expected kind
-- while a mis-kinded application is still rejected.
htCheckTypeKind
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> HKind
    -> HType
    -> Either String ()
htCheckTypeKind = Implementation.htCheckTypeKind

-- | Check several expected-kind\/type pairs in one kind-inference scope, so
-- that a type variable shared between the types is assigned a single kind.
htCheckTypesKinds
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> [(HKind, HType)]
    -> Either String ()
htCheckTypesKinds = Implementation.htCheckTypesKinds

-- | Infer the kind of each class parameter from the class's method types, as
-- Haskell 98 does; unconstrained parameters default to @*@.  The result pairs
-- the parameters, in the given order, with their inferred kinds.
htInferClassKinds
    :: [(HSymbol, ([HSymbol], HType, HKind))]
    -> [HSymbol]
    -> [HType]
    -> Either String [(HSymbol, HKind)]
htInferClassKinds = Implementation.htInferClassKinds

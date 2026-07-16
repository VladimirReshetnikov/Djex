{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | Negative API fixtures for opaque invariant-bearing values.
--
-- Missing dictionaries are deliberately deferred so the ordinary test runner
-- can assert that they remain missing. If an abstract type regains 'Generic',
-- or an ordinary projection becomes a record field again, the corresponding
-- thunk evaluates successfully and turns the API regression into a test
-- failure. This module is a separate Cabal component, so it sees exactly the
-- public @djex@ surface rather than home-module constructors.
module AbstractionBoundary
  ( allowedConstructionAttempts
  , forbiddenConstructionAttempts
  ) where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Djinn.Internal.LJTFormula (Formula, Symbol)
-- Whole-module imports are deliberate for modules whose former record labels
-- are probed below. GHC solves built-in 'HasField' constraints only when the
-- corresponding field selector is in scope; importing just the owner type
-- would let a record-field regression pass this test unnoticed.
import Djinn.Internal.ProofEnv
import GHC.Generics (Generic, Rep, from)
import GHC.Records (HasField, getField)
import qualified GHC.TypeLits as TypeLits
import Language.Haskell.Djex.Djinn (DjinnRequest, DjinnSession)
import Language.Haskell.Djex.Exference
  ( ExferenceEnvironment
  , ExferenceRequest
  , ExferenceSession
  )
import Language.Haskell.Exference.Core
  ( ExferenceSourceTypeVariableHints )
import qualified Language.Haskell.Exference.Core as ExferenceCore
import Language.Haskell.Exference.Core.Declaration
  ( PreparedSynthesisInventory
  , preparedSynthesisBackend
  , preparedSynthesisWitness
  )
import Language.Haskell.Exference.Core.FunctionBinding (EnvDictionary)
import Language.Haskell.Exference.Core.Internal.Scope (ScopeId, Scopes)
import Language.Haskell.Exference.Core.RigidInstantiation
import Language.Haskell.Exference.Core.Types
  ( HsConstraint
  , HsInstance
  , HsTypeClass
  , QueryClassEnv
  , SynthesisVariable
  , StaticClassEnv
  , qClassEnv_constraints
  , qClassEnv_env
  , qClassEnv_inflatedConstraints
  , sClassEnv_explicitInstances
  , sClassEnv_instances
  , sClassEnv_tclasses
  )
import Language.Haskell.Synthesis.Collection (DuplicateSummary)
import Language.Haskell.Synthesis.Diagnostic
  ( SourceLocation
  , SourcePosition
  , SourceSpan
  )
import Language.Haskell.Synthesis.Environment (Environment)
import Language.Haskell.Synthesis.Generated (DefinitionName)
import Language.Haskell.Synthesis.Inventory
import Language.Haskell.Synthesis.KindInference
  ( KindAssumptions )
import Language.Haskell.Synthesis.Name (ModuleName, Name)
import Language.Haskell.Synthesis.Query
import Language.Haskell.Synthesis.Search (SearchBatch)
import Language.Haskell.Synthesis.TypeSynonym
  ( PreparedInventory
  , TypeSynonyms
  , preparedInventory
  , preparedTypeSynonyms
  )

forbiddenConstructionAttempts :: [(String, ())]
forbiddenConstructionAttempts =
  [ noGeneric @(Environment Int Void ()) "Environment"
  , noGeneric @(Inventory Int ()) "Inventory"
  , noGeneric @(TypeSynonyms Int) "TypeSynonyms"
  , noGeneric @(PreparedInventory Int ()) "PreparedInventory"
  , noGeneric @(QueryResult () ()) "QueryResult"
  , noGeneric @(CachedQuery () () ()) "CachedQuery"
  , noGeneric @DefinitionName "DefinitionName"
  , noGeneric @Name "Name"
  , noGeneric @ModuleName "ModuleName"
  , noGeneric @SourcePosition "SourcePosition"
  , noGeneric @SourceSpan "SourceSpan"
  , noGeneric @SourceLocation "SourceLocation"
  , noGeneric @(DuplicateSummary Int) "DuplicateSummary"
  , noGeneric @DjinnSession "DjinnSession"
  , noGeneric @DjinnRequest "DjinnRequest"
  , noGeneric @ExferenceSession "ExferenceSession"
  , noGeneric @ExferenceEnvironment "ExferenceEnvironment"
  , noGeneric @ExferenceRequest "ExferenceRequest"
  , noGeneric @ExferenceCore.ExferenceEnvironment
      "Core.ExferenceEnvironment"
  , noGeneric @ExferenceSourceTypeVariableHints
      "ExferenceSourceTypeVariableHints"
  , noGeneric @(PreparedSynthesisInventory ())
      "PreparedSynthesisInventory"
  , noGeneric @RigidInstantiationContext "RigidInstantiationContext"
  , noGeneric @RigidInstantiationPlan "RigidInstantiationPlan"
  , noGeneric @StaticClassEnv "StaticClassEnv"
  , noGeneric @QueryClassEnv "QueryClassEnv"
  , noGeneric @ScopeId "ScopeId"
  , noGeneric @(Scopes ()) "Scopes"
  , noGeneric @ProofEnvironment "ProofEnvironment"
  , noField
      @"inventoryEnvironment"
      @(Inventory Int ())
      @(Environment Int Void ())
      "Inventory.inventoryEnvironment"
  , noField
      @"inventoryKindAssumptions"
      @(Inventory Int ())
      @KindAssumptions
      "Inventory.inventoryKindAssumptions"
  , noField
      @"resultEvidence"
      @(QueryResult () ())
      @QueryEvidence
      "QueryResult.resultEvidence"
  , noField
      @"resultSearch"
      @(QueryResult () ())
      @(SearchBatch () ())
      "QueryResult.resultSearch"
  , noField
      @"rigidInstantiations"
      @RigidInstantiationPlan
      @[(Int, Int)]
      "RigidInstantiationPlan.rigidInstantiations"
  , noField
      @"preparedInventory"
      @(PreparedInventory Int ())
      @(Inventory Int ())
      "PreparedInventory.preparedInventory"
  , noField
      @"preparedTypeSynonyms"
      @(PreparedInventory Int ())
      @(TypeSynonyms Int)
      "PreparedInventory.preparedTypeSynonyms"
  , noField
      @"preparedSynthesisWitness"
      @(PreparedSynthesisInventory ())
      @(PreparedInventory SynthesisVariable ())
      "PreparedSynthesisInventory.preparedSynthesisWitness"
  , noField
      @"preparedSynthesisBackend"
      @(PreparedSynthesisInventory ())
      @EnvDictionary
      "PreparedSynthesisInventory.preparedSynthesisBackend"
  , noField
      @"sClassEnv_tclasses"
      @StaticClassEnv
      @(Map.Map Name HsTypeClass)
      "StaticClassEnv.sClassEnv_tclasses"
  , noField
      @"sClassEnv_explicitInstances"
      @StaticClassEnv
      @[HsInstance]
      "StaticClassEnv.sClassEnv_explicitInstances"
  , noField
      @"sClassEnv_instances"
      @StaticClassEnv
      @(Map.Map Name [HsInstance])
      "StaticClassEnv.sClassEnv_instances"
  , noField
      @"qClassEnv_env"
      @QueryClassEnv
      @StaticClassEnv
      "QueryClassEnv.qClassEnv_env"
  , noField
      @"qClassEnv_constraints"
      @QueryClassEnv
      @(Set.Set HsConstraint)
      "QueryClassEnv.qClassEnv_constraints"
  , noField
      @"qClassEnv_inflatedConstraints"
      @QueryClassEnv
      @(Set.Set HsConstraint)
      "QueryClassEnv.qClassEnv_inflatedConstraints"
  , noField
      @"proofBindings"
      @ProofEnvironment
      @[(Symbol, Formula)]
      "ProofEnvironment.proofBindings"
  , noField
      @"proofBindingsIncludingTarget"
      @ProofEnvironment
      @[(Symbol, Formula)]
      "ProofEnvironment.proofBindingsIncludingTarget"
  , noField
      @"targetWasExcluded"
      @ProofEnvironment
      @Bool
      "ProofEnvironment.targetWasExcluded"
  ]

-- Positive controls prove that both dictionary-forcing helpers work and that
-- a public record label is visible to the built-in 'HasField' solver.
allowedConstructionAttempts :: [(String, ())]
allowedConstructionAttempts =
  [ ("QueryRequest Generic", genericMethod @(QueryRequest () ()))
  , ( "QueryRequest.requestGoal HasField"
    , fieldMethod @"requestGoal" @(QueryRequest () ()) @()
    )
  ]

noGeneric :: forall value. Generic value => String -> (String, ())
noGeneric label =
  (label ++ " unexpectedly has Generic", genericMethod @value)

-- Selecting the method forces the instance dictionary without needing a
-- value of the abstract type or forcing a representation.
genericMethod :: forall value. Generic value => ()
genericMethod = (from :: value -> Rep value ()) `seq` ()

noField
  :: forall (label :: TypeLits.Symbol) record field
   . HasField label record field
  => String
  -> (String, ())
noField description =
  ( description ++ " unexpectedly remains a record field"
  , fieldMethod @label @record @field
  )

-- Selecting a method forces its instance dictionary without requiring a
-- record value. Keeping every probed projection in this expression is also
-- semantically significant: a built-in 'HasField' constraint is solvable only
-- when the corresponding selector is in scope.
fieldMethod
  :: forall (label :: TypeLits.Symbol) record field
   . HasField label record field
  => ()
fieldMethod = selectorNamesInScope `seq`
  getField @label @record @field `seq`
  (Proxy @label `seq` Proxy @record `seq` Proxy @field `seq` ())

selectorNamesInScope :: ()
selectorNamesInScope =
  inventoryEnvironment `seq`
  inventoryKindAssumptions `seq`
  resultEvidence `seq`
  resultSearch `seq`
  rigidInstantiations `seq`
  preparedInventory `seq`
  preparedTypeSynonyms `seq`
  preparedSynthesisWitness `seq`
  preparedSynthesisBackend `seq`
  sClassEnv_tclasses `seq`
  sClassEnv_explicitInstances `seq`
  sClassEnv_instances `seq`
  qClassEnv_env `seq`
  qClassEnv_constraints `seq`
  qClassEnv_inflatedConstraints `seq`
  proofBindings `seq`
  proofBindingsIncludingTarget `seq`
  targetWasExcluded `seq`
  ()

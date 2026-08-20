{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private canonical identity for shared checked typed term graphs.
--
-- The input is re-sealed with 'sharedTypeStructure': 'TermGraph' deliberately
-- does not retain the caller-supplied type checker which originally admitted
-- it.  Only that fresh shared check may produce an identity bearing this
-- module's nominal subject.
module Language.Haskell.Synthesis.Internal.TypedGenerated.Fingerprint
  ( TermGraphFingerprintSubject
  , defaultTermGraphFingerprintByteLimit
  , TermGraphFingerprintError (..)
  , fingerprintSharedTermGraph
  , fingerprintTermGraphWithTypeStructure
  , fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
  ( StateT
  , evalStateT
  , get
  , put
  )
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Constraint (Constraint)
import qualified Language.Haskell.Synthesis.Generated as Generated
import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import qualified Language.Haskell.Synthesis.Internal.Alpha as Alpha
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
import Language.Haskell.Synthesis.Internal.Fingerprint.Type
  ( asciiFingerprintBytes
  , canonicalTypeFingerprintForm
  , constraintFingerprintField
  , taggedFingerprintField
  , typeFingerprintField
  )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  ( CheckedTypeApplicationCertificateStep
  , TypeApplicationCertificatePlanVariable (..)
  , checkedTypeApplicationCertificateStepObligations
  , checkedTypeApplicationCertificateStepResult
  , checkedTypeApplicationCertificateStepSelected
  , checkedTypeApplicationCertificateStepSlot
  , checkedTypeApplicationCertificateStepSource
  )
import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  ( CheckedTypeApplicationCertificateGraph
  , checkedTypeApplicationCertificateGraph
  , foldCheckedTypeApplicationCertificateGraph
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Type (Type, Variable (..))
import Language.Haskell.Synthesis.TypedGenerated
  ( ApplicationWitness (..)
  , CertificateId
  , TermGraph
  , TermGraphError
  , TermGraphLimits
  , TermGraphSource (..)
  , TermNode (..)
  , TermNodeForm (..)
  , TermNodeId
  , TypeApplicationWitness (..)
  , TypeStructure
  , validTypeApplicationWitness
  , TypedPattern (..)
  , TypedPatternNode (..)
  , lookupTermNode
  , sealTermGraph
  , sharedTypeStructure
  , termGraphNodes
  , termGraphRoot
  )

-- | Nominal identity domain for canonical shared term-graph encodings.
data TermGraphFingerprintSubject

-- | Conservative retained-key bound for one graph sealed under
-- 'defaultTermGraphLimits'.  Callers with a different policy pass their exact
-- graph limits and byte limit explicitly to 'fingerprintSharedTermGraph'.
defaultTermGraphFingerprintByteLimit :: Natural
defaultTermGraphFingerprintByteLimit = 1048576

-- | Why an opaque graph could not receive a shared structural identity.
--
-- Certificate allocation numbers are intentionally not encoded.  A bare
-- certificate-bearing graph fails closed; only the private carrier-aware
-- entrance can replace its coordinates with checked substitution and
-- obligation semantics.
data TermGraphFingerprintError identity local
  = TermGraphFingerprintSharedResealError
      (TermGraphError (Type (Variable identity)) local)
  | TermGraphFingerprintUnsupportedCertificate !CertificateId
  | TermGraphFingerprintMissingNode !TermNodeId
  | TermGraphFingerprintUnboundLocal !TermNodeId local
  | TermGraphFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance (NFData identity, NFData local)
    => NFData (TermGraphFingerprintError identity local)

-- | Canonicalize and fingerprint one shared checked graph.
--
-- The structural v1 key ignores node-table order plus raw node, occurrence,
-- local-binder, hole, and private type-variable allocation numbers.  It
-- preserves the rooted term and shared-checkable pattern tree, lexical binding and hole
-- equality classes, flexible-versus-rigid free-variable flavor, exact global
-- names, every normalized type, application witnesses, visible type
-- arguments, and branch order.  It performs no beta, eta, let, or behavioral
-- quotienting.
--
-- 'sharedTypeStructure' deliberately has no constructor-family schema.  A
-- graph containing a constructor pattern therefore fails during the fresh
-- reseal.  An inventory-bound domain sealer must use the package-private
-- schema-parameterized entrance below to establish that authority atomically
-- before reusing this structural encoder for such a candidate.
--
-- This is a graph identity, not source-inventory or behavioral evidence.  A
-- domain-owned problem sealer must still resolve globals, bind the exact
-- request/inventory, reject incomplete candidates, and wrap these canonical
-- bytes in its own candidate role.
fingerprintSharedTermGraph
  :: (Ord identity, Ord local)
  => TermGraphLimits
  -> Natural
  -> TermGraph (Type (Variable identity)) local
  -> Either
      (TermGraphFingerprintError identity local)
      (Fingerprint TermGraphFingerprintSubject)
fingerprintSharedTermGraph graphLimits maximumBytes original = do
  fingerprintTermGraphWithTypeStructure
    sharedTypeStructure graphLimits maximumBytes original

-- | Package-private authority-parametric counterpart.
--
-- The supplied structure is consumed only by the atomic fresh reseal which
-- immediately precedes canonical encoding.  This lets a domain whose opaque
-- checked inventory owns constructor schemas admit those exact patterns
-- without widening 'fingerprintSharedTermGraph' or retaining a detachable
-- schema beside the resulting key.
fingerprintTermGraphWithTypeStructure
  :: (Ord identity, Ord local)
  => TypeStructure (Type (Variable identity))
  -> TermGraphLimits
  -> Natural
  -> TermGraph (Type (Variable identity)) local
  -> Either
      (TermGraphFingerprintError identity local)
      (Fingerprint TermGraphFingerprintSubject)
fingerprintTermGraphWithTypeStructure typeStructure graphLimits maximumBytes
    original = do
  graph <- first TermGraphFingerprintSharedResealError $
    sealTermGraph typeStructure graphLimits TermGraphSource
      { termGraphSourceRoot = termGraphRoot original
      , termGraphSourceNodes = termGraphNodes original
      }
  rootField <- evalStateT
    (fingerprintNode Map.empty graph Map.empty $ termGraphRoot graph)
    emptyFingerprintState
  first fingerprintLimitError $ buildFingerprintWithin maximumBytes
    FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole =
          asciiFingerprintBytes "shared-typed-term-graph"
      , fingerprintBuilderFields =
          [ taggedFingerprintField "dialect"
              [FingerprintBytes $ asciiFingerprintBytes
                "shared-typed-term-graph/v1"]
          , taggedFingerprintField "normalization"
              [ FingerprintBytes $ asciiFingerprintBytes
                  "rooted-tree-structural/v1"
              , FingerprintBytes $ asciiFingerprintBytes
                  "allocation-insensitive/v1"
              , FingerprintBytes $ asciiFingerprintBytes
                  "lexical-alpha/v1"
              , FingerprintBytes $ asciiFingerprintBytes
                  "flexible-rigid-free-slots/v1"
              , FingerprintBytes $ asciiFingerprintBytes
                  "no-beta-eta-let-quotient/v1"
              ]
          , taggedFingerprintField "root" [rootField]
          ]
      }
 where
  fingerprintLimitError (FingerprintLimitExceeded maximumBytes' observed) =
    TermGraphFingerprintByteLimitExceeded maximumBytes' observed

-- | Package-private carrier-aware canonical identity.
--
-- An empty association delegates literally to
-- 'fingerprintTermGraphWithTypeStructure', retaining its exact v1 bytes,
-- errors, and demand behavior.  A nonempty carrier is freshly resealed under
-- the caller's structure before any semantic row payload is inspected.  The
-- reseal provisionally admits only stamped visible applications; the opaque
-- carrier remains the authority for their already-checked semantics.
--
-- Certificate, node, occurrence, and raw source-slot identifiers are lookup
-- coordinates only.  The v2 key replaces a stamped witness with its canonical
-- rooted-row and source-order-step ordinals and encodes the corresponding
-- owner scheme plus complete checked structural plan.
fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
  :: (Ord identity, Ord local)
  => TypeStructure (Type (Variable identity))
  -> TermGraphLimits
  -> Natural
  -> CheckedTypeApplicationCertificateGraph (Variable identity) local
  -> Either
      (TermGraphFingerprintError identity local)
      (Fingerprint TermGraphFingerprintSubject)
fingerprintCheckedTypeApplicationCertificateGraphWithTypeStructure
    typeStructure graphLimits maximumBytes checked
  | not $ hasCertificateAssociations checked =
      fingerprintTermGraphWithTypeStructure typeStructure graphLimits
        maximumBytes $ checkedTypeApplicationCertificateGraph checked
  | otherwise = do
      graph <- first TermGraphFingerprintSharedResealError $
        sealTermGraph (provisionalCertificateTypeStructure typeStructure)
          graphLimits TermGraphSource
            { termGraphSourceRoot = termGraphRoot projected
            , termGraphSourceNodes = termGraphNodes projected
            }
      let associations = certificateSemanticAssociations checked
      references <- certificateSemanticReferences associations
      (rootField, associationFields) <- evalStateT
        (do
          root <- fingerprintNode references graph Map.empty
            $ termGraphRoot graph
          ensureAllCertificateReferencesConsumed references
          rows <- mapM fingerprintCertificateAssociation associations
          pure (root, rows))
        emptyFingerprintState
      first fingerprintLimitError $ buildFingerprintWithin maximumBytes
        FingerprintBuilder
          { fingerprintBuilderVersion = 2
          , fingerprintBuilderRole =
              asciiFingerprintBytes "shared-typed-term-graph"
          , fingerprintBuilderFields =
              [ taggedFingerprintField "dialect"
                  [ FingerprintBytes $ asciiFingerprintBytes
                      "shared-typed-term-graph/v2"
                  ]
              , taggedFingerprintField "normalization"
                  [ FingerprintBytes $ asciiFingerprintBytes
                      "rooted-tree-structural/v1"
                  , FingerprintBytes $ asciiFingerprintBytes
                      "allocation-insensitive/v1"
                  , FingerprintBytes $ asciiFingerprintBytes
                      "lexical-alpha/v1"
                  , FingerprintBytes $ asciiFingerprintBytes
                      "flexible-rigid-free-slots/v1"
                  , FingerprintBytes $ asciiFingerprintBytes
                      "no-beta-eta-let-quotient/v1"
                  , FingerprintBytes $ asciiFingerprintBytes
                      "certificate-semantic-associations/v1"
                  ]
              , taggedFingerprintField "root" [rootField]
              , taggedFingerprintField "certificate-associations"
                  [FingerprintSequence associationFields]
              ]
          }
 where
  projected = checkedTypeApplicationCertificateGraph checked
  fingerprintLimitError
      (FingerprintLimitExceeded maximumBytes' observed) =
    TermGraphFingerprintByteLimitExceeded maximumBytes' observed

hasCertificateAssociations
  :: CheckedTypeApplicationCertificateGraph variable local
  -> Bool
hasCertificateAssociations =
  foldCheckedTypeApplicationCertificateGraph
    (\_ _ _ _ _ _ _ -> True) False

provisionalCertificateTypeStructure
  :: TypeStructure ty -> TypeStructure ty
provisionalCertificateTypeStructure base = base
  { validTypeApplicationWitness = \argument witness ->
      case typeApplicationCertificate witness of
        Nothing -> validTypeApplicationWitness base argument witness
        Just _ -> True
  }

data CertificateSemanticAssociation identity =
  CertificateSemanticAssociation
    !CertificateId
    !Name
    !(Type (Variable identity))
    ![(TermNodeId,
       CheckedTypeApplicationCertificateStep (Variable identity))]

certificateSemanticAssociations
  :: CheckedTypeApplicationCertificateGraph (Variable identity) local
  -> [CertificateSemanticAssociation identity]
certificateSemanticAssociations =
  foldCheckedTypeApplicationCertificateGraph collect []
 where
  collect rows certificate owner scheme _ _ receipts = rows ++
    [ CertificateSemanticAssociation certificate owner scheme
        [(node, step) | (node, _, step) <- receipts]
    ]

data CertificateSemanticReference = CertificateSemanticReference
  !CertificateId !Natural !Natural !Natural

certificateSemanticReferences
  :: [CertificateSemanticAssociation identity]
  -> Either
      (TermGraphFingerprintError identity local)
      (Map TermNodeId CertificateSemanticReference)
certificateSemanticReferences associations = foldM insertReference Map.empty
  [ (node, CertificateSemanticReference certificate rawSlot
      rowOrdinal stepOrdinal)
  | (rowOrdinal, CertificateSemanticAssociation certificate _ _ receipts) <-
      zip [0 ..] associations
  , (stepOrdinal, (node, step)) <- zip [0 ..] receipts
  , let rawSlot = checkedTypeApplicationCertificateStepSlot step
  ]
 where
  insertReference references (node, reference@(CertificateSemanticReference
      certificate _ _ _)) = case Map.lookup node references of
    Nothing -> Right $ Map.insert node reference references
    Just _ -> Left $ TermGraphFingerprintUnsupportedCertificate certificate

fingerprintCertificateAssociation
  :: Ord identity
  => CertificateSemanticAssociation identity
  -> FingerprintM identity local FingerprintField
fingerprintCertificateAssociation
    (CertificateSemanticAssociation _ owner scheme receipts) = do
  schemeField <- fingerprintType scheme
  stepFields <- mapM (fingerprintCertificateStep . snd) receipts
  pure $ taggedFingerprintField "certificate-association"
    [ FingerprintName owner
    , taggedFingerprintField "owner-scheme" [schemeField]
    , taggedFingerprintField "steps" [FingerprintSequence stepFields]
    ]

fingerprintCertificateStep
  :: Ord identity
  => CheckedTypeApplicationCertificateStep (Variable identity)
  -> FingerprintM identity local FingerprintField
fingerprintCertificateStep step = do
  sourceField <- fingerprintCertificateType
    $ checkedTypeApplicationCertificateStepSource step
  selectedField <- fingerprintCertificateType
    $ checkedTypeApplicationCertificateStepSelected step
  resultField <- fingerprintCertificateType
    $ checkedTypeApplicationCertificateStepResult step
  obligationFields <- mapM fingerprintCertificateConstraint
    $ checkedTypeApplicationCertificateStepObligations step
  pure $ taggedFingerprintField "certificate-step"
    [ taggedFingerprintField "source" [sourceField]
    , taggedFingerprintField "selected" [selectedField]
    , taggedFingerprintField "result" [resultField]
    , taggedFingerprintField "activated-obligations"
        [FingerprintSequence obligationFields]
    ]

data CanonicalCertificateVariable
  = CanonicalCertificateSourceBound !Natural !Natural
  | CanonicalCertificateSelectionBound !Natural !Natural !Natural
  | CanonicalCertificateFree !CanonicalTypeVariable

fingerprintCertificateType
  :: Ord identity
  => Type
      (TypeApplicationCertificatePlanVariable (Variable identity))
  -> FingerprintM identity local FingerprintField
fingerprintCertificateType source = do
  canonical <- traverse canonicalCertificateVariable
    $ canonicalTypeFingerprintForm source
  pure $ typeFingerprintField canonicalCertificateVariableField canonical

fingerprintCertificateConstraint
  :: Ord identity
  => Constraint
      (Type (TypeApplicationCertificatePlanVariable (Variable identity)))
  -> FingerprintM identity local FingerprintField
fingerprintCertificateConstraint source = do
  canonical <- traverse (traverse canonicalCertificateVariable)
    $ fmap canonicalTypeFingerprintForm source
  pure $ constraintFingerprintField canonicalCertificateVariableField canonical

canonicalCertificateVariable
  :: Ord identity
  => TypeApplicationCertificatePlanVariable (Variable identity)
  -> FingerprintM identity local CanonicalCertificateVariable
canonicalCertificateVariable variable = case variable of
  TypeApplicationCertificateSourceBound scope slot -> pure $
    CanonicalCertificateSourceBound scope slot
  TypeApplicationCertificateSelectionBound ordinal scope slot -> pure $
    CanonicalCertificateSelectionBound ordinal scope slot
  TypeApplicationCertificateFree free -> CanonicalCertificateFree <$>
    case free of
      FlexibleVariable identity -> CanonicalFlexibleVariable
        <$> canonicalFlexibleSlot identity
      RigidVariable identity -> CanonicalRigidVariable
        <$> canonicalRigidSlot identity

canonicalCertificateVariableField
  :: CanonicalCertificateVariable -> FingerprintField
canonicalCertificateVariableField variable = case variable of
  CanonicalCertificateSourceBound scope slot ->
    taggedFingerprintField "source-bound"
      [FingerprintNatural scope, FingerprintNatural slot]
  CanonicalCertificateSelectionBound ordinal scope slot ->
    taggedFingerprintField "selection-bound"
      [ FingerprintNatural ordinal
      , FingerprintNatural scope
      , FingerprintNatural slot
      ]
  CanonicalCertificateFree free -> canonicalTypeVariableField free

data CanonicalTypeVariable
  = CanonicalBoundVariable !Natural !Natural
  | CanonicalFlexibleVariable !Natural
  | CanonicalRigidVariable !Natural

data FingerprintState identity local = FingerprintState
  { fingerprintFlexibleSlots :: !(Map identity Natural)
  , fingerprintNextFlexibleSlot :: !Natural
  , fingerprintRigidSlots :: !(Map identity Natural)
  , fingerprintNextRigidSlot :: !Natural
  , fingerprintNextLocalSlot :: !Natural
  , fingerprintHoleSlots :: !(Map local Natural)
  , fingerprintNextHoleSlot :: !Natural
  , fingerprintConsumedCertificateNodes :: !(Set TermNodeId)
  }

emptyFingerprintState :: FingerprintState identity local
emptyFingerprintState = FingerprintState
  { fingerprintFlexibleSlots = Map.empty
  , fingerprintNextFlexibleSlot = 0
  , fingerprintRigidSlots = Map.empty
  , fingerprintNextRigidSlot = 0
  , fingerprintNextLocalSlot = 0
  , fingerprintHoleSlots = Map.empty
  , fingerprintNextHoleSlot = 0
  , fingerprintConsumedCertificateNodes = Set.empty
  }

type FingerprintM identity local = StateT
  (FingerprintState identity local)
  (Either (TermGraphFingerprintError identity local))

fingerprintNode
  :: (Ord identity, Ord local)
  => Map TermNodeId CertificateSemanticReference
  -> TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> TermNodeId
  -> FingerprintM identity local FingerprintField
fingerprintNode certificateReferences graph locals nodeId =
  case lookupTermNode nodeId graph of
  Nothing -> lift $ Left $ TermGraphFingerprintMissingNode nodeId
  Just (TermNode nodeType nodeForm) -> do
    typeField <- fingerprintType nodeType
    formField <- fingerprintNodeForm certificateReferences graph locals
      nodeId nodeForm
    pure $ taggedFingerprintField "term-node" [typeField, formField]

fingerprintNodeForm
  :: (Ord identity, Ord local)
  => Map TermNodeId CertificateSemanticReference
  -> TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> TermNodeId
  -> TermNodeForm (Type (Variable identity)) local
  -> FingerprintM identity local FingerprintField
fingerprintNodeForm certificateReferences graph locals owner form = case form of
  TypedLocal _ local -> case Map.lookup local locals of
    Nothing -> lift $ Left $ TermGraphFingerprintUnboundLocal owner local
    Just slot -> pure $ taggedFingerprintField "local"
      [FingerprintNatural slot]
  TypedGlobal _ name -> pure $ taggedFingerprintField "global"
    [FingerprintName name]
  TypedLambda patterns body -> do
    (patternFields, bodyLocals) <- fingerprintPatterns patterns locals
    bodyField <- fingerprintNode certificateReferences graph bodyLocals body
    pure $ taggedFingerprintField "lambda"
      [FingerprintSequence patternFields, bodyField]
  TypedApply function argument witness -> do
    functionField <- fingerprintNode certificateReferences graph locals function
    argumentField <- fingerprintNode certificateReferences graph locals argument
    domainField <- fingerprintType $ applicationDomain witness
    resultField <- fingerprintType $ applicationResult witness
    pure $ taggedFingerprintField "apply"
      [ functionField
      , argumentField
      , taggedFingerprintField "application-witness"
          [domainField, resultField]
      ]
  TypedVisibleTypeApplication _ function argument witness -> do
    functionField <- fingerprintNode certificateReferences graph locals function
    argumentField <- fingerprintVisibleTypeArgument argument
    sourceField <- fingerprintType $ typeApplicationSource witness
    selectedField <- fingerprintType $ typeApplicationSelected witness
    resultField <- fingerprintType $ typeApplicationResult witness
    certificateField <- case typeApplicationCertificate witness of
      Nothing -> pure $ taggedFingerprintField "no-certificate" []
      Just (certificate, _) -> case Map.lookup owner certificateReferences of
        Nothing -> unsupported certificate
        Just (CertificateSemanticReference expectedCertificate expectedSlot
            rowOrdinal stepOrdinal)
          | Just (expectedCertificate, expectedSlot) ==
              typeApplicationCertificate witness -> do
              consumeCertificateReference owner certificate
              pure $ taggedFingerprintField "certificate-semantic-reference"
                [ FingerprintNatural rowOrdinal
                , FingerprintNatural stepOrdinal
                ]
          | otherwise -> unsupported certificate
    pure $ taggedFingerprintField "visible-type-application"
      [ functionField
      , argumentField
      , taggedFingerprintField "type-application-witness"
          [ sourceField
          , selectedField
          , resultField
          , certificateField
          ]
      ]
  TypedTuple elements -> do
    elementFields <- mapM
      (fingerprintNode certificateReferences graph locals) elements
    pure $ taggedFingerprintField "tuple-term"
      [FingerprintSequence elementFields]
  TypedHole _ local -> do
    slot <- canonicalHoleSlot local
    pure $ taggedFingerprintField "hole" [FingerprintNatural slot]
  TypedLet pattern binding body -> do
    (patternField, bodyLocals) <- fingerprintPattern pattern locals
    bindingField <- fingerprintNode certificateReferences graph locals binding
    bodyField <- fingerprintNode certificateReferences graph bodyLocals body
    pure $ taggedFingerprintField "let"
      [patternField, bindingField, bodyField]
  TypedCase scrutinee alternatives -> do
    scrutineeField <- fingerprintNode certificateReferences graph locals
      scrutinee
    alternativeFields <- mapM
      (fingerprintAlternative certificateReferences graph locals) alternatives
    pure $ taggedFingerprintField "case"
      [scrutineeField, FingerprintSequence alternativeFields]
 where
  unsupported certificate = lift $ Left $
    TermGraphFingerprintUnsupportedCertificate certificate

consumeCertificateReference
  :: TermNodeId
  -> CertificateId
  -> FingerprintM identity local ()
consumeCertificateReference node certificate = do
  state <- get
  let consumed = fingerprintConsumedCertificateNodes state
  if node `Set.member` consumed
    then lift $ Left $ TermGraphFingerprintUnsupportedCertificate certificate
    else put state
      { fingerprintConsumedCertificateNodes = Set.insert node consumed }

ensureAllCertificateReferencesConsumed
  :: Map TermNodeId CertificateSemanticReference
  -> FingerprintM identity local ()
ensureAllCertificateReferencesConsumed references = do
  state <- get
  let remaining = Map.withoutKeys references
        $ fingerprintConsumedCertificateNodes state
  case Map.lookupMin remaining of
    Nothing -> pure ()
    Just (_, CertificateSemanticReference certificate _ _ _) -> lift $ Left $
      TermGraphFingerprintUnsupportedCertificate certificate

fingerprintAlternative
  :: (Ord identity, Ord local)
  => Map TermNodeId CertificateSemanticReference
  -> TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> (TypedPattern (Type (Variable identity)) local, TermNodeId)
  -> FingerprintM identity local FingerprintField
fingerprintAlternative certificateReferences graph locals (pattern, body) = do
  (patternField, bodyLocals) <- fingerprintPattern pattern locals
  bodyField <- fingerprintNode certificateReferences graph bodyLocals body
  pure $ taggedFingerprintField "case-alternative"
    [patternField, bodyField]

fingerprintPatterns
  :: (Ord identity, Ord local)
  => [TypedPattern (Type (Variable identity)) local]
  -> Map local Natural
  -> FingerprintM identity local ([FingerprintField], Map local Natural)
fingerprintPatterns [] locals = pure ([], locals)
fingerprintPatterns (pattern : patterns) locals = do
  (field, nestedLocals) <- fingerprintPattern pattern locals
  (fields, finalLocals) <- fingerprintPatterns patterns nestedLocals
  pure (field : fields, finalLocals)

fingerprintPattern
  :: (Ord identity, Ord local)
  => TypedPattern (Type (Variable identity)) local
  -> Map local Natural
  -> FingerprintM identity local (FingerprintField, Map local Natural)
fingerprintPattern pattern locals = do
  typeField <- fingerprintType $ typedPatternType pattern
  case typedPatternNode pattern of
    TypedBind local -> do
      (slot, nestedLocals) <- bindCanonicalLocal local locals
      pure
        ( taggedFingerprintField "bind-pattern"
            [typeField, FingerprintNatural slot]
        , nestedLocals
        )
    TypedWildcard -> pure
      (taggedFingerprintField "wildcard-pattern" [typeField], locals)
    TypedConstructor name fields -> do
      (fieldFields, nestedLocals) <- fingerprintPatterns fields locals
      pure
        ( taggedFingerprintField "constructor-pattern"
            [ FingerprintName name
            , typeField
            , FingerprintSequence fieldFields
            ]
        , nestedLocals
        )
    TypedTuplePattern fields -> do
      (fieldFields, nestedLocals) <- fingerprintPatterns fields locals
      pure
        ( taggedFingerprintField "tuple-pattern"
            [typeField, FingerprintSequence fieldFields]
        , nestedLocals
        )
    TypedAs local nested -> do
      (slot, asLocals) <- bindCanonicalLocal local locals
      (nestedField, nestedLocals) <- fingerprintPattern nested asLocals
      pure
        ( taggedFingerprintField "as-pattern"
            [typeField, FingerprintNatural slot, nestedField]
        , nestedLocals
        )

bindCanonicalLocal
  :: Ord local
  => local
  -> Map local Natural
  -> FingerprintM identity local (Natural, Map local Natural)
bindCanonicalLocal local locals = do
  state <- get
  let slot = fingerprintNextLocalSlot state
  put state {fingerprintNextLocalSlot = slot + 1}
  pure (slot, Map.insert local slot locals)

canonicalHoleSlot
  :: Ord local
  => local
  -> FingerprintM identity local Natural
canonicalHoleSlot = canonicalSlotIn fingerprintHoleSlots fingerprintNextHoleSlot
  $ \slots next state ->
    state {fingerprintHoleSlots = slots, fingerprintNextHoleSlot = next}

-- Allocate or reuse the canonical slot of one key in a slot table, numbering
-- fresh keys in first-observation order.  The flexible, rigid and hole
-- tables differ only in which state fields they read and write.
canonicalSlotIn
  :: Ord key
  => (FingerprintState identity local -> Map key Natural)
  -> (FingerprintState identity local -> Natural)
  -> (Map key Natural -> Natural
      -> FingerprintState identity local -> FingerprintState identity local)
  -> key
  -> FingerprintM identity local Natural
canonicalSlotIn slots nextSlot store key = do
  state <- get
  case Map.lookup key $ slots state of
    Just slot -> pure slot
    Nothing -> do
      let slot = nextSlot state
      put $ store (Map.insert key slot $ slots state) (slot + 1) state
      pure slot

fingerprintType
  :: Ord identity
  => Type (Variable identity)
  -> FingerprintM identity local FingerprintField
fingerprintType source = do
  canonical <- traverse canonicalTypeVariable
    $ Alpha.alphaNormalizeTypeWith Alpha.PositionalBinderSlots
    $ canonicalTypeFingerprintForm source
  pure $ typeFingerprintField canonicalTypeVariableField canonical

canonicalTypeVariable
  :: Ord identity
  => Alpha.AlphaVariable (Variable identity)
  -> FingerprintM identity local CanonicalTypeVariable
canonicalTypeVariable variable = case variable of
  Alpha.AlphaBoundVariable scope slot ->
    pure $ CanonicalBoundVariable scope slot
  Alpha.AlphaFreeVariable free -> case free of
    FlexibleVariable identity -> CanonicalFlexibleVariable
      <$> canonicalFlexibleSlot identity
    RigidVariable identity -> CanonicalRigidVariable
      <$> canonicalRigidSlot identity

canonicalFlexibleSlot
  :: Ord identity
  => identity
  -> FingerprintM identity local Natural
canonicalFlexibleSlot =
  canonicalSlotIn fingerprintFlexibleSlots fingerprintNextFlexibleSlot
    $ \slots next state -> state
      {fingerprintFlexibleSlots = slots, fingerprintNextFlexibleSlot = next}

canonicalRigidSlot
  :: Ord identity
  => identity
  -> FingerprintM identity local Natural
canonicalRigidSlot =
  canonicalSlotIn fingerprintRigidSlots fingerprintNextRigidSlot
    $ \slots next state ->
      state {fingerprintRigidSlots = slots, fingerprintNextRigidSlot = next}

canonicalTypeVariableField :: CanonicalTypeVariable -> FingerprintField
canonicalTypeVariableField variable = case variable of
  CanonicalBoundVariable scope slot -> taggedFingerprintField "bound"
    [FingerprintNatural scope, FingerprintNatural slot]
  CanonicalFlexibleVariable slot -> taggedFingerprintField "flexible-free"
    [FingerprintNatural slot]
  CanonicalRigidVariable slot -> taggedFingerprintField "rigid-free"
    [FingerprintNatural slot]

fingerprintVisibleTypeArgument
  :: Generated.VisibleTypeArgument
  -> FingerprintM identity local FingerprintField
fingerprintVisibleTypeArgument argument =
  case Generated.visibleTypeArgumentClosedType argument of
    Nothing -> pure $ taggedFingerprintField "inferred-visible-type" []
    Just selected -> pure $ taggedFingerprintField "specified-visible-type"
      [ typeFingerprintField closedVisibleVariableField
          $ canonicalTypeFingerprintForm selected
      ]

closedVisibleVariableField
  :: Generated.ClosedVisibleTypeVariable
  -> FingerprintField
closedVisibleVariableField variable = taggedFingerprintField "bound"
  [ FingerprintNatural $
      Generated.closedVisibleTypeVariableScope variable
  , FingerprintNatural $
      Generated.closedVisibleTypeVariableSlot variable
  ]

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
  ) where

import Control.DeepSeq (NFData)
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
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

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
  , taggedFingerprintField
  , typeFingerprintField
  )
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
  , TypedPattern (..)
  , TypedPatternNode (..)
  , lookupTermNode
  , sealTermGraph
  , sharedTypeStructure
  , termGraphNodes
  , termGraphRoot
  )

-- | Nominal identity domain for the v1 canonical shared term-graph encoding.
data TermGraphFingerprintSubject

-- | Conservative retained-key bound for one graph sealed under
-- 'defaultTermGraphLimits'.  Callers with a different policy pass their exact
-- graph limits and byte limit explicitly to 'fingerprintSharedTermGraph'.
defaultTermGraphFingerprintByteLimit :: Natural
defaultTermGraphFingerprintByteLimit = 1048576

-- | Why an opaque graph could not receive a shared structural identity.
--
-- Certificate allocation numbers are intentionally not encoded.  Until a
-- checked certificate table supplies semantic substitution and obligation
-- identities, any certificate-bearing graph fails closed.
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
-- reseal; an inventory-bound sealer must eventually establish that authority
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
  graph <- first TermGraphFingerprintSharedResealError $
    sealTermGraph sharedTypeStructure graphLimits TermGraphSource
      { termGraphSourceRoot = termGraphRoot original
      , termGraphSourceNodes = termGraphNodes original
      }
  rootField <- evalStateT
    (fingerprintNode graph Map.empty $ termGraphRoot graph)
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
  }

type FingerprintM identity local = StateT
  (FingerprintState identity local)
  (Either (TermGraphFingerprintError identity local))

fingerprintNode
  :: (Ord identity, Ord local)
  => TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> TermNodeId
  -> FingerprintM identity local FingerprintField
fingerprintNode graph locals nodeId = case lookupTermNode nodeId graph of
  Nothing -> lift $ Left $ TermGraphFingerprintMissingNode nodeId
  Just (TermNode nodeType nodeForm) -> do
    typeField <- fingerprintType nodeType
    formField <- fingerprintNodeForm graph locals nodeId nodeForm
    pure $ taggedFingerprintField "term-node" [typeField, formField]

fingerprintNodeForm
  :: (Ord identity, Ord local)
  => TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> TermNodeId
  -> TermNodeForm (Type (Variable identity)) local
  -> FingerprintM identity local FingerprintField
fingerprintNodeForm graph locals owner form = case form of
  TypedLocal _ local -> case Map.lookup local locals of
    Nothing -> lift $ Left $ TermGraphFingerprintUnboundLocal owner local
    Just slot -> pure $ taggedFingerprintField "local"
      [FingerprintNatural slot]
  TypedGlobal _ name -> pure $ taggedFingerprintField "global"
    [FingerprintName name]
  TypedLambda patterns body -> do
    (patternFields, bodyLocals) <- fingerprintPatterns patterns locals
    bodyField <- fingerprintNode graph bodyLocals body
    pure $ taggedFingerprintField "lambda"
      [FingerprintSequence patternFields, bodyField]
  TypedApply function argument witness -> do
    functionField <- fingerprintNode graph locals function
    argumentField <- fingerprintNode graph locals argument
    domainField <- fingerprintType $ applicationDomain witness
    resultField <- fingerprintType $ applicationResult witness
    pure $ taggedFingerprintField "apply"
      [ functionField
      , argumentField
      , taggedFingerprintField "application-witness"
          [domainField, resultField]
      ]
  TypedVisibleTypeApplication _ function argument witness -> do
    functionField <- fingerprintNode graph locals function
    argumentField <- fingerprintVisibleTypeArgument argument
    sourceField <- fingerprintType $ typeApplicationSource witness
    selectedField <- fingerprintType $ typeApplicationSelected witness
    resultField <- fingerprintType $ typeApplicationResult witness
    certificateField <- case typeApplicationCertificate witness of
      Nothing -> pure $ taggedFingerprintField "no-certificate" []
      Just (certificate, _) -> lift $ Left $
        TermGraphFingerprintUnsupportedCertificate certificate
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
    elementFields <- mapM (fingerprintNode graph locals) elements
    pure $ taggedFingerprintField "tuple-term"
      [FingerprintSequence elementFields]
  TypedHole _ local -> do
    slot <- canonicalHoleSlot local
    pure $ taggedFingerprintField "hole" [FingerprintNatural slot]
  TypedLet pattern binding body -> do
    (patternField, bodyLocals) <- fingerprintPattern pattern locals
    bindingField <- fingerprintNode graph locals binding
    bodyField <- fingerprintNode graph bodyLocals body
    pure $ taggedFingerprintField "let"
      [patternField, bindingField, bodyField]
  TypedCase scrutinee alternatives -> do
    scrutineeField <- fingerprintNode graph locals scrutinee
    alternativeFields <- mapM (fingerprintAlternative graph locals)
      alternatives
    pure $ taggedFingerprintField "case"
      [scrutineeField, FingerprintSequence alternativeFields]

fingerprintAlternative
  :: (Ord identity, Ord local)
  => TermGraph (Type (Variable identity)) local
  -> Map local Natural
  -> (TypedPattern (Type (Variable identity)) local, TermNodeId)
  -> FingerprintM identity local FingerprintField
fingerprintAlternative graph locals (pattern, body) = do
  (patternField, bodyLocals) <- fingerprintPattern pattern locals
  bodyField <- fingerprintNode graph bodyLocals body
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
canonicalHoleSlot local = do
  state <- get
  case Map.lookup local $ fingerprintHoleSlots state of
    Just slot -> pure slot
    Nothing -> do
      let slot = fingerprintNextHoleSlot state
      put state
        { fingerprintHoleSlots = Map.insert local slot
            $ fingerprintHoleSlots state
        , fingerprintNextHoleSlot = slot + 1
        }
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
canonicalFlexibleSlot identity = do
  state <- get
  case Map.lookup identity $ fingerprintFlexibleSlots state of
    Just slot -> pure slot
    Nothing -> do
      let slot = fingerprintNextFlexibleSlot state
      put state
        { fingerprintFlexibleSlots = Map.insert identity slot
            $ fingerprintFlexibleSlots state
        , fingerprintNextFlexibleSlot = slot + 1
        }
      pure slot

canonicalRigidSlot
  :: Ord identity
  => identity
  -> FingerprintM identity local Natural
canonicalRigidSlot identity = do
  state <- get
  case Map.lookup identity $ fingerprintRigidSlots state of
    Just slot -> pure slot
    Nothing -> do
      let slot = fingerprintNextRigidSlot state
      put state
        { fingerprintRigidSlots = Map.insert identity slot
            $ fingerprintRigidSlots state
        , fingerprintNextRigidSlot = slot + 1
        }
      pure slot

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

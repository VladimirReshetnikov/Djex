{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Package-private structural plans for source type-application
-- certificates.
--
-- A checked table proves only bounded, capture-avoiding replay of one complete
-- leading forall telescope and the ordered constraints made unconditional by
-- that replay.  It does /not/ prove that a scheme belongs to an inventory,
-- that either source or selection is closed, that a selected type has the
-- source binder's kind, that an obligation was discharged, or that a row
-- belongs to a particular graph occurrence.
-- Consequently this module grants no fingerprint authority: the public term
-- graph fingerprint entrance continues to reject every certificate-bearing
-- graph until an engine-owned origin association is added separately.
module Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate
  ( TypeApplicationCertificateLimitField (..)
  , TypeApplicationCertificateLimitError (..)
  , TypeApplicationCertificateLimits
  , mkTypeApplicationCertificateLimits
  , defaultTypeApplicationCertificateLimits
  , maximumTypeApplicationCertificates
  , maximumTypeApplicationCertificateSelections
  , maximumTypeApplicationCertificateObligations
  , maximumTypeApplicationCertificateTypeNodes
  , maximumTypeApplicationCertificateCollectionWidth
  , TypeApplicationCertificateSource (..)
  , TypeApplicationCertificateObservation (..)
  , TypeApplicationCertificateTypeSite (..)
  , TypeApplicationCertificateError (..)
  , CheckedTypeApplicationCertificateTable
  , CheckedTypeApplicationCertificatePlan
  , CheckedTypeApplicationCertificateStep
  , TypeApplicationCertificatePlanVariable (..)
  , sealTypeApplicationCertificateTable
  , matchCheckedTypeApplicationCertificateObservations
  , checkedTypeApplicationCertificateCount
  , lookupCheckedTypeApplicationCertificatePlan
  , checkedTypeApplicationCertificateStepCount
  , checkedTypeApplicationCertificateSteps
  , checkedTypeApplicationCertificateStepSlot
  , checkedTypeApplicationCertificateStepSource
  , checkedTypeApplicationCertificateStepSelected
  , checkedTypeApplicationCertificateStepResult
  , checkedTypeApplicationCertificateStepObligations
  , checkedTypeApplicationCertificateStepObligationCount
  , checkedTypeApplicationCertificateObligationCount
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Internal.Alpha
  ( AlphaVariable (..)
  , BinderSlotPolicy (PositionalBinderSlots)
  , alphaNormalizeTypeWith
  , eraseVacuousForalls
  )
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , TypeError
  , canonicalizeType
  , normalizeType
  )
import qualified Language.Haskell.Synthesis.TypeAtom as TypeAtom
import Language.Haskell.Synthesis.TypedGenerated
  ( CertificateId
  , TypeStructure (observeTypeWithin)
  , TypeStructureLimitError (..)
  , sharedTypeStructure
  )

-- | Independently configurable finite fields for one structural table.
data TypeApplicationCertificateLimitField
  = TypeApplicationCertificateEntries
  | TypeApplicationCertificateSelections
  | TypeApplicationCertificateObligations
  | TypeApplicationCertificateTypeNodes
  | TypeApplicationCertificateCollectionWidth
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData TypeApplicationCertificateLimitField

-- | Why 'mkTypeApplicationCertificateLimits' rejected its arguments: the
-- first negative field, in declaration order, together with its value.
data TypeApplicationCertificateLimitError
  = NegativeTypeApplicationCertificateLimit
      !TypeApplicationCertificateLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData TypeApplicationCertificateLimitError

-- | Validated structural resource policy.  These bounds are deliberately
-- independent of any engine's operational search limits.
data TypeApplicationCertificateLimits = TypeApplicationCertificateLimits
  !Int !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

instance NFData TypeApplicationCertificateLimits where
  rnf (TypeApplicationCertificateLimits entries selections obligations nodes
      width) =
    rnf entries `seq` rnf selections `seq` rnf obligations `seq`
      rnf nodes `seq` rnf width

-- | Validate the five bounds in the order entries, selections, obligations,
-- type nodes, collection width, rejecting the first negative one.  Zero is
-- accepted for every field.
mkTypeApplicationCertificateLimits
  :: Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either TypeApplicationCertificateLimitError
      TypeApplicationCertificateLimits
mkTypeApplicationCertificateLimits entries selections obligations nodes width
  | entries < 0 = negative TypeApplicationCertificateEntries entries
  | selections < 0 = negative TypeApplicationCertificateSelections selections
  | obligations < 0 = negative TypeApplicationCertificateObligations obligations
  | nodes < 0 = negative TypeApplicationCertificateTypeNodes nodes
  | width < 0 = negative TypeApplicationCertificateCollectionWidth width
  | otherwise = Right $ TypeApplicationCertificateLimits
      entries selections obligations nodes width
 where
  negative field value = Left $
    NegativeTypeApplicationCertificateLimit field value

-- | Default bounds: 32 certificates, 64 selections per certificate, 256
-- obligations per certificate, 4096 nodes per type, and a collection width
-- of 256.
defaultTypeApplicationCertificateLimits :: TypeApplicationCertificateLimits
defaultTypeApplicationCertificateLimits = TypeApplicationCertificateLimits
  32 64 256 4096 256

-- | Maximum number of certificate sources one table may hold; exceeding it
-- fails sealing with 'TypeApplicationCertificateEntryLimitExceeded' before
-- any source payload is inspected.
maximumTypeApplicationCertificates
  :: TypeApplicationCertificateLimits -> Int
maximumTypeApplicationCertificates
    (TypeApplicationCertificateLimits value _ _ _ _) = value

-- | Maximum number of selected types, and hence leading binders, per
-- certificate.  It bounds the selection list, the telescope, and the number
-- of observations 'matchCheckedTypeApplicationCertificateObservations'
-- will count.
maximumTypeApplicationCertificateSelections
  :: TypeApplicationCertificateLimits -> Int
maximumTypeApplicationCertificateSelections
    (TypeApplicationCertificateLimits _ value _ _ _) = value

-- | Maximum total number of obligations accumulated across all steps of one
-- certificate's replay, and likewise across one observation list when
-- matching.
maximumTypeApplicationCertificateObligations
  :: TypeApplicationCertificateLimits -> Int
maximumTypeApplicationCertificateObligations
    (TypeApplicationCertificateLimits _ _ value _ _) = value

-- | Maximum node count of any single scheme, selection, derived, or observed
-- type, measured by 'observeTypeWithin' before normalization.
maximumTypeApplicationCertificateTypeNodes
  :: TypeApplicationCertificateLimits -> Int
maximumTypeApplicationCertificateTypeNodes
    (TypeApplicationCertificateLimits _ _ _ value _) = value

-- | Maximum width of any collection inside a bounded type, and of an
-- observed obligation's argument list.
maximumTypeApplicationCertificateCollectionWidth
  :: TypeApplicationCertificateLimits -> Int
maximumTypeApplicationCertificateCollectionWidth
    (TypeApplicationCertificateLimits _ _ _ _ value) = value

-- | Caller-built coordinates and structural specialization payload.  Only the
-- coordinate is strict, so entry/duplicate/selection-width failures do not
-- accidentally demand later type payloads.
data TypeApplicationCertificateSource variable =
  TypeApplicationCertificateSource
    { typeApplicationCertificateSourceId :: !CertificateId
    , typeApplicationCertificateSourceScheme :: Type variable
    , typeApplicationCertificateSourceSelections :: [Type variable]
    }
  deriving (Eq, Ord, Show, Generic)

instance NFData variable =>
    NFData (TypeApplicationCertificateSource variable)

-- | Independently checked observations for one source-telescope slot.
--
-- These values deliberately repeat the structural plan inputs and results.
-- A later atomic association boundary compares every field with the plan it
-- rebuilds from the exact scheme and ordered selected types; merely carrying
-- this record grants no graph, inventory, kind, discharge, or fingerprint
-- authority.
data TypeApplicationCertificateObservation variable =
  TypeApplicationCertificateObservation
    { typeApplicationCertificateObservationSlot :: !Natural
    , typeApplicationCertificateObservationSource :: Type variable
    , typeApplicationCertificateObservationSelected :: Type variable
    , typeApplicationCertificateObservationResult :: Type variable
    , typeApplicationCertificateObservationObligations ::
        [Constraint (Type variable)]
    }
  deriving (Eq, Ord, Show, Generic)

instance NFData variable =>
    NFData (TypeApplicationCertificateObservation variable)

-- | Exact type payload being bounded or structurally validated.
data TypeApplicationCertificateTypeSite
  = TypeApplicationCertificateSchemeType !CertificateId
  | TypeApplicationCertificateSelectionType !CertificateId !Natural
  | TypeApplicationCertificateDerivedResultType !CertificateId !Natural
  | TypeApplicationCertificateDerivedObligationType
      !CertificateId !Natural !Natural !Natural
  | TypeApplicationCertificateObservedSourceType !CertificateId !Natural
  | TypeApplicationCertificateObservedSelectedType !CertificateId !Natural
  | TypeApplicationCertificateObservedResultType !CertificateId !Natural
  | TypeApplicationCertificateObservedObligationArguments
      !CertificateId !Natural !Natural
  | TypeApplicationCertificateObservedObligationType
      !CertificateId !Natural !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData TypeApplicationCertificateTypeSite

-- | Closed rejection vocabulary shared by 'sealTypeApplicationCertificateTable'
-- and 'matchCheckedTypeApplicationCertificateObservations'.  Limit
-- constructors carry the maximum followed by the observed count; slot and
-- ordinal fields are zero-based; a bounded observed count stops at maximum
-- plus one.
data TypeApplicationCertificateError variable
  = TypeApplicationCertificateEntryLimitExceeded !Int !Int
  | DuplicateTypeApplicationCertificateId !CertificateId
  | TypeApplicationCertificateSelectionLimitExceeded
      !CertificateId !Int !Int
  | TypeApplicationCertificateTypeNodeLimitExceeded
      !TypeApplicationCertificateTypeSite !Int !Int
  | TypeApplicationCertificateTypeCollectionLimitExceeded
      !TypeApplicationCertificateTypeSite !Int !Int
  | InvalidTypeApplicationCertificateType
      !TypeApplicationCertificateTypeSite !(TypeError variable)
  | TypeApplicationCertificateHasNoLeadingBinder !CertificateId
  | TypeApplicationCertificateTelescopeLimitExceeded
      !CertificateId !Int !Int
  | TypeApplicationCertificateSelectionArityMismatch
      !CertificateId !Int !Int
  | TypeApplicationCertificateReplayEndedEarly !CertificateId !Natural
  | TypeApplicationCertificateObligationLimitExceeded
      !CertificateId !Int !Int
  | TypeApplicationCertificateObservationLimitExceeded
      !CertificateId !Int !Int
  | TypeApplicationCertificateObservationArityMismatch
      !CertificateId !Int !Int
  | TypeApplicationCertificateObservationMissingPlan !CertificateId
  | TypeApplicationCertificateObservationSlotMismatch
      !CertificateId !Natural !Natural
  | TypeApplicationCertificateObservationSourceMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateObservationSelectedMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateObservationResultMismatch
      !CertificateId !Natural
  | TypeApplicationCertificateObservedObligationLimitExceeded
      !CertificateId !Int !Int
  | TypeApplicationCertificateObservedObligationCountMismatch
      !CertificateId !Natural !Int !Int
  | TypeApplicationCertificateObservedObligationClassMismatch
      !CertificateId !Natural !Natural !Name !Name
  | TypeApplicationCertificateObservedObligationArgumentCountMismatch
      !CertificateId !Natural !Natural !Int !Int
  | TypeApplicationCertificateObservedObligationArgumentMismatch
      !CertificateId !Natural !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData variable => NFData (TypeApplicationCertificateError variable)

-- | Variable namespace of every type stored in a checked plan: positional
-- source binders, positional binders of the ordinal-th selected type, and
-- free variables carried verbatim.
-- Private variable namespaces make substitution capture-free without asking a
-- caller for fresh backend identities.  Bound spelling is never an authority;
-- free variables retain their exact nominal identity.
data TypeApplicationCertificatePlanVariable variable
  = TypeApplicationCertificateSourceBound !Natural !Natural
  | TypeApplicationCertificateSelectionBound !Natural !Natural !Natural
  | TypeApplicationCertificateFree variable
  deriving (Eq, Ord, Show, Generic)

instance NFData variable =>
    NFData (TypeApplicationCertificatePlanVariable variable)

-- | One replayed binder consumption: the zero-based slot, the source type
-- before consumption, the selected type, the canonical result, and the
-- ordered obligations made unconditional by this step.  Read through the
-- @checkedTypeApplicationCertificateStep...@ projections.
data CheckedTypeApplicationCertificateStep variable =
  CheckedTypeApplicationCertificateStep
    !Natural
    !(Type (TypeApplicationCertificatePlanVariable variable))
    !(Type (TypeApplicationCertificatePlanVariable variable))
    !(Type (TypeApplicationCertificatePlanVariable variable))
    ![Constraint (Type (TypeApplicationCertificatePlanVariable variable))]

type role CheckedTypeApplicationCertificateStep nominal

instance NFData variable =>
    NFData (CheckedTypeApplicationCertificateStep variable) where
  rnf (CheckedTypeApplicationCertificateStep slot source selected result
      obligations) =
    rnf slot `seq` rnf source `seq` rnf selected `seq` rnf result `seq`
      rnf obligations

-- | Complete replay of one certificate's leading telescope: the steps in
-- slot order (one per selected type) and the total obligation count across
-- them.  Obtained only from a sealed table via
-- 'lookupCheckedTypeApplicationCertificatePlan' or a successful match.
data CheckedTypeApplicationCertificatePlan variable =
  CheckedTypeApplicationCertificatePlan
    ![CheckedTypeApplicationCertificateStep variable]
    !Int

type role CheckedTypeApplicationCertificatePlan nominal

instance NFData variable =>
    NFData (CheckedTypeApplicationCertificatePlan variable) where
  rnf (CheckedTypeApplicationCertificatePlan steps obligations) =
    rnf steps `seq` rnf obligations

-- | Sealed map from certificate identifier to its checked plan, with every
-- identifier distinct.  Constructed only by
-- 'sealTypeApplicationCertificateTable'; it proves structural replay only,
-- not inventory membership, kinds, discharge, or graph occurrence.
data CheckedTypeApplicationCertificateTable variable =
  CheckedTypeApplicationCertificateTable
    !(Map CertificateId (CheckedTypeApplicationCertificatePlan variable))

type role CheckedTypeApplicationCertificateTable nominal

instance NFData variable =>
    NFData (CheckedTypeApplicationCertificateTable variable) where
  rnf (CheckedTypeApplicationCertificateTable plans) = rnf plans

-- | Bound, normalize, and replay every complete leading specialization.
-- Successful construction retains a finite canonical plan whose 'NFData'
-- instance reaches every payload.  Like the other checked values in this
-- package, construction need not force identities carried by otherwise valid
-- variables; explicit deep evaluation is the honest demand boundary.  Every
-- failure observes only the prefix required by its documented check.
sealTypeApplicationCertificateTable
  :: Ord variable
  => TypeApplicationCertificateLimits
  -> [TypeApplicationCertificateSource variable]
  -> Either
      (TypeApplicationCertificateError variable)
      (CheckedTypeApplicationCertificateTable variable)
sealTypeApplicationCertificateTable limits sources = do
  let maximumEntries = maximumTypeApplicationCertificates limits
      observedEntries = observedListLength maximumEntries sources
  if observedEntries <= maximumEntries
    then pure ()
    else Left $ TypeApplicationCertificateEntryLimitExceeded
      maximumEntries observedEntries
  checkDuplicateIds Set.empty sources
  plans <- mapM (sealCertificatePlan limits) sources
  let table = CheckedTypeApplicationCertificateTable $ Map.fromList
        [ (typeApplicationCertificateSourceId source, plan)
        | (source, plan) <- zip sources plans
        ]
  pure table

checkDuplicateIds
  :: Set.Set CertificateId
  -> [TypeApplicationCertificateSource variable]
  -> Either (TypeApplicationCertificateError variable) ()
checkDuplicateIds _ [] = Right ()
checkDuplicateIds seen (source : remaining)
  | certificate `Set.member` seen = Left $
      DuplicateTypeApplicationCertificateId certificate
  | otherwise = checkDuplicateIds (Set.insert certificate seen) remaining
 where
  certificate = typeApplicationCertificateSourceId source

sealCertificatePlan
  :: Ord variable
  => TypeApplicationCertificateLimits
  -> TypeApplicationCertificateSource variable
  -> Either
      (TypeApplicationCertificateError variable)
      (CheckedTypeApplicationCertificatePlan variable)
sealCertificatePlan limits source = do
  let certificate = typeApplicationCertificateSourceId source
      selections = typeApplicationCertificateSourceSelections source
      maximumSelections = maximumTypeApplicationCertificateSelections limits
      observedSelections = observedListLength maximumSelections selections
  if observedSelections <= maximumSelections
    then pure ()
    else Left $ TypeApplicationCertificateSelectionLimitExceeded
      certificate maximumSelections observedSelections
  normalizedScheme <- observeAndNormalizeInputType limits
    (TypeApplicationCertificateSchemeType certificate)
    $ typeApplicationCertificateSourceScheme source
  let binderCount = observedLeadingBinderCount
        maximumSelections normalizedScheme
  if binderCount == 0
    then Left $ TypeApplicationCertificateHasNoLeadingBinder certificate
    else pure ()
  if binderCount <= maximumSelections
    then pure ()
    else Left $ TypeApplicationCertificateTelescopeLimitExceeded
      certificate maximumSelections $ maximumSelections + 1
  if observedSelections == binderCount
    then pure ()
    else Left $ TypeApplicationCertificateSelectionArityMismatch
      certificate binderCount observedSelections
  normalizedSelections <- sequence
    [ observeAndNormalizeInputType limits
        (TypeApplicationCertificateSelectionType certificate slot)
        selection
    | (slot, selection) <- zip [0 ..] selections
    ]
  let preparedScheme = prepareSourceType normalizedScheme
      preparedSelections = zipWith prepareSelectionType [0 ..]
        normalizedSelections
  (steps, obligationCount) <- replaySelections limits certificate
    preparedScheme preparedSelections
  let plan = CheckedTypeApplicationCertificatePlan steps obligationCount
  plan `seq` pure plan

observedLeadingBinderCount :: Int -> Type variable -> Int
observedLeadingBinderCount maximumBinders = go 0
 where
  go !count (ForallType binders _ body)
    | count > maximumBinders = count
    | otherwise =
        let remaining = maximumBinders - count
            observed = observedListLength remaining binders
            next = count + observed
        in if observed > remaining
            then maximumBinders + 1
            else go next body
  go count _ = count

observeAndNormalizeInputType
  :: Ord variable
  => TypeApplicationCertificateLimits
  -> TypeApplicationCertificateTypeSite
  -> Type variable
  -> Either (TypeApplicationCertificateError variable) (Type variable)
observeAndNormalizeInputType limits site source = do
  observeType limits site source
  case normalizeType source of
    Left failure -> Left $ InvalidTypeApplicationCertificateType site failure
    Right normalized -> Right normalized

observeType
  :: Ord typeVariable
  => TypeApplicationCertificateLimits
  -> TypeApplicationCertificateTypeSite
  -> Type typeVariable
  -> Either (TypeApplicationCertificateError rejection) ()
observeType limits site source = case observeTypeWithin sharedTypeStructure
    maximumNodes maximumWidth source of
  Left (TypeStructureNodeLimitExceeded observed) -> Left $
    TypeApplicationCertificateTypeNodeLimitExceeded
      site maximumNodes observed
  Left (TypeStructureCollectionLimitExceeded observed) -> Left $
    TypeApplicationCertificateTypeCollectionLimitExceeded
      site maximumWidth observed
  Right () -> Right ()
 where
  maximumNodes = maximumTypeApplicationCertificateTypeNodes limits
  maximumWidth = maximumTypeApplicationCertificateCollectionWidth limits

prepareSourceType
  :: Ord variable
  => Type variable
  -> Type (TypeApplicationCertificatePlanVariable variable)
prepareSourceType = fmap convert
  . alphaNormalizeTypeWith PositionalBinderSlots
  . eraseVacuousForalls
 where
  convert alpha = case alpha of
    AlphaBoundVariable scope slot ->
      TypeApplicationCertificateSourceBound scope slot
    AlphaFreeVariable variable -> TypeApplicationCertificateFree variable

prepareSelectionType
  :: Ord variable
  => Natural
  -> Type variable
  -> Type (TypeApplicationCertificatePlanVariable variable)
prepareSelectionType ordinal = fmap convert
  . alphaNormalizeTypeWith PositionalBinderSlots
  . eraseVacuousForalls
 where
  convert alpha = case alpha of
    AlphaBoundVariable scope slot ->
      TypeApplicationCertificateSelectionBound ordinal scope slot
    AlphaFreeVariable variable -> TypeApplicationCertificateFree variable

replaySelections
  :: Ord variable
  => TypeApplicationCertificateLimits
  -> CertificateId
  -> Type (TypeApplicationCertificatePlanVariable variable)
  -> [Type (TypeApplicationCertificatePlanVariable variable)]
  -> Either
      (TypeApplicationCertificateError variable)
      ([CheckedTypeApplicationCertificateStep variable], Int)
replaySelections limits certificate initial selections =
  go 0 initial selections [] 0
 where
  go _ _ [] reversedSteps obligationCount =
    Right (reverse reversedSteps, obligationCount)
  go slot current (selected : remaining) reversedSteps obligationCount = do
    (result, obligations) <- case consumeSelection selected current of
      Nothing -> Left $ TypeApplicationCertificateReplayEndedEarly
        certificate slot
      Just replayed -> Right replayed
    observeType limits
      (TypeApplicationCertificateDerivedResultType certificate slot)
      result
    let maximumObligations =
          maximumTypeApplicationCertificateObligations limits
        remainingObligations = maximumObligations - obligationCount
        observedAdded = observedListLength remainingObligations obligations
    if observedAdded <= remainingObligations
      then pure ()
      else Left $ TypeApplicationCertificateObligationLimitExceeded
        certificate maximumObligations $ maximumObligations + 1
    mapM_ (observeObligation slot) $ zip [0 ..] obligations
    -- Substitution can newly saturate a partial arrow or tuple constructor.
    -- Bound the raw expanded trees before traversing them canonically, then
    -- retain and replay only the shared storage form.
    let canonicalResult = canonicalizeType result
        canonicalObligations = map (fmap canonicalizeType) obligations
    let observedObligations = obligationCount + observedAdded
    let step = CheckedTypeApplicationCertificateStep
          slot current selected canonicalResult canonicalObligations
    go (slot + 1) canonicalResult remaining (step : reversedSteps)
      observedObligations

  observeObligation slot (obligation, Constraint _ arguments) =
    mapM_ (\(argument, ty) -> observeType limits
        (TypeApplicationCertificateDerivedObligationType
          certificate slot obligation argument)
        ty)
      $ zip [0 ..] arguments

-- Mirror visible specified-binder consumption. Binderless contexts before a
-- binder remain outside the next result. Contexts become obligations only
-- when the final binder of the complete leading chain has been consumed.
consumeSelection
  :: Eq variable
  => Type variable
  -> Type variable
  -> Maybe (Type variable, [Constraint (Type variable)])
consumeSelection selected = consume []
 where
  consume outerContexts (ForallType [] contexts body) =
    consume (outerContexts ++ contexts) body
  consume outerContexts (ForallType (binder : remaining) contexts body) =
    let substitute = replaceVariable binder selected
        instantiatedContexts = map (fmap substitute) contexts
        instantiatedBody = substitute body
    in if not $ null remaining
        then Just
          ( wrapContexts outerContexts $ ForallType remaining
              instantiatedContexts instantiatedBody
          , []
          )
        else case peelBinderlessContexts [] body of
          (_, ForallType (_ : _) _ _) -> Just
            ( wrapContexts (outerContexts ++ instantiatedContexts)
                instantiatedBody
            , []
            )
          (trailingContexts, sourceResult) ->
            finish instantiatedContexts
              (map (fmap substitute) trailingContexts)
              (substitute sourceResult)
   where
    finish instantiatedContexts trailingContexts result = Just
      ( result
      , outerContexts ++ instantiatedContexts ++ trailingContexts
      )
  consume _ _ = Nothing

  wrapContexts [] body = body
  wrapContexts contexts body = ForallType [] contexts body

peelBinderlessContexts
  :: [Constraint (Type variable)]
  -> Type variable
  -> ([Constraint (Type variable)], Type variable)
peelBinderlessContexts contexts (ForallType [] nested body) =
  peelBinderlessContexts (contexts ++ nested) body
peelBinderlessContexts contexts body = (contexts, body)

replaceVariable
  :: Eq variable
  => variable
  -> Type variable
  -> Type variable
  -> Type variable
replaceVariable selected replacement source = case source of
  TypeVariable variable
    | variable == selected -> replacement
    | otherwise -> source
  TypeConstructor { } -> source
  TypeApplication function argument -> TypeApplication
    (replaceVariable selected replacement function)
    (replaceVariable selected replacement argument)
  FunctionType parameter result -> FunctionType
    (replaceVariable selected replacement parameter)
    (replaceVariable selected replacement result)
  TupleType boxity elements -> TupleType boxity
    $ map (replaceVariable selected replacement) elements
  ForallType binders constraints body
    | selected `elem` binders -> source
    | otherwise -> ForallType binders
        (map (fmap $ replaceVariable selected replacement) constraints)
        (replaceVariable selected replacement body)

-- | Match an independent checker's final observations against one structural
-- replay.  The matcher performs its own bounded normalization of every
-- repeated payload before comparing it with the private plan namespaces.
-- Bound variables compare alpha-equivalently; genuinely free variables keep
-- their exact nominal identity.
matchCheckedTypeApplicationCertificateObservations
  :: Ord variable
  => TypeApplicationCertificateLimits
  -> CertificateId
  -> [TypeApplicationCertificateObservation variable]
  -> CheckedTypeApplicationCertificateTable variable
  -> Either
      (TypeApplicationCertificateError variable)
      (CheckedTypeApplicationCertificatePlan variable)
matchCheckedTypeApplicationCertificateObservations limits certificate
    observations table = do
  plan <- case lookupCheckedTypeApplicationCertificatePlan certificate table of
    Nothing -> Left $
      TypeApplicationCertificateObservationMissingPlan certificate
    Just matched -> Right matched
  let maximumObservations =
        maximumTypeApplicationCertificateSelections limits
      observedObservations = observedListLength maximumObservations observations
      expectedSteps = checkedTypeApplicationCertificateSteps plan
      expectedCount = checkedTypeApplicationCertificateStepCount plan
  unless (observedObservations <= maximumObservations) $ Left $
    TypeApplicationCertificateObservationLimitExceeded certificate
      maximumObservations observedObservations
  unless (observedObservations == expectedCount) $ Left $
    TypeApplicationCertificateObservationArityMismatch certificate
      expectedCount observedObservations
  _ <- matchSteps expectedCount observedObservations 0
    observations expectedSteps
  pure plan
 where
  matchSteps _ _ obligationCount [] [] = Right obligationCount
  matchSteps expectedCount observedCount obligationCount
      (observation : remainingObservations)
      (expected : remainingExpected) = do
    let expectedSlot = checkedTypeApplicationCertificateStepSlot expected
        actualSlot = typeApplicationCertificateObservationSlot observation
    unless (actualSlot == expectedSlot) $ Left $
      TypeApplicationCertificateObservationSlotMismatch certificate
        expectedSlot actualSlot
    observedSource <- normalizeObserved
      (TypeApplicationCertificateObservedSourceType certificate expectedSlot)
      $ typeApplicationCertificateObservationSource observation
    unless (matchesPlan observedSource $
        checkedTypeApplicationCertificateStepSource expected) $ Left $
      TypeApplicationCertificateObservationSourceMismatch
        certificate expectedSlot
    observedSelected <- normalizeObserved
      (TypeApplicationCertificateObservedSelectedType certificate expectedSlot)
      $ typeApplicationCertificateObservationSelected observation
    unless (matchesPlan observedSelected $
        checkedTypeApplicationCertificateStepSelected expected) $ Left $
      TypeApplicationCertificateObservationSelectedMismatch
        certificate expectedSlot
    observedResult <- normalizeObserved
      (TypeApplicationCertificateObservedResultType certificate expectedSlot)
      $ typeApplicationCertificateObservationResult observation
    unless (matchesPlan observedResult $
        checkedTypeApplicationCertificateStepResult expected) $ Left $
      TypeApplicationCertificateObservationResultMismatch
        certificate expectedSlot
    nextObligationCount <- matchObligations expectedSlot obligationCount
      (typeApplicationCertificateObservationObligations observation)
      (checkedTypeApplicationCertificateStepObligations expected)
    matchSteps expectedCount observedCount nextObligationCount
      remainingObservations remainingExpected
  matchSteps expectedCount observedCount _ _ _ = Left $
    TypeApplicationCertificateObservationArityMismatch certificate
      expectedCount observedCount

  matchObligations slot prior actual expected = do
    let maximumObligations =
          maximumTypeApplicationCertificateObligations limits
        remainingCapacity = maximumObligations - prior
        observedCount = observedListLength remainingCapacity actual
        expectedCount = length expected
    unless (observedCount <= remainingCapacity) $ Left $
      TypeApplicationCertificateObservedObligationLimitExceeded certificate
        maximumObligations $ maximumObligations + 1
    unless (observedCount == expectedCount) $ Left $
      TypeApplicationCertificateObservedObligationCountMismatch certificate
        slot expectedCount observedCount
    mapM_ (matchObligation slot) $ zip3 [0 ..] actual expected
    pure $ prior + observedCount

  matchObligation slot (ordinal, actual, expected) = do
    let actualName = constraintClass actual
        expectedName = constraintClass expected
    unless (actualName == expectedName) $ Left $
      TypeApplicationCertificateObservedObligationClassMismatch certificate
        slot ordinal expectedName actualName
    let actualArguments = constraintArguments actual
        expectedArguments = constraintArguments expected
        maximumWidth =
          maximumTypeApplicationCertificateCollectionWidth limits
        observedArguments = observedListLength maximumWidth actualArguments
        expectedArgumentsCount = length expectedArguments
    unless (observedArguments <= maximumWidth) $ Left $
      TypeApplicationCertificateTypeCollectionLimitExceeded
        (TypeApplicationCertificateObservedObligationArguments
          certificate slot ordinal)
        maximumWidth observedArguments
    unless (observedArguments == expectedArgumentsCount) $ Left $
      TypeApplicationCertificateObservedObligationArgumentCountMismatch
        certificate slot ordinal expectedArgumentsCount observedArguments
    mapM_ (matchArgument slot ordinal) $
      zip3 [0 ..] actualArguments expectedArguments

  matchArgument slot obligation (argument, actual, expected) = do
    normalized <- normalizeObserved
      (TypeApplicationCertificateObservedObligationType
        certificate slot obligation argument)
      actual
    unless (matchesPlan normalized expected) $ Left $
      TypeApplicationCertificateObservedObligationArgumentMismatch
        certificate slot obligation argument

  normalizeObserved = observeAndNormalizeInputType limits

  matchesPlan actual expected = TypeAtom.alphaEquivalentTypes
    (fmap TypeApplicationCertificateFree actual) expected

-- | Number of distinct certificates in a sealed table.
checkedTypeApplicationCertificateCount
  :: CheckedTypeApplicationCertificateTable variable -> Int
checkedTypeApplicationCertificateCount
    (CheckedTypeApplicationCertificateTable plans) = Map.size plans

-- | Checked plan for one certificate identifier, or 'Nothing' when the
-- table holds no certificate with that identifier.
lookupCheckedTypeApplicationCertificatePlan
  :: CertificateId
  -> CheckedTypeApplicationCertificateTable variable
  -> Maybe (CheckedTypeApplicationCertificatePlan variable)
lookupCheckedTypeApplicationCertificatePlan certificate
    (CheckedTypeApplicationCertificateTable plans) =
  Map.lookup certificate plans

-- | Number of steps in a plan, equal to the number of selected types and
-- of consumed leading binders.
checkedTypeApplicationCertificateStepCount
  :: CheckedTypeApplicationCertificatePlan variable -> Int
checkedTypeApplicationCertificateStepCount
    (CheckedTypeApplicationCertificatePlan steps _) = length steps

-- | Steps of a plan in slot order, starting at slot zero.
checkedTypeApplicationCertificateSteps
  :: CheckedTypeApplicationCertificatePlan variable
  -> [CheckedTypeApplicationCertificateStep variable]
checkedTypeApplicationCertificateSteps
    (CheckedTypeApplicationCertificatePlan steps _) = steps

-- | Zero-based position of the consumed binder within the leading telescope.
checkedTypeApplicationCertificateStepSlot
  :: CheckedTypeApplicationCertificateStep variable -> Natural
checkedTypeApplicationCertificateStepSlot
    (CheckedTypeApplicationCertificateStep slot _ _ _ _) = slot

-- | Type the step consumed a binder from: the prepared scheme for slot zero,
-- otherwise the previous step's result.
checkedTypeApplicationCertificateStepSource
  :: CheckedTypeApplicationCertificateStep variable
  -> Type (TypeApplicationCertificatePlanVariable variable)
checkedTypeApplicationCertificateStepSource
    (CheckedTypeApplicationCertificateStep _ source _ _ _) = source

-- | Prepared selected type substituted for the consumed binder.
checkedTypeApplicationCertificateStepSelected
  :: CheckedTypeApplicationCertificateStep variable
  -> Type (TypeApplicationCertificatePlanVariable variable)
checkedTypeApplicationCertificateStepSelected
    (CheckedTypeApplicationCertificateStep _ _ selected _ _) = selected

-- | Canonical type remaining after the substitution; it is the source of the
-- next step when one exists.
checkedTypeApplicationCertificateStepResult
  :: CheckedTypeApplicationCertificateStep variable
  -> Type (TypeApplicationCertificatePlanVariable variable)
checkedTypeApplicationCertificateStepResult
    (CheckedTypeApplicationCertificateStep _ _ _ result _) = result

-- | Ordered canonical obligations made unconditional by this step.  They are
-- non-empty only for the step consuming the final binder of the complete
-- leading chain.
checkedTypeApplicationCertificateStepObligations
  :: CheckedTypeApplicationCertificateStep variable
  -> [Constraint (Type (TypeApplicationCertificatePlanVariable variable))]
checkedTypeApplicationCertificateStepObligations
    (CheckedTypeApplicationCertificateStep _ _ _ _ obligations) = obligations

-- | Length of 'checkedTypeApplicationCertificateStepObligations'.
checkedTypeApplicationCertificateStepObligationCount
  :: CheckedTypeApplicationCertificateStep variable -> Int
checkedTypeApplicationCertificateStepObligationCount
    (CheckedTypeApplicationCertificateStep _ _ _ _ obligations) =
  length obligations

-- | Total obligations across every step of the plan, as counted during
-- replay against 'maximumTypeApplicationCertificateObligations'.
checkedTypeApplicationCertificateObligationCount
  :: CheckedTypeApplicationCertificatePlan variable -> Int
checkedTypeApplicationCertificateObligationCount
    (CheckedTypeApplicationCertificatePlan _ obligations) = obligations

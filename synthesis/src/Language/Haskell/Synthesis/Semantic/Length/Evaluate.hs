{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Bounded, solver-independent evaluation for checked length contracts,
-- provider summaries, and sealed candidate problems.
--
-- This is the replay authority for concrete natural-number assignments.  It
-- deliberately consumes only opaque checked values: evaluating a caller-built
-- raw syntax tree here could diverge before the length sealer's structural
-- bounds were established.  Detached contract and provider results classify
-- one assignment without evidence authority.  Whole-problem replay can bind
-- an exact model-relative counterexample receipt to the sealed problem
-- identities; it still supplies neither universal evidence nor permission to
-- prune other candidates.
module Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationLimitSource (..)
  , LengthEvaluationLimits
  , LengthEvaluationLimitField (..)
  , LengthEvaluationLimitError (..)
  , mkLengthEvaluationLimits
  , defaultLengthEvaluationLimitSource
  , defaultLengthEvaluationLimits
  , lengthAssignmentValueBitLimit
  , lengthIntermediateValueBitLimit
  , LengthContractAssignment (..)
  , LengthProblemAssignment (..)
  , LengthProviderArgumentValue (..)
  , LengthEvaluationValueSite (..)
  , LengthEvaluationError (..)
  , LengthContractEvaluation (..)
  , LengthCounterexampleBasis (..)
  , ValidatedLengthCounterexample
  , validatedLengthCounterexampleInputs
  , validatedLengthCounterexampleResult
  , validatedLengthCounterexampleBasis
  , evaluateLengthContractAssignment
  , evaluateLengthProviderApplication
  , validateLengthProblemCounterexample
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM, unless)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Name (Name)
import Language.Haskell.Synthesis.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthProviderSummary
  , FiniteListSpineLengthV1
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  , LengthProviderArgumentRole (..)
  , LengthProviderVariable (..)
  , checkedLengthContractInputCount
  , checkedLengthContractPostcondition
  , checkedLengthContractPrecondition
  , checkedLengthProviderArgumentRoles
  , checkedLengthProviderTransfer
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthProblem
  , checkedLengthCandidateResult
  , checkedLengthCandidateUsedProviders
  , checkedLengthProblemBehavioralProblem
  , checkedLengthProblemCandidate
  , checkedLengthProblemInputCount
  , checkedLengthProblemPostcondition
  , checkedLengthProblemPrecondition
  )
import Language.Haskell.Synthesis.Internal.Semantic.Problem
  ( BehavioralEvidence
  , mkBehavioralEvidence
  )

-- | Raw operational bounds for concrete replay. Zero is valid: only the
-- natural number zero has a zero-bit representation.
data LengthEvaluationLimitSource = LengthEvaluationLimitSource
  { lengthEvaluationLimitSourceAssignmentValueBits :: Int
  , lengthEvaluationLimitSourceIntermediateValueBits :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitSource

-- | Validated nonnegative replay limits. The constructor stays private so
-- every evaluator can rely on both fields being usable as finite bounds.
data LengthEvaluationLimits = LengthEvaluationLimits !Int !Int
  deriving (Eq, Ord, Show)

instance NFData LengthEvaluationLimits where
  rnf (LengthEvaluationLimits assignments intermediate) =
    rnf assignments `seq` rnf intermediate

-- | Stable field identity for limit diagnostics.
data LengthEvaluationLimitField
  = LengthAssignmentValueBits
  | LengthIntermediateValueBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitField

-- | Failure to construct replay limits.
data LengthEvaluationLimitError = NegativeLengthEvaluationLimit
  !LengthEvaluationLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationLimitError

-- | Validate raw limits in declaration order.
mkLengthEvaluationLimits
  :: LengthEvaluationLimitSource
  -> Either LengthEvaluationLimitError LengthEvaluationLimits
mkLengthEvaluationLimits source = do
  nonnegative LengthAssignmentValueBits
    $ lengthEvaluationLimitSourceAssignmentValueBits source
  nonnegative LengthIntermediateValueBits
    $ lengthEvaluationLimitSourceIntermediateValueBits source
  pure $ LengthEvaluationLimits
    (lengthEvaluationLimitSourceAssignmentValueBits source)
    (lengthEvaluationLimitSourceIntermediateValueBits source)
 where
  nonnegative field value
    | value < 0 = Left $ NegativeLengthEvaluationLimit field value
    | otherwise = Right ()

-- | Conservative defaults for independently replaying checked syntax.
defaultLengthEvaluationLimitSource :: LengthEvaluationLimitSource
defaultLengthEvaluationLimitSource = LengthEvaluationLimitSource
  { lengthEvaluationLimitSourceAssignmentValueBits = 4096
  , lengthEvaluationLimitSourceIntermediateValueBits = 4096
  }

-- | Validated form of 'defaultLengthEvaluationLimitSource'.
defaultLengthEvaluationLimits :: LengthEvaluationLimits
defaultLengthEvaluationLimits = LengthEvaluationLimits 4096 4096

-- | Maximum bit width of every caller-supplied spine length.
lengthAssignmentValueBitLimit :: LengthEvaluationLimits -> Int
lengthAssignmentValueBitLimit (LengthEvaluationLimits value _) = value

-- | Maximum bit width of literals and arithmetic results during replay.
lengthIntermediateValueBitLimit :: LengthEvaluationLimits -> Int
lengthIntermediateValueBitLimit (LengthEvaluationLimits _ value) = value

-- | Concrete list-spine lengths for one contract application.
--
-- Inputs remain in the checked contract's source order.  The result is kept
-- separate because preconditions cannot refer to it, while postconditions may.
data LengthContractAssignment = LengthContractAssignment
  { lengthContractAssignmentInputs :: [Natural]
  , lengthContractAssignmentResult :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthContractAssignment

-- | Source-ordered natural inputs decoded for one exact candidate problem.
--
-- There is deliberately no caller-supplied result.  The validator computes
-- that value from the checked candidate retained by the problem, preventing
-- a solver model decoder from pairing valid inputs with a spoofed output.
data LengthProblemAssignment = LengthProblemAssignment
  { lengthProblemAssignmentInputs :: [Natural]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProblemAssignment

-- | A provider call supplies a number only where the checked role exposes a
-- list spine.  Requiring the explicit unobserved marker prevents callers from
-- smuggling a semantic claim about an opaque argument into replay.
data LengthProviderArgumentValue
  = ObservedSpineLength Natural
  | UnobservedLengthArgument
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthProviderArgumentValue

-- | Exact assignment or arithmetic site which exceeded its bit bound.
data LengthEvaluationValueSite
  = LengthContractInputValue Int
  | LengthContractResultValue
  | LengthProblemInputValue Int
  | LengthProviderSpineValue Int
  | LengthIntermediateValue
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationValueSite

-- | Deterministic failure while replaying one checked value.
data LengthEvaluationError
  = LengthContractAssignmentArityMismatch !Int !Int
  | LengthProblemAssignmentArityMismatch !Int !Int
  | LengthProviderAssignmentArityMismatch !Int !Int
  | LengthProviderArgumentRoleMismatch
      !Int !LengthProviderArgumentRole !LengthProviderArgumentValue
  | LengthEvaluationValueBitLimitExceeded
      !LengthEvaluationValueSite !Int !Int
  | LengthEvaluationInternalContractReference !LengthContractVariable
  | LengthEvaluationInternalProviderReference !LengthProviderVariable
  | LengthEvaluationInternalModuloDivisorZero
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthEvaluationError

-- | Complete classification of one concrete contract assignment.
data LengthContractEvaluation
  = LengthPreconditionNotMet
  | LengthPostconditionSatisfied
  | LengthPostconditionViolated
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthContractEvaluation

-- | Explicit semantic basis of a replayed Length counterexample.
--
-- Even the provider-independent case is a result in the versioned total
-- finite-spine model, not automatically a realized counterexample in a source
-- language with bottoms or effects.  Provider-backed results additionally
-- depend on every named assumed law in the retained list.
data LengthCounterexampleBasis
  = ProviderIndependentFiniteSpineModel
  | FiniteSpineModelUnderAssumedProviderLaws [Name]
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthCounterexampleBasis

-- | Independently replayed model-relative violation of one sealed Length
-- problem.
--
-- The constructor stays private.  The receipt is exact relative to the
-- problem's fingerprinted semantic encoding.  Its explicit basis records any
-- assumed provider laws; it is not evidence about an unverified provider
-- implementation or source-language realization.  Its enclosing
-- 'BehavioralEvidence' can reveal this value only after replay against the
-- same complete problem identity succeeds.
data ValidatedLengthCounterexample = ValidatedLengthCounterexampleReceipt
  ![Natural]
  !Natural
  !LengthCounterexampleBasis
  deriving (Eq, Ord, Show)

instance NFData ValidatedLengthCounterexample where
  rnf (ValidatedLengthCounterexampleReceipt inputs result basis) =
    rnf inputs `seq` rnf result `seq` rnf basis

-- | Source-ordered inputs which make the sealed bad-state formula true.
validatedLengthCounterexampleInputs
  :: ValidatedLengthCounterexample
  -> [Natural]
validatedLengthCounterexampleInputs
    (ValidatedLengthCounterexampleReceipt inputs _ _) = inputs

-- | Result computed from the sealed candidate, never supplied by the caller.
validatedLengthCounterexampleResult
  :: ValidatedLengthCounterexample
  -> Natural
validatedLengthCounterexampleResult
    (ValidatedLengthCounterexampleReceipt _ result _) = result

-- | Whether replay was provider-independent or conditional on named laws.
validatedLengthCounterexampleBasis
  :: ValidatedLengthCounterexample
  -> LengthCounterexampleBasis
validatedLengthCounterexampleBasis
    (ValidatedLengthCounterexampleReceipt _ _ basis) = basis

-- | Classify one concrete contract assignment.  Arity is checked before any
-- value, inputs are bounded left-to-right before the result, and a false
-- precondition does not evaluate the postcondition.
evaluateLengthContractAssignment
  :: LengthEvaluationLimits
  -> CheckedLengthContract variable
  -> LengthContractAssignment
  -> Either LengthEvaluationError LengthContractEvaluation
evaluateLengthContractAssignment limits contract assignment = do
  inputs <- exactAssignment
    LengthContractAssignmentArityMismatch
    (checkedLengthContractInputCount contract)
    $ lengthContractAssignmentInputs assignment
  mapM_ (uncurry $ checkAssignedValue limits . LengthContractInputValue)
    $ zip [0 ..] inputs
  checkAssignedValue limits LengthContractResultValue
    $ lengthContractAssignmentResult assignment
  let lookupVariable variable = case variable of
        LengthInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left $ LengthEvaluationInternalContractReference variable
        LengthResult -> Right $ lengthContractAssignmentResult assignment
  precondition <- evaluateFormula limits lookupVariable
    $ checkedLengthContractPrecondition contract
  if not precondition
    then Right LengthPreconditionNotMet
    else do
      postcondition <- evaluateFormula limits lookupVariable
        $ checkedLengthContractPostcondition contract
      pure $ if postcondition
        then LengthPostconditionSatisfied
        else LengthPostconditionViolated

-- | Evaluate one exact provider application under its checked assumed law.
-- The result remains conditional on that explicit assumption and carries no
-- behavioral-evidence authority.
evaluateLengthProviderApplication
  :: LengthEvaluationLimits
  -> CheckedLengthProviderSummary variable
  -> [LengthProviderArgumentValue]
  -> Either LengthEvaluationError Natural
evaluateLengthProviderApplication limits summary rawArguments = do
  let roles = checkedLengthProviderArgumentRoles summary
  arguments <- exactAssignment LengthProviderAssignmentArityMismatch
    (length roles) rawArguments
  observed <- mapM validateArgument $ zip3 [0 ..] roles arguments
  evaluateExpression limits (lookupObserved observed)
    $ checkedLengthProviderTransfer summary
 where
  validateArgument (index, role, argument) = case (role, argument) of
    (LengthSpineArgument, ObservedSpineLength value) -> do
      checkAssignedValue limits (LengthProviderSpineValue index) value
      Right $ Just value
    (LengthUnobservedArgument, UnobservedLengthArgument) -> Right Nothing
    _ -> Left $ LengthProviderArgumentRoleMismatch index role argument

  lookupObserved observed variable@(LengthProviderArgument position) =
    case indexNatural position observed of
      Just (Just value) -> Right value
      _ -> Left $ LengthEvaluationInternalProviderReference variable

-- | Independently validate decoded inputs against one exact candidate
-- problem.  The checked precondition is evaluated first.  A false
-- precondition is an ordinary non-counterexample and does not force the
-- candidate result.  Otherwise one shared lazy result computation is bound
-- while evaluating the checked postcondition.  A result-independent true
-- postcondition does not force it; a false postcondition forces it before
-- constructing a problem-bound evidence receipt.
--
-- In particular, this function does not consume a raw solver observation and
-- cannot strengthen @unsat@ or @unknown@.  A satisfiable model remains a hint
-- until its decoded natural inputs pass this replay boundary.
validateLengthProblemCounterexample
  :: LengthEvaluationLimits
  -> CheckedLengthProblem identity local
  -> LengthProblemAssignment
  -> Either LengthEvaluationError
      (Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))
validateLengthProblemCounterexample limits problem assignment = do
  inputs <- exactAssignment
    LengthProblemAssignmentArityMismatch
    (checkedLengthProblemInputCount problem)
    $ lengthProblemAssignmentInputs assignment
  mapM_ (uncurry $ checkAssignedValue limits . LengthProblemInputValue)
    $ zip [0 ..] inputs
  let lookupInput variable = case variable of
        LengthInput position -> case indexNatural position inputs of
          Just value -> Right value
          Nothing -> Left $ LengthEvaluationInternalContractReference variable
        LengthResult -> Left
          $ LengthEvaluationInternalContractReference LengthResult
  precondition <- evaluateFormula limits lookupInput
    $ checkedLengthProblemPrecondition problem
  if not precondition
    then Right Nothing
    else do
      let resultOr = evaluateExpression limits lookupInput
            $ checkedLengthCandidateResult
            $ checkedLengthProblemCandidate problem
          lookupResult variable = case variable of
            LengthResult -> resultOr
            LengthInput position -> case indexNatural position inputs of
              Just value -> Right value
              Nothing -> Left
                $ LengthEvaluationInternalContractReference variable
      postcondition <- evaluateFormula limits lookupResult
        $ checkedLengthProblemPostcondition problem
      if postcondition
        then Right Nothing
        else do
          result <- resultOr
          let usedProviders = checkedLengthCandidateUsedProviders
                $ checkedLengthProblemCandidate problem
              basis = case usedProviders of
                [] -> ProviderIndependentFiniteSpineModel
                names -> FiniteSpineModelUnderAssumedProviderLaws names
              receipt = ValidatedLengthCounterexampleReceipt
                inputs result basis
          pure $ Just $ mkBehavioralEvidence
            (checkedLengthProblemBehavioralProblem problem) receipt

exactAssignment
  :: (Int -> Int -> LengthEvaluationError)
  -> Int
  -> [value]
  -> Either LengthEvaluationError [value]
exactAssignment mismatch expected values =
  let observed = observedListLength expected values
  in if observed == expected
      then Right values
      else Left $ mismatch expected observed

checkAssignedValue
  :: LengthEvaluationLimits
  -> LengthEvaluationValueSite
  -> Natural
  -> Either LengthEvaluationError ()
checkAssignedValue limits site value =
  checkValueWithin site (lengthAssignmentValueBitLimit limits) value

checkIntermediate
  :: LengthEvaluationLimits
  -> Natural
  -> Either LengthEvaluationError Natural
checkIntermediate limits value = value <$ checkValueWithin
  LengthIntermediateValue (lengthIntermediateValueBitLimit limits) value

checkValueWithin
  :: LengthEvaluationValueSite
  -> Int
  -> Natural
  -> Either LengthEvaluationError ()
checkValueWithin site maximumBits value =
  let observedBits = observedNaturalBits maximumBits value
  in unless (observedBits <= maximumBits) $ Left
      $ LengthEvaluationValueBitLimitExceeded
          site maximumBits observedBits

evaluateExpression
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthExpression variable
  -> Either LengthEvaluationError Natural
evaluateExpression limits lookupVariable source = case source of
  LengthVariable variable -> lookupVariable variable
  LengthLiteral value -> checkIntermediate limits value
  LengthSum terms -> foldM add 0 terms
  LengthScale factor expression -> do
    value <- evaluateExpression limits lookupVariable expression
    checkIntermediate limits $ factor * value
  LengthModulo divisor expression
    | divisor == 0 -> Left LengthEvaluationInternalModuloDivisorZero
    | otherwise -> do
        value <- evaluateExpression limits lookupVariable expression
        checkIntermediate limits $ value `mod` divisor
  LengthMonus left right -> do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    checkIntermediate limits $ leftValue `monus` rightValue
  LengthMinimum left right -> binary min left right
  LengthMaximum left right -> binary max left right
  LengthIf condition whenTrue whenFalse -> do
    selected <- evaluateFormula limits lookupVariable condition
    evaluateExpression limits lookupVariable
      $ if selected then whenTrue else whenFalse
 where
  add total term = do
    value <- evaluateExpression limits lookupVariable term
    checkIntermediate limits $ total + value

  binary operation left right = do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    checkIntermediate limits $ operation leftValue rightValue

evaluateFormula
  :: LengthEvaluationLimits
  -> (variable -> Either LengthEvaluationError Natural)
  -> LengthFormula variable
  -> Either LengthEvaluationError Bool
evaluateFormula limits lookupVariable source = case source of
  LengthTruth value -> Right value
  LengthEqual left right -> compareWith (==) left right
  LengthAtMost left right -> compareWith (<=) left right
  LengthNot formula -> not <$> evaluateFormula limits lookupVariable formula
  LengthAll formulas -> allM formulas
 where
  compareWith relation left right = do
    leftValue <- evaluateExpression limits lookupVariable left
    rightValue <- evaluateExpression limits lookupVariable right
    pure $ relation leftValue rightValue

  allM [] = Right True
  allM (formula : remaining) = do
    value <- evaluateFormula limits lookupVariable formula
    if value then allM remaining else Right False

monus :: Natural -> Natural -> Natural
monus left right
  | left >= right = left - right
  | otherwise = 0

indexNatural :: Natural -> [value] -> Maybe value
indexNatural 0 (value : _) = Just value
indexNatural position (_ : remaining) = indexNatural (position - 1) remaining
indexNatural _ [] = Nothing

observedNaturalBits :: Int -> Natural -> Int
observedNaturalBits maximumBits = go 0
 where
  bound = max 0 maximumBits

  go !observed 0 = observed
  go !observed remaining
    | observed >= bound = saturatedSuccessor bound
    | otherwise = go (observed + 1) $ remaining `quot` 2

saturatedSuccessor :: Int -> Int
saturatedSuccessor value
  | value == maxBound = maxBound
  | otherwise = value + 1

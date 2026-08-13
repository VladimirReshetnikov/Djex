{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private construction of canonical SMT-LIB queries for checked Length
-- problems.
--
-- The typed query plan is constructed transiently beside its rendering and
-- bound structurally into the sealed fingerprint.  Raw solver statuses do not
-- enter this module: a decoded input assignment can acquire evidence only by
-- passing the independent concrete replay boundary.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib
  ( LengthSMTLibQueryFingerprintSubject
  , lengthSMTLibQuerySchemaTag
  , lengthSMTLibQueryLogic
  , LengthSMTLibLimitSource (..)
  , LengthSMTLibLimits
  , LengthSMTLibLimitField (..)
  , LengthSMTLibLimitError (..)
  , mkLengthSMTLibLimits
  , defaultLengthSMTLibLimitSource
  , defaultLengthSMTLibLimits
  , lengthSMTLibCommandByteLimit
  , lengthSMTLibFingerprintByteLimit
  , lengthSMTLibNumeralBitLimit
  , LengthSMTLibCommandPart (..)
  , LengthSMTLibNumeralSite (..)
  , LengthSMTLibQueryError (..)
  , LengthSMTLibQuery
  , sealLengthSMTLibQuery
  , lengthSMTLibQueryInputSymbols
  , lengthSMTLibQueryCheckBytes
  , lengthSMTLibQueryInputValueRequestBytes
  , lengthSMTLibQueryFingerprint
  , lengthSMTLibQueryBehavioralProblem
  , LengthSMTLibIntegerBinding (..)
  , LengthSMTLibModelError (..)
  , validateLengthSMTLibCounterexample
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  , fingerprintCanonicalBytes
  )
import Language.Haskell.Synthesis.Semantic.Length
  ( FiniteListSpineLengthV1
  , LengthContractVariable (..)
  , LengthExpression (..)
  , LengthFormula (..)
  )
import Language.Haskell.Synthesis.Semantic.Length.Evaluate
  ( LengthEvaluationError
  , LengthEvaluationLimits
  , LengthProblemAssignment (..)
  , ValidatedLengthCounterexample
  , validateLengthProblemCounterexample
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthProblem
  , checkedLengthProblemBehavioralProblem
  , checkedLengthProblemCounterexampleCondition
  , checkedLengthProblemInputCount
  )
import Language.Haskell.Synthesis.Semantic.Problem
  ( BehavioralEvidence
  , BehavioralProblem
  , behavioralProblemCandidateFingerprint
  , behavioralProblemDomain
  , behavioralProblemEncodingFingerprint
  , behavioralProblemFingerprint
  , behavioralProblemInventoryFingerprint
  )

-- | Identity of the fixed translator, typed plan, exact problem, and rendered
-- request bytes.  A future execution identity must additionally bind the Z3
-- build, executable, process protocol, resource limits, and runtime options.
data LengthSMTLibQueryFingerprintSubject

-- | Stable translator schema.  This is deliberately distinct from the
-- solver-neutral Length encoding fingerprint.
lengthSMTLibQuerySchemaTag :: [Word8]
lengthSMTLibQuerySchemaTag = ascii "djex-length-z3-qf-lia-smtlib2/v2"

-- | The only logic emitted by this translator.  Positive-literal natural
-- quotient and modulo are lowered to existential quotient/remainder
-- constraints containing only linear integer arithmetic; this translator
-- never emits SMT-LIB @div@ or @mod@.
lengthSMTLibQueryLogic :: [Word8]
lengthSMTLibQueryLogic = ascii "QF_LIA"

-- | Raw independent bounds for one canonical query.  Natural-valued byte
-- fields admit zero; only the signed bit limit requires validation.
data LengthSMTLibLimitSource = LengthSMTLibLimitSource
  { lengthSMTLibLimitSourceCommandBytes :: Natural
  , lengthSMTLibLimitSourceFingerprintBytes :: Natural
  , lengthSMTLibLimitSourceNumeralBits :: Int
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitSource

-- | Validated query construction limits.
data LengthSMTLibLimits = LengthSMTLibLimits !Natural !Natural !Int
  deriving (Eq, Ord, Show)

instance NFData LengthSMTLibLimits where
  rnf (LengthSMTLibLimits commands fingerprint numerals) =
    rnf commands `seq` rnf fingerprint `seq` rnf numerals

data LengthSMTLibLimitField = LengthSMTLibNumeralBits
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitField

data LengthSMTLibLimitError = NegativeLengthSMTLibLimit
  !LengthSMTLibLimitField !Int
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibLimitError

mkLengthSMTLibLimits
  :: LengthSMTLibLimitSource
  -> Either LengthSMTLibLimitError LengthSMTLibLimits
mkLengthSMTLibLimits source
  | numeralBits < 0 = Left $ NegativeLengthSMTLibLimit
      LengthSMTLibNumeralBits numeralBits
  | otherwise = Right $ LengthSMTLibLimits
      (lengthSMTLibLimitSourceCommandBytes source)
      (lengthSMTLibLimitSourceFingerprintBytes source)
      numeralBits
 where
  numeralBits = lengthSMTLibLimitSourceNumeralBits source

defaultLengthSMTLibLimitSource :: LengthSMTLibLimitSource
defaultLengthSMTLibLimitSource = LengthSMTLibLimitSource
  { lengthSMTLibLimitSourceCommandBytes = 65536
  , lengthSMTLibLimitSourceFingerprintBytes = 262144
  , lengthSMTLibLimitSourceNumeralBits = 4096
  }

defaultLengthSMTLibLimits :: LengthSMTLibLimits
defaultLengthSMTLibLimits = LengthSMTLibLimits 65536 262144 4096

lengthSMTLibCommandByteLimit :: LengthSMTLibLimits -> Natural
lengthSMTLibCommandByteLimit (LengthSMTLibLimits value _ _) = value

lengthSMTLibFingerprintByteLimit :: LengthSMTLibLimits -> Natural
lengthSMTLibFingerprintByteLimit (LengthSMTLibLimits _ value _) = value

lengthSMTLibNumeralBitLimit :: LengthSMTLibLimits -> Int
lengthSMTLibNumeralBitLimit (LengthSMTLibLimits _ _ value) = value

-- | Independently bounded command artifact.
data LengthSMTLibCommandPart
  = LengthSMTLibCheckCommand
  | LengthSMTLibInputValueRequest
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibCommandPart

-- | Semantic numeral whose decimal rendering was rejected first.
data LengthSMTLibNumeralSite
  = LengthSMTLibLiteralNumeral
  | LengthSMTLibScaleNumeral
  | LengthSMTLibModuloDivisorNumeral
  | LengthSMTLibQuotientDivisorNumeral
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibNumeralSite

-- | Fixed-precedence failure while constructing one exact query.
data LengthSMTLibQueryError
  = LengthSMTLibUnexpectedResultVariable
  | LengthSMTLibInputVariableOutOfRange !Natural !Int
  | LengthSMTLibQuotientDivisorZero
  | LengthSMTLibModuloDivisorZero
  | LengthSMTLibNumeralBitLimitExceeded
      !LengthSMTLibNumeralSite !Int !Int
  | LengthSMTLibCommandByteLimitExceeded
      !LengthSMTLibCommandPart !Natural !Natural
  | LengthSMTLibFingerprintByteLimitExceeded !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibQueryError

data SMTIntegerExpression
  = SMTIntegerSymbol [Word8]
  | SMTNaturalNumeral !Natural
  | SMTIntegerSum [SMTIntegerExpression]
  | SMTIntegerScale !Natural SMTIntegerExpression
  | SMTIntegerDifference SMTIntegerExpression SMTIntegerExpression
  | SMTIntegerHelper
      !SMTBinaryHelper SMTIntegerExpression SMTIntegerExpression
  | SMTIntegerIf
      SMTBooleanExpression SMTIntegerExpression SMTIntegerExpression
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTIntegerExpression

data SMTBooleanExpression
  = SMTBooleanTruth !Bool
  | SMTIntegerEqual SMTIntegerExpression SMTIntegerExpression
  | SMTIntegerAtMost SMTIntegerExpression SMTIntegerExpression
  | SMTBooleanNot SMTBooleanExpression
  | SMTBooleanAll [SMTBooleanExpression]
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTBooleanExpression

-- | Which Euclidean witness component is the value of one normalized source
-- expression.  The distinction also selects operation-specific private names
-- and the conditional lowering-policy tag.
data SMTEuclideanProjection
  = SMTNaturalQuotientProjection
  | SMTNaturalModuloProjection
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTEuclideanProjection

-- | One private existential lowering receipt shared by positive-literal
-- natural quotient and modulo.  Both projections use the unique Euclidean
-- pair @e = k*q + r@ with @0 <= r < k@.  Names are allocated in deterministic
-- expression preorder, and only original input symbols are requested back.
data SMTEuclideanWitness = SMTEuclideanWitness
  !SMTEuclideanProjection
  !Natural
  ![Word8]
  ![Word8]
  SMTIntegerExpression
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTEuclideanWitness

data SMTTranslationState = SMTTranslationState
  !Natural
  !(Map Natural SMTEuclideanWitness)

data SMTBinaryHelper = SMTNaturalMonus | SMTIntegerMinimum | SMTIntegerMaximum
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData SMTBinaryHelper

data SMTCommand
  = SMTSetLogic [Word8]
  | SMTSetOption [Word8] [Word8]
  | SMTDefineBinaryInteger
      [Word8] [Word8] [Word8] SMTIntegerExpression
  | SMTDeclareInteger [Word8]
  | SMTAssert SMTBooleanExpression
  | SMTCheckSatisfiable
  | SMTGetValues [SMTIntegerExpression]
  deriving (Eq, Ord, Show, Generic)

instance NFData SMTCommand

-- The complete typed plan remains local through bounded rendering and
-- structural fingerprinting.  Rendering therefore never becomes the semantic
-- source of truth, while the sealed query can retain only the runtime/replay
-- material which has a post-seal consumer.
data LengthSMTLibPlan = LengthSMTLibPlan
  ![[Word8]]
  !SMTBooleanExpression
  ![SMTCommand]
  !(Maybe SMTCommand)
  ![SMTEuclideanWitness]

-- | Opaque association of one checked problem, bounded check commands, and
-- collision-free structural translation identity.  Exact input symbols and
-- the canonical value request are derived from the problem's sealed arity.
data LengthSMTLibQuery identity local = LengthSMTLibQuery
  !(CheckedLengthProblem identity local)
  ![Word8]
  !(Fingerprint LengthSMTLibQueryFingerprintSubject)

type role LengthSMTLibQuery nominal nominal

instance NFData (LengthSMTLibQuery identity local) where
  rnf (LengthSMTLibQuery problem check fingerprint) =
    rnf problem `seq` rnf check `seq` rnf fingerprint

-- | Translate and seal one exact checked problem.  The combined bad-state
-- formula is never accepted separately from its problem authority.
sealLengthSMTLibQuery
  :: LengthSMTLibLimits
  -> CheckedLengthProblem identity local
  -> Either LengthSMTLibQueryError (LengthSMTLibQuery identity local)
sealLengthSMTLibQuery limits problem = do
  let inputCount = checkedLengthProblemInputCount problem
      symbols = inputSymbolsForCount inputCount
      valueRequest = inputValueRequestForSymbols symbols
  (condition, translation) <- translateFormula limits inputCount
    emptySMTTranslationState
    $ checkedLengthProblemCounterexampleCondition problem
  let witnesses = orderedEuclideanWitnesses translation
      witnessSymbols = concatMap euclideanWitnessSymbols witnesses
  let checkCommands = fixedPreamble
        ++ map SMTDeclareInteger symbols
        ++ map SMTDeclareInteger witnessSymbols
        ++ map (SMTAssert . nonnegative . SMTIntegerSymbol) symbols
        ++ concatMap euclideanWitnessCommands witnesses
        ++ [SMTAssert condition, SMTCheckSatisfiable]
      plan = LengthSMTLibPlan
        symbols condition checkCommands valueRequest witnesses
  checkBytes <- retainCommand limits LengthSMTLibCheckCommand
    $ renderCommands checkCommands
  valueRequestBytes <- case valueRequest of
    Nothing -> Right Nothing
    Just command -> Just <$> retainCommand limits
      LengthSMTLibInputValueRequest (renderCommand command)
  fingerprint <- buildQueryFingerprint limits problem plan
    checkBytes valueRequestBytes
  pure $ LengthSMTLibQuery problem checkBytes fingerprint

lengthSMTLibQueryInputSymbols
  :: LengthSMTLibQuery identity local
  -> [[Word8]]
lengthSMTLibQueryInputSymbols = fst . lengthSMTLibQueryInputArtifacts

lengthSMTLibQueryCheckBytes
  :: LengthSMTLibQuery identity local
  -> [Word8]
lengthSMTLibQueryCheckBytes (LengthSMTLibQuery _ bytes _) = bytes

lengthSMTLibQueryInputValueRequestBytes
  :: LengthSMTLibQuery identity local
  -> Maybe [Word8]
lengthSMTLibQueryInputValueRequestBytes =
  snd . lengthSMTLibQueryInputArtifacts

lengthSMTLibQueryInputArtifacts
  :: LengthSMTLibQuery identity local
  -> ([[Word8]], Maybe [Word8])
lengthSMTLibQueryInputArtifacts (LengthSMTLibQuery problem _ _) =
  let symbols = inputSymbolsForCount
        $ checkedLengthProblemInputCount problem
  in (symbols, fmap renderCommand $ inputValueRequestForSymbols symbols)

lengthSMTLibQueryFingerprint
  :: LengthSMTLibQuery identity local
  -> Fingerprint LengthSMTLibQueryFingerprintSubject
lengthSMTLibQueryFingerprint (LengthSMTLibQuery _ _ fingerprint) =
  fingerprint

lengthSMTLibQueryBehavioralProblem
  :: LengthSMTLibQuery identity local
  -> BehavioralProblem FiniteListSpineLengthV1
lengthSMTLibQueryBehavioralProblem (LengthSMTLibQuery problem _ _) =
  checkedLengthProblemBehavioralProblem problem

-- | One parser-decoded integer associated with its exact returned symbol.
-- Construction is intentionally public: this value is untrusted input and
-- cannot produce evidence without exact symbol checks and semantic replay.
data LengthSMTLibIntegerBinding = LengthSMTLibIntegerBinding
  { lengthSMTLibIntegerBindingSymbol :: [Word8]
  , lengthSMTLibIntegerBindingValue :: Integer
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibIntegerBinding

-- | Structural model rejection or independent replay failure.  Symbols
-- retained in errors have already passed the generated-symbol byte bound.
data LengthSMTLibModelError
  = LengthSMTLibBindingArityMismatch !Int !Int
  | LengthSMTLibBindingSymbolByteLimitExceeded
      !Int !Natural !Natural
  | LengthSMTLibUnknownInputSymbol !Int [Word8]
  | LengthSMTLibDuplicateInputSymbol !Int [Word8]
  | LengthSMTLibNegativeInputValue !Int [Word8] !Integer
  | LengthSMTLibMissingInputSymbol [Word8]
  | LengthSMTLibCounterexampleReplayRejected !LengthEvaluationError
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibModelError

-- | Decode exactly the tracked input symbols, restore source order, and ask
-- the existing domain evaluator to recompute the candidate result.  @Nothing@
-- means that the supplied assignment did not satisfy the sealed bad-state
-- semantics; it is not evidence and normally indicates a stale or spurious
-- solver model.
validateLengthSMTLibCounterexample
  :: LengthEvaluationLimits
  -> LengthSMTLibQuery identity local
  -> [LengthSMTLibIntegerBinding]
  -> Either LengthSMTLibModelError
      (Maybe
        (BehavioralEvidence
          FiniteListSpineLengthV1
          ValidatedLengthCounterexample))
validateLengthSMTLibCounterexample evaluationLimits query rawBindings = do
  let symbols = lengthSMTLibQueryInputSymbols query
      expected = length symbols
      observed = observedListLength expected rawBindings
  if observed == expected
    then pure ()
    else Left $ LengthSMTLibBindingArityMismatch expected observed
  let maximumSymbolBytes = fromIntegral $ maximum
        $ 0 : map length symbols
      expectedSymbols = Map.fromList $ zip symbols [0 :: Int ..]
  decoded <- foldM
    (decodeBinding maximumSymbolBytes expectedSymbols)
    Map.empty
    $ zip [0 :: Int ..] rawBindings
  ordered <- mapM (lookupDecoded decoded) symbols
  either (Left . LengthSMTLibCounterexampleReplayRejected) Right
    $ validateLengthProblemCounterexample evaluationLimits
        (queryProblem query)
        $ LengthProblemAssignment ordered

queryProblem
  :: LengthSMTLibQuery identity local
  -> CheckedLengthProblem identity local
queryProblem (LengthSMTLibQuery problem _ _) = problem

decodeBinding
  :: Natural
  -> Map [Word8] Int
  -> Map [Word8] Natural
  -> (Int, LengthSMTLibIntegerBinding)
  -> Either LengthSMTLibModelError (Map [Word8] Natural)
decodeBinding maximumBytes expected decoded
    (bindingIndex, LengthSMTLibIntegerBinding rawSymbol rawValue) = do
  symbol <- retainModelSymbol bindingIndex maximumBytes rawSymbol
  case Map.lookup symbol expected of
    Nothing -> Left $ LengthSMTLibUnknownInputSymbol bindingIndex symbol
    Just _ -> pure ()
  if Map.member symbol decoded
    then Left $ LengthSMTLibDuplicateInputSymbol bindingIndex symbol
    else pure ()
  if rawValue < 0
    then Left $ LengthSMTLibNegativeInputValue bindingIndex symbol rawValue
    else pure $ Map.insert symbol (fromInteger rawValue) decoded

lookupDecoded
  :: Map [Word8] Natural
  -> [Word8]
  -> Either LengthSMTLibModelError Natural
lookupDecoded decoded symbol = case Map.lookup symbol decoded of
  Nothing -> Left $ LengthSMTLibMissingInputSymbol symbol
  Just value -> Right value

retainModelSymbol
  :: Int
  -> Natural
  -> [Word8]
  -> Either LengthSMTLibModelError [Word8]
retainModelSymbol bindingIndex maximumBytes = go maximumBytes
 where
  go _ [] = Right []
  go 0 (_ : _) = Left $ LengthSMTLibBindingSymbolByteLimitExceeded
    bindingIndex maximumBytes (maximumBytes + 1)
  go remaining (byte : bytes) =
    (byte :) <$> go (remaining - 1) bytes

fixedPreamble :: [SMTCommand]
fixedPreamble =
  [ SMTSetOption (ascii ":produce-models") (ascii "true")
  , SMTSetOption (ascii ":random-seed") (ascii "1")
  , SMTSetLogic lengthSMTLibQueryLogic
  , helperDefinition SMTNaturalMonus
  , helperDefinition SMTIntegerMinimum
  , helperDefinition SMTIntegerMaximum
  ]

helperDefinition :: SMTBinaryHelper -> SMTCommand
helperDefinition helper = SMTDefineBinaryInteger
  (helperSymbol helper) x y body
 where
  x = ascii "x"
  y = ascii "y"
  left = SMTIntegerSymbol x
  right = SMTIntegerSymbol y
  body = case helper of
    SMTNaturalMonus -> SMTIntegerIf
      (SMTIntegerAtMost right left)
      (SMTIntegerDifference left right)
      (SMTNaturalNumeral 0)
    SMTIntegerMinimum -> SMTIntegerIf
      (SMTIntegerAtMost left right) left right
    SMTIntegerMaximum -> SMTIntegerIf
      (SMTIntegerAtMost left right) right left

nonnegative :: SMTIntegerExpression -> SMTBooleanExpression
nonnegative expression = SMTIntegerAtMost (SMTNaturalNumeral 0) expression

translateExpression
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> LengthExpression LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (SMTIntegerExpression, SMTTranslationState)
translateExpression limits inputCount state source = case source of
  LengthVariable variable -> case variable of
    LengthResult -> Left LengthSMTLibUnexpectedResultVariable
    LengthInput position
      | position < fromIntegral inputCount ->
          Right (SMTIntegerSymbol $ inputSymbolNatural position, state)
      | otherwise -> Left $ LengthSMTLibInputVariableOutOfRange
          position inputCount
  LengthLiteral value -> do
    checkNumeral limits LengthSMTLibLiteralNumeral value
    pure (SMTNaturalNumeral value, state)
  LengthSum terms -> do
    (translated, afterTerms) <- translateExpressions state terms
    pure (SMTIntegerSum translated, afterTerms)
  LengthScale factor expression -> do
    checkNumeral limits LengthSMTLibScaleNumeral factor
    (translated, afterExpression) <- translateExpression
      limits inputCount state expression
    pure (SMTIntegerScale factor translated, afterExpression)
  LengthQuotient divisor expression -> translateEuclideanProjection
    limits inputCount state
    SMTNaturalQuotientProjection
    LengthSMTLibQuotientDivisorNumeral
    LengthSMTLibQuotientDivisorZero
    divisor expression
  LengthModulo divisor expression -> translateEuclideanProjection
    limits inputCount state
    SMTNaturalModuloProjection
    LengthSMTLibModuloDivisorNumeral
    LengthSMTLibModuloDivisorZero
    divisor expression
  LengthMonus left right -> binary SMTNaturalMonus state left right
  LengthMinimum left right -> binary SMTIntegerMinimum state left right
  LengthMaximum left right -> binary SMTIntegerMaximum state left right
  LengthIf condition whenTrue whenFalse -> do
    (translatedCondition, afterCondition) <- translateFormula
      limits inputCount state condition
    (translatedTrue, afterTrue) <- translateExpression
      limits inputCount afterCondition whenTrue
    (translatedFalse, afterFalse) <- translateExpression
      limits inputCount afterTrue whenFalse
    pure
      ( SMTIntegerIf translatedCondition translatedTrue translatedFalse
      , afterFalse
      )
 where
  translateExpressions current terms = case terms of
    [] -> Right ([], current)
    term : remaining -> do
      (translated, afterTerm) <- translateExpression
        limits inputCount current term
      (following, afterFollowing) <- translateExpressions afterTerm remaining
      pure (translated : following, afterFollowing)

  binary helper current left right = do
    (translatedLeft, afterLeft) <- translateExpression
      limits inputCount current left
    (translatedRight, afterRight) <- translateExpression
      limits inputCount afterLeft right
    pure
      (SMTIntegerHelper helper translatedLeft translatedRight, afterRight)

translateFormula
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> LengthFormula LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (SMTBooleanExpression, SMTTranslationState)
translateFormula limits inputCount state source = case source of
  LengthTruth value -> Right (SMTBooleanTruth value, state)
  LengthEqual left right -> comparison SMTIntegerEqual left right
  LengthAtMost left right -> comparison SMTIntegerAtMost left right
  LengthNot formula -> do
    (translated, afterFormula) <- translateFormula
      limits inputCount state formula
    pure (SMTBooleanNot translated, afterFormula)
  LengthAll formulas -> do
    (translated, afterFormulas) <- translateFormulas state formulas
    pure (SMTBooleanAll translated, afterFormulas)
 where
  comparison constructor left right = do
    (translatedLeft, afterLeft) <- translateExpression
      limits inputCount state left
    (translatedRight, afterRight) <- translateExpression
      limits inputCount afterLeft right
    pure (constructor translatedLeft translatedRight, afterRight)

  translateFormulas current formulas = case formulas of
    [] -> Right ([], current)
    formula : remaining -> do
      (translated, afterFormula) <- translateFormula
        limits inputCount current formula
      (following, afterFollowing) <- translateFormulas afterFormula remaining
      pure (translated : following, afterFollowing)

emptySMTTranslationState :: SMTTranslationState
emptySMTTranslationState = SMTTranslationState 0 Map.empty

translateEuclideanProjection
  :: LengthSMTLibLimits
  -> Int
  -> SMTTranslationState
  -> SMTEuclideanProjection
  -> LengthSMTLibNumeralSite
  -> LengthSMTLibQueryError
  -> Natural
  -> LengthExpression LengthContractVariable
  -> Either
      LengthSMTLibQueryError
      (SMTIntegerExpression, SMTTranslationState)
translateEuclideanProjection limits inputCount state projection numeralSite
    zeroError divisor expression = do
  if divisor == 0
    then Left zeroError
    else pure ()
  checkNumeral limits numeralSite divisor
  let (ordinal, quotient, remainder, reserved) =
        reserveEuclideanWitness projection state
  (translated, afterExpression) <- translateExpression
    limits inputCount reserved expression
  let witness = SMTEuclideanWitness
        projection divisor quotient remainder translated
      completed = retainEuclideanWitness ordinal witness afterExpression
      projected = case projection of
        SMTNaturalQuotientProjection -> quotient
        SMTNaturalModuloProjection -> remainder
  pure (SMTIntegerSymbol projected, completed)

reserveEuclideanWitness
  :: SMTEuclideanProjection
  -> SMTTranslationState
  -> (Natural, [Word8], [Word8], SMTTranslationState)
reserveEuclideanWitness projection
    (SMTTranslationState ordinal witnesses) =
  ( ordinal
  , euclideanQuotientSymbol projection ordinal
  , euclideanRemainderSymbol projection ordinal
  , SMTTranslationState (ordinal + 1) witnesses
  )

retainEuclideanWitness
  :: Natural
  -> SMTEuclideanWitness
  -> SMTTranslationState
  -> SMTTranslationState
retainEuclideanWitness ordinal witness (SMTTranslationState next witnesses) =
  SMTTranslationState next $ Map.insert ordinal witness witnesses

orderedEuclideanWitnesses :: SMTTranslationState -> [SMTEuclideanWitness]
orderedEuclideanWitnesses (SMTTranslationState _ witnesses) =
  Map.elems witnesses

euclideanWitnessSymbols :: SMTEuclideanWitness -> [[Word8]]
euclideanWitnessSymbols
    (SMTEuclideanWitness _ _ quotient remainder _) =
  [quotient, remainder]

euclideanWitnessCommands :: SMTEuclideanWitness -> [SMTCommand]
euclideanWitnessCommands
    (SMTEuclideanWitness _ divisor quotientSymbol remainderSymbol expression) =
  [ SMTAssert $ nonnegative quotient
  , SMTAssert $ nonnegative remainder
  , SMTAssert $ SMTIntegerAtMost remainder
      $ SMTNaturalNumeral $ divisor - 1
  , SMTAssert $ SMTIntegerEqual expression $ SMTIntegerSum
      [SMTIntegerScale divisor quotient, remainder]
  ]
 where
  quotient = SMTIntegerSymbol quotientSymbol
  remainder = SMTIntegerSymbol remainderSymbol

checkNumeral
  :: LengthSMTLibLimits
  -> LengthSMTLibNumeralSite
  -> Natural
  -> Either LengthSMTLibQueryError ()
checkNumeral limits site value =
  let maximumBits = lengthSMTLibNumeralBitLimit limits
      observedBits = observedNaturalBits maximumBits value
  in if observedBits <= maximumBits
      then Right ()
      else Left $ LengthSMTLibNumeralBitLimitExceeded
        site maximumBits observedBits

retainCommand
  :: LengthSMTLibLimits
  -> LengthSMTLibCommandPart
  -> [Word8]
  -> Either LengthSMTLibQueryError [Word8]
retainCommand limits part = go maximumBytes
 where
  maximumBytes = lengthSMTLibCommandByteLimit limits

  go _ [] = Right []
  go 0 (_ : _) = Left $ LengthSMTLibCommandByteLimitExceeded
    part maximumBytes (maximumBytes + 1)
  go remaining (byte : bytes) =
    (byte :) <$> go (remaining - 1) bytes

buildQueryFingerprint
  :: LengthSMTLibLimits
  -> CheckedLengthProblem identity local
  -> LengthSMTLibPlan
  -> [Word8]
  -> Maybe [Word8]
  -> Either LengthSMTLibQueryError
      (Fingerprint LengthSMTLibQueryFingerprintSubject)
buildQueryFingerprint limits problem plan checkBytes valueRequestBytes =
  case buildFingerprintWithin
      (lengthSMTLibFingerprintByteLimit limits) builder of
    Left FingerprintLimitExceeded
        { fingerprintMaximumBytes = maximumBytes
        , fingerprintObservedBytesAtLeast = observedBytes
        } -> Left $ LengthSMTLibFingerprintByteLimitExceeded
          maximumBytes observedBytes
    Right fingerprint -> Right fingerprint
 where
  behavioral = checkedLengthProblemBehavioralProblem problem
  builder = FingerprintBuilder
    { fingerprintBuilderVersion = 1
    , fingerprintBuilderRole = ascii
        "finite-list-spine-length/z3-qf-lia-query"
    , fingerprintBuilderFields =
        [ tagged "schema" [FingerprintBytes lengthSMTLibQuerySchemaTag]
        , tagged "logic" [FingerprintBytes lengthSMTLibQueryLogic]
        , tagged "fixed-options"
            [ FingerprintBytes $ ascii ":produce-models=true"
            , FingerprintBytes $ ascii ":random-seed=1"
            ]
        , tagged "problem-domain"
            [FingerprintBytes $ behavioralProblemDomain behavioral]
        , tagged "problem-inventory"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemInventoryFingerprint behavioral
            ]
        , tagged "problem-encoding"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemEncodingFingerprint behavioral
            ]
        , tagged "problem-candidate"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemCandidateFingerprint behavioral
            ]
        , tagged "complete-problem"
            [ FingerprintBytes $ fingerprintCanonicalBytes
                $ behavioralProblemFingerprint behavioral
            ]
        , tagged "typed-plan" [planField plan]
        , tagged "check-command" [FingerprintBytes checkBytes]
        , tagged "input-value-request" [case valueRequestBytes of
            Nothing -> tagged "absent" []
            Just bytes -> tagged "present" [FingerprintBytes bytes]]
        ]
    }

planField :: LengthSMTLibPlan -> FingerprintField
planField (LengthSMTLibPlan symbols condition commands request witnesses) =
  tagged "plan" $
    [ tagged "input-symbols" $ map FingerprintBytes symbols
    , tagged "condition" [booleanField condition]
    , tagged "check-commands" $ map commandField commands
    , tagged "value-request" [case request of
        Nothing -> tagged "absent" []
        Just command -> tagged "present" [commandField command]]
    ] ++ expressionLoweringFields witnesses

expressionLoweringFields :: [SMTEuclideanWitness] -> [FingerprintField]
expressionLoweringFields witnesses = case loweringTags of
  [] -> []
  _ -> [tagged "expression-lowering" $ map FingerprintBytes loweringTags]
 where
  projections = map euclideanWitnessProjection witnesses
  loweringTags =
    [ tag
    | (projection, tag) <-
        [ ( SMTNaturalModuloProjection
          , positiveLiteralNaturalModuloWitnessSchemaTag
          )
        , ( SMTNaturalQuotientProjection
          , positiveLiteralNaturalQuotientWitnessSchemaTag
          )
        ]
    , projection `elem` projections
    ]

euclideanWitnessProjection :: SMTEuclideanWitness -> SMTEuclideanProjection
euclideanWitnessProjection (SMTEuclideanWitness projection _ _ _ _) =
  projection

commandField :: SMTCommand -> FingerprintField
commandField command = case command of
  SMTSetLogic logic -> tagged "set-logic" [FingerprintBytes logic]
  SMTSetOption name value -> tagged "set-option"
    [FingerprintBytes name, FingerprintBytes value]
  SMTDefineBinaryInteger name left right body -> tagged "define-int2"
    [ FingerprintBytes name
    , FingerprintBytes left
    , FingerprintBytes right
    , integerField body
    ]
  SMTDeclareInteger symbol -> tagged "declare-int" [FingerprintBytes symbol]
  SMTAssert formula -> tagged "assert" [booleanField formula]
  SMTCheckSatisfiable -> tagged "check-sat" []
  SMTGetValues terms -> tagged "get-value" $ map integerField terms

integerField :: SMTIntegerExpression -> FingerprintField
integerField expression = case expression of
  SMTIntegerSymbol symbol -> tagged "symbol" [FingerprintBytes symbol]
  SMTNaturalNumeral value -> tagged "natural" [FingerprintNatural value]
  SMTIntegerSum terms -> tagged "sum" $ map integerField terms
  SMTIntegerScale factor term -> tagged "scale"
    [FingerprintNatural factor, integerField term]
  SMTIntegerDifference left right -> tagged "difference"
    [integerField left, integerField right]
  SMTIntegerHelper helper left right -> tagged "helper"
    [ helperField helper
    , integerField left
    , integerField right
    ]
  SMTIntegerIf condition whenTrue whenFalse -> tagged "ite"
    [ booleanField condition
    , integerField whenTrue
    , integerField whenFalse
    ]

booleanField :: SMTBooleanExpression -> FingerprintField
booleanField formula = case formula of
  SMTBooleanTruth value -> tagged (if value then "true" else "false") []
  SMTIntegerEqual left right -> tagged "equal"
    [integerField left, integerField right]
  SMTIntegerAtMost left right -> tagged "at-most"
    [integerField left, integerField right]
  SMTBooleanNot nested -> tagged "not" [booleanField nested]
  SMTBooleanAll nested -> tagged "all" $ map booleanField nested

helperField :: SMTBinaryHelper -> FingerprintField
helperField helper = FingerprintBytes $ helperSymbol helper

tagged :: String -> [FingerprintField] -> FingerprintField
tagged name = FingerprintTag (ascii name)

renderCommands :: [SMTCommand] -> [Word8]
renderCommands = concatMap renderCommand

renderCommand :: SMTCommand -> [Word8]
renderCommand command = case command of
  SMTSetLogic logic -> line $ parenthesized
    [ascii "set-logic", logic]
  SMTSetOption name value -> line $ parenthesized
    [ascii "set-option", name, value]
  SMTDefineBinaryInteger name left right body -> line $ parenthesized
    [ ascii "define-fun"
    , name
    , parenthesized
        [ parenthesized [left, ascii "Int"]
        , parenthesized [right, ascii "Int"]
        ]
    , ascii "Int"
    , renderInteger body
    ]
  SMTDeclareInteger symbol -> line $ parenthesized
    [ascii "declare-const", symbol, ascii "Int"]
  SMTAssert formula -> line $ parenthesized
    [ascii "assert", renderBoolean formula]
  SMTCheckSatisfiable -> line $ parenthesized [ascii "check-sat"]
  SMTGetValues terms -> line $ parenthesized
    [ascii "get-value", parenthesized $ map renderInteger terms]

renderInteger :: SMTIntegerExpression -> [Word8]
renderInteger expression = case expression of
  SMTIntegerSymbol symbol -> symbol
  SMTNaturalNumeral value -> ascii $ show value
  SMTIntegerSum [] -> ascii "0"
  SMTIntegerSum [term] -> renderInteger term
  SMTIntegerSum terms -> parenthesized $ ascii "+" : map renderInteger terms
  SMTIntegerScale factor term -> parenthesized
    [ascii "*", ascii $ show factor, renderInteger term]
  SMTIntegerDifference left right -> parenthesized
    [ascii "-", renderInteger left, renderInteger right]
  SMTIntegerHelper helper left right -> parenthesized
    [helperSymbol helper, renderInteger left, renderInteger right]
  SMTIntegerIf condition whenTrue whenFalse -> parenthesized
    [ ascii "ite"
    , renderBoolean condition
    , renderInteger whenTrue
    , renderInteger whenFalse
    ]

renderBoolean :: SMTBooleanExpression -> [Word8]
renderBoolean formula = case formula of
  SMTBooleanTruth True -> ascii "true"
  SMTBooleanTruth False -> ascii "false"
  SMTIntegerEqual left right -> parenthesized
    [ascii "=", renderInteger left, renderInteger right]
  SMTIntegerAtMost left right -> parenthesized
    [ascii "<=", renderInteger left, renderInteger right]
  SMTBooleanNot nested -> parenthesized
    [ascii "not", renderBoolean nested]
  SMTBooleanAll [] -> ascii "true"
  SMTBooleanAll [nested] -> renderBoolean nested
  SMTBooleanAll nested -> parenthesized
    $ ascii "and" : map renderBoolean nested

helperSymbol :: SMTBinaryHelper -> [Word8]
helperSymbol helper = ascii $ case helper of
  SMTNaturalMonus -> "djex_nat_monus"
  SMTIntegerMinimum -> "djex_nat_min"
  SMTIntegerMaximum -> "djex_nat_max"

-- | Canonical decoder symbols derived from the exact sealed contract arity.
-- Query sealing uses this list before rendering and fingerprinting; public
-- projections later recompute the same bounded values without retaining a
-- parallel cache inside the opaque query.
inputSymbolsForCount :: Int -> [[Word8]]
inputSymbolsForCount inputCount = map inputSymbol [0 .. inputCount - 1]

inputValueRequestForSymbols :: [[Word8]] -> Maybe SMTCommand
inputValueRequestForSymbols symbols = case symbols of
  [] -> Nothing
  _ -> Just $ SMTGetValues $ map SMTIntegerSymbol symbols

inputSymbol :: Int -> [Word8]
inputSymbol = inputSymbolNatural . fromIntegral

inputSymbolNatural :: Natural -> [Word8]
inputSymbolNatural position = ascii "djex_length_input_" ++ ascii (show position)

moduloQuotientSymbol :: Natural -> [Word8]
moduloQuotientSymbol ordinal =
  ascii "djex_length_modulo_quotient_" ++ ascii (show ordinal)

moduloRemainderSymbol :: Natural -> [Word8]
moduloRemainderSymbol ordinal =
  ascii "djex_length_modulo_remainder_" ++ ascii (show ordinal)

quotientQuotientSymbol :: Natural -> [Word8]
quotientQuotientSymbol ordinal =
  ascii "djex_length_quotient_quotient_" ++ ascii (show ordinal)

quotientRemainderSymbol :: Natural -> [Word8]
quotientRemainderSymbol ordinal =
  ascii "djex_length_quotient_remainder_" ++ ascii (show ordinal)

euclideanQuotientSymbol :: SMTEuclideanProjection -> Natural -> [Word8]
euclideanQuotientSymbol projection = case projection of
  SMTNaturalQuotientProjection -> quotientQuotientSymbol
  SMTNaturalModuloProjection -> moduloQuotientSymbol

euclideanRemainderSymbol :: SMTEuclideanProjection -> Natural -> [Word8]
euclideanRemainderSymbol projection = case projection of
  SMTNaturalQuotientProjection -> quotientRemainderSymbol
  SMTNaturalModuloProjection -> moduloRemainderSymbol

positiveLiteralNaturalModuloWitnessSchemaTag :: [Word8]
positiveLiteralNaturalModuloWitnessSchemaTag = ascii
  "djex-length-z3-qf-lia-positive-literal-modulo-witness/v1"

positiveLiteralNaturalQuotientWitnessSchemaTag :: [Word8]
positiveLiteralNaturalQuotientWitnessSchemaTag = ascii
  "djex-length-z3-qf-lia-positive-literal-natural-quotient-witness/v1"

parenthesized :: [[Word8]] -> [Word8]
parenthesized fields = [openParen] ++ separated fields ++ [closeParen]
 where
  separated [] = []
  separated [field] = field
  separated (field : remaining) = field ++ [space] ++ separated remaining

line :: [Word8] -> [Word8]
line bytes = bytes ++ [newline]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . fromEnum

openParen, closeParen, space, newline :: Word8
openParen = 40
closeParen = 41
space = 32
newline = 10

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

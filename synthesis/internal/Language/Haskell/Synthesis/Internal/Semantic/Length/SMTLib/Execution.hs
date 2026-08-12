{-# LANGUAGE DeriveGeneric #-}

-- | Length-specific sealing around the shared pure Z3 execution profile.
--
-- This module does not resolve, inspect, hash, or launch an executable.  In
-- particular, caller-supplied SHA-256 bytes are an expectation to be checked
-- by a later live session opener, not an attestation or receipt.  That opener
-- must bind the observed digest even when no expectation is configured and
-- must defend the resolution/hash/spawn path against replacement races.
-- The shared profile owns only reusable launch facts; this module retains the
-- Length protocol, artifact, response, fingerprint, and public-error policy.
-- The complete policy fingerprint is kept package-private because its
-- canonical representation is reversible rather than a digest.
module Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution
  ( lengthSMTLibExecutionPolicySchemaTag
  , lengthSMTLibExecutionProtocolSchemaTag
  , lengthSMTLibExecutionArgumentPrefix
  , lengthSMTLibExecutionArgumentVector
  , lengthSMTLibExecutionConfiguredArgumentVector
  , lengthSMTLibExecutionStartupCommandBytes
  , lengthSMTLibExecutionQueryResetBytes
  , lengthSMTLibExecutionEnvironmentPolicyTag
  , lengthSMTLibExecutionWorkingDirectoryPolicyTag
  , lengthSMTLibExecutionExpectedDigestSchemaTag
  , lengthSMTLibMinimumHostDeadlineMarginMilliseconds
  , LengthSMTLibArtifactPolicy (..)
  , LengthSMTLibExecutionLimitSource (..)
  , LengthSMTLibExecutionLimits
  , mkLengthSMTLibExecutionLimits
  , defaultLengthSMTLibExecutionLimitSource
  , defaultLengthSMTLibExecutionLimits
  , lengthSMTLibExecutionExecutablePathCharacterLimit
  , lengthSMTLibExecutionPolicyFingerprintByteLimit
  , LengthSMTLibExecutionConfigSource (..)
  , defaultLengthSMTLibExecutionConfigSource
  , LengthSMTLibExecutionConfig
  , LengthSMTLibExecutableDigestExpectation (..)
  , lengthSMTLibExecutionExecutableDigestExpectation
  , LengthSMTLibExecutionConfigField (..)
  , LengthSMTLibExecutionPathCharacterError (..)
  , LengthSMTLibExecutionConfigError (..)
  , mkLengthSMTLibExecutionConfig
  , lengthSMTLibExecutionSolverTimeoutMilliseconds
  , lengthSMTLibExecutionSolverResourceLimit
  , lengthSMTLibExecutionHostDeadlineMilliseconds
  , lengthSMTLibExecutionArtifactPolicy
  , lengthSMTLibExecutionResponseLimits
  , LengthSMTLibExecutionPolicyFingerprintSubject
  , lengthSMTLibExecutionPolicyFingerprint
  , lengthSMTLibExecutionZ3Profile
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint
  , FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
import qualified Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  as Z3
import Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  ( LengthSMTLibResponseLimits
  , defaultLengthSMTLibResponseLimits
  , lengthSMTLibResponseByteLimit
  , lengthSMTLibResponseIntegerBitLimit
  , lengthSMTLibResponseNestingDepthLimit
  , lengthSMTLibResponseNodeLimit
  , lengthSMTLibResponseSchemaTag
  , lengthSMTLibResponseTokenByteLimit
  )

-- | Version of the pure policy only.  Live handshake, framing, session, and
-- run identities will use distinct schema tags.
lengthSMTLibExecutionPolicySchemaTag :: [Word8]
lengthSMTLibExecutionPolicySchemaTag =
  ascii "djex-length-z3-smtlib2-execution-policy/v2"

-- | Exact protocol contract whose command bytes and launch spelling are bound
-- into the pure policy.  A live opener must still probe the corresponding Z3
-- behavior before accepting a worker.
lengthSMTLibExecutionProtocolSchemaTag :: [Word8]
lengthSMTLibExecutionProtocolSchemaTag =
  ascii "djex-length-z3-smtlib2-session-protocol/v1"

-- | Fixed argument prefix passed directly to @process@ without a shell.
-- Compliance mode gives @echo@ its standard quoted response spelling.  It
-- also starts with @:print-success true@, which the startup command disables.
lengthSMTLibExecutionArgumentPrefix :: [String]
lengthSMTLibExecutionArgumentPrefix = Z3.z3SMTLibExecutionArgumentPrefix

-- | Legacy compatibility spelling for the fixed prefix.  This is not a
-- complete argv: process launchers must use
-- 'lengthSMTLibExecutionConfiguredArgumentVector' so timeout and resource
-- arguments cannot be omitted silently.
lengthSMTLibExecutionArgumentVector :: [String]
lengthSMTLibExecutionArgumentVector = lengthSMTLibExecutionArgumentPrefix

-- | Complete ordered argv for one sealed policy.  Resource controls are Z3
-- launch parameters rather than input commands so updating them cannot
-- re-enable compliance-mode success responses mid-session.
lengthSMTLibExecutionConfiguredArgumentVector
  :: LengthSMTLibExecutionConfig
  -> [String]
lengthSMTLibExecutionConfiguredArgumentVector =
  Z3.z3SMTLibExecutionConfiguredArgumentVector
    . lengthSMTLibExecutionZ3Profile

-- | Exact startup command.  In required compliance mode the option affects
-- its own response, so a conforming Z3 worker emits no @success@ for it.
lengthSMTLibExecutionStartupCommandBytes :: [Word8]
lengthSMTLibExecutionStartupCommandBytes =
  Z3.z3SMTLibExecutionStartupCommandBytes

-- | Exact reset prefix replayed before every canonical query.  The required
-- Z3 capability retains disabled success printing across @reset@, so reset
-- emits no frame; suppression is then repeated explicitly for the new query.
-- A future capability handshake must establish this response behavior before
-- the worker can enter the query phase.
lengthSMTLibExecutionQueryResetBytes :: [Word8]
lengthSMTLibExecutionQueryResetBytes = Z3.z3SMTLibExecutionQueryResetBytes

-- | V2 launches with @env = Just []@.  It never inherits ambient variables.
-- A later platform-specific allowlist requires a new versioned policy.
lengthSMTLibExecutionEnvironmentPolicyTag :: [Word8]
lengthSMTLibExecutionEnvironmentPolicyTag =
  Z3.z3SMTLibExecutionEnvironmentPolicyTag

-- | V2 requires a fresh empty working directory owned by the live session.
-- Its concrete path belongs to that session identity and must never be an
-- inherited caller directory.
lengthSMTLibExecutionWorkingDirectoryPolicyTag :: [Word8]
lengthSMTLibExecutionWorkingDirectoryPolicyTag =
  Z3.z3SMTLibExecutionWorkingDirectoryPolicyTag

-- | Meaning of optional expected digest bytes.  The digest is a named
-- external digest/pin format, not a collision-free identity for file bytes.
lengthSMTLibExecutionExpectedDigestSchemaTag :: [Word8]
lengthSMTLibExecutionExpectedDigestSchemaTag =
  Z3.z3SMTLibExecutionExpectedDigestSchemaTag

-- | Minimum host-side response and cleanup time beyond the solver timeout.
lengthSMTLibMinimumHostDeadlineMarginMilliseconds :: Natural
lengthSMTLibMinimumHostDeadlineMarginMilliseconds =
  Z3.z3SMTLibMinimumHostDeadlineMarginMilliseconds

-- | Artifacts the future executor may request after a check response.
-- Neither policy grants semantic authority to the returned status or bytes.
data LengthSMTLibArtifactPolicy
  = LengthSMTLibStatusOnly
  | LengthSMTLibInputValuesAfterSatisfiable
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibArtifactPolicy

-- | Independent admission bounds.  They do not change execution semantics
-- after a policy has been sealed and therefore are not fingerprint fields.
data LengthSMTLibExecutionLimitSource = LengthSMTLibExecutionLimitSource
  { lengthSMTLibExecutionLimitSourceExecutablePathCharacters :: Natural
  , lengthSMTLibExecutionLimitSourcePolicyFingerprintBytes :: Natural
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibExecutionLimitSource

data LengthSMTLibExecutionLimits = LengthSMTLibExecutionLimits
  !Z3.Z3SMTLibExecutionLimits !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibExecutionLimits where
  rnf (LengthSMTLibExecutionLimits z3 fingerprint) =
    rnf z3 `seq` rnf fingerprint

mkLengthSMTLibExecutionLimits
  :: LengthSMTLibExecutionLimitSource
  -> LengthSMTLibExecutionLimits
mkLengthSMTLibExecutionLimits source = LengthSMTLibExecutionLimits
  (Z3.mkZ3SMTLibExecutionLimits
    $ lengthSMTLibExecutionLimitSourceExecutablePathCharacters source)
  (lengthSMTLibExecutionLimitSourcePolicyFingerprintBytes source)

defaultLengthSMTLibExecutionLimitSource
  :: LengthSMTLibExecutionLimitSource
defaultLengthSMTLibExecutionLimitSource = LengthSMTLibExecutionLimitSource
  { lengthSMTLibExecutionLimitSourceExecutablePathCharacters = 4096
  , lengthSMTLibExecutionLimitSourcePolicyFingerprintBytes = 262144
  }

defaultLengthSMTLibExecutionLimits :: LengthSMTLibExecutionLimits
defaultLengthSMTLibExecutionLimits =
  mkLengthSMTLibExecutionLimits defaultLengthSMTLibExecutionLimitSource

lengthSMTLibExecutionExecutablePathCharacterLimit
  :: LengthSMTLibExecutionLimits
  -> Natural
lengthSMTLibExecutionExecutablePathCharacterLimit
    (LengthSMTLibExecutionLimits value _) =
  Z3.z3SMTLibExecutionExecutablePathCharacterLimit value

lengthSMTLibExecutionPolicyFingerprintByteLimit
  :: LengthSMTLibExecutionLimits
  -> Natural
lengthSMTLibExecutionPolicyFingerprintByteLimit
    (LengthSMTLibExecutionLimits _ value) = value

-- | Raw caller policy.  The path is an absolute @process@ 'FilePath'.  Its
-- Unicode code points are fingerprinted exactly, while platform path encoding
-- and resolved file identity remain obligations of the live session opener.
--
-- Timeout and deadline fields are milliseconds.  Z3's timeout and resource
-- limit are required to be finite and nonzero in this deterministic policy.
data LengthSMTLibExecutionConfigSource = LengthSMTLibExecutionConfigSource
  { lengthSMTLibExecutionConfigSourceExecutablePath :: FilePath
  , lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256
      :: Maybe [Word8]
  , lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds :: Int
  , lengthSMTLibExecutionConfigSourceSolverResourceLimit :: Int
  , lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds :: Int
  , lengthSMTLibExecutionConfigSourceArtifactPolicy
      :: LengthSMTLibArtifactPolicy
  , lengthSMTLibExecutionConfigSourceResponseLimits
      :: LengthSMTLibResponseLimits
  }
  deriving (Eq, Ord)

instance NFData LengthSMTLibExecutionConfigSource where
  rnf source =
    rnf (lengthSMTLibExecutionConfigSourceExecutablePath source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceSolverResourceLimit source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceArtifactPolicy source) `seq`
    rnf (lengthSMTLibExecutionConfigSourceResponseLimits source)

-- | Conservative defaults around a caller-chosen absolute executable and
-- optional expected digest.  The later live boundary must still resolve and
-- inspect the executable, compare any expectation, and probe capabilities.
defaultLengthSMTLibExecutionConfigSource
  :: FilePath
  -> Maybe [Word8]
  -> LengthSMTLibExecutionConfigSource
defaultLengthSMTLibExecutionConfigSource executable expectedDigest =
  LengthSMTLibExecutionConfigSource
    { lengthSMTLibExecutionConfigSourceExecutablePath = executable
    , lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 = expectedDigest
    , lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds = 1000
    , lengthSMTLibExecutionConfigSourceSolverResourceLimit = 100000
    , lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds = 1500
    , lengthSMTLibExecutionConfigSourceArtifactPolicy =
        LengthSMTLibInputValuesAfterSatisfiable
    , lengthSMTLibExecutionConfigSourceResponseLimits =
        defaultLengthSMTLibResponseLimits
    }

-- | Opaque validated policy.  There is intentionally no 'Show' or 'Generic'
-- instance and no public projection of its reversible canonical fingerprint.
data LengthSMTLibExecutionConfig = LengthSMTLibExecutionConfig
  !Z3.Z3SMTLibExecutionProfile
  !LengthSMTLibArtifactPolicy
  !LengthSMTLibResponseLimits
  !(Fingerprint LengthSMTLibExecutionPolicyFingerprintSubject)

-- Equality intentionally means exact sealed-policy equivalence and observes
-- only the private complete key.  V2 contains no inherited environment or
-- caller-supplied secret field.  A future schema must revisit this instance
-- before admitting secret launch material.
instance Eq LengthSMTLibExecutionConfig where
  left == right = lengthSMTLibExecutionPolicyFingerprint left ==
    lengthSMTLibExecutionPolicyFingerprint right

instance NFData LengthSMTLibExecutionConfig where
  rnf (LengthSMTLibExecutionConfig
      z3 artifacts responses fingerprint) =
    rnf z3 `seq` rnf artifacts `seq` rnf responses `seq` rnf fingerprint

-- | Whether one sealed policy contains an executable-file digest expectation.
--
-- This classification deliberately reveals neither the expected SHA-256
-- bytes nor whether a later live executable observation matched them.  It is
-- only enough for an outer configuration owner to require that an expectation
-- was explicitly supplied before granting permission to open a live scope.
data LengthSMTLibExecutableDigestExpectation
  = LengthSMTLibExecutableDigestExpectationAbsent
  | LengthSMTLibExecutableDigestExpectationPresent
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibExecutableDigestExpectation

-- | Classify the sealed policy without projecting its path or digest bytes.
-- Only the optional field's outer constructor is inspected.
lengthSMTLibExecutionExecutableDigestExpectation
  :: LengthSMTLibExecutionConfig
  -> LengthSMTLibExecutableDigestExpectation
lengthSMTLibExecutionExecutableDigestExpectation config =
  case Z3.z3SMTLibExecutionExpectedExecutableSHA256
      $ lengthSMTLibExecutionZ3Profile config of
    Nothing -> LengthSMTLibExecutableDigestExpectationAbsent
    Just _ -> LengthSMTLibExecutableDigestExpectationPresent

data LengthSMTLibExecutionConfigField
  = LengthSMTLibExecutionSolverTimeoutMilliseconds
  | LengthSMTLibExecutionSolverResourceLimit
  | LengthSMTLibExecutionHostDeadlineMilliseconds
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibExecutionConfigField

data LengthSMTLibExecutionPathCharacterError
  = LengthSMTLibExecutionPathContainsNul
  | LengthSMTLibExecutionPathContainsSurrogate
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData LengthSMTLibExecutionPathCharacterError

-- | Fixed-precedence pure policy rejection.  Errors never retain the path or
-- digest bytes.  Observed collection sizes stop at maximum plus one.
data LengthSMTLibExecutionConfigError
  = NegativeLengthSMTLibExecutionConfigField
      !LengthSMTLibExecutionConfigField !Int
  | ZeroLengthSMTLibExecutionConfigField
      !LengthSMTLibExecutionConfigField
  | LengthSMTLibExecutionConfigFieldAboveMaximum
      !LengthSMTLibExecutionConfigField !Integer !Integer
  | LengthSMTLibExecutionHostDeadlineMarginTooSmall
      !Int !Int !Natural
  | LengthSMTLibExecutionHostDeadlineMicrosecondsOverflow !Int
  | LengthSMTLibExecutionExecutablePathCharacterLimitExceeded
      !Natural !Natural
  | LengthSMTLibExecutionInvalidExecutablePathCharacter
      !Natural !LengthSMTLibExecutionPathCharacterError
  | LengthSMTLibExecutionEmptyExecutablePath
  | LengthSMTLibExecutionExecutablePathNotAbsolute
  | LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
      !Natural !Natural
  | LengthSMTLibExecutionPolicyFingerprintByteLimitExceeded
      !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData LengthSMTLibExecutionConfigError

-- | Seal one pure launch and response policy.  Successful construction says
-- nothing about the filesystem or a running process.
mkLengthSMTLibExecutionConfig
  :: LengthSMTLibExecutionLimits
  -> LengthSMTLibExecutionConfigSource
  -> Either LengthSMTLibExecutionConfigError LengthSMTLibExecutionConfig
mkLengthSMTLibExecutionConfig limits source = do
  z3 <- case Z3.mkZ3SMTLibExecutionProfile z3Limits Z3.Z3SMTLibExecutionSource
      { Z3.z3SMTLibExecutionSourceExecutablePath =
          lengthSMTLibExecutionConfigSourceExecutablePath source
      , Z3.z3SMTLibExecutionSourceExpectedExecutableSHA256 =
          lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 source
      , Z3.z3SMTLibExecutionSourceSolverTimeoutMilliseconds =
          lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds source
      , Z3.z3SMTLibExecutionSourceSolverResourceLimit =
          lengthSMTLibExecutionConfigSourceSolverResourceLimit source
      , Z3.z3SMTLibExecutionSourceHostDeadlineMilliseconds =
          lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds source
      } of
    Left failure -> Left $ mapZ3ExecutionError failure
    Right profile -> Right profile
  let artifacts = lengthSMTLibExecutionConfigSourceArtifactPolicy source
      responses = lengthSMTLibExecutionConfigSourceResponseLimits source
  fingerprint <- buildPolicyFingerprint limits z3 artifacts responses
  pure $ LengthSMTLibExecutionConfig z3 artifacts responses fingerprint
 where
  LengthSMTLibExecutionLimits z3Limits _ = limits

lengthSMTLibExecutionSolverTimeoutMilliseconds
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionSolverTimeoutMilliseconds =
  Z3.z3SMTLibExecutionSolverTimeoutMilliseconds
    . lengthSMTLibExecutionZ3Profile

lengthSMTLibExecutionSolverResourceLimit
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionSolverResourceLimit =
  Z3.z3SMTLibExecutionSolverResourceLimit
    . lengthSMTLibExecutionZ3Profile

lengthSMTLibExecutionHostDeadlineMilliseconds
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionHostDeadlineMilliseconds =
  Z3.z3SMTLibExecutionHostDeadlineMilliseconds
    . lengthSMTLibExecutionZ3Profile

lengthSMTLibExecutionArtifactPolicy
  :: LengthSMTLibExecutionConfig
  -> LengthSMTLibArtifactPolicy
lengthSMTLibExecutionArtifactPolicy
    (LengthSMTLibExecutionConfig _ value _ _) = value

lengthSMTLibExecutionResponseLimits
  :: LengthSMTLibExecutionConfig
  -> LengthSMTLibResponseLimits
lengthSMTLibExecutionResponseLimits
    (LengthSMTLibExecutionConfig _ _ value _) = value

data LengthSMTLibExecutionPolicyFingerprintSubject

lengthSMTLibExecutionPolicyFingerprint
  :: LengthSMTLibExecutionConfig
  -> Fingerprint LengthSMTLibExecutionPolicyFingerprintSubject
lengthSMTLibExecutionPolicyFingerprint
    (LengthSMTLibExecutionConfig _ _ _ value) = value

lengthSMTLibExecutionZ3Profile
  :: LengthSMTLibExecutionConfig
  -> Z3.Z3SMTLibExecutionProfile
lengthSMTLibExecutionZ3Profile
    (LengthSMTLibExecutionConfig value _ _ _) = value

mapZ3ExecutionError
  :: Z3.Z3SMTLibExecutionError
  -> LengthSMTLibExecutionConfigError
mapZ3ExecutionError failure = case failure of
  Z3.NegativeZ3SMTLibExecutionField field value ->
    NegativeLengthSMTLibExecutionConfigField
      (mapZ3ExecutionField field) value
  Z3.ZeroZ3SMTLibExecutionField field ->
    ZeroLengthSMTLibExecutionConfigField $ mapZ3ExecutionField field
  Z3.Z3SMTLibExecutionFieldAboveMaximum field maximumValue observed ->
    LengthSMTLibExecutionConfigFieldAboveMaximum
      (mapZ3ExecutionField field) maximumValue observed
  Z3.Z3SMTLibExecutionHostDeadlineMarginTooSmall timeout deadline margin ->
    LengthSMTLibExecutionHostDeadlineMarginTooSmall timeout deadline margin
  Z3.Z3SMTLibExecutionHostDeadlineMicrosecondsOverflow deadline ->
    LengthSMTLibExecutionHostDeadlineMicrosecondsOverflow deadline
  Z3.Z3SMTLibExecutionExecutablePathCharacterLimitExceeded
      maximumCharacters observed ->
    LengthSMTLibExecutionExecutablePathCharacterLimitExceeded
      maximumCharacters observed
  Z3.Z3SMTLibExecutionInvalidExecutablePathCharacter offset reason ->
    LengthSMTLibExecutionInvalidExecutablePathCharacter offset
      $ mapZ3PathCharacterError reason
  Z3.Z3SMTLibExecutionEmptyExecutablePath ->
    LengthSMTLibExecutionEmptyExecutablePath
  Z3.Z3SMTLibExecutionExecutablePathNotAbsolute ->
    LengthSMTLibExecutionExecutablePathNotAbsolute
  Z3.Z3SMTLibExecutionExpectedExecutableSHA256LengthMismatch
      expected observed ->
    LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
      expected observed

mapZ3ExecutionField
  :: Z3.Z3SMTLibExecutionField
  -> LengthSMTLibExecutionConfigField
mapZ3ExecutionField field = case field of
  Z3.Z3SMTLibExecutionSolverTimeoutMilliseconds ->
    LengthSMTLibExecutionSolverTimeoutMilliseconds
  Z3.Z3SMTLibExecutionSolverResourceLimit ->
    LengthSMTLibExecutionSolverResourceLimit
  Z3.Z3SMTLibExecutionHostDeadlineMilliseconds ->
    LengthSMTLibExecutionHostDeadlineMilliseconds

mapZ3PathCharacterError
  :: Z3.Z3SMTLibExecutionPathCharacterError
  -> LengthSMTLibExecutionPathCharacterError
mapZ3PathCharacterError reason = case reason of
  Z3.Z3SMTLibExecutionPathContainsNul ->
    LengthSMTLibExecutionPathContainsNul
  Z3.Z3SMTLibExecutionPathContainsSurrogate ->
    LengthSMTLibExecutionPathContainsSurrogate

buildPolicyFingerprint
  :: LengthSMTLibExecutionLimits
  -> Z3.Z3SMTLibExecutionProfile
  -> LengthSMTLibArtifactPolicy
  -> LengthSMTLibResponseLimits
  -> Either
      LengthSMTLibExecutionConfigError
      (Fingerprint LengthSMTLibExecutionPolicyFingerprintSubject)
buildPolicyFingerprint limits z3 artifacts responses =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii "length-z3-execution-policy"
      , fingerprintBuilderFields =
          [ FingerprintBytes lengthSMTLibExecutionPolicySchemaTag
          , FingerprintBytes lengthSMTLibExecutionProtocolSchemaTag
          ] ++ Z3.z3SMTLibExecutionFingerprintFields z3 ++
          [ artifactPolicyField artifacts
          , FingerprintBytes lengthSMTLibResponseSchemaTag
          , FingerprintNatural $ lengthSMTLibResponseByteLimit responses
          , FingerprintNatural $ fromIntegral
              $ lengthSMTLibResponseNestingDepthLimit responses
          , FingerprintNatural $ lengthSMTLibResponseNodeLimit responses
          , FingerprintNatural $ lengthSMTLibResponseTokenByteLimit responses
          , FingerprintNatural $ fromIntegral
              $ lengthSMTLibResponseIntegerBitLimit responses
          ]
      } of
    Left FingerprintLimitExceeded
        { fingerprintMaximumBytes = maximumBytesObserved
        , fingerprintObservedBytesAtLeast = observed
        } -> Left $ LengthSMTLibExecutionPolicyFingerprintByteLimitExceeded
          maximumBytesObserved observed
    Right fingerprint -> Right fingerprint
 where
 maximumBytes = lengthSMTLibExecutionPolicyFingerprintByteLimit limits

artifactPolicyField :: LengthSMTLibArtifactPolicy -> FingerprintField
artifactPolicyField policy = FingerprintTag (ascii "artifact-policy")
  [ FingerprintBytes $ case policy of
      LengthSMTLibStatusOnly -> ascii "status-only"
      LengthSMTLibInputValuesAfterSatisfiable ->
        ascii "input-values-after-satisfiable"
  ]

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord

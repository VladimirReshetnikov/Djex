{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Private sealing for the first, deliberately pure Z3 execution policy.
--
-- This module does not resolve, inspect, hash, or launch an executable.  In
-- particular, caller-supplied SHA-256 bytes are an expectation to be checked
-- by a later live session opener, not an attestation or receipt.  That opener
-- must bind the observed digest even when no expectation is configured and
-- must defend the resolution/hash/spawn path against replacement races.
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
  , lengthSMTLibExecutionExecutablePath
  , lengthSMTLibExecutionExpectedExecutableSHA256
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.FilePath (isAbsolute)

import Language.Haskell.Synthesis.Fingerprint (Fingerprint)
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprintWithin
  )
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
lengthSMTLibExecutionArgumentPrefix =
  ["-in", "-smt2", "smtlib2_compliant=true"]

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
lengthSMTLibExecutionConfiguredArgumentVector config =
  configuredArgumentVector
    (lengthSMTLibExecutionSolverTimeoutMilliseconds config)
    (lengthSMTLibExecutionSolverResourceLimit config)

configuredArgumentVector :: Int -> Int -> [String]
configuredArgumentVector timeout resource =
  lengthSMTLibExecutionArgumentPrefix ++
    [ "timeout=" ++ show timeout
    , "rlimit=" ++ show resource
    ]

-- | Exact startup command.  In required compliance mode the option affects
-- its own response, so a conforming Z3 worker emits no @success@ for it.
lengthSMTLibExecutionStartupCommandBytes :: [Word8]
lengthSMTLibExecutionStartupCommandBytes =
  ascii "(set-option :print-success false)\n"

-- | Exact reset prefix replayed before every canonical query.  The required
-- Z3 capability retains disabled success printing across @reset@, so reset
-- emits no frame; suppression is then repeated explicitly for the new query.
-- A future capability handshake must establish this response behavior before
-- the worker can enter the query phase.
lengthSMTLibExecutionQueryResetBytes :: [Word8]
lengthSMTLibExecutionQueryResetBytes = ascii
  "(reset)\n(set-option :print-success false)\n"

-- | V2 launches with @env = Just []@.  It never inherits ambient variables.
-- A later platform-specific allowlist requires a new versioned policy.
lengthSMTLibExecutionEnvironmentPolicyTag :: [Word8]
lengthSMTLibExecutionEnvironmentPolicyTag = ascii "empty-environment/v1"

-- | V2 requires a fresh empty working directory owned by the live session.
-- Its concrete path belongs to that session identity and must never be an
-- inherited caller directory.
lengthSMTLibExecutionWorkingDirectoryPolicyTag :: [Word8]
lengthSMTLibExecutionWorkingDirectoryPolicyTag =
  ascii "fresh-empty-working-directory/v1"

-- | Meaning of optional expected digest bytes.  The digest is a named
-- external digest/pin format, not a collision-free identity for file bytes.
lengthSMTLibExecutionExpectedDigestSchemaTag :: [Word8]
lengthSMTLibExecutionExpectedDigestSchemaTag =
  ascii "sha256/exact-executable-file-bytes/v1"

-- | Minimum host-side response and cleanup time beyond the solver timeout.
lengthSMTLibMinimumHostDeadlineMarginMilliseconds :: Natural
lengthSMTLibMinimumHostDeadlineMarginMilliseconds = 100

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
  !Natural !Natural
  deriving (Eq, Ord)

instance NFData LengthSMTLibExecutionLimits where
  rnf (LengthSMTLibExecutionLimits path fingerprint) =
    rnf path `seq` rnf fingerprint

mkLengthSMTLibExecutionLimits
  :: LengthSMTLibExecutionLimitSource
  -> LengthSMTLibExecutionLimits
mkLengthSMTLibExecutionLimits source = LengthSMTLibExecutionLimits
  (lengthSMTLibExecutionLimitSourceExecutablePathCharacters source)
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
    (LengthSMTLibExecutionLimits value _) = value

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
  FilePath
  (Maybe [Word8])
  !Int
  !Int
  !Int
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
      executable expectedDigest timeout resource deadline artifacts responses
      fingerprint) =
    rnf executable `seq` rnf expectedDigest `seq` rnf timeout `seq`
    rnf resource `seq` rnf deadline `seq` rnf artifacts `seq`
    rnf responses `seq` rnf fingerprint

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
  timeout <- validateSolverTimeout
    $ lengthSMTLibExecutionConfigSourceSolverTimeoutMilliseconds source
  resource <- validateSolverResourceLimit
    $ lengthSMTLibExecutionConfigSourceSolverResourceLimit source
  deadline <- validateHostDeadline timeout
    $ lengthSMTLibExecutionConfigSourceHostDeadlineMilliseconds source
  executable <- retainExecutablePath
    (lengthSMTLibExecutionExecutablePathCharacterLimit limits)
    $ lengthSMTLibExecutionConfigSourceExecutablePath source
  expectedDigest <- traverse retainExpectedDigest
    $ lengthSMTLibExecutionConfigSourceExpectedExecutableSHA256 source
  let artifacts = lengthSMTLibExecutionConfigSourceArtifactPolicy source
      responses = lengthSMTLibExecutionConfigSourceResponseLimits source
  fingerprint <- buildPolicyFingerprint limits executable expectedDigest
    timeout resource deadline artifacts responses
  pure $ LengthSMTLibExecutionConfig executable expectedDigest
    timeout resource deadline artifacts responses fingerprint

lengthSMTLibExecutionSolverTimeoutMilliseconds
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionSolverTimeoutMilliseconds
    (LengthSMTLibExecutionConfig _ _ value _ _ _ _ _) = value

lengthSMTLibExecutionSolverResourceLimit
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionSolverResourceLimit
    (LengthSMTLibExecutionConfig _ _ _ value _ _ _ _) = value

lengthSMTLibExecutionHostDeadlineMilliseconds
  :: LengthSMTLibExecutionConfig
  -> Int
lengthSMTLibExecutionHostDeadlineMilliseconds
    (LengthSMTLibExecutionConfig _ _ _ _ value _ _ _) = value

lengthSMTLibExecutionArtifactPolicy
  :: LengthSMTLibExecutionConfig
  -> LengthSMTLibArtifactPolicy
lengthSMTLibExecutionArtifactPolicy
    (LengthSMTLibExecutionConfig _ _ _ _ _ value _ _) = value

lengthSMTLibExecutionResponseLimits
  :: LengthSMTLibExecutionConfig
  -> LengthSMTLibResponseLimits
lengthSMTLibExecutionResponseLimits
    (LengthSMTLibExecutionConfig _ _ _ _ _ _ value _) = value

data LengthSMTLibExecutionPolicyFingerprintSubject

lengthSMTLibExecutionPolicyFingerprint
  :: LengthSMTLibExecutionConfig
  -> Fingerprint LengthSMTLibExecutionPolicyFingerprintSubject
lengthSMTLibExecutionPolicyFingerprint
    (LengthSMTLibExecutionConfig _ _ _ _ _ _ _ value) = value

lengthSMTLibExecutionExecutablePath
  :: LengthSMTLibExecutionConfig
  -> FilePath
lengthSMTLibExecutionExecutablePath
    (LengthSMTLibExecutionConfig value _ _ _ _ _ _ _) = value

lengthSMTLibExecutionExpectedExecutableSHA256
  :: LengthSMTLibExecutionConfig
  -> Maybe [Word8]
lengthSMTLibExecutionExpectedExecutableSHA256
    (LengthSMTLibExecutionConfig _ value _ _ _ _ _ _) = value

validateSolverTimeout
  :: Int
  -> Either LengthSMTLibExecutionConfigError Int
validateSolverTimeout value = validatePositiveWord32BelowInfinity
  LengthSMTLibExecutionSolverTimeoutMilliseconds True value

validateSolverResourceLimit
  :: Int
  -> Either LengthSMTLibExecutionConfigError Int
validateSolverResourceLimit value = validatePositiveWord32BelowInfinity
  LengthSMTLibExecutionSolverResourceLimit False value

validatePositiveWord32BelowInfinity
  :: LengthSMTLibExecutionConfigField
  -> Bool
  -> Int
  -> Either LengthSMTLibExecutionConfigError Int
validatePositiveWord32BelowInfinity field excludesMaximum value
  | value < 0 = Left $ NegativeLengthSMTLibExecutionConfigField field value
  | value == 0 = Left $ ZeroLengthSMTLibExecutionConfigField field
  | toInteger value > maximumValue = Left $
      LengthSMTLibExecutionConfigFieldAboveMaximum
        field maximumValue $ toInteger value
  | otherwise = Right value
 where
  word32Maximum = 4294967295
  maximumValue
    | excludesMaximum = word32Maximum - 1
    | otherwise = word32Maximum

validateHostDeadline
  :: Int
  -> Int
  -> Either LengthSMTLibExecutionConfigError Int
validateHostDeadline timeout value
  | value < 0 = Left $ NegativeLengthSMTLibExecutionConfigField
      LengthSMTLibExecutionHostDeadlineMilliseconds value
  | value == 0 = Left $ ZeroLengthSMTLibExecutionConfigField
      LengthSMTLibExecutionHostDeadlineMilliseconds
  | toInteger value * 1000 > toInteger (maxBound :: Int) =
      Left $ LengthSMTLibExecutionHostDeadlineMicrosecondsOverflow value
  | toInteger value < toInteger timeout + toInteger minimumMargin =
      Left $ LengthSMTLibExecutionHostDeadlineMarginTooSmall
        timeout value minimumMargin
  | otherwise = Right value
 where
  minimumMargin = lengthSMTLibMinimumHostDeadlineMarginMilliseconds

retainExecutablePath
  :: Natural
  -> FilePath
  -> Either LengthSMTLibExecutionConfigError FilePath
retainExecutablePath maximumCharacters = go 0 maximumCharacters []
 where
  go !_ _ reversed [] =
    let retained = reverse reversed
    in if null retained
        then Left LengthSMTLibExecutionEmptyExecutablePath
        else if isAbsolute retained
          then Right retained
          else Left LengthSMTLibExecutionExecutablePathNotAbsolute
  go !_ 0 _ (_ : _) = Left $
    LengthSMTLibExecutionExecutablePathCharacterLimitExceeded
      maximumCharacters (maximumCharacters + 1)
  go !offset remaining reversed (character : characters)
    | character == '\0' = Left $
        LengthSMTLibExecutionInvalidExecutablePathCharacter offset
          LengthSMTLibExecutionPathContainsNul
    | isSurrogate character = Left $
        LengthSMTLibExecutionInvalidExecutablePathCharacter offset
          LengthSMTLibExecutionPathContainsSurrogate
    | otherwise = go (offset + 1) (remaining - 1)
        (character : reversed) characters

retainExpectedDigest
  :: [Word8]
  -> Either LengthSMTLibExecutionConfigError [Word8]
retainExpectedDigest = go 0 []
 where
  expected = 32
  go !observed reversed []
    | observed == expected = Right $ reverse reversed
    | otherwise = Left $
        LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
          expected observed
  go !observed _ (_ : _)
    | observed == expected = Left $
        LengthSMTLibExecutionExpectedExecutableSHA256LengthMismatch
          expected (expected + 1)
  go !observed reversed (byte : bytes) =
    go (observed + 1) (byte : reversed) bytes

buildPolicyFingerprint
  :: LengthSMTLibExecutionLimits
  -> FilePath
  -> Maybe [Word8]
  -> Int
  -> Int
  -> Int
  -> LengthSMTLibArtifactPolicy
  -> LengthSMTLibResponseLimits
  -> Either
      LengthSMTLibExecutionConfigError
      (Fingerprint LengthSMTLibExecutionPolicyFingerprintSubject)
buildPolicyFingerprint limits executable expectedDigest timeout resource
    deadline artifacts responses =
  case buildFingerprintWithin maximumBytes FingerprintBuilder
      { fingerprintBuilderVersion = 1
      , fingerprintBuilderRole = ascii "length-z3-execution-policy"
      , fingerprintBuilderFields =
          [ FingerprintBytes lengthSMTLibExecutionPolicySchemaTag
          , FingerprintBytes lengthSMTLibExecutionProtocolSchemaTag
          , textField executable
          , FingerprintSequence $ map textField
              $ configuredArgumentVector timeout resource
          , FingerprintBytes lengthSMTLibExecutionStartupCommandBytes
          , FingerprintBytes lengthSMTLibExecutionQueryResetBytes
          , FingerprintBytes lengthSMTLibExecutionEnvironmentPolicyTag
          , FingerprintBytes lengthSMTLibExecutionWorkingDirectoryPolicyTag
          , optionalBytesField
              lengthSMTLibExecutionExpectedDigestSchemaTag expectedDigest
          , FingerprintNatural $ fromIntegral timeout
          , FingerprintNatural $ fromIntegral resource
          , FingerprintNatural $ fromIntegral deadline
          , FingerprintNatural
              lengthSMTLibMinimumHostDeadlineMarginMilliseconds
          , artifactPolicyField artifacts
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

optionalBytesField :: [Word8] -> Maybe [Word8] -> FingerprintField
optionalBytesField tag value = FingerprintTag tag $ case value of
  Nothing -> [FingerprintTag (ascii "absent") []]
  Just bytes -> [FingerprintTag (ascii "present") [FingerprintBytes bytes]]

artifactPolicyField :: LengthSMTLibArtifactPolicy -> FingerprintField
artifactPolicyField policy = FingerprintTag (ascii "artifact-policy")
  [ FingerprintBytes $ case policy of
      LengthSMTLibStatusOnly -> ascii "status-only"
      LengthSMTLibInputValuesAfterSatisfiable ->
        ascii "input-values-after-satisfiable"
  ]

textField :: String -> FingerprintField
textField = FingerprintSequence
  . map (FingerprintNatural . fromIntegral . ord)

isSurrogate :: Char -> Bool
isSurrogate character =
  let code = ord character
  in code >= 0xd800 && code <= 0xdfff

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord

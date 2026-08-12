{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Package-private, domain-neutral Z3 launch policy.
--
-- This module validates and retains only the process facts shared by SMT-LIB
-- domains: executable path and optional digest expectation, solver limits,
-- host deadline, exact argv, startup/reset commands, empty environment, and
-- fresh-working-directory policy. It deliberately owns no behavioral-domain
-- protocol tag, artifact policy, response grammar, fingerprint, or byte
-- budget. A domain splices 'z3SMTLibExecutionFingerprintFields' into its own
-- complete sealed identity without adding a nested fingerprint node.
module Language.Haskell.Synthesis.Internal.SMTLib.Z3.Execution
  ( z3SMTLibExecutionArgumentPrefix
  , z3SMTLibExecutionConfiguredArgumentVector
  , z3SMTLibExecutionStartupCommandBytes
  , z3SMTLibExecutionQueryResetBytes
  , z3SMTLibExecutionChildEnvironment
  , z3SMTLibExecutionEnvironmentPolicyTag
  , z3SMTLibExecutionWorkingDirectoryPolicyTag
  , z3SMTLibExecutionExpectedDigestSchemaTag
  , z3SMTLibMinimumHostDeadlineMarginMilliseconds
  , Z3SMTLibExecutionLimits
  , mkZ3SMTLibExecutionLimits
  , z3SMTLibExecutionExecutablePathCharacterLimit
  , Z3SMTLibExecutionSource (..)
  , Z3SMTLibExecutionProfile
  , Z3SMTLibExecutionField (..)
  , Z3SMTLibExecutionPathCharacterError (..)
  , Z3SMTLibExecutionError (..)
  , mkZ3SMTLibExecutionProfile
  , z3SMTLibExecutionExecutablePath
  , z3SMTLibExecutionExpectedExecutableSHA256
  , z3SMTLibExecutionSolverTimeoutMilliseconds
  , z3SMTLibExecutionSolverResourceLimit
  , z3SMTLibExecutionHostDeadlineMilliseconds
  , z3SMTLibExecutionFingerprintFields
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.FilePath (isAbsolute)

import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..))

-- | Fixed argument prefix passed directly to @process@ without a shell.
z3SMTLibExecutionArgumentPrefix :: [String]
z3SMTLibExecutionArgumentPrefix =
  ["-in", "-smt2", "smtlib2_compliant=true"]

-- | Complete ordered argv for one admitted profile.
z3SMTLibExecutionConfiguredArgumentVector
  :: Z3SMTLibExecutionProfile
  -> [String]
z3SMTLibExecutionConfiguredArgumentVector profile =
  configuredArgumentVector
    (z3SMTLibExecutionSolverTimeoutMilliseconds profile)
    (z3SMTLibExecutionSolverResourceLimit profile)

configuredArgumentVector :: Int -> Int -> [String]
configuredArgumentVector timeout resource =
  z3SMTLibExecutionArgumentPrefix ++
    [ "timeout=" ++ show timeout
    , "rlimit=" ++ show resource
    ]

-- | Exact startup command. In required compliance mode this suppresses its
-- own success response.
z3SMTLibExecutionStartupCommandBytes :: [Word8]
z3SMTLibExecutionStartupCommandBytes =
  ascii "(set-option :print-success false)\n"

-- | Exact reset prefix replayed before a domain query.
z3SMTLibExecutionQueryResetBytes :: [Word8]
z3SMTLibExecutionQueryResetBytes = ascii
  "(reset)\n(set-option :print-success false)\n"

-- | Child processes inherit no ambient environment variables.
z3SMTLibExecutionChildEnvironment :: Maybe [(String, String)]
z3SMTLibExecutionChildEnvironment = Just []

z3SMTLibExecutionEnvironmentPolicyTag :: [Word8]
z3SMTLibExecutionEnvironmentPolicyTag = ascii "empty-environment/v1"

z3SMTLibExecutionWorkingDirectoryPolicyTag :: [Word8]
z3SMTLibExecutionWorkingDirectoryPolicyTag =
  ascii "fresh-empty-working-directory/v1"

z3SMTLibExecutionExpectedDigestSchemaTag :: [Word8]
z3SMTLibExecutionExpectedDigestSchemaTag =
  ascii "sha256/exact-executable-file-bytes/v1"

-- | Minimum host-side response and cleanup time beyond the solver timeout.
z3SMTLibMinimumHostDeadlineMarginMilliseconds :: Natural
z3SMTLibMinimumHostDeadlineMarginMilliseconds = 100

-- | Admission-only bound for retaining an executable path.
data Z3SMTLibExecutionLimits = Z3SMTLibExecutionLimits !Natural
  deriving (Eq, Ord)

instance NFData Z3SMTLibExecutionLimits where
  rnf (Z3SMTLibExecutionLimits pathCharacters) = rnf pathCharacters

mkZ3SMTLibExecutionLimits :: Natural -> Z3SMTLibExecutionLimits
mkZ3SMTLibExecutionLimits = Z3SMTLibExecutionLimits

z3SMTLibExecutionExecutablePathCharacterLimit
  :: Z3SMTLibExecutionLimits
  -> Natural
z3SMTLibExecutionExecutablePathCharacterLimit
    (Z3SMTLibExecutionLimits value) = value

-- | Raw domain-supplied launch facts. All fields remain lazy so validation
-- can preserve its declared first-failure order.
data Z3SMTLibExecutionSource = Z3SMTLibExecutionSource
  { z3SMTLibExecutionSourceExecutablePath :: FilePath
  , z3SMTLibExecutionSourceExpectedExecutableSHA256 :: Maybe [Word8]
  , z3SMTLibExecutionSourceSolverTimeoutMilliseconds :: Int
  , z3SMTLibExecutionSourceSolverResourceLimit :: Int
  , z3SMTLibExecutionSourceHostDeadlineMilliseconds :: Int
  }
  deriving (Eq, Ord)

instance NFData Z3SMTLibExecutionSource where
  rnf source =
    rnf (z3SMTLibExecutionSourceExecutablePath source) `seq`
    rnf (z3SMTLibExecutionSourceExpectedExecutableSHA256 source) `seq`
    rnf (z3SMTLibExecutionSourceSolverTimeoutMilliseconds source) `seq`
    rnf (z3SMTLibExecutionSourceSolverResourceLimit source) `seq`
    rnf (z3SMTLibExecutionSourceHostDeadlineMilliseconds source)

-- | Opaque admitted launch profile. Path and digest payloads stay lazy as in
-- the Length compatibility policy; the three validated numeric controls are
-- strict.
data Z3SMTLibExecutionProfile = Z3SMTLibExecutionProfile
  FilePath
  (Maybe [Word8])
  !Int
  !Int
  !Int

instance NFData Z3SMTLibExecutionProfile where
  rnf (Z3SMTLibExecutionProfile executable expected timeout resource deadline) =
    rnf executable `seq` rnf expected `seq` rnf timeout `seq`
    rnf resource `seq` rnf deadline

data Z3SMTLibExecutionField
  = Z3SMTLibExecutionSolverTimeoutMilliseconds
  | Z3SMTLibExecutionSolverResourceLimit
  | Z3SMTLibExecutionHostDeadlineMilliseconds
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData Z3SMTLibExecutionField

data Z3SMTLibExecutionPathCharacterError
  = Z3SMTLibExecutionPathContainsNul
  | Z3SMTLibExecutionPathContainsSurrogate
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

instance NFData Z3SMTLibExecutionPathCharacterError

-- | Closed, sanitized launch-profile rejection. No constructor retains the
-- executable path or digest bytes.
data Z3SMTLibExecutionError
  = NegativeZ3SMTLibExecutionField !Z3SMTLibExecutionField !Int
  | ZeroZ3SMTLibExecutionField !Z3SMTLibExecutionField
  | Z3SMTLibExecutionFieldAboveMaximum
      !Z3SMTLibExecutionField !Integer !Integer
  | Z3SMTLibExecutionHostDeadlineMarginTooSmall !Int !Int !Natural
  | Z3SMTLibExecutionHostDeadlineMicrosecondsOverflow !Int
  | Z3SMTLibExecutionExecutablePathCharacterLimitExceeded
      !Natural !Natural
  | Z3SMTLibExecutionInvalidExecutablePathCharacter
      !Natural !Z3SMTLibExecutionPathCharacterError
  | Z3SMTLibExecutionEmptyExecutablePath
  | Z3SMTLibExecutionExecutablePathNotAbsolute
  | Z3SMTLibExecutionExpectedExecutableSHA256LengthMismatch
      !Natural !Natural
  deriving (Eq, Ord, Show, Generic)

instance NFData Z3SMTLibExecutionError

-- | Admit one pure launch profile without touching the filesystem or opening
-- a process. Validation order is timeout, resource limit, host deadline,
-- executable path, then optional digest.
mkZ3SMTLibExecutionProfile
  :: Z3SMTLibExecutionLimits
  -> Z3SMTLibExecutionSource
  -> Either Z3SMTLibExecutionError Z3SMTLibExecutionProfile
mkZ3SMTLibExecutionProfile limits source = do
  timeout <- validateSolverTimeout
    $ z3SMTLibExecutionSourceSolverTimeoutMilliseconds source
  resource <- validateSolverResourceLimit
    $ z3SMTLibExecutionSourceSolverResourceLimit source
  deadline <- validateHostDeadline timeout
    $ z3SMTLibExecutionSourceHostDeadlineMilliseconds source
  executable <- retainExecutablePath
    (z3SMTLibExecutionExecutablePathCharacterLimit limits)
    $ z3SMTLibExecutionSourceExecutablePath source
  expectedDigest <- traverse retainExpectedDigest
    $ z3SMTLibExecutionSourceExpectedExecutableSHA256 source
  pure $ Z3SMTLibExecutionProfile executable expectedDigest
    timeout resource deadline

z3SMTLibExecutionExecutablePath :: Z3SMTLibExecutionProfile -> FilePath
z3SMTLibExecutionExecutablePath
    (Z3SMTLibExecutionProfile value _ _ _ _) = value

z3SMTLibExecutionExpectedExecutableSHA256
  :: Z3SMTLibExecutionProfile
  -> Maybe [Word8]
z3SMTLibExecutionExpectedExecutableSHA256
    (Z3SMTLibExecutionProfile _ value _ _ _) = value

z3SMTLibExecutionSolverTimeoutMilliseconds
  :: Z3SMTLibExecutionProfile
  -> Int
z3SMTLibExecutionSolverTimeoutMilliseconds
    (Z3SMTLibExecutionProfile _ _ value _ _) = value

z3SMTLibExecutionSolverResourceLimit
  :: Z3SMTLibExecutionProfile
  -> Int
z3SMTLibExecutionSolverResourceLimit
    (Z3SMTLibExecutionProfile _ _ _ value _) = value

z3SMTLibExecutionHostDeadlineMilliseconds
  :: Z3SMTLibExecutionProfile
  -> Int
z3SMTLibExecutionHostDeadlineMilliseconds
    (Z3SMTLibExecutionProfile _ _ _ _ value) = value

-- | Canonical flat field slice for a domain-owned complete identity. There is
-- intentionally no generic schema tag, fingerprint, or admission pass.
z3SMTLibExecutionFingerprintFields
  :: Z3SMTLibExecutionProfile
  -> [FingerprintField]
z3SMTLibExecutionFingerprintFields profile =
  [ textField $ z3SMTLibExecutionExecutablePath profile
  , FingerprintSequence $ map textField
      $ z3SMTLibExecutionConfiguredArgumentVector profile
  , FingerprintBytes z3SMTLibExecutionStartupCommandBytes
  , FingerprintBytes z3SMTLibExecutionQueryResetBytes
  , FingerprintBytes z3SMTLibExecutionEnvironmentPolicyTag
  , FingerprintBytes z3SMTLibExecutionWorkingDirectoryPolicyTag
  , optionalBytesField z3SMTLibExecutionExpectedDigestSchemaTag
      $ z3SMTLibExecutionExpectedExecutableSHA256 profile
  , FingerprintNatural $ fromIntegral
      $ z3SMTLibExecutionSolverTimeoutMilliseconds profile
  , FingerprintNatural $ fromIntegral
      $ z3SMTLibExecutionSolverResourceLimit profile
  , FingerprintNatural $ fromIntegral
      $ z3SMTLibExecutionHostDeadlineMilliseconds profile
  , FingerprintNatural z3SMTLibMinimumHostDeadlineMarginMilliseconds
  ]

validateSolverTimeout :: Int -> Either Z3SMTLibExecutionError Int
validateSolverTimeout value = validatePositiveWord32BelowInfinity
  Z3SMTLibExecutionSolverTimeoutMilliseconds True value

validateSolverResourceLimit :: Int -> Either Z3SMTLibExecutionError Int
validateSolverResourceLimit value = validatePositiveWord32BelowInfinity
  Z3SMTLibExecutionSolverResourceLimit False value

validatePositiveWord32BelowInfinity
  :: Z3SMTLibExecutionField
  -> Bool
  -> Int
  -> Either Z3SMTLibExecutionError Int
validatePositiveWord32BelowInfinity field excludesMaximum value
  | value < 0 = Left $ NegativeZ3SMTLibExecutionField field value
  | value == 0 = Left $ ZeroZ3SMTLibExecutionField field
  | toInteger value > maximumValue = Left $
      Z3SMTLibExecutionFieldAboveMaximum
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
  -> Either Z3SMTLibExecutionError Int
validateHostDeadline timeout value
  | value < 0 = Left $ NegativeZ3SMTLibExecutionField
      Z3SMTLibExecutionHostDeadlineMilliseconds value
  | value == 0 = Left $ ZeroZ3SMTLibExecutionField
      Z3SMTLibExecutionHostDeadlineMilliseconds
  | toInteger value * 1000 > toInteger (maxBound :: Int) =
      Left $ Z3SMTLibExecutionHostDeadlineMicrosecondsOverflow value
  | toInteger value < toInteger timeout + toInteger minimumMargin =
      Left $ Z3SMTLibExecutionHostDeadlineMarginTooSmall
        timeout value minimumMargin
  | otherwise = Right value
 where
  minimumMargin = z3SMTLibMinimumHostDeadlineMarginMilliseconds

retainExecutablePath
  :: Natural
  -> FilePath
  -> Either Z3SMTLibExecutionError FilePath
retainExecutablePath maximumCharacters = go 0 maximumCharacters []
 where
  go !_ _ reversed [] =
    let retained = reverse reversed
    in if null retained
        then Left Z3SMTLibExecutionEmptyExecutablePath
        else if isAbsolute retained
          then Right retained
          else Left Z3SMTLibExecutionExecutablePathNotAbsolute
  go !_ 0 _ (_ : _) = Left $
    Z3SMTLibExecutionExecutablePathCharacterLimitExceeded
      maximumCharacters (maximumCharacters + 1)
  go !offset remaining reversed (character : characters)
    | character == '\0' = Left $
        Z3SMTLibExecutionInvalidExecutablePathCharacter offset
          Z3SMTLibExecutionPathContainsNul
    | isSurrogate character = Left $
        Z3SMTLibExecutionInvalidExecutablePathCharacter offset
          Z3SMTLibExecutionPathContainsSurrogate
    | otherwise = go (offset + 1) (remaining - 1)
        (character : reversed) characters

retainExpectedDigest
  :: [Word8]
  -> Either Z3SMTLibExecutionError [Word8]
retainExpectedDigest = go 0 []
 where
  expected = 32
  go !observed reversed []
    | observed == expected = Right $ reverse reversed
    | otherwise = Left $
        Z3SMTLibExecutionExpectedExecutableSHA256LengthMismatch
          expected observed
  go !observed _ (_ : _)
    | observed == expected = Left $
        Z3SMTLibExecutionExpectedExecutableSHA256LengthMismatch
          expected (expected + 1)
  go !observed reversed (byte : bytes) =
    go (observed + 1) (byte : reversed) bytes

optionalBytesField :: [Word8] -> Maybe [Word8] -> FingerprintField
optionalBytesField tag value = FingerprintTag tag $ case value of
  Nothing -> [FingerprintTag (ascii "absent") []]
  Just bytes -> [FingerprintTag (ascii "present") [FingerprintBytes bytes]]

textField :: String -> FingerprintField
textField = FingerprintSequence
  . map (FingerprintNatural . fromIntegral . ord)

isSurrogate :: Char -> Bool
isSurrogate character =
  let code = ord character
  in code >= 0xd800 && code <= 0xdfff

ascii :: String -> [Word8]
ascii = map $ fromIntegral . ord

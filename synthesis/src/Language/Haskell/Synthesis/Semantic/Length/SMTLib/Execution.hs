-- | Pure configuration for the Length/Z3 live execution boundary.
--
-- This module seals launch spelling, finite solver and host budgets, artifact
-- policy, and the exact response-decoder policy.  It performs no IO.  A
-- successful value is not proof that the executable exists, matches an
-- expected SHA-256 digest, speaks the required protocol, or has any solver
-- capability.  The scoped live facade probes and binds those observations;
-- constructing this value alone establishes none of them.
--
-- V2 uses a fixed direct argument prefix, policy-derived resource arguments,
-- and an exact empty child environment;
-- it never invokes a shell or inherits ambient variables.  A live session
-- must also create and select a fresh empty working directory instead of
-- inheriting the caller's directory.  The optional
-- SHA-256 value means "reject unless the live executable file has these digest
-- bytes".  SHA-256 is an external digest/pin format, not Djex's
-- collision-free representation of the executable's unbounded contents.
-- The live opener must always bind the observed digest, even without a pin,
-- and must defend executable resolution, hashing, and spawn against path
-- replacement races.
-- 'lengthSMTLibExecutionExecutableDigestExpectation' reveals only whether the
-- sealed policy contains such an expectation.  It exposes no digest bytes and
-- says nothing about a later executable observation or match.
--
-- No policy or parsed response grants semantic authority.  Solver reports
-- remain heuristic, and only independent Length replay can create
-- model-relative counterexample evidence.
--
-- Opaque configurations support equality as exact policy equivalence.  V2
-- contains no inherited environment or secret launch material; a future
-- schema must revisit that API before admitting any secret-bearing field.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Execution
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
  , LengthSMTLibExecutableLaunchStrategy (..)
  , LengthSMTLibExecutableDigestExpectation (..)
  , lengthSMTLibExecutionExecutableDigestExpectation
  , LengthSMTLibExecutionConfigField (..)
  , LengthSMTLibExecutionPathCharacterError (..)
  , LengthSMTLibExecutionConfigError (..)
  , mkLengthSMTLibExecutionConfig
  , mkLengthSMTLibDescriptorBoundExecutionConfig
  , mkLengthSMTLibDescriptorBoundEffectiveIDExecutableAccessExecutionConfig
  , mkLengthSMTLibDescriptorBoundExecveCheckExecutableAccessExecutionConfig
  , lengthSMTLibExecutionExecutableLaunchStrategy
  , lengthSMTLibExecutionSolverTimeoutMilliseconds
  , lengthSMTLibExecutionSolverResourceLimit
  , lengthSMTLibExecutionHostDeadlineMilliseconds
  , lengthSMTLibExecutionArtifactPolicy
  , lengthSMTLibExecutionResponseLimits
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Execution

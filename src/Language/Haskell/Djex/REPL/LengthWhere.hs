-- | Conservative built-in-list defaults for Djex REPL Length constraints.
--
-- This package-private adapter receives an already elaborated Exference target
-- and its exact inventory. It observes every structural list argument, treats
-- every other physical arrow argument as opaque, and accepts only a scalar
-- list result or a boxed pair of list results. The resulting session and
-- contract are checked by the existing Length boundaries; no solver or
-- candidate authority is created here.
module Language.Haskell.Djex.REPL.LengthWhere
  ( ReplLengthWhereResolution (..)
  , ReplLengthWhereResolutionError (..)
  , replLengthWhereResolutionProfile
  , replLengthWhereResolutionObservedInputCount
  , resolveReplLengthWhereSource
  ) where

import Control.Monad (when)
import Data.Bifunctor (first)

import Language.Haskell.Djex.Exference
  ( ExferenceInventory
  , ExferenceLocal
  , ExferenceType
  , ExferenceTypeVariable
  )
import Language.Haskell.Synthesis.Collection (observedListLength)
import Language.Haskell.Synthesis.Name (Boxity (Boxed), listName)
import Language.Haskell.Synthesis.Semantic.Length
  ( CheckedLengthContract
  , CheckedLengthSpinePairContract
  , LengthContractError
  , LengthSpineModelSource (BuiltinListSpine)
  , LengthSpinePairContractError
  , LengthTargetArgumentRole (..)
  , checkedLengthContractInputCount
  , checkedLengthSpinePairContractInputCount
  , defaultLengthLimits
  , lengthContractInputLimit
  )
import Language.Haskell.Synthesis.Semantic.Length.Problem
  ( CheckedLengthSession
  , LengthSessionError
  , sealExactSpineCaseLengthSession
  , sealLengthContractInSession
  , sealLengthSpinePairContractInSession
  )
import Language.Haskell.Synthesis.Semantic.Length.Where
  ( LengthWhereContractSource (..)
  , LengthWhereDomain (..)
  , LengthWhereElaborationError
  , LengthWhereSource
  , elaborateLengthWhereSource
  )
import Language.Haskell.Synthesis.Type
  ( Type (..)
  , applicationSpine
  , functionSpine
  , splitLeadingForalls
  )

-- | Fully checked scalar or binary-product profile ready for candidate use.
-- Constructors are package-private because the enclosing module is not part
-- of the public facade. The nominal checked values remain distinct.
data ReplLengthWhereResolution
  = ReplLengthWhereScalarResolution
      (CheckedLengthSession ExferenceLocal ())
      (CheckedLengthContract ExferenceTypeVariable)
  | ReplLengthWhereBinaryProductResolution
      (CheckedLengthSession ExferenceLocal ())
      (CheckedLengthSpinePairContract ExferenceTypeVariable)

-- | Closed target/profile failures. No constructor retains clause source.
data ReplLengthWhereResolutionError
  = ReplLengthWherePhysicalArgumentLimitExceeded !Int !Int
  | ReplLengthWhereUnsupportedResult
  | ReplLengthWhereElaborationRejected !LengthWhereElaborationError
  | ReplLengthWhereSessionRejected !(LengthSessionError ExferenceLocal)
  | ReplLengthWhereScalarContractRejected
      !(LengthContractError ExferenceTypeVariable)
  | ReplLengthWhereBinaryProductContractRejected
      !(LengthSpinePairContractError ExferenceTypeVariable)
  deriving (Eq, Show)

-- | Stable diagnostic name of the selected built-in profile.
replLengthWhereResolutionProfile :: ReplLengthWhereResolution -> String
replLengthWhereResolutionProfile resolution = case resolution of
  ReplLengthWhereScalarResolution{} -> "list-scalar-exact-cases"
  ReplLengthWhereBinaryProductResolution{} ->
    "list-binary-product-exact-cases"

-- | Number of structural list arguments observed in physical source order.
replLengthWhereResolutionObservedInputCount
  :: ReplLengthWhereResolution
  -> Int
replLengthWhereResolutionObservedInputCount resolution = case resolution of
  ReplLengthWhereScalarResolution _ contract ->
    checkedLengthContractInputCount contract
  ReplLengthWhereBinaryProductResolution _ contract ->
    checkedLengthSpinePairContractInputCount contract

-- | Infer, elaborate, and seal the one conservative built-in profile.
resolveReplLengthWhereSource
  :: ExferenceInventory
  -> ExferenceType
  -> LengthWhereSource
  -> Either ReplLengthWhereResolutionError ReplLengthWhereResolution
resolveReplLengthWhereSource inventory target source = do
  let (_, _, body) = splitLeadingForalls target
      (parameters, result) = functionSpine body
      maximumArguments = lengthContractInputLimit defaultLengthLimits
      observedArguments = observedListLength maximumArguments parameters
  when (observedArguments > maximumArguments)
    $ Left $ ReplLengthWherePhysicalArgumentLimitExceeded
      maximumArguments observedArguments
  let roles = map argumentRole parameters
  domain <- resultDomain result
  session <- first ReplLengthWhereSessionRejected
    $ sealExactSpineCaseLengthSession
      defaultLengthLimits roles inventory BuiltinListSpine []
  contractSource <- first ReplLengthWhereElaborationRejected
    $ elaborateLengthWhereSource domain roles source
  case contractSource of
    LengthWhereScalarContractSource _ contract ->
      ReplLengthWhereScalarResolution session
        <$> first ReplLengthWhereScalarContractRejected
          (sealLengthContractInSession session target contract)
    LengthWhereBinaryProductContractSource _ contract ->
      ReplLengthWhereBinaryProductResolution session
        <$> first ReplLengthWhereBinaryProductContractRejected
          (sealLengthSpinePairContractInSession session target contract)

argumentRole :: ExferenceType -> LengthTargetArgumentRole
argumentRole argument
  | isBuiltinList argument = LengthObservedSpine
  | otherwise = LengthUnobservedTarget

resultDomain
  :: ExferenceType
  -> Either ReplLengthWhereResolutionError LengthWhereDomain
resultDomain result
  | isBuiltinList result = Right LengthWhereScalar
resultDomain (TupleType Boxed [left, right])
  | isBuiltinList left
  , isBuiltinList right = Right LengthWhereBinaryProduct
resultDomain _ = Left ReplLengthWhereUnsupportedResult

isBuiltinList :: ExferenceType -> Bool
isBuiltinList source = case applicationSpine source of
  (TypeConstructor constructor, [_]) -> constructor == listName
  _ -> False

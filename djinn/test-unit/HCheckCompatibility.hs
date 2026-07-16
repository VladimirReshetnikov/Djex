{-# OPTIONS_GHC
  -fdefer-out-of-scope-variables
  -Wno-deferred-out-of-scope-variables
  #-}

-- | Downstream import checks for the deliberately small raw HCheck facade.
--
-- The five positive imports are the pre-cache compatibility API. References
-- to prepared cache machinery are qualified and deliberately out of scope;
-- deferred name errors let the ordinary test runner fail if one of those
-- implementation hooks becomes public again.
module HCheckCompatibility
  ( hCheckCompatibilityTests
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (isInfixOf)

import Djinn.Internal.HCheck
  ( htCheckEnv
  , htCheckType
  , htCheckTypeKind
  , htCheckTypesKinds
  , htInferClassKinds
  )
import qualified Djinn.Internal.Environment as Environment
import qualified Djinn.Internal.HCheck as HCheck
import Djinn.Internal.HTypes (HKind, HSymbol, HType)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

type TypeDefinitions = [(HSymbol, ([HSymbol], HType, HKind))]

-- A separate binding keeps the metadata parameter genuinely polymorphic;
-- merely specializing the imported function to @()@ would not detect an
-- accidental narrowing of this historical signature.
historicalHtCheckEnv
  :: [(HSymbol, ([HSymbol], HType, metadata))]
  -> Either String TypeDefinitions
historicalHtCheckEnv = htCheckEnv

historicalHtCheckType
  :: TypeDefinitions -> HType -> Either String ()
historicalHtCheckType = htCheckType

historicalHtCheckTypeKind
  :: TypeDefinitions -> HKind -> HType -> Either String ()
historicalHtCheckTypeKind = htCheckTypeKind

historicalHtCheckTypesKinds
  :: TypeDefinitions -> [(HKind, HType)] -> Either String ()
historicalHtCheckTypesKinds = htCheckTypesKinds

historicalHtInferClassKinds
  :: TypeDefinitions
  -> [HSymbol]
  -> [HType]
  -> Either String [(HSymbol, HKind)]
historicalHtInferClassKinds = htInferClassKinds

-- Exact signatures keep the five compatibility imports useful independently
-- of any prepared representation used inside the library.
historicalHCheckApi :: ()
historicalHCheckApi =
  historicalHtCheckEnv `seq`
  -- Keep the qualified module import live as well: the negative probes below
  -- deliberately name absent members, which GHC's unused-import analysis does
  -- not count even though name resolution must inspect this import.
  (HCheck.htCheckEnv
    :: [(HSymbol, ([HSymbol], HType, ()))]
    -> Either String TypeDefinitions) `seq`
  -- Likewise keep the Environment module live while its removed cache
  -- projection is probed below.
  Environment.prepareEnvironment `seq`
  historicalHtCheckType `seq`
  historicalHtCheckTypeKind `seq`
  historicalHtCheckTypesKinds `seq`
  historicalHtInferClassKinds `seq`
  ()

forbiddenPreparedHCheckAttempts :: [(String, String, ())]
forbiddenPreparedHCheckAttempts =
  [ ("PreparedKindCheck", "PreparedKindCheck constructor unexpectedly exposed",
      HCheck.PreparedKindCheck `seq` ())
  , ("prepareKindCheck", "prepareKindCheck unexpectedly exposed",
      HCheck.prepareKindCheck `seq` ())
  , ("prepareKindEnvironment", "prepareKindEnvironment unexpectedly exposed",
      HCheck.prepareKindEnvironment `seq` ())
  , ("prepareKindCheckWithAssumptions",
      "prepareKindCheckWithAssumptions unexpectedly exposed",
      HCheck.prepareKindCheckWithAssumptions `seq` ())
  , ("htCheckTypePrepared", "htCheckTypePrepared unexpectedly exposed",
      HCheck.htCheckTypePrepared `seq` ())
  , ("htCheckTypeKindPrepared", "htCheckTypeKindPrepared unexpectedly exposed",
      HCheck.htCheckTypeKindPrepared `seq` ())
  , ("htCheckTypesKindsPrepared", "htCheckTypesKindsPrepared unexpectedly exposed",
      HCheck.htCheckTypesKindsPrepared `seq` ())
  , ("htCheckTypesKindsWith", "htCheckTypesKindsWith unexpectedly exposed",
      HCheck.htCheckTypesKindsWith `seq` ())
  , ("htInferClassKindsPrepared", "htInferClassKindsPrepared unexpectedly exposed",
      HCheck.htInferClassKindsPrepared `seq` ())
  , ("preparedEnvironmentKindCheck",
      "prepared Environment kind-check cache unexpectedly exposed",
      Environment.preparedEnvironmentKindCheck `seq` ())
  ]

hCheckCompatibilityTests :: [(String, Assertion)]
hCheckCompatibilityTests =
  [ ("retain historical HCheck operations and hide prepared authority", do
      historicalHCheckApi `seq` pure ()
      forM_ forbiddenPreparedHCheckAttempts $
          \(memberName, description, attempt) -> do
        result <- try $ evaluate attempt
        case result :: Either SomeException () of
          Left exception -> do
            let message = map toLower $ displayException exception
                expectedName = map toLower memberName
            assertBool
              (description ++ " raised an unrelated exception: " ++ message)
              ("not in scope" `isInfixOf` message &&
                expectedName `isInfixOf` message)
          Right () -> assertFailure description
    )
  ]

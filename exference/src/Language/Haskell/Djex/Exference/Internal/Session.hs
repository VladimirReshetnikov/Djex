-- | Private ownership of the checked Exference session invariant.
--
-- The stable Djex adapter and the explicitly named compatibility bridge both
-- need to seal the parser-specific source environment, but only this module can
-- construct a session. Keeping the representation here prevents either public
-- surface from assembling mismatched source, inventory, and search views.
module Language.Haskell.Djex.Exference.Internal.Session
  ( ExferenceSession
  , SessionOmission (..)
  , SessionOmissionCapability (..)
  , SessionOmissionReason (..)
  , sealExferenceSession
  , sealExferenceSessionWithExclusions
  , sessionSource
  , sessionSearchEnvironment
  , sessionTypeDeclarations
  , exferenceSessionInventory
  , sessionOmissions
  ) where

import Data.Bifunctor (first)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Language.Haskell.Exference.Core
  ( ExferenceEnvironment
  , ExferenceInputError
  , mkExferenceEnvironment
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (constructorFields)
  , DeconstructorBinding (..)
  , EnvDictionary (EnvDictionary)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.TypeUtils
  ( containsForall
  , typeConstructorHead
  )
import Language.Haskell.Exference.Core.Types
  ( constraint_params
  , toSynthesisName
  )
import Language.Haskell.Exference.EnvironmentParser
  ( CheckedSourceEnvironment
  , SourceEnvironment (..)
  , checkedSourceInventory
  , checkedSourceProjection
  , sourceBindingFunction
  , sourceFunctions
  , sourceTypeSynonymMap
  )
import Language.Haskell.Exference.TypeDeclsFromHaskellSrc (TypeDeclMap)
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , diagnostic
  , withCode
  , withContext
  )
import Language.Haskell.Synthesis.Inventory (Inventory)
import Language.Haskell.Synthesis.Name
  ( Name )
import Language.Haskell.Synthesis.Type (Variable)

data SessionOmissionCapability
  = BindingIntroduction
  | DataElimination
  deriving (Eq, Ord, Show)

data SessionOmissionReason
  = UnsupportedNestedForall
  | ExcludedByPolicy
  deriving (Eq, Ord, Show)

data SessionOmission = SessionOmission
  { sessionOmissionName :: Name
  , sessionOmissionCapability :: SessionOmissionCapability
  , sessionOmissionReason :: SessionOmissionReason
  }
  deriving (Eq, Ord, Show)

data ExferenceSession = ExferenceSession
  { sourceView :: SourceEnvironment
  , searchView :: ExferenceEnvironment
  , inventoryView :: Inventory (Variable Int) ()
  , typeDeclarationView :: TypeDeclMap
  , omissionView :: [SessionOmission]
  }

sealExferenceSession
  :: CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
sealExferenceSession = sealExferenceSessionWithExclusions []

sealExferenceSessionWithExclusions
  :: [Name]
  -> CheckedSourceEnvironment
  -> Either Diagnostic ExferenceSession
sealExferenceSessionWithExclusions exclusions checked = do
  let source = checkedSourceProjection checked
      excludedBindings = Set.fromList exclusions
      functionExcluded binding = Set.member
        (toSynthesisName $ functionName binding) excludedBindings
      supportedBindings =
        [ sourceBinding
        | sourceBinding <- sourceBindings source
        , let binding = sourceBindingFunction sourceBinding
        , not $ functionExcluded binding
        , functionSupported binding
        ]
      (supportedDeconstructors, omittedDeconstructors) =
        partition deconstructorSupported $ sourceDeconstructors source
      supportedSource = source
        { sourceBindings = supportedBindings
        , sourceDeconstructors = supportedDeconstructors
        }
      omissions =
        [ SessionOmission
            (toSynthesisName $ functionName binding)
            BindingIntroduction
            reason
        | binding <- sourceFunctions source
        , reason <- if functionExcluded binding
            then [ExcludedByPolicy]
            else [UnsupportedNestedForall | not $ functionSupported binding]
        ] ++ mapMaybe deconstructorOmission omittedDeconstructors
  searchEnvironment <- first sessionFailureDiagnostic
    $ mkExferenceEnvironment $ EnvDictionary
        (sourceFunctions supportedSource)
        (sourceDeconstructors supportedSource)
        (sourceClasses supportedSource)
  -- Ratings and recursion markers belong to the private search projection.
  -- Mapping them away preserves the already-validated declaration indexes and
  -- exact inferred kind assumptions without rerunning either whole-inventory
  -- pass at session construction.
  let neutralInventory = fmap (const ()) $ checkedSourceInventory checked
  pure ExferenceSession
    { sourceView = supportedSource
    , searchView = searchEnvironment
    , inventoryView = neutralInventory
    , typeDeclarationView = sourceTypeSynonymMap source
    , omissionView = omissions
    }

sessionSource :: ExferenceSession -> SourceEnvironment
sessionSource = sourceView

sessionSearchEnvironment :: ExferenceSession -> ExferenceEnvironment
sessionSearchEnvironment = searchView

sessionTypeDeclarations :: ExferenceSession -> TypeDeclMap
sessionTypeDeclarations = typeDeclarationView

exferenceSessionInventory
  :: ExferenceSession
  -> Inventory (Variable Int) ()
exferenceSessionInventory = inventoryView

sessionOmissions :: ExferenceSession -> [SessionOmission]
sessionOmissions = omissionView

functionSupported :: FunctionBinding -> Bool
functionSupported binding = all (not . containsForall)
  $ functionResult binding
  : functionParameters binding
  ++ concatMap constraint_params (functionConstraints binding)

deconstructorSupported :: DeconstructorBinding -> Bool
deconstructorSupported binding = all (not . containsForall)
  $ deconstructorInput binding
  : concatMap constructorFields (deconstructorConstructors binding)

deconstructorOmission :: DeconstructorBinding -> Maybe SessionOmission
deconstructorOmission binding = do
  name <- typeConstructorHead $ deconstructorInput binding
  pure $ SessionOmission
    (toSynthesisName name)
    DataElimination
    UnsupportedNestedForall

sessionFailureDiagnostic :: ExferenceInputError -> Diagnostic
sessionFailureDiagnostic detail = withContext (show detail)
  $ withCode "DJEX_EXF_ENV"
  $ diagnostic Error "cannot seal the Exference session environment"

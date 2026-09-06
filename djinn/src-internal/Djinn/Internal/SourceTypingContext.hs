-- | Checked nominal source authority, independent of the proof search and
-- lowering history attached to a particular candidate.
module Djinn.Internal.SourceTypingContext
  ( SourceTypingContext
  , sourceTypingContext
  , sourceTypingContextWithProviderKinds
  , sourceTypingPreparedEnvironment
  , sourceTypingGoal
  , sourceTypingProviderKinds
  , sourceTypingTermSchemes
  ) where

import qualified Data.Map.Strict as Map

import Djinn.Internal.Environment
  ( PreparedEnvironment, preparedEnvironmentInventory
  , elaboratePreparedSynthesisTypes )
import Djinn.Internal.HTypes (HKind(KStar))
import qualified Language.Haskell.Synthesis.Declaration as Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import qualified Language.Haskell.Synthesis.Inventory as Inventory
import Language.Haskell.Synthesis.KindInference (GroundKind)
import Language.Haskell.Synthesis.Name (Name)
import qualified Language.Haskell.Synthesis.Type as Type

-- The prepared environment is the original, nominal source inventory. In
-- particular it is not the structural datatype expansion searched by LJT.
-- The goal has already crossed the query's kind and synonym checks.
data SourceTypingContext = SourceTypingContext
  PreparedEnvironment
  (Type.Type String)
  (Map.Map Name [GroundKind])

sourceTypingContext
  :: PreparedEnvironment -> Type.Type String -> SourceTypingContext
sourceTypingContext prepared goal = SourceTypingContext prepared goal Map.empty

-- | Kinds admitted by the provider-assignment checker, keyed by the exact
-- source provider. Ordinary inferred requests carry no extra kind authority.
sourceTypingContextWithProviderKinds
  :: PreparedEnvironment
  -> Type.Type String
  -> Map.Map Name [GroundKind]
  -> SourceTypingContext
sourceTypingContextWithProviderKinds = SourceTypingContext

sourceTypingPreparedEnvironment :: SourceTypingContext -> PreparedEnvironment
sourceTypingPreparedEnvironment (SourceTypingContext prepared _ _) = prepared

sourceTypingGoal :: SourceTypingContext -> Type.Type String
sourceTypingGoal (SourceTypingContext _ goal _) = goal

sourceTypingProviderKinds :: SourceTypingContext -> Map.Map Name [GroundKind]
sourceTypingProviderKinds (SourceTypingContext _ _ kinds) = kinds

-- | Exact source names and independently scoped schemes. Class methods
-- remain excluded, consistently with Djinn's dictionary-independent search.
-- Constructor signatures retain their nominal datatype applications.
sourceTypingTermSchemes
  :: SourceTypingContext -> Either String (Map.Map Name (Type.Type String))
sourceTypingTermSchemes (SourceTypingContext prepared _ _) =
  Map.fromList . concat <$> mapM elaborate declarations
 where
  declarations = Environment.environmentDeclarations
    $ Inventory.inventoryEnvironment $ preparedEnvironmentInventory prepared
  elaborate declaration = case declaration of
    Declaration.ValueDeclaration{} -> signatures declaration
    Declaration.DataTypeDeclaration{} -> signatures declaration
    _ -> Right []
  signatures declaration = mapM elaborateSignature
    $ Declaration.declarationTermSchemes declaration
  elaborateSignature signature = do
    result <- elaboratePreparedSynthesisTypes prepared
      [(KStar, Declaration.valueType signature)]
    case result of
      [ty] -> Right (Declaration.valueName signature, ty)
      _ -> Left "source signature elaboration changed its singleton shape"

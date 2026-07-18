{-# OPTIONS_HADDOCK not-home #-}

-- | Private ownership of the checked Djinn session invariant.
--
-- A session retains the authoritative shared inventory together with the
-- exact prepared proof-search indexes projected from it. Compatibility edits
-- always reseal both views transactionally before publishing a replacement.
module Language.Haskell.Djex.Djinn.Internal.Session
  ( DjinnSession
  , DjinnEnvironment
  , DjinnInventory
  , DjinnDeclarationSnapshot
  , mkDjinnSession
  , standardDjinnSession
  , djinnSessionEnvironment
  , djinnSessionInventory
  , djinnSessionDeclarationSnapshot
  , djinnSnapshotTypeDeclarations
  , djinnSnapshotFunctionDeclarations
  , djinnSnapshotClassDeclarations
  , declareDjinnDeclaration
  , removeDjinnDeclaration
  , resolveDjinnInstanceMethods
  , sessionPreparedEnvironment
  ) where

import Data.Bifunctor (first)
import Data.Void (Void)

import Djinn.Core
  ( Declaration
  , PreparedEnvironment
  , preparedEnvironmentInventory
  , preparedEnvironmentSource
  , resolvePreparedInstanceMethods
  )
import qualified Djinn.Core as Core
import qualified Djinn.Internal.Environment as RawEnvironment
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , contextualDiagnostic
  , shownErrorDiagnostic
  )
import Language.Haskell.Synthesis.Environment (Environment)
import qualified Language.Haskell.Synthesis.Environment as SharedEnvironment
import Language.Haskell.Synthesis.Inventory (Inventory)
import qualified Language.Haskell.Synthesis.Inventory as SharedInventory

-- | The neutral declaration environment accepted by the Djinn adapter.
-- Djinn uses textual source variables, while explicit declaration kinds are
-- ground at this stable boundary just as they are for Exference. Historical
-- @KVar@ values remain confined to the raw compatibility API.
type DjinnEnvironment = Environment String Void ()

-- | The checked neutral inventory sealed into a Djinn session.
type DjinnInventory = Inventory String ()

-- | One prepared shared inventory and the exact backend indexes projected
-- from it. The constructor is private; compatibility editing derives its
-- neutral source view on demand and publishes a replacement only after the
-- complete environment has been sealed transactionally.
newtype DjinnSession = DjinnSession PreparedEnvironment

-- | One coherent projection of the declaration tables retained for Djinn's
-- compatibility frontend. The constructor is private so the stable adapter
-- never exposes the historical raw 'Core.Environment'.
data DjinnDeclarationSnapshot = DjinnDeclarationSnapshot
  { snapshotTypeDeclarations
      :: [(Core.HSymbol, ([Core.HSymbol], Core.HType, Core.HKind))]
  , snapshotFunctionDeclarations
      :: [(Core.HSymbol, Core.HType)]
  , snapshotClassDeclarations
      :: [(Core.HSymbol,
          ([(Core.HSymbol, Core.HKind)], [(Core.HSymbol, Core.HType)]))]
  }

-- | Lower a shared declaration environment through Djinn's stricter lexical,
-- dependency, and kind checks, then seal it into a reusable session.
mkDjinnSession :: DjinnEnvironment -> Either Diagnostic DjinnSession
mkDjinnSession sharedEnvironment = DjinnSession <$>
  first environmentFailure
    (RawEnvironment.prepareGroundSynthesisEnvironment sharedEnvironment)

-- | The historical checked Djinn prelude. Its raw declaration spelling is a
-- compatibility source only: convert it once and use the same neutral session
-- constructor as every other caller.
standardDjinnSession :: Either Diagnostic DjinnSession
standardDjinnSession = do
  compatibilityEnvironment <- first environmentFailure
    $ Core.toSynthesisEnvironment Core.standardEnvironment
  environment <- first
    (environmentFailure . Core.InvalidSynthesisInventory
      . SharedInventory.UngroundedInventoryKind)
    $ SharedEnvironment.groundEnvironmentKinds compatibilityEnvironment
  mkDjinnSession environment

-- | Recover the exact neutral declaration environment sealed into a session.
djinnSessionEnvironment :: DjinnSession -> DjinnEnvironment
djinnSessionEnvironment =
  SharedInventory.inventoryEnvironment
    . djinnSessionInventory

-- | Recover the checked inventory authoritative for every session projection.
djinnSessionInventory :: DjinnSession -> DjinnInventory
djinnSessionInventory (DjinnSession prepared) =
  preparedEnvironmentInventory prepared

-- | Reconstruct all historical declaration tables from the authoritative
-- shared inventory in one compatibility projection. Bind this snapshot when
-- several views are needed together: reconstructing the raw source traverses
-- and validates the complete sealed declaration stream.
djinnSessionDeclarationSnapshot
  :: DjinnSession
  -> DjinnDeclarationSnapshot
djinnSessionDeclarationSnapshot (DjinnSession prepared) =
  let compatibilityEnvironment = preparedEnvironmentSource prepared
  in DjinnDeclarationSnapshot
      { snapshotTypeDeclarations =
          Core.typeDeclarations compatibilityEnvironment
      , snapshotFunctionDeclarations =
          Core.functionDeclarations compatibilityEnvironment
      , snapshotClassDeclarations =
          Core.classDeclarations compatibilityEnvironment
      }

-- | Project the historical type-declaration table from one coherent snapshot.
djinnSnapshotTypeDeclarations
  :: DjinnDeclarationSnapshot
  -> [(Core.HSymbol, ([Core.HSymbol], Core.HType, Core.HKind))]
djinnSnapshotTypeDeclarations = snapshotTypeDeclarations

-- | Project the historical value-declaration table from one coherent snapshot.
djinnSnapshotFunctionDeclarations
  :: DjinnDeclarationSnapshot
  -> [(Core.HSymbol, Core.HType)]
djinnSnapshotFunctionDeclarations = snapshotFunctionDeclarations

-- | Project the historical class-declaration table from one coherent snapshot.
djinnSnapshotClassDeclarations
  :: DjinnDeclarationSnapshot
  -> [(Core.HSymbol,
      ([(Core.HSymbol, Core.HKind)], [(Core.HSymbol, Core.HType)]))]
djinnSnapshotClassDeclarations = snapshotClassDeclarations

-- | Apply one historical declaration to the authoritative shared environment
-- and publish the replacement session only after complete validation. The
-- sealed ground environment is edited directly: kinds are never weakened
-- merely to be re-grounded during resealing.
declareDjinnDeclaration
  :: Declaration
  -> DjinnSession
  -> Either Diagnostic DjinnSession
declareDjinnDeclaration declaration session = do
  (_, prepared) <- first environmentEditFailure
    $ RawEnvironment.declareGroundSynthesisEnvironment declaration
    $ djinnSessionEnvironment session
  pure $ DjinnSession prepared

-- | Transactional counterpart of the historical @:delete@ command.
removeDjinnDeclaration
  :: String
  -> DjinnSession
  -> Either Diagnostic DjinnSession
removeDjinnDeclaration name session = do
  (_, prepared) <- first environmentEditFailure
    $ RawEnvironment.removeGroundSynthesisDeclaration name
    $ djinnSessionEnvironment session
  pure $ DjinnSession prepared

-- | Check one historical instance head and instantiate its declared methods.
resolveDjinnInstanceMethods
  :: DjinnSession
  -> [Core.Context]
  -> Core.Context
  -> Either Diagnostic [(Core.HSymbol, Core.HType)]
resolveDjinnInstanceMethods (DjinnSession prepared) prerequisites target =
  first instanceResolutionFailure
    $ resolvePreparedInstanceMethods prepared prerequisites target

-- | Recover the prepared core projection for the facade's query worker. This
-- accessor stays in the private module and is never re-exported by the stable
-- adapter, so callers cannot combine a session with unrelated raw indexes.
sessionPreparedEnvironment :: DjinnSession -> PreparedEnvironment
sessionPreparedEnvironment (DjinnSession prepared) = prepared

environmentFailure
  :: Core.SynthesisEnvironmentError
  -> Diagnostic
environmentFailure = shownErrorDiagnostic "DJEX_DJINN_ENV"
  "cannot lower the shared environment to Djinn"

-- The legacy wording lives in the core's single edit-failure renderer, so
-- the raw string API and these structured diagnostics cannot drift apart.
environmentEditFailure
  :: Core.SynthesisEnvironmentError
  -> Diagnostic
environmentEditFailure = contextualDiagnostic Error
  "DJEX_DJINN_ENV" "cannot update the shared Djinn environment"
  . Core.renderEnvironmentEditFailure

instanceResolutionFailure :: String -> Diagnostic
instanceResolutionFailure = contextualDiagnostic Error
  "DJEX_DJINN_QUERY" "Djinn rejected the instance context"

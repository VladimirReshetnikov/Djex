-- | Capture-safe matching of context-free leading forall schemes.
--
-- The actual type is never unified or solved. Only the source's complete
-- leading binder prefix may receive inferred selections, including a whole
-- impredicative type. Nested binders are paired before their bodies are
-- compared, so an outer selection cannot capture a nested forall variable.
module Language.Haskell.Synthesis.TypeInstantiation
  ( ContextFreeSchemeMatchError (..)
  , ContextFreeSchemeMatch
  , ContextFreeSchemeSelection
  , matchContextFreeScheme
  , contextFreeSchemeSelections
  , contextFreeSchemeSelectionFreeVariables
  , contextFreeSchemeSelectionVariable
  ) where

import Language.Haskell.Synthesis.Internal.TypeInstantiation

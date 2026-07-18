{-# LANGUAGE DeriveGeneric #-}

-- | One qualification policy for every generated Haskell surface.
--
-- The generated-term and type renderers deliberately share these helpers.
-- Keeping the identifier/operator distinction here prevents a candidate and
-- its residual constraints from spelling the same global name differently.
module Language.Haskell.Synthesis.Qualification
  ( Qualification (..)
  , renderNamePrefix
  , renderNameInfix
  , emittedNameModule
  , emittedIdentifier
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import Language.Haskell.Synthesis.Name
  ( LexicalClass (VariableLike)
  , ModuleName
  , Name
  , Occurrence (..)
  , SpecialName (ConsConstructor, FunctionConstructor)
  , nameModule
  , nameOccurrence
  , renderModuleName
  , renderPrefix
  )

-- | How module qualifiers are emitted.
--
-- 'QualifyIdentifiers' matches Exference's middle policy: ordinary names keep
-- their modules, while symbolic names remain unqualified.
data Qualification
  = Unqualified
  | QualifyIdentifiers
  | FullyQualified
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData Qualification

-- | Render a global name in a prefix position under a qualification policy.
renderNamePrefix :: Qualification -> Name -> String
renderNamePrefix qualification name = case nameOccurrence name of
  IdentifierOccurrence _ spelling -> qualify spelling
  OperatorOccurrence _ spelling -> "(" ++ qualify spelling ++ ")"
  SpecialOccurrence _ -> renderPrefix name
 where
  qualify spelling = maybe "" ((++ ".") . renderModuleName)
      (emittedNameModule qualification name)
    ++ spelling

-- | Render a global name in an infix position under a qualification policy.
renderNameInfix :: Qualification -> Name -> String
renderNameInfix qualification name = case nameOccurrence name of
  IdentifierOccurrence _ spelling -> "`" ++ qualify spelling ++ "`"
  OperatorOccurrence _ spelling -> qualify spelling
  SpecialOccurrence ConsConstructor -> ":"
  SpecialOccurrence FunctionConstructor -> "->"
  SpecialOccurrence _ -> renderPrefix name
 where
  qualify spelling = maybe "" ((++ ".") . renderModuleName)
      (emittedNameModule qualification name)
    ++ spelling

-- | Module emitted for an ordinary name under the supplied policy.
-- Structural built-ins never carry modules.  In the middle policy only
-- symbolic operators lose their qualifier.
emittedNameModule :: Qualification -> Name -> Maybe ModuleName
emittedNameModule qualification name = case qualification of
  Unqualified -> Nothing
  QualifyIdentifiers -> case nameOccurrence name of
    OperatorOccurrence{} -> Nothing
    _ -> nameModule name
  FullyQualified -> nameModule name

-- | Recover an emitted unqualified variable identifier, when the policy makes
-- the global spelling eligible to conflict with a generated local name.
emittedIdentifier :: Qualification -> Name -> Maybe String
emittedIdentifier qualification name = case nameOccurrence name of
  IdentifierOccurrence VariableLike spelling
    | emittedNameModule qualification name == Nothing -> Just spelling
  _ -> Nothing

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Exference's compatibility view of the shared synthesis name model.
--
-- The engine now stores the shared 'Shared.Name' directly. Older constructor
-- spellings remain as separately exported patterns for source migration;
-- input-bearing ordinary and tuple patterns are match-only because their
-- builders cannot report validation failures. Construct names with the
-- checked functions below.
module Language.Haskell.Exference.Core.Name
  ( QualifiedName
  , pattern QualifiedName
  , pattern ListCon
  , pattern TupleCon
  , pattern UnboxedTupleCon
  , pattern Cons
  , QualifiedNameError (..)
  , mkQualifiedName
  , mkBoxedTupleName
  , qualifiedNameModule
  , qualifiedNameOccurrence
  , qualifiedNameOperator
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Name as Shared

-- | Exference's historical name alias is now the shared nominal identity.
--
-- The checked builders below retain the historical separated-source API, but
-- the search engine no longer stores or compares a second wrapper around every
-- shared name.
type QualifiedName = Shared.Name

-- | A checked source spelling or tuple arity was not a valid Haskell name.
data QualifiedNameError
  = InvalidQualifiedName Shared.NameError
  deriving (Eq, Show, Generic)

instance NFData QualifiedNameError

-- | Historical ordinary-name view.  The payload of an operator is bare in
-- the view even when the legacy builder was given parenthesized syntax.
-- The function constructor retains its old @QualifiedName [] "->"@ view.
pattern QualifiedName :: [String] -> String -> QualifiedName
pattern QualifiedName modules spelling <- (ordinaryNameView -> Just (modules, spelling))

-- | Historical structural list-constructor view.
pattern ListCon :: QualifiedName
pattern ListCon <- (specialNameView -> Just Shared.ListConstructor)
  where
    ListCon = Shared.listName

-- | Historical boxed-tuple view. Construction is intentionally unavailable
-- through this pattern because invalid arities require structured rejection;
-- use 'mkBoxedTupleName' for values.
pattern TupleCon :: Int -> QualifiedName
pattern TupleCon arity <- (specialNameView -> Just (Shared.TupleConstructor Shared.Boxed arity))

-- | Structural unboxed-tuple view admitted by the shared name domain.
pattern UnboxedTupleCon :: Int -> QualifiedName
pattern UnboxedTupleCon arity <-
  (specialNameView -> Just (Shared.TupleConstructor Shared.Unboxed arity))

-- | Historical structural list-cons view.
pattern Cons :: QualifiedName
pattern Cons <- (specialNameView -> Just Shared.ConsConstructor)
  where
    Cons = Shared.consName

{-# COMPLETE QualifiedName, ListCon, TupleCon, UnboxedTupleCon, Cons #-}

-- | Construct an ordinary identifier or operator from its separated legacy
-- components.  A parenthesized operator payload is accepted and normalized to
-- a bare occurrence.  The unqualified spellings @"->"@ and @"(->)"@ denote
-- the shared function constructor, matching Exference's historical view.
mkQualifiedName
  :: [String]
  -> String
  -> Either QualifiedNameError QualifiedName
mkQualifiedName [] "->" = Right Shared.functionName
mkQualifiedName [] "(->)" = Right Shared.functionName
mkQualifiedName modules source =
  case legacyOperatorPayload source of
    Just spelling -> qualifyOperator modules spelling
    Nothing -> case Shared.mkIdentifier source of
      Right _ -> qualifyIdentifier modules source
      Left identifierError -> case qualifyOperator modules source of
        Right name -> Right name
        Left operatorError
          | not (null source) && all Shared.isOperatorCharacter source ->
              Left operatorError
          | otherwise -> Left $ InvalidQualifiedName identifierError
  where
    qualifyIdentifier [] spelling =
      mapLeft InvalidQualifiedName $ Shared.mkIdentifier spelling
    qualifyIdentifier segments spelling = do
      qualifier <- mapLeft InvalidQualifiedName
        $ Shared.mkModuleNameSegments segments
      mapLeft InvalidQualifiedName
        $ Shared.mkQualifiedIdentifier qualifier spelling

    qualifyOperator [] spelling =
      mapLeft InvalidQualifiedName $ Shared.mkOperator spelling
    qualifyOperator segments spelling = do
      qualifier <- mapLeft InvalidQualifiedName
        $ Shared.mkModuleNameSegments segments
      mapLeft InvalidQualifiedName
        $ Shared.mkQualifiedOperator qualifier spelling

    -- Only the historical, exact parenthesized operator payload is accepted.
    -- Shared.parseName intentionally accepts whitespace and contextual infix
    -- syntax as well, but those are not valid values for this separated API.
    legacyOperatorPayload value = case value of
      '(' : rest -> case reverse rest of
        ')' : reversed -> case Shared.mkOperator (reverse reversed) of
          Right _ -> Just $ reverse reversed
          Left _ -> Nothing
        _ -> Nothing
      _ -> Nothing

-- | Construct a boxed tuple constructor.  Exference accepts unit (arity zero)
-- and ordinary tuples (arity at least two), but not singleton or negative
-- arities.
mkBoxedTupleName :: Int -> Either QualifiedNameError QualifiedName
mkBoxedTupleName arity =
  mapLeft InvalidQualifiedName $ Shared.tupleName Shared.Boxed arity

qualifiedNameModule :: QualifiedName -> Maybe Shared.ModuleName
qualifiedNameModule = Shared.nameModule

qualifiedNameOccurrence :: QualifiedName -> Shared.Occurrence
qualifiedNameOccurrence = Shared.nameOccurrence

-- | Return the bare spelling of an ordinary symbolic occurrence.
qualifiedNameOperator :: QualifiedName -> Maybe String
qualifiedNameOperator name = case qualifiedNameOccurrence name of
  Shared.OperatorOccurrence _ spelling -> Just spelling
  Shared.SpecialOccurrence Shared.FunctionConstructor -> Just "->"
  _ -> Nothing

ordinaryNameView :: QualifiedName -> Maybe ([String], String)
ordinaryNameView name = case qualifiedNameOccurrence name of
  Shared.IdentifierOccurrence _ spelling ->
    Just (moduleSegments name, spelling)
  Shared.OperatorOccurrence _ spelling ->
    Just (moduleSegments name, spelling)
  Shared.SpecialOccurrence Shared.FunctionConstructor -> Just ([], "->")
  Shared.SpecialOccurrence _ -> Nothing

specialNameView :: QualifiedName -> Maybe Shared.SpecialName
specialNameView = Shared.nameSpecial

moduleSegments :: QualifiedName -> [String]
moduleSegments = maybe [] Shared.moduleNameSegments . qualifiedNameModule

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft transform = either (Left . transform) Right

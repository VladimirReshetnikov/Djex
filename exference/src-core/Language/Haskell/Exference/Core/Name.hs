{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Exference's compatibility view of the shared synthesis name model.
--
-- The representation is deliberately opaque.  Older clients may continue to
-- import @QualifiedName(..)@ and use the four historical constructors through
-- the bundled pattern synonyms when inspecting values. Ordinary names and
-- tuples are match-only because their input-bearing builders cannot report
-- validation failures; construct them with the checked functions below.
module Language.Haskell.Exference.Core.Name
  ( QualifiedName (QualifiedName, ListCon, TupleCon, Cons)
  , QualifiedNameError (..)
  , mkQualifiedName
  , mkBoxedTupleName
  , fromSynthesisName
  , toSynthesisName
  , qualifiedNameModule
  , qualifiedNameOccurrence
  , qualifiedNameOperator
  )
where

import Control.DeepSeq (NFData (rnf))
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Name as Shared

-- | A validated name accepted by Exference.
--
-- Exference has no representation for unboxed tuples, so this wrapper is a
-- proper subset of the shared 'Shared.Name' domain.
newtype QualifiedName = QualifiedName_ Shared.Name
  deriving (Eq, Ord)

-- | A checked conversion failed either because the spelling was not a valid
-- Haskell name or because it denotes syntax that Exference cannot represent.
data QualifiedNameError
  = InvalidQualifiedName Shared.NameError
  | UnsupportedSpecialName Shared.SpecialName
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
    ListCon = QualifiedName_ Shared.listName

-- | Historical boxed-tuple view. Construction is intentionally unavailable
-- through this pattern because invalid arities require structured rejection;
-- use 'mkBoxedTupleName' for values.
pattern TupleCon :: Int -> QualifiedName
pattern TupleCon arity <- (specialNameView -> Just (Shared.TupleConstructor Shared.Boxed arity))

-- | Historical structural list-cons view.
pattern Cons :: QualifiedName
pattern Cons <- (specialNameView -> Just Shared.ConsConstructor)
  where
    Cons = QualifiedName_ Shared.consName

{-# COMPLETE QualifiedName, ListCon, TupleCon, Cons #-}

-- | Construct an ordinary identifier or operator from its separated legacy
-- components.  A parenthesized operator payload is accepted and normalized to
-- a bare occurrence.  The unqualified spellings @"->"@ and @"(->)"@ denote
-- the shared function constructor, matching Exference's historical view.
mkQualifiedName
  :: [String]
  -> String
  -> Either QualifiedNameError QualifiedName
mkQualifiedName [] "->" = Right $ QualifiedName_ Shared.functionName
mkQualifiedName [] "(->)" = Right $ QualifiedName_ Shared.functionName
mkQualifiedName modules source = do
  shared <- case legacyOperatorPayload source of
    Just spelling -> qualifyOperator modules spelling
    Nothing -> case Shared.mkIdentifier source of
      Right _ -> qualifyIdentifier modules source
      Left identifierError -> case qualifyOperator modules source of
        Right name -> Right name
        Left operatorError
          | not (null source) && all Shared.isOperatorCharacter source ->
              Left operatorError
          | otherwise -> Left $ InvalidQualifiedName identifierError
  fromSynthesisName shared
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
mkBoxedTupleName arity = do
  name <- mapLeft InvalidQualifiedName $ Shared.tupleName Shared.Boxed arity
  fromSynthesisName name

-- | Narrow a shared name to Exference's representable subset.
fromSynthesisName
  :: Shared.Name
  -> Either QualifiedNameError QualifiedName
fromSynthesisName name = case Shared.nameSpecial name of
  Just special@(Shared.TupleConstructor Shared.Unboxed _) ->
    Left $ UnsupportedSpecialName special
  _ -> Right $ QualifiedName_ name

-- | Recover the shared, parser-independent representation.
toSynthesisName :: QualifiedName -> Shared.Name
toSynthesisName (QualifiedName_ name) = name

qualifiedNameModule :: QualifiedName -> Maybe Shared.ModuleName
qualifiedNameModule = Shared.nameModule . toSynthesisName

qualifiedNameOccurrence :: QualifiedName -> Shared.Occurrence
qualifiedNameOccurrence = Shared.nameOccurrence . toSynthesisName

-- | Return the bare spelling of an ordinary symbolic occurrence.
qualifiedNameOperator :: QualifiedName -> Maybe String
qualifiedNameOperator name = case qualifiedNameOccurrence name of
  Shared.OperatorOccurrence _ spelling -> Just spelling
  Shared.SpecialOccurrence Shared.FunctionConstructor -> Just "->"
  _ -> Nothing

instance Show QualifiedName where
  show = Shared.renderCanonical . toSynthesisName

instance NFData QualifiedName where
  rnf = rnf . toSynthesisName

ordinaryNameView :: QualifiedName -> Maybe ([String], String)
ordinaryNameView name = case qualifiedNameOccurrence name of
  Shared.IdentifierOccurrence _ spelling ->
    Just (moduleSegments name, spelling)
  Shared.OperatorOccurrence _ spelling ->
    Just (moduleSegments name, spelling)
  Shared.SpecialOccurrence Shared.FunctionConstructor -> Just ([], "->")
  Shared.SpecialOccurrence _ -> Nothing

specialNameView :: QualifiedName -> Maybe Shared.SpecialName
specialNameView = Shared.nameSpecial . toSynthesisName

moduleSegments :: QualifiedName -> [String]
moduleSegments = maybe [] Shared.moduleNameSegments . qualifiedNameModule

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft transform = either (Left . transform) Right

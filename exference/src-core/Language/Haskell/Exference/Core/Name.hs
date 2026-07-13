{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Exference's compatibility view of the shared synthesis name model.
--
-- The representation is deliberately opaque.  Older clients may continue to
-- import @QualifiedName(..)@ and use the four historical constructors through
-- the bundled pattern synonyms, while new code should use the checked smart
-- constructors below.  In particular, the legacy builders report invalid
-- input with 'error' because a bidirectional pattern synonym cannot return an
-- 'Either'; Exference itself never uses those builders.
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
import Data.Data
  ( Constr
  , Data (..)
  , DataType
  , Fixity (Prefix)
  , constrIndex
  , mkConstr
  , mkDataType
  )
import GHC.Generics (Generic)
import qualified Language.Haskell.Synthesis.Name as Shared

-- | A validated name accepted by Exference.
--
-- Exference has no representation for unboxed tuples, so this wrapper is a
-- proper subset of the shared 'Shared.Name' domain.
newtype QualifiedName = QualifiedName_ Shared.Name
  deriving (Eq, Ord, Generic)

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
  where
    QualifiedName modules spelling = legacyQualifiedName modules spelling

-- | Historical structural list-constructor view.
pattern ListCon :: QualifiedName
pattern ListCon <- (specialNameView -> Just Shared.ListConstructor)
  where
    ListCon = QualifiedName_ Shared.listName

-- | Historical boxed-tuple view.  Invalid arities fail in the legacy builder;
-- use 'mkBoxedTupleName' when the arity is not statically known.
pattern TupleCon :: Int -> QualifiedName
pattern TupleCon arity <- (specialNameView -> Just (Shared.TupleConstructor Shared.Boxed arity))
  where
    TupleCon arity = legacyBoxedTupleName arity

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

-- Preserve the old four-constructor reflection surface.  'Data' cannot encode
-- checked construction, so 'gunfold' necessarily uses the compatibility
-- builders; malformed generic input fails rather than creating an invalid
-- shared name.  Ordinary observation ('toConstr', 'gfoldl', 'gmapQ', ...) is
-- total and retains the historical constructor names and fields.
instance Data QualifiedName where
  gfoldl apply seed name = case ordinaryNameView name of
    Just (modules, spelling) ->
      seed legacyQualifiedName `apply` modules `apply` spelling
    Nothing -> case specialNameView name of
      Just Shared.ListConstructor -> seed ListCon
      Just (Shared.TupleConstructor Shared.Boxed arity) ->
        seed legacyBoxedTupleName `apply` arity
      Just Shared.ConsConstructor -> seed Cons
      -- The function constructor deliberately has the legacy ordinary view.
      Just Shared.FunctionConstructor ->
        seed legacyQualifiedName `apply` ([] :: [String]) `apply` ("->" :: String)
      Just (Shared.TupleConstructor Shared.Unboxed _) ->
        error "Exference QualifiedName contains an unboxed tuple"
      Nothing -> error "Exference QualifiedName has no legacy Data view"

  gunfold apply seed constructor = case constrIndex constructor of
    1 -> apply $ apply $ seed legacyQualifiedName
    2 -> seed ListCon
    3 -> apply $ seed legacyBoxedTupleName
    4 -> seed Cons
    index -> error $ "invalid QualifiedName constructor index " ++ show index

  toConstr name = case ordinaryNameView name of
    Just _ -> qualifiedNameConstr
    Nothing -> case specialNameView name of
      Just Shared.ListConstructor -> listConConstr
      Just (Shared.TupleConstructor Shared.Boxed _) -> tupleConConstr
      Just Shared.ConsConstructor -> consConstr
      Just Shared.FunctionConstructor -> qualifiedNameConstr
      Just (Shared.TupleConstructor Shared.Unboxed _) ->
        error "Exference QualifiedName contains an unboxed tuple"
      Nothing -> error "Exference QualifiedName has no legacy Data constructor"

  dataTypeOf _ = qualifiedNameDataType

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

legacyQualifiedName :: [String] -> String -> QualifiedName
legacyQualifiedName modules spelling = either
  (error . ("invalid legacy QualifiedName: " ++) . show)
  id
  (mkQualifiedName modules spelling)

legacyBoxedTupleName :: Int -> QualifiedName
legacyBoxedTupleName arity = either
  (error . ("invalid legacy TupleCon: " ++) . show)
  id
  (mkBoxedTupleName arity)

mapLeft :: (left -> other) -> Either left right -> Either other right
mapLeft transform = either (Left . transform) Right

qualifiedNameDataType :: DataType
qualifiedNameDataType = dataType
  where
    dataType = mkDataType
      -- Keep the reflection identity of the original declaration, which
      -- lived in Core.Types before this representation was extracted.
      "Language.Haskell.Exference.Core.Types.QualifiedName"
      [ qualifiedNameConstr
      , listConConstr
      , tupleConConstr
      , consConstr
      ]

qualifiedNameConstr, listConConstr, tupleConConstr, consConstr :: Constr
qualifiedNameConstr = mkConstr qualifiedNameDataType "QualifiedName" [] Prefix
listConConstr = mkConstr qualifiedNameDataType "ListCon" [] Prefix
tupleConConstr = mkConstr qualifiedNameDataType "TupleCon" [] Prefix
consConstr = mkConstr qualifiedNameDataType "Cons" [] Prefix

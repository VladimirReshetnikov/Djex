{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}

module Language.Haskell.Exference.TypeFromHaskellSrc
  ( ConvData
  , emptyConvData
  , convDataFromTypeVarIndex
  , convDataTypeVarIndex
  , convDataReservedIds
  , ConversionT
  , runConversionT
  , runConversionTWithState
  , convertTypeNoDecl
  , convertTypeNoDeclInternal
  , normalizeConvertedForalls
  , convertName
  , convertQName
  , convertModuleName
  , getVar
  -- , ConversionMonad
  , parseQualifiedName
  , tyVarTransform
  , convertConstraint
  , validateConstraintArity
  , haskellSrcExtsParseMode
  , findInvalidNames
  )
where



import Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Parser as P
import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo )

import qualified Language.Haskell.Exference.Core.Types as T
import qualified Language.Haskell.Exference.Core.TypeUtils as TypeUtils
import qualified Language.Haskell.Djex.Exference.Internal.Frontend as Frontend
import Language.Haskell.Exference.Diagnostic
import Language.Haskell.Exference.HaskellSrcUtils
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IntSet
import qualified Data.Set as S

import Data.Maybe ( fromMaybe )
import Data.List ( intercalate )
import System.FilePath ( (<.>), takeExtension )

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
  ( StateT
  , evalStateT
  , get
  , put
  , runStateT
  )
import Control.Monad.Trans.Except
import qualified Language.Haskell.Synthesis.Name as SharedName

import Language.Haskell.Exts.Extension ( Language (..)
                                       , Extension (..)
                                       , KnownExtension (..) )




-- | The private type-variable inventory of one source-conversion scope.
--
-- Source spellings are only rendering hints: several spellings may name one
-- ID, while alpha-renamed lexical binders deliberately have no unambiguous
-- spelling. The exact reserved set therefore remains separate from the hint
-- map. Keeping this representation opaque prevents callers from forging a
-- colliding next-ID cursor.
data ConvData = ConvData
  { conversionTypeVarIndex :: !T.TypeVarIndex
  , conversionReservedIds :: !IntSet.IntSet
  }

-- | An empty, collision-free source-conversion inventory.
emptyConvData :: ConvData
emptyConvData = convDataFromTypeVarIndex M.empty

-- | Start an inventory from compatibility spelling hints.
--
-- Duplicate values are intentional aliases, not an error. Every referenced
-- ID is reserved exactly once, including sparse, negative, and boundary IDs.
convDataFromTypeVarIndex :: T.TypeVarIndex -> ConvData
convDataFromTypeVarIndex variables = ConvData variables
  $ IntSet.fromList $ M.elems variables

-- | Recover the spelling hints accumulated by a conversion.
convDataTypeVarIndex :: ConvData -> T.TypeVarIndex
convDataTypeVarIndex = conversionTypeVarIndex

-- | Inspect the complete read-only inventory, including alpha-renamed IDs
-- with no hint. Use the smart constructors to create conversion state.
convDataReservedIds :: ConvData -> IntSet.IntSet
convDataReservedIds = conversionReservedIds

-- | A source-conversion action with one private type-variable inventory.
-- Keeping errors inside the state transformer means a caught failure retains
-- allocations made before it failed; several historical conversion passes
-- rely on that behavior when they recover and continue in the same scope.
type ConversionT error m result = ExceptT error (StateT ConvData m) result

-- | Run one isolated source-conversion scope, discarding its private variable
-- inventory.  Keeping 'ExceptT' inside 'StateT' preserves the historical
-- behavior: a caught conversion failure does not roll back allocations made
-- before the failure.
runConversionT
  :: Monad m
  => ConvData
  -> ConversionT error m result
  -> ExceptT error m result
runConversionT initial action = ExceptT
  $ evalStateT (runExceptT action) initial

-- | Run an isolated source-conversion scope and retain its final inventory on
-- success.  A failed action also produces a final internal state, but the
-- public 'Either' result deliberately hides it.
runConversionTWithState
  :: Monad m
  => ConvData
  -> ConversionT error m result
  -> ExceptT error m (result, ConvData)
runConversionTWithState initial action = ExceptT $ do
  (result, finalState) <- runStateT (runExceptT action) initial
  pure $ fmap (\value -> (value, finalState)) result

haskellSrcExtsParseMode :: String -> P.ParseMode
haskellSrcExtsParseMode sourceName = P.ParseMode parseSourceName
                                      Haskell2010
                                      exts2
                                      False
                                      False
                                      Nothing
                                      False
  where
    -- Historical callers supplied extensionless labels, while module loading
    -- supplies actual paths. Appending only when there is no extension keeps
    -- both forms readable and avoids diagnostics such as @Prelude.hs.hs@.
    parseSourceName
      | null $ takeExtension sourceName = sourceName <.> "hs"
      | otherwise = sourceName
    exts1 = [ TypeOperators
            , ExplicitForAll
            , ExistentialQuantification
            , TypeFamilies
            , FunctionalDependencies
            , FlexibleContexts
            , MultiParamTypeClasses ]
    exts2 = map EnableExtension exts1

-- | Convert one source type in an isolated type-variable namespace.
--
-- The returned 'T.TypeVarIndex' contains rendering hints for source spellings,
-- not the complete namespace. Alpha-renamed binders can reserve identifiers
-- without acquiring an unambiguous spelling, so the index must not be used to
-- resume this conversion. Use 'runConversionTWithState' and retain 'ConvData'
-- when later conversions must share the exact namespace.
convertTypeNoDecl
  :: Monad m
  => M.Map T.QualifiedName T.HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> Type SrcSpanInfo
  -> ExceptT String m (T.HsType, T.TypeVarIndex)
convertTypeNoDecl tcs mn ds t = do
  (converted, finalState) <- runConversionTWithState
    emptyConvData
    (convertTypeNoDeclInternal tcs mn ds t)
  pure (converted, convDataTypeVarIndex finalState)

convertTypeNoDeclInternal
  :: Monad m
  => M.Map T.QualifiedName T.HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo) -- default (for unqualified stuff)
                      -- Nothing uses a broad search for lookups
  -> [T.QualifiedName] -- list of fully qualified data types
                                         -- (to keep things unique)
  -> Type SrcSpanInfo
  -> ConversionT String m T.HsType
convertTypeNoDeclInternal tcs defModuleName ds ty = do
  ambientVariables <- convDataReservedIds <$> lift get
  converted <- helper ty
  normalizeConvertedForalls ambientVariables converted
 where
  helper (TyFun _ a b)      = T.TypeArrow
                              <$> helper a
                              <*> helper b
  helper tuple@(TyTuple _ Boxed ts)
    | length ts >= 2 = do
        tupleName <- either throwE pure $ qualifiedNameResult
          $ T.mkBoxedTupleName (length ts)
        foldl T.TypeApp (T.TypeCons tupleName) <$> mapM helper ts
    | otherwise = throwE $ "invalid boxed tuple arity " ++ show (length ts)
        ++ " in " ++ prettyPrint tuple
  helper tuple@(TyTuple _ Unboxed _)
                            = throwE $ "unsupported unboxed tuple type: "
                              ++ prettyPrint tuple
  helper (TyApp _ a b)      = T.TypeApp
                              <$> helper a
                              <*> helper b
  helper (TyVar _ vname)    = do
                              i <- getVar vname
                              return $ T.TypeVar i
  helper (TyCon _ name)     = T.TypeCons
                          <$> either throwE pure
                                (convertQName defModuleName ds name)
  helper (TyList _ t)       = do
    listName <- either throwE pure $ qualifiedNameResult
      $ T.fromSynthesisName SharedName.listName
    T.TypeApp (T.TypeCons listName) <$> helper t
  helper (TyParen _ t)      = helper t
  helper TyInfix{}        = throwE "infix operator"
  helper TyKind{}         = throwE "kind annotation"
  helper TyPromoted{}     = throwE "promoted type"
  helper (TyForall _ maybeTVars context t) =
    T.TypeForall
      <$> case maybeTVars of
            Nothing -> return []
            Just tvs -> tyVarTransform `mapM` tvs
      <*> convertConstraint tcs defModuleName ds `mapM` contextConstraints context
      <*> helper t
  helper x                = throwE $ "unknown type element: " ++ show x -- TODO

-- HSE's spelling map deliberately remains the compatibility hint index, but
-- one spelling can denote several lexically shadowing binders. Normalize only
-- after raw conversion succeeds, retain the original hints, and reserve the
-- complete returned namespace, including identities with no possible hint.
-- The claimed set must be the ambient namespace captured before converting
-- this type; IDs allocated while converting the type are source occurrences,
-- not enclosing binders.
normalizeConvertedForalls
  :: Monad m
  => IntSet.IntSet
  -> T.HsType
  -> ConversionT String m T.HsType
normalizeConvertedForalls claimed typeExpression = case
    TypeUtils.alphaNormalizeForalls claimed typeExpression of
  Left failure -> throwE $ renderForallNormalizationError failure
  Right (normalized, reserved) -> do
    state <- lift get
    lift $ put state
      { conversionReservedIds = reserved `IntSet.union`
          conversionReservedIds state
      }
    pure normalized

renderForallNormalizationError
  :: TypeUtils.ForallNormalizationError
  -> String
renderForallNormalizationError failure = case failure of
  TypeUtils.DuplicateForallBinder variable ->
    "duplicate explicitly quantified type variable " ++ show variable
  TypeUtils.ForallNormalizationSupplyExhausted ->
    "cannot allocate a fresh explicitly quantified type variable"

-- | Look up or allocate one spelling in the current conversion scope.
--
-- The update happens before returning, so a later error caught in the same
-- 'ConversionT' retains the allocation. Exhaustion itself leaves state
-- unchanged. Boundary IDs use the same gap-finding allocator as the core and
-- therefore never wrap or collide.
getVar :: Monad m => Name SrcSpanInfo -> ConversionT String m T.TVarId
getVar n = do
  state <- lift get
  let key = prettyPrint n
      variables = conversionTypeVarIndex state
  case M.lookup key variables of
    Nothing -> do
      identifier <- maybe
        (throwE "type-variable conversion namespace is exhausted")
        pure
        $ Frontend.allocateFreshTypeVariableId
        $ conversionReservedIds state
      lift $ put state
        { conversionTypeVarIndex = M.insert key identifier variables
        , conversionReservedIds = IntSet.insert identifier
            $ conversionReservedIds state
        }
      pure identifier
    Just i ->
      pure i

-- defaultModule -> potentially-qualified-name-thingy -> exference-q-name
--
-- Unboxed tuples deliberately have no core representation.  Returning an
-- error here prevents them from being confused with boxed tuples at every
-- elaboration site, including constraints and instance heads.
convertQName
  :: Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> QName SrcSpanInfo
  -> Either String T.QualifiedName
convertQName _ _ (Special _ (UnitCon _)) = qualifiedNameResult
  $ T.mkBoxedTupleName 0
convertQName _ _ (Special _ (ListCon _)) = qualifiedNameResult
  $ T.fromSynthesisName SharedName.listName
convertQName _ _ (Special _ (FunCon _)) = qualifiedNameResult
  $ T.fromSynthesisName SharedName.functionName
convertQName _ _ (Special _ special@(TupleCon _ Unboxed _)) = Left
  $ "unsupported unboxed tuple constructor: " ++ prettyPrint special
convertQName _ _ (Special _ special@(TupleCon _ Boxed arity))
  | arity >= 2 = qualifiedNameResult $ T.mkBoxedTupleName arity
  | otherwise = Left $ "invalid boxed tuple constructor arity " ++ show arity
      ++ ": " ++ prettyPrint special
convertQName _ _ (Special _ (Cons _)) = qualifiedNameResult
  $ T.fromSynthesisName SharedName.consName
convertQName _ _ (Special _ special@(UnboxedSingleCon _)) = Left
  $ "unsupported unboxed single constructor: " ++ prettyPrint special
convertQName _ _ (Special _ special@(ExprHole _)) = Left
  $ "unsupported special constructor: " ++ prettyPrint special
convertQName _ _ (Qual _ mn syntaxName) =
  convertModuleName mn syntaxName
convertQName (Just defaultModule) knownNames (UnQual _ syntaxName) = do
  localName <- convertModuleName defaultModule syntaxName
  externalName <- convertName syntaxName
  resolveUnqualifiedName localName externalName knownNames
convertQName Nothing knownNames (UnQual _ syntaxName) = do
  unqualified <- convertName syntaxName
  resolveUnqualifiedName unqualified unqualified knownNames

-- The historical environment modules intentionally omit their imports.  We
-- can still model the useful part of Haskell name lookup without manufacturing
-- recursive placeholder declarations: a declaration in the current module
-- wins, otherwise a unique known occurrence is imported, and multiple matches
-- are rejected as ambiguous.  When no declaration is known, the unqualified
-- spelling is retained as one external identity instead of fabricating a
-- different current-module declaration in every environment file.
resolveUnqualifiedName
  :: T.QualifiedName
  -> T.QualifiedName
  -> [T.QualifiedName]
  -> Either String T.QualifiedName
resolveUnqualifiedName localName externalName knownNames
  | localName `elem` candidates = Right localName
  | otherwise = case candidates of
      [] -> Right externalName
      [candidate] -> Right candidate
      _ -> Left $ "ambiguous unqualified name " ++ show externalName
        ++ "; matches " ++ intercalate ", " (map show candidates)
 where
  candidates = S.toAscList $ S.fromList
        [ candidate
        | candidate <- knownNames
        , T.qualifiedNameOccurrence candidate
            == T.qualifiedNameOccurrence externalName
        ]

-- | Convert an HSE occurrence without trusting its publicly constructible
-- payload. Parsed names are valid, while malformed hand-built syntax remains
-- an ordinary conversion failure rather than an exception.
convertName :: Name SrcSpanInfo -> Either String T.QualifiedName
convertName (Ident _ spelling) = qualifiedNameResult
  $ T.mkQualifiedName [] spelling
convertName (Symbol _ spelling) = qualifiedNameResult
  $ T.mkQualifiedName [] spelling

-- | Qualify an occurrence after validating both the HSE module payload and
-- the occurrence. This is total even for caller-constructed syntax trees.
convertModuleName
  :: ModuleName SrcSpanInfo
  -> Name SrcSpanInfo
  -> Either String T.QualifiedName
convertModuleName (ModuleName _ moduleSource) syntaxName = do
  qualifier <- either (Left . SharedName.renderNameError) Right
    $ SharedName.mkModuleName moduleSource
  let modules = SharedName.moduleNameSegments qualifier
      spelling = case syntaxName of
        Ident _ value -> value
        Symbol _ value -> value
  qualifiedNameResult $ T.mkQualifiedName modules spelling

-- | Parse the external spelling used by rating files.  Operators use the
-- conventional @Module.(<*>)@ form, but their core payload is bare.  Built-in
-- constructors are recovered as their structural 'T.QualifiedName' variants
-- so rating lookup does not depend on rendered-text coincidences.
parseQualifiedName :: String -> Either Diagnostic T.QualifiedName
parseQualifiedName input = do
  shared <- either (invalid . SharedName.renderNameError) Right
    $ SharedName.parseName input
  either (invalid . show) Right $ T.fromSynthesisName shared
  where
    invalid :: String -> Either Diagnostic a
    invalid message = Left $ diagnostic
      $ "invalid qualified name " ++ show input ++ ": " ++ message

convertConstraint
  :: Monad m
  => M.Map T.QualifiedName T.HsTypeClass
  -> Maybe (ModuleName SrcSpanInfo)
  -> [T.QualifiedName]
  -> Asst SrcSpanInfo
  -> ConversionT String m T.HsConstraint
convertConstraint tcs defModuleName ds (TypeA _ classType) = do
  (qname, types) <- maybe
    (throwE $ "invalid class constraint: " ++ prettyPrint classType)
    pure
    (splitClassApplication classType)
  name <- either throwE pure
    $ convertQName defModuleName (M.keys tcs) qname
  parameters <- mapM (convertTypeNoDeclInternal tcs defModuleName ds) types
  either throwE pure $ validateConstraintArity tcs name (length parameters)
  pure $ T.HsConstraint name parameters
convertConstraint env defModuleName ds (ParenA _ c)
  = convertConstraint env defModuleName ds c
convertConstraint _ _ _ c
  = throwE $ "bad constraint: " ++ show c

-- | Reject an application whose class is known but whose number of
-- parameters disagrees with its declaration.  An unknown class stays
-- representable: signatures can legitimately mention imported classes whose
-- declarations were not part of the supplied module set.
validateConstraintArity
  :: M.Map T.QualifiedName T.HsTypeClass
  -> T.QualifiedName
  -> Int
  -> Either String ()
validateConstraintArity classes name actual = case M.lookup name classes of
  Just typeClass
    | expected <- length $ T.tclass_params typeClass
    , expected /= actual -> Left
        $ "wrong number of parameters for type class "
        ++ unqualifiedClassName name
        ++ ": expected " ++ show expected ++ ", got " ++ show actual
  _ -> Right ()
 where
  unqualifiedClassName qualifiedName = fromMaybe (show qualifiedName)
    $ SharedName.nameSpelling
    $ T.toSynthesisName qualifiedName

tyVarTransform :: Monad m
               => TyVarBind SrcSpanInfo
               -> ConversionT String m T.TVarId
tyVarTransform (KindedVar _ _ _) = throwE "kinded type variable"
tyVarTransform (UnkindedVar _ n) = getVar n

findInvalidNames :: [T.QualifiedName] -> T.HsType -> [T.QualifiedName]
findInvalidNames _ T.TypeVar {}          = []
findInvalidNames _ T.TypeConstant {}     = []
findInvalidNames valids (T.TypeCons qn) = case qn of
    n | SharedName.SpecialOccurrence _ <- T.qualifiedNameOccurrence n -> []
      | n `notElem` valids -> [n]
      | otherwise -> []
findInvalidNames valids (T.TypeArrow t1 t2)   =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeApp t1 t2)     =
  findInvalidNames valids t1 ++ findInvalidNames valids t2
findInvalidNames valids (T.TypeForall _ constraints t1) =
  findInvalidNames valids t1
  ++ concatMap (concatMap (findInvalidNames valids) . T.constraint_params) constraints

qualifiedNameResult
  :: Either T.QualifiedNameError T.QualifiedName
  -> Either String T.QualifiedName
qualifiedNameResult = either (Left . show) Right

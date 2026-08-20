-- | Private source-type parsing primitives shared by the compatibility
-- frontend and the Djex REPL. Keeping resolver construction here lets prompt
-- inspection reuse the authoritative Inventory without exposing a parser that
-- temporarily represents class heads as ordinary type constructors.
module Language.Haskell.Exference.Internal.TypeParsing
  ( parseHaskellSrcType
  , parseTypeWithResolver
  , typeResolverFromInventory
  ) where

import Control.Monad.Trans.Except
  ( ExceptT (..)
  , runExceptT
  , throwE
  )
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map

import qualified Language.Haskell.Exts.Parser as HSE
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax (ModuleName, Type)

import Language.Haskell.Exference.Core.Types
  ( HsType
  , TypeVarIndex
  )
import Language.Haskell.Exference.HaskellSrcUtils
  ( withHaskellSrcLocation )
import Language.Haskell.Exference.TypeFromHaskellSrc
  ( TypeResolver (..)
  , convertTypeNoDeclWithResolver
  )
import Language.Haskell.Synthesis.Diagnostic
  ( Diagnostic
  , Severity (Error)
  , diagnostic
  , sourceTextSpan
  , withLocation
  )
import qualified Language.Haskell.Synthesis.Declaration as Declaration
import qualified Language.Haskell.Synthesis.Environment as Environment
import qualified Language.Haskell.Synthesis.Inventory as Inventory

-- | Parse a source type with HSE and hand the syntax tree to a conversion
-- action. A parse failure becomes an error t'Diagnostic' at the HSE
-- location; a conversion failure becomes an error t'Diagnostic' spanning
-- the whole input text in the parse mode's file name.
parseHaskellSrcType
  :: Monad m
  => (Type SrcSpanInfo -> ExceptT String m result)
  -> HSE.ParseMode
  -> String
  -> ExceptT Diagnostic m result
parseHaskellSrcType convert mode source = case
    HSE.parseTypeWithMode mode source of
  HSE.ParseFailed location message -> throwE
    $ withHaskellSrcLocation location
    $ diagnostic Error message
  HSE.ParseOk typeExpression -> ExceptT $ first conversionDiagnostic
    <$> runExceptT (convert typeExpression)
 where
  conversionDiagnostic message =
    withLocation (HSE.parseFilename mode) (sourceTextSpan source)
      $ diagnostic Error message

-- | Parse a source type and convert it nominally with
-- 'convertTypeNoDeclWithResolver', so type synonyms stay unexpanded. The
-- optional module name is the current module: an unqualified name resolves
-- to that module's own visible declaration before any other candidate.
parseTypeWithResolver
  :: Monad m
  => TypeResolver
  -> Maybe (ModuleName SrcSpanInfo)
  -> HSE.ParseMode
  -> String
  -> ExceptT Diagnostic m (HsType, TypeVarIndex)
parseTypeWithResolver resolver currentModule = parseHaskellSrcType
  $ convertTypeNoDeclWithResolver resolver currentModule

-- | Derive precisely the nominal facts needed by source-type lookup. Class
-- methods, superclasses, and backend solver indexes are intentionally absent.
typeResolverFromInventory
  :: Inventory.Inventory typeVariable annotation
  -> TypeResolver
typeResolverFromInventory inventory = TypeResolver
  { resolverTypeNames = Map.keys
      $ Environment.typeDeclarationMap environment
  , resolverClassArities = Map.mapMaybe classArity
      $ Environment.classDeclarationMap environment
  , resolverUnqualifiedTypeNames = Map.keys
      $ Environment.typeDeclarationMap environment
  , resolverUnqualifiedClassNames = Map.keys
      $ Environment.classDeclarationMap environment
  , resolverModuleAliases = []
  , resolverQualifiedNames = Nothing
  }
 where
  environment = Inventory.inventoryEnvironment inventory
  classArity declaration = case declaration of
    Declaration.ClassDeclaration _ _ parameters _ _ ->
      Just $ length parameters
    _ -> Nothing

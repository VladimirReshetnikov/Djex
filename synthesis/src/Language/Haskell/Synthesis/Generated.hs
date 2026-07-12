{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Backend-independent generated Haskell syntax and rendering.
--
-- Search engines retain their own typed or proof-oriented terms.  At the
-- checked output boundary they can erase those private annotations into this
-- small tree, which distinguishes local identities from validated global
-- 'Name's and gives both backends one scope and printing policy.
module Language.Haskell.Synthesis.Generated
  ( Pattern (..)
  , Expression (..)
  , FunctionClause (..)
  , Qualification (..)
  , RenderOptions (..)
  , defaultRenderOptions
  , RenderError (..)
  , ScopeError (..)
  , validateExpressionScope
  , validateFunctionClauseScope
  , allocateLocalNames
  , allocateClauseLocalNames
  , renderExpression
  , renderFunctionClause
  ) where

import Prelude hiding ((<>))

import Control.DeepSeq (NFData)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Haskell.Synthesis.Name
import Text.PrettyPrint.HughesPJ
  ( Doc
  , ($$)
  , (<+>)
  , (<>)
  , comma
  , fsep
  , nest
  , parens
  , punctuate
  , renderStyle
  , sep
  , style
  , text
  , vcat
  )

-- | Patterns supported by both Djinn and Exference's generated output.
-- Constructor application is structural rather than an arbitrary pattern
-- application, so malformed variable-headed patterns are unrepresentable.
data Pattern local
  = Bind local
  | Wildcard
  | Constructor Name [Pattern local]
  | TuplePattern [Pattern local]
  | As local (Pattern local)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (Pattern local)

-- | Surface expression tree after backend-specific checking.
--
-- 'Hole' is retained for Exference's inspectable intermediate candidates; a
-- completed checked result normally contains none.
data Expression local
  = Local local
  | Global Name
  | Lambda [Pattern local] (Expression local)
  | Apply (Expression local) (Expression local)
  | Tuple [Expression local]
  | Hole local
  | Let (Pattern local) (Expression local) (Expression local)
  | Case (Expression local) [(Pattern local, Expression local)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (Expression local)

-- | One ordinary top-level function equation.
data FunctionClause local = FunctionClause
  { clauseName :: Name
  , clausePatterns :: [Pattern local]
  , clauseBody :: Expression local
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (FunctionClause local)

-- | How module qualifiers are emitted.
--
-- 'QualifyIdentifiers' matches Exference's middle policy: ordinary names keep
-- their modules, while symbolic names stay infix-friendly and unqualified.
data Qualification
  = Unqualified
  | QualifyIdentifiers
  | FullyQualified
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData Qualification

-- | Rendering choices that depend on a backend's local identity type.
data RenderOptions local = RenderOptions
  { renderQualification :: Qualification
  , localNamePreference :: local -> String
  , reservedLocalNames :: [String]
  }

defaultRenderOptions :: (local -> String) -> RenderOptions local
defaultRenderOptions preference = RenderOptions
  { renderQualification = FullyQualified
  , localNamePreference = preference
  , reservedLocalNames = []
  }

data RenderError
  = InvalidLocalName String NameError
  | LocalNameIsWildcard
  | InvalidTupleExpressionArity Int
  | InvalidTuplePatternArity Int
  | InvalidConstructorPattern Name
  | InvalidConstructorPatternArity Name Int Int
  | EmptyLambda
  | InvalidFunctionName Name
  deriving (Eq, Ord, Show, Generic)

instance NFData RenderError

data ScopeError local
  = UnboundLocal local
  | DuplicatePatternBinder local
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData local => NFData (ScopeError local)

-- | Check lexical local-variable scope independently of rendering.
validateExpressionScope
  :: Ord local
  => Expression local
  -> Either (ScopeError local) ()
validateExpressionScope = validateExpression Set.empty

validateFunctionClauseScope
  :: Ord local
  => FunctionClause local
  -> Either (ScopeError local) ()
validateFunctionClauseScope (FunctionClause _ patterns body) = do
  binders <- distinctPatternBinders patterns
  validateExpression (Set.fromList binders) body

validateExpression
  :: Ord local
  => Set local
  -> Expression local
  -> Either (ScopeError local) ()
validateExpression bound expression = case expression of
  Local local
    | local `Set.member` bound -> Right ()
    | otherwise -> Left $ UnboundLocal local
  Global{} -> Right ()
  Lambda patterns body -> do
    binders <- distinctPatternBinders patterns
    extended <- extendScope bound binders
    validateExpression extended body
  Apply function argument ->
    validateExpression bound function >> validateExpression bound argument
  Tuple elements -> mapM_ (validateExpression bound) elements
  Hole{} -> Right ()
  Let pattern binding body -> do
    binders <- distinctPatternBinders [pattern]
    validateExpression bound binding
    extended <- extendScope bound binders
    validateExpression extended body
  Case scrutinee alternatives -> do
    validateExpression bound scrutinee
    mapM_ validateAlternative alternatives
    where
      validateAlternative (pattern, body) = do
        binders <- distinctPatternBinders [pattern]
        extended <- extendScope bound binders
        validateExpression extended body

extendScope
  :: Ord local
  => Set local
  -> [local]
  -> Either (ScopeError local) (Set local)
extendScope bound binders = case filter (`Set.member` bound) binders of
  duplicate : _ -> Left $ DuplicatePatternBinder duplicate
  [] -> Right $ bound `Set.union` Set.fromList binders

distinctPatternBinders
  :: Ord local
  => [Pattern local]
  -> Either (ScopeError local) [local]
distinctPatternBinders patterns = collect Set.empty [] $ concatMap binders patterns
  where
    collect _ result [] = Right $ reverse result
    collect seen result (local : rest)
      | local `Set.member` seen = Left $ DuplicatePatternBinder local
      | otherwise = collect (Set.insert local seen) (local : result) rest

    binders pattern = case pattern of
      Bind local -> [local]
      Wildcard -> []
      Constructor _ arguments -> concatMap binders arguments
      TuplePattern elements -> concatMap binders elements
      As local nested -> local : binders nested

-- | Allocate stable, distinct Haskell spellings for every local identity.
-- Global identifiers emitted without qualification and explicit caller
-- reservations participate in the same collision set.
allocateLocalNames
  :: Ord local
  => RenderOptions local
  -> Expression local
  -> Either RenderError (Map local String)
allocateLocalNames options expression =
  allocate options (expressionLocals expression) (expressionGlobals expression)

allocateClauseLocalNames
  :: Ord local
  => RenderOptions local
  -> FunctionClause local
  -> Either RenderError (Map local String)
allocateClauseLocalNames options (FunctionClause name patterns body) =
  allocate options
    (concatMap patternLocals patterns ++ expressionLocals body)
    (name : concatMap patternGlobals patterns ++ expressionGlobals body)

allocate
  :: Ord local
  => RenderOptions local
  -> [local]
  -> [Name]
  -> Either RenderError (Map local String)
allocate options locals globals = fmap fst $ List.foldl' allocateOne initial locals
  where
    initial = Right
      ( Map.empty
      , Set.fromList (reservedLocalNames options)
          `Set.union` Set.fromList
            [ spelling
            | name <- globals
            , Just spelling <- [emittedIdentifier
                (renderQualification options) name]
            ]
      )

    allocateOne state local = do
      (allocated, used) <- state
      case Map.lookup local allocated of
        Just _ -> Right (allocated, used)
        Nothing -> do
          preferred <- validateLocalName $ localNamePreference options local
          let chosen = freshName used preferred
          Right
            ( Map.insert local chosen allocated
            , Set.insert chosen used
            )

validateLocalName :: String -> Either RenderError String
validateLocalName "_" = Left LocalNameIsWildcard
validateLocalName spelling = case mkIdentifier spelling of
  Left nameError -> Left $ InvalidLocalName spelling nameError
  Right name
    | nameLexicalClass name == VariableLike -> Right spelling
    | otherwise -> Left $ InvalidLocalName spelling
        (InvalidIdentifier spelling)

freshName :: Set String -> String -> String
freshName used candidate
  | candidate `Set.member` used = freshName used $ candidate ++ "'"
  | otherwise = candidate

renderExpression
  :: Ord local
  => RenderOptions local
  -> Expression local
  -> Either RenderError String
renderExpression options expression = do
  validateExpressionSyntax expression
  names <- allocateLocalNames options expression
  Right $ renderStyle style $ ppExpression options names 0 expression

renderFunctionClause
  :: Ord local
  => RenderOptions local
  -> FunctionClause local
  -> Either RenderError String
renderFunctionClause options clause@(FunctionClause name patterns body) = do
  validateFunctionName name
  mapM_ validatePatternSyntax patterns
  validateExpressionSyntax body
  names <- allocateClauseLocalNames options clause
  Right $ renderStyle style $ sep
    [ text (renderNamePrefix (renderQualification options) name) <+>
        sep (map (ppPattern options names 10) patterns) <+> text "="
    , nest 2 $ ppExpression options names 0 body
    ]

validateExpressionSyntax :: Expression local -> Either RenderError ()
validateExpressionSyntax expression = case expression of
  Local{} -> Right ()
  Global{} -> Right ()
  Lambda [] _ -> Left EmptyLambda
  Lambda patterns body ->
    mapM_ validatePatternSyntax patterns >> validateExpressionSyntax body
  Apply function argument ->
    validateExpressionSyntax function >> validateExpressionSyntax argument
  Tuple [_] -> Left $ InvalidTupleExpressionArity 1
  Tuple elements -> mapM_ validateExpressionSyntax elements
  Hole{} -> Right ()
  Let pattern binding body ->
    validatePatternSyntax pattern >>
      validateExpressionSyntax binding >>
      validateExpressionSyntax body
  Case scrutinee alternatives ->
    validateExpressionSyntax scrutinee >> mapM_ validateAlternative alternatives
    where
      validateAlternative (pattern, body) =
        validatePatternSyntax pattern >> validateExpressionSyntax body

validatePatternSyntax :: Pattern local -> Either RenderError ()
validatePatternSyntax pattern = case pattern of
  Bind{} -> Right ()
  Wildcard -> Right ()
  Constructor name arguments
    | nameLexicalClass name /= ConstructorLike ->
        Left $ InvalidConstructorPattern name
    | Just FunctionConstructor <- nameSpecial name ->
        Left $ InvalidConstructorPattern name
    | Just (TupleConstructor Unboxed _) <- nameSpecial name ->
        Left $ InvalidConstructorPattern name
    | Just expected <- patternConstructorArity name
    , expected /= length arguments ->
        Left $ InvalidConstructorPatternArity
          name expected (length arguments)
    | otherwise -> mapM_ validatePatternSyntax arguments
  TuplePattern [_] -> Left $ InvalidTuplePatternArity 1
  TuplePattern elements -> mapM_ validatePatternSyntax elements
  As _ nested -> validatePatternSyntax nested

validateFunctionName :: Name -> Either RenderError ()
validateFunctionName name
  | nameModule name == Nothing
  , nameLexicalClass name == VariableLike
  , nameSpelling name /= Just "_" = Right ()
  | otherwise = Left $ InvalidFunctionName name

ppExpression
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Expression local
  -> Doc
ppExpression options names precedence expression = case expression of
  Local local -> text $ localName options names local
  Global name -> text $ renderNamePrefix qualification name
  Lambda patterns body -> parenthesize (precedence > 0) $
    sep
      [ text "\\" <> sep (map (ppPattern options names 10) patterns)
          <+> text "->"
      , nest 2 $ ppExpression options names 0 body
      ]
  application@Apply{} -> ppApplication options names precedence application
  Tuple elements -> parens $ fsep $ punctuate comma
    $ map (ppExpression options names 0) elements
  Hole local -> text $ '_' : localName options names local
  Let pattern binding body -> parenthesize (precedence > 0) $
    sep
      [ text "let" <+> ppPattern options names 0 pattern <+> text "="
          <+> ppExpression options names 0 binding
      , text "in" <+> ppExpression options names 0 body
      ]
  Case scrutinee alternatives -> parenthesize (precedence > 0) $
    (text "case" <+> ppExpression options names 0 scrutinee <+> text "of")
      $$ vcat (map ppAlternative alternatives)
  where
    qualification = renderQualification options
    ppAlternative (pattern, body) =
      ppPattern options names 0 pattern <+> text "->" <+>
        ppExpression options names 0 body

ppApplication
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Expression local
  -> Doc
ppApplication options names precedence expression =
  case applicationSpine expression of
    (Global name, arguments)
      | Just arity <- boxedTupleArity name
      , arity == length arguments ->
          parens $ fsep $ punctuate comma
            $ map (ppExpression options names 0) arguments
    (Global name, [left, right])
      | isExpressionOperator name -> parenthesize (precedence > 4) $
          ppExpression options names 5 left <+>
          text (renderNameInfix (renderQualification options) name) <+>
          ppExpression options names 5 right
    (function, arguments) -> parenthesize (precedence > 11) $
      sep $ ppExpression options names 11 function
        : map (ppExpression options names 12) arguments

applicationSpine :: Expression local -> (Expression local, [Expression local])
applicationSpine = collect []
  where
    collect arguments (Apply function argument) =
      collect (argument : arguments) function
    collect arguments function = (function, arguments)

ppPattern
  :: Ord local
  => RenderOptions local
  -> Map local String
  -> Int
  -> Pattern local
  -> Doc
ppPattern options names precedence pattern = case pattern of
  Bind local -> text $ localName options names local
  Wildcard -> text "_"
  Constructor name arguments
    | Just arity <- boxedTupleArity name
    , arity == length arguments ->
        parens $ fsep $ punctuate comma
          $ map (ppPattern options names 0) arguments
    | [left, right] <- arguments
    , isPatternOperator name -> parenthesize (precedence > 1) $
        ppPattern options names 2 left <+>
        text (renderNameInfix (renderQualification options) name) <+>
        ppPattern options names 2 right
    | otherwise -> parenthesize (precedence > 1 && not (null arguments)) $
        sep $ text (renderNamePrefix (renderQualification options) name)
          : map (ppPattern options names 2) arguments
  TuplePattern elements -> parens $ fsep $ punctuate comma
    $ map (ppPattern options names 0) elements
  As local nested ->
    text (localName options names local) <> text "@" <>
      ppPattern options names 10 nested

parenthesize :: Bool -> Doc -> Doc
parenthesize True = parens
parenthesize False = id

localName :: Ord local => RenderOptions local -> Map local String -> local -> String
localName options names local =
  Map.findWithDefault (localNamePreference options local) local names

boxedTupleArity :: Name -> Maybe Int
boxedTupleArity name = case nameSpecial name of
  Just (TupleConstructor Boxed arity) -> Just arity
  _ -> Nothing

patternConstructorArity :: Name -> Maybe Int
patternConstructorArity name = case nameSpecial name of
  Just ListConstructor -> Just 0
  Just ConsConstructor -> Just 2
  Just FunctionConstructor -> Nothing
  Just (TupleConstructor _ arity) -> Just arity
  Nothing -> Nothing

isExpressionOperator :: Name -> Bool
isExpressionOperator name = case nameOccurrence name of
  OperatorOccurrence _ _ -> True
  SpecialOccurrence ConsConstructor -> True
  _ -> False

isPatternOperator :: Name -> Bool
isPatternOperator name = case nameOccurrence name of
  OperatorOccurrence ConstructorLike _ -> True
  SpecialOccurrence ConsConstructor -> True
  _ -> False

renderNamePrefix :: Qualification -> Name -> String
renderNamePrefix qualification name = case nameOccurrence name of
  IdentifierOccurrence _ spelling -> qualify OrdinaryIdentifier spelling
  OperatorOccurrence _ spelling -> "(" ++ qualify SymbolicOperator spelling ++ ")"
  SpecialOccurrence _ -> renderPrefix name
  where
    qualify form spelling = maybe "" ((++ ".") . renderModuleName)
        (emittedModule qualification form name)
      ++ spelling

renderNameInfix :: Qualification -> Name -> String
renderNameInfix qualification name = case nameOccurrence name of
  IdentifierOccurrence _ spelling ->
    "`" ++ qualify OrdinaryIdentifier spelling ++ "`"
  OperatorOccurrence _ spelling -> qualify SymbolicOperator spelling
  SpecialOccurrence ConsConstructor -> ":"
  SpecialOccurrence FunctionConstructor -> "->"
  SpecialOccurrence _ -> renderPrefix name
  where
    qualify form spelling = maybe "" ((++ ".") . renderModuleName)
        (emittedModule qualification form name)
      ++ spelling

data NameForm = OrdinaryIdentifier | SymbolicOperator
  deriving (Eq)

emittedModule :: Qualification -> NameForm -> Name -> Maybe ModuleName
emittedModule qualification form name = case qualification of
  Unqualified -> Nothing
  QualifyIdentifiers
    | form == SymbolicOperator -> Nothing
    | otherwise -> nameModule name
  FullyQualified -> nameModule name

emittedIdentifier :: Qualification -> Name -> Maybe String
emittedIdentifier qualification name = case nameOccurrence name of
  IdentifierOccurrence VariableLike spelling
    | emittedModule qualification OrdinaryIdentifier name == Nothing ->
        Just spelling
  _ -> Nothing

patternLocals :: Pattern local -> [local]
patternLocals pattern = case pattern of
  Bind local -> [local]
  Wildcard -> []
  Constructor _ arguments -> concatMap patternLocals arguments
  TuplePattern elements -> concatMap patternLocals elements
  As local nested -> local : patternLocals nested

patternGlobals :: Pattern local -> [Name]
patternGlobals pattern = case pattern of
  Bind{} -> []
  Wildcard -> []
  Constructor name arguments -> name : concatMap patternGlobals arguments
  TuplePattern elements -> concatMap patternGlobals elements
  As _ nested -> patternGlobals nested

expressionLocals :: Expression local -> [local]
expressionLocals expression = case expression of
  Local local -> [local]
  Global{} -> []
  Lambda patterns body ->
    concatMap patternLocals patterns ++ expressionLocals body
  Apply function argument ->
    expressionLocals function ++ expressionLocals argument
  Tuple elements -> concatMap expressionLocals elements
  Hole local -> [local]
  Let pattern binding body ->
    patternLocals pattern ++ expressionLocals binding ++ expressionLocals body
  Case scrutinee alternatives ->
    expressionLocals scrutinee ++ concat
      [ patternLocals pattern ++ expressionLocals body
      | (pattern, body) <- alternatives
      ]

expressionGlobals :: Expression local -> [Name]
expressionGlobals expression = case expression of
  Local{} -> []
  Global name -> [name]
  Lambda patterns body ->
    concatMap patternGlobals patterns ++ expressionGlobals body
  Apply function argument ->
    expressionGlobals function ++ expressionGlobals argument
  Tuple elements -> concatMap expressionGlobals elements
  Hole{} -> []
  Let pattern binding body ->
    patternGlobals pattern ++ expressionGlobals binding ++ expressionGlobals body
  Case scrutinee alternatives ->
    expressionGlobals scrutinee ++ concat
      [ patternGlobals pattern ++ expressionGlobals body
      | (pattern, body) <- alternatives
      ]

-- | Compact Haskell-source rendering for shared types and constraints.
--
-- Variable spelling remains a caller policy because backends retain different
-- source-name and rigidity information. Nominal constructor and class names
-- are validated by the shared AST; callers that accept untrusted variable-name
-- hints must validate those hints before supplying the callback. Rendering
-- with a total callback remains total for structurally unchecked types as
-- well, so a validation diagnostic can safely print the input it rejected.
module Language.Haskell.Synthesis.TypeRender
  ( renderType
  , renderConstraint
  , renderTypeWithQualification
  , renderConstraintWithQualification
  , showsType
  , showsConstraint
  , showsTypeWithQualification
  , showsConstraintWithQualification
  ) where

import Data.List (intercalate)

import Language.Haskell.Synthesis.Constraint
  ( Constraint
  , showsConstraintWithName
  )
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed, Unboxed)
  , SpecialName (ListConstructor)
  , nameSpecial
  )
import Language.Haskell.Synthesis.Qualification
  ( Qualification (FullyQualified)
  , renderNamePrefix
  )
import Language.Haskell.Synthesis.Type (Type (..))

-- | Render a complete type in source form.
renderType :: (variable -> String) -> Type variable -> String
renderType = renderTypeWithQualification FullyQualified

-- | Render a complete class constraint in source form.
renderConstraint
  :: (variable -> String)
  -> Constraint (Type variable)
  -> String
renderConstraint = renderConstraintWithQualification FullyQualified

-- | Render a complete type using one qualification policy for every nominal
-- constructor and nested constraint.
renderTypeWithQualification
  :: Qualification
  -> (variable -> String)
  -> Type variable
  -> String
renderTypeWithQualification qualification variableName typeExpression =
  showsTypeWithQualification qualification variableName 0 typeExpression ""

-- | Render a complete class constraint under the supplied qualification
-- policy.  The class and all constructor names in its arguments use the same
-- policy as generated terms.
renderConstraintWithQualification
  :: Qualification
  -> (variable -> String)
  -> Constraint (Type variable)
  -> String
renderConstraintWithQualification qualification variableName constraint =
  showsConstraintWithQualification qualification variableName 0 constraint ""

-- | Precedence-aware counterpart of 'renderConstraint' for compositional
-- renderers and 'Show' instances.
showsConstraint
  :: (variable -> String)
  -> Int
  -> Constraint (Type variable)
  -> ShowS
showsConstraint = showsConstraintWithQualification FullyQualified

-- | Qualification-aware counterpart of 'showsConstraint'.
showsConstraintWithQualification
  :: Qualification
  -> (variable -> String)
  -> Int
  -> Constraint (Type variable)
  -> ShowS
showsConstraintWithQualification qualification variableName =
  showsConstraintWithName (renderNamePrefix qualification)
    $ showsTypeWithQualification qualification variableName 2

-- | Render a type at the supplied Haskell precedence.
showsType
  :: (variable -> String)
  -> Int
  -> Type variable
  -> ShowS
showsType = showsTypeWithQualification FullyQualified

-- | Render a type at the supplied Haskell precedence and qualification level.
showsTypeWithQualification
  :: Qualification
  -> (variable -> String)
  -> Int
  -> Type variable
  -> ShowS
showsTypeWithQualification qualification variableName precedence typeExpression =
  case typeExpression of
  TypeVariable variable -> showString $ variableName variable
  -- Lists have no dedicated 'Type' node. Parenthesize the higher-kinded
  -- constructor, sugar its first application, and let the generic case render
  -- any trailing overapplication as @[a] b@.
  TypeConstructor name
    | nameSpecial name == Just ListConstructor -> showString "([])"
    | otherwise -> showString $ renderNamePrefix qualification name
  TypeApplication (TypeConstructor name) argument
    | nameSpecial name == Just ListConstructor -> showChar '['
      -- haskell-src-exts 1.24 requires an impredicative list element to be
      -- parenthesized even when its parser mode enables ImpredicativeTypes.
      . showsTypeWithQualification qualification variableName 1 argument
      . showChar ']'
  TypeApplication function argument -> showParen (precedence > 1)
    $ showsTypeWithQualification qualification variableName 1 function
    . showChar ' '
    . showsTypeWithQualification qualification variableName 2 argument
  FunctionType parameter result -> showParen (precedence > 0)
    $ showsTypeWithQualification qualification variableName 1 parameter
    . showString " -> "
    . showsTypeWithQualification qualification variableName 0 result
  TupleType boxity elements -> showString $ renderTuple boxity
    [ showsTypeWithQualification qualification variableName 0 element ""
    | element <- elements
    ]
  ForallType [] [] body ->
    showsTypeWithQualification qualification variableName precedence body
  ForallType variables constraints body -> showParen (precedence > 0)
    $ renderBinders variables
    . renderContext constraints
    . showsTypeWithQualification qualification variableName 0 body
 where
  renderBinders [] = id
  renderBinders variables = showString "forall "
    . showString (unwords $ map variableName variables)
    . showString ". "

  renderContext [] = id
  renderContext constraints = showChar '('
    . showString (intercalate ", "
        $ map (renderConstraintWithQualification qualification variableName)
            constraints)
    . showString ") => "

renderTuple :: Boxity -> [String] -> String
renderTuple Boxed elements = "(" ++ intercalate ", " elements ++ ")"
renderTuple Unboxed [] = "(# #)"
renderTuple Unboxed elements = "(# " ++ intercalate ", " elements ++ " #)"

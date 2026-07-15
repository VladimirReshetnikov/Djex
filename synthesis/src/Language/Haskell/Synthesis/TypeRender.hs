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
  , showsType
  , showsConstraint
  ) where

import Data.List (intercalate)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed, Unboxed)
  , SpecialName (ListConstructor)
  , nameSpecial
  , renderPrefix
  )
import Language.Haskell.Synthesis.Type (Type (..))

-- | Render a complete type in source form.
renderType :: (variable -> String) -> Type variable -> String
renderType variableName typeExpression =
  showsType variableName 0 typeExpression ""

-- | Render a complete class constraint in source form.
renderConstraint
  :: (variable -> String)
  -> Constraint (Type variable)
  -> String
renderConstraint variableName constraint =
  showsConstraint variableName 0 constraint ""

-- | Precedence-aware counterpart of 'renderConstraint' for compositional
-- renderers and 'Show' instances.
showsConstraint
  :: (variable -> String)
  -> Int
  -> Constraint (Type variable)
  -> ShowS
showsConstraint variableName precedence (Constraint className arguments) =
  showParen (precedence > 0 && not (null arguments))
    $ showString (renderPrefix className)
    . foldr showArgument id arguments
 where
  showArgument argument rest = showChar ' '
    . showsType variableName 2 argument
    . rest

-- | Render a type at the supplied Haskell precedence.
showsType
  :: (variable -> String)
  -> Int
  -> Type variable
  -> ShowS
showsType variableName precedence typeExpression = case typeExpression of
  TypeVariable variable -> showString $ variableName variable
  -- Lists have no dedicated 'Type' node. Parenthesize the higher-kinded
  -- constructor, sugar its first application, and let the generic case render
  -- any trailing overapplication as @[a] b@.
  TypeConstructor name
    | nameSpecial name == Just ListConstructor -> showString "([])"
    | otherwise -> showString $ renderPrefix name
  TypeApplication (TypeConstructor name) argument
    | nameSpecial name == Just ListConstructor -> showChar '['
      . showsType variableName 0 argument
      . showChar ']'
  TypeApplication function argument -> showParen (precedence > 1)
    $ showsType variableName 1 function
    . showChar ' '
    . showsType variableName 2 argument
  FunctionType parameter result -> showParen (precedence > 0)
    $ showsType variableName 1 parameter
    . showString " -> "
    . showsType variableName 0 result
  TupleType boxity elements -> showString $ renderTuple boxity
    [showsType variableName 0 element "" | element <- elements]
  ForallType [] [] body -> showsType variableName precedence body
  ForallType variables constraints body -> showParen (precedence > 0)
    $ renderBinders variables
    . renderContext constraints
    . showsType variableName 0 body
 where
  renderBinders [] = id
  renderBinders variables = showString "forall "
    . showString (unwords $ map variableName variables)
    . showString ". "

  renderContext [] = id
  renderContext constraints = showChar '('
    . showString (intercalate ", "
        $ map (renderConstraint variableName) constraints)
    . showString ") => "

renderTuple :: Boxity -> [String] -> String
renderTuple Boxed elements = "(" ++ intercalate ", " elements ++ ")"
renderTuple Unboxed [] = "(# #)"
renderTuple Unboxed elements = "(# " ++ intercalate ", " elements ++ " #)"

-- | Compact Haskell-source rendering for shared types and constraints.
--
-- Variable spelling remains a caller policy because backends retain different
-- source-name and rigidity information. Names and structural syntax are
-- already validated by the shared AST, so rendering itself is total.
module Language.Haskell.Synthesis.TypeRender
  ( renderType
  , renderConstraint
  ) where

import Data.List (intercalate)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Name
  ( Boxity (Boxed, Unboxed)
  , renderPrefix
  )
import Language.Haskell.Synthesis.Type (Type (..))

renderType :: (variable -> String) -> Type variable -> String
renderType variableName typeExpression =
  showsType variableName 0 typeExpression ""

renderConstraint
  :: (variable -> String)
  -> Constraint (Type variable)
  -> String
renderConstraint variableName (Constraint className arguments) =
  unwords $ renderPrefix className
    : map (\argument -> showsType variableName 2 argument "") arguments

showsType
  :: (variable -> String)
  -> Int
  -> Type variable
  -> ShowS
showsType variableName precedence typeExpression = case typeExpression of
  TypeVariable variable -> showString $ variableName variable
  TypeConstructor name -> showString $ renderPrefix name
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
renderTuple Unboxed elements = "(# " ++ intercalate ", " elements ++ " #)"

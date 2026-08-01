--
-- Small parser-free workloads for the Exference search core.  Every query
-- owns both a processed-node limit and a retained-queue limit: a benchmark
-- must remain finite even when a future heuristic makes its frontier worse.
--
{-# LANGUAGE PatternSynonyms #-}

module Corpus
  ( Demand (..)
  , Entry (..)
  , corpus
  ) where

import Language.Haskell.Exference.Core
  ( ExferenceEnvironment
  , ExferenceOptions (..)
  , ExferenceQuery (..)
  , defaultExferenceOptions
  , mkExferenceEnvironment
  )
import Language.Haskell.Exference.Core.FunctionBinding
  ( ConstructorBinding (..)
  , DeconstructorBinding (..)
  , EnvDictionary (..)
  , FunctionBinding (..)
  )
import Language.Haskell.Exference.Core.Name
  ( QualifiedName
  , mkQualifiedName
  )
import Language.Haskell.Exference.Core.Types
  ( pattern HsConstraint
  , HsInstance (..)
  , HsType
  , HsTypeClass (..)
  , StaticClassEnv
  , emptyStaticClassEnv
  , mkStaticClassEnv
  , pattern TypeArrow
  , pattern TypeCons
  , pattern TypeVar
  )
-- | How much of a lazy search trace a benchmark is intended to observe.
-- The more specific first-candidate demands also guard the algorithmic route
-- represented by their fixture.
data Demand
  = FirstCandidate
  | CandidatePrefix Int
  | FirstCaseCandidate
  | FirstConstraintFreeCandidate
  | BoundedNoCandidates

data Entry = Entry
  { entryName :: String
  , entryEnvironment :: ExferenceEnvironment
  , entryQuery :: ExferenceQuery
  , entryDemand :: Demand
  }

corpus :: [Entry]
corpus =
  [ Entry
      "first/identity"
      emptyEnvironment
      (boundedQuery identityType False)
      FirstCandidate
  , Entry
      "prefix/endomorphisms-8"
      endomorphismEnvironment
      (boundedQuery identityType False)
      (CandidatePrefix 8)
  , Entry
      "negative/cyclic-frontier"
      cyclicEnvironment
      (boundedQuery resultType False)
      BoundedNoCandidates
  , Entry
      "pattern/two-constructors"
      patternEnvironment
      (boundedQuery (TypeArrow choiceType resultType) True)
      FirstCaseCandidate
  , Entry
      "pattern/recursive-one-layer"
      recursivePatternEnvironment
      (boundedQuery recursiveDispatchType True)
      FirstCaseCandidate
  , Entry
      "class/ground-instance"
      classEnvironment
      (boundedQuery (TypeArrow integerType booleanType) False)
      FirstConstraintFreeCandidate
  ]

-- These are deliberately shared by every entry.  First-result and prefix
-- consumers normally stop earlier; the limits remain hard safety rails.
boundedQuery :: HsType -> Bool -> ExferenceQuery
boundedQuery goal multiConstructorPatterns = ExferenceQuery
  { queryGoalType = goal
  , queryExcludedBindings = mempty
  , querySearchOptions = defaultExferenceOptions
      { exferenceConstraintDeferralSteps = 0
      , exferenceMultiConstructorPatterns = multiConstructorPatterns
      , exferenceMaximumSteps = 100
      , exferenceMaximumQueueSize = Just 2048
      }
  }

identityType :: HsType
identityType = TypeArrow variableType variableType

variableType :: HsType
variableType = TypeVar 0

emptyEnvironment :: ExferenceEnvironment
emptyEnvironment = checkedEnvironment
  $ EnvDictionary [] [] emptyStaticClassEnv

endomorphismEnvironment :: ExferenceEnvironment
endomorphismEnvironment = checkedEnvironment
  $ EnvDictionary (map endomorphism ([0 .. 7] :: [Int]))
      [] emptyStaticClassEnv
 where
  endomorphism index = FunctionBinding
    variableType (name $ "f" ++ show index) 0 [] [variableType]

-- There is no source of A.  Each attempted A can only grow another layer of
-- loop applications, so this case reaches the exact step limit.  Twenty-four
-- equal-cost loops also make the retained frontier cross the queue limit.
cyclicEnvironment :: ExferenceEnvironment
cyclicEnvironment = checkedEnvironment
  $ EnvDictionary (toResults ++ loops) [] emptyStaticClassEnv
 where
  toResults =
    [ FunctionBinding resultType (name $ "toR" ++ show index) 0 [] [atomType]
    | index <- [0 .. 3 :: Int]
    ]
  loops =
    [ FunctionBinding atomType (name $ "loop" ++ show index) 0 [] [atomType]
    | index <- [0 .. 23 :: Int]
    ]

atomType :: HsType
atomType = TypeCons $ name "A"

resultType :: HsType
resultType = TypeCons $ name "R"

patternEnvironment :: ExferenceEnvironment
patternEnvironment = checkedEnvironment $ EnvDictionary
  [FunctionBinding resultType (name "result") 0 [] []]
  [ DeconstructorBinding choiceType
      [ ConstructorBinding (name "LeftChoice") []
      , ConstructorBinding (name "RightChoice") []
      ]
      False
  ]
  emptyStaticClassEnv

choiceType :: HsType
choiceType = TypeCons $ name "Choice"

recursivePatternEnvironment :: ExferenceEnvironment
recursivePatternEnvironment = checkedEnvironment $ EnvDictionary
  []
  [ DeconstructorBinding naturalType
      [ ConstructorBinding (name "Zero") []
      , ConstructorBinding (name "Successor") [naturalType]
      ]
      True
  ]
  emptyStaticClassEnv

naturalType :: HsType
naturalType = TypeCons $ name "Natural"

recursiveDispatchType :: HsType
recursiveDispatchType = TypeArrow resultType
  $ TypeArrow (TypeArrow naturalType resultType)
  $ TypeArrow naturalType resultType

classEnvironment :: ExferenceEnvironment
classEnvironment = checkedEnvironment $ EnvDictionary
  [ FunctionBinding booleanType (name "test") 0
      [HsConstraint className [variableType]]
      [variableType]
  ]
  []
  classDeclarations

classDeclarations :: StaticClassEnv
classDeclarations = case mkStaticClassEnv
    [HsTypeClass className [0] []]
    [HsInstance [] $ HsConstraint className [integerType]] of
  Left failure -> error $ "invalid benchmark class environment: " ++ show failure
  Right environment -> environment

className :: QualifiedName
className = name "EqLike"

integerType :: HsType
integerType = TypeCons $ name "Int"

booleanType :: HsType
booleanType = TypeCons $ name "Bool"

checkedEnvironment :: EnvDictionary -> ExferenceEnvironment
checkedEnvironment dictionary = case mkExferenceEnvironment dictionary of
  Left failure -> error $ "invalid benchmark environment: " ++ show failure
  Right environment -> environment

name :: String -> QualifiedName
name spelling = case mkQualifiedName [] spelling of
  Left failure -> error $ "invalid benchmark name " ++ show spelling
    ++ ": " ++ show failure
  Right result -> result

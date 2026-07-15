module Main (main) where

import Text.Read (readMaybe)

import Djinn.Internal.HTypes
import Djinn.Internal.LJT
import Djinn.Internal.ProofCheck (checkProof)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain $
    adjustOption (max (QC.QuickCheckTests 200)) $
        testGroup "Djinn properties"
            [ QC.testProperty
                "bounded generated proofs independently check and render"
                propGeneratedProofsCheckAndRender
            , QC.testProperty
                "identity is provable for every bounded formula"
                propIdentityAlwaysProvable
            , QC.testProperty
                "surface HType rendering round-trips through its parser"
                propHTypeRoundTrip
            , QC.testProperty
                "budgeted search yields a prefix and stays honest"
                propBudgetedSearch
            ]

newtype ProvableFormula = ProvableFormula Formula
    deriving (Show)

instance QC.Arbitrary ProvableFormula where
    arbitrary = QC.sized $ \ size -> do
        first <- genFormula $ min 2 size
        second <- genFormula $ min 2 size
        third <- genFormula $ min 2 size
        theorem <- QC.elements
            [ first :-> first
            , Conj [first, second] :-> Conj [second, first]
            , first :-> (first |: second)
            , (first :-> second) :-> first :-> second
            , (first :-> third) :->
                (second :-> third) :-> (first |: second) :-> third
            , Empty (Symbol "GeneratedEmpty") :-> first
            ]
        return $ ProvableFormula theorem

propGeneratedProofsCheckAndRender :: ProvableFormula -> QC.Property
propGeneratedProofsCheckAndRender (ProvableFormula formula) =
    QC.checkCoverage $
        QC.cover 85 (not $ null proofs) "proof generated" $
            QC.conjoin $ map (proofIsValid formula) proofs
  where
    proofs = searchProofs $ boundedSearch False formula

newtype ArbitraryFormula = ArbitraryFormula Formula
    deriving (Show)

instance QC.Arbitrary ArbitraryFormula where
    arbitrary = QC.sized $ \ size ->
        ArbitraryFormula `fmap` genFormula (min 2 size)

propIdentityAlwaysProvable :: ArbitraryFormula -> QC.Property
propIdentityAlwaysProvable (ArbitraryFormula formula) =
    case searchProofs $ boundedSearch False $ formula :-> formula of
        [] -> QC.counterexample
            ("identity was not proved for " ++ show formula) False
        proof : _ -> proofIsValid (formula :-> formula) proof

proofIsValid :: Formula -> Term -> QC.Property
proofIsValid formula proof =
    QC.conjoin
        [ QC.counterexample (context ++ "\nchecker: " ++ show checked) $
            checked == Right ()
        , QC.counterexample (context ++ "\nrenderer: " ++ rendered) $
            either (const False) (const True) rendering
        ]
  where
    context = "formula: " ++ show formula ++ "\nproof: " ++ show proof
    checked = checkProof [] formula proof
    rendering = termToHClause "generated" proof >>= hPrClause
    rendered = either id (const "success") rendering

-- Under the default depth-first strategy, a smaller finite search must produce
-- a prefix of a larger one. If the smaller search claims completion, extra
-- fuel cannot reveal another proof or report exhaustion. Keeping both sides
-- finite is essential: taking a prefix of an unbounded proof stream does not
-- bound the work needed to discover that the stream has ended.
propBudgetedSearch :: ArbitraryFormula -> QC.NonNegative Int
                   -> QC.Property
propBudgetedSearch (ArbitraryFormula formula) (QC.NonNegative requestedFuel) =
    QC.counterexample context $
        (bounded `isPrefixOf` reference) &&
        (searchExhausted outcome ||
            (not (searchExhausted referenceOutcome) && bounded == reference))
  where
    goal = formula :-> formula
    fuel = fromIntegral requestedFuel
    outcome = proveWithMode (defaultSearchMode True)
        { searchBudget = Just fuel } [] goal
    referenceOutcome = proveWithMode (defaultSearchMode True)
        { searchBudget = Just $ fuel + referenceSearchFuel } [] goal
    bounded = searchProofs outcome
    reference = searchProofs referenceOutcome
    context = "goal: " ++ show goal ++ "\nfuel: " ++ show fuel ++
        "\nbounded: " ++ show bounded ++ "\nreference: " ++ show reference
    isPrefixOf xs ys = take (length xs) ys == xs

-- Formula generators are deliberately shallow, but alternative proof
-- enumeration can still contain fewer results than a caller asks for. The
-- search budget, rather than a list prefix, is the actual work bound.
referenceSearchFuel :: Integer
referenceSearchFuel = 256

boundedSearch :: Bool -> Formula -> SearchOutcome
boundedSearch alternatives = proveWithMode (defaultSearchMode alternatives)
    { searchBudget = Just referenceSearchFuel } []

genFormula :: Int -> QC.Gen Formula
genFormula depth
    | depth <= 0 = QC.elements atoms
    | otherwise = QC.frequency
        [ (4, QC.elements atoms)
        , (2, genProduct)
        , (2, genSum)
        , (3, (:->) `fmap` smaller <*> smaller)
        ]
  where
    smaller = genFormula (depth - 1)
    genProduct = do
        count <- QC.elements [0, 1, 2, 3]
        Conj `fmap` QC.vectorOf count smaller
    genSum = do
        payload <- smaller
        return $ Disj
            [ (ConsDesc "Nothing" 0, Conj [])
            , (ConsDesc "Just" 1, payload)
            ]

atoms :: [Formula]
atoms =
    [ PVar $ Symbol "a"
    , PVar $ Symbol "b"
    , PVar $ Symbol "c"
    , Empty $ Symbol "EmptyA"
    , Empty $ Symbol "EmptyB"
    ]

newtype SurfaceType = SurfaceType HType
    deriving (Show)

instance QC.Arbitrary SurfaceType where
    arbitrary = QC.sized $ \ size ->
        SurfaceType `fmap` genSurfaceType (min 4 size)

propHTypeRoundTrip :: SurfaceType -> QC.Property
propHTypeRoundTrip (SurfaceType htype) =
    QC.counterexample ("rendered as: " ++ show htype) $
        readMaybe (show htype) == Just htype

genSurfaceType :: Int -> QC.Gen HType
genSurfaceType depth
    | depth <= 0 = QC.elements surfaceAtoms
    | otherwise = QC.frequency
        [ (5, QC.elements surfaceAtoms)
        , (3, HTArrow `fmap` smaller <*> smaller)
        , (2, do
                first <- smaller
                second <- smaller
                return $ HTTuple [first, second])
        , (2, HTApp (HTCon "Maybe") `fmap` smaller)
        , (2, HTApp (HTCon "[]") `fmap` smaller)
        ]
  where
    smaller = genSurfaceType (depth - 1)

surfaceAtoms :: [HType]
surfaceAtoms =
    [ HTVar "a"
    , HTVar "b'"
    , HTVar "_value"
    , HTCon "Bool"
    , HTCon "Data.Example.Token"
    , HTCon "()"
    ]

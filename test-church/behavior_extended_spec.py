"""Additional Church behavioral specifications; no synthesis receipt is implied.

This is a new, opt-in specification module. The six-operation behavior_spec.py
and its runners remain unchanged. Source-derived signatures are rendered from
the checked manifest AST, including its original binder order. The separate
standard-numeral extension does not claim Church.hs provenance: that file has
no Church Nat declaration. Its length operation returns the source's Int.

The API mirrors behavior_spec: OPERATIONS, HASKELL_TYPES, LEAN_TYPES,
OBSERVATIONS, haskell_predicate, lean_prelude, lean_predicate, and the two
control_source functions. An adapter must also honor search_provider_inventory:
only length permits the explicit zero/successor fixture; all other cases use
no providers. Observation adapters, witnesses, and wrong controls must NEVER
be added to the search inventory. Lean observation declarations require
provider discovery off and, for length, the exact numeric provider allowlist.

PARTIAL_COVERAGE is a proposed coverage register, deliberately excluded from
OPERATIONS and both control generators. It is not behavioral acceptance.
Finite observations and independently checked positive/negative controls do
not constitute a proof of extensional correctness on all inputs.
"""
from __future__ import annotations

import hashlib
import itertools
import json
from pathlib import Path
import re

from behavior_runtime import render_type

HERE = Path(__file__).resolve().parent
EXPECTED_SOURCE_SHA256 = "782e4edaa5bf813e30e39ae02d52278ab0566315ebc521401a947b98c44cfd11"
SOURCE_ROWS = (
    ("foldr", "church_case_036", 286, "(a -> b -> b) -> b -> List a -> b"),
    ("foldl", "church_case_037", 289, "_ -> b -> List a -> b"),
    ("length", "church_case_060", 409, "List a -> Int"),
    ("fromMaybe", "church_case_094", 562, "a -> Maybe a -> a"),
    ("maybeToList", "church_case_095", 567, "Maybe a -> List a"),
    ("listToMaybe", "church_case_096", 571, "List a -> Maybe a"),
    ("catMaybes", "church_case_097", 576, "List (Maybe a) -> List a"),
    ("squashMaybe", "church_case_099", 584, "Maybe (Maybe a) -> Maybe a"),
    ("isLeft", "church_case_103", 602, "Either _ _ -> Bool"),
    ("maybeEither", "church_case_107", 615, "Either (Maybe a) (Maybe b) -> Maybe (Either a b)"),
    ("either", "church_case_109", 624, "(a -> c) -> _ -> Either a b -> c"),
)
SOURCE_OPERATIONS = tuple(row[0] for row in SOURCE_ROWS)
NUMERAL_OPERATIONS = ("numeralSuccessor", "numeralAdd")
OPERATIONS = SOURCE_OPERATIONS + NUMERAL_OPERATIONS
SOURCE_CASE_IDS = {name: case_id for name, case_id, _, _ in SOURCE_ROWS}
SOURCE_LINES = {name: line for name, _, line, _ in SOURCE_ROWS}
SOURCE_SIGNATURES = {name: signature for name, _, _, signature in SOURCE_ROWS}
NUMERAL_PROVENANCE = {
    "classification": "standard_church_numeral_extension",
    "source": None,
    "manifest_case_id": None,
    "semantics": "Natural iteration counts encoded as forall R. (R -> R) -> R -> R.",
    "acceptance_status": "specification_only_not_probed",
}


def _load_source_manifest(manifest_path):
    manifest_path = Path(manifest_path).resolve()
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    source_path = manifest_path.parent.parent / manifest["source"]
    source_text = source_path.read_text(encoding="utf-8-sig")
    normalized_hash = hashlib.sha256(source_text.encode("utf-8")).hexdigest()
    if normalized_hash != EXPECTED_SOURCE_SHA256 or normalized_hash != manifest["source_sha256"]:
        raise ValueError("Church source or its manifest changed; review extended specifications")
    by_id = {row["id"]: row for row in manifest["cases"]}
    if len(by_id) != len(manifest["cases"]):
        raise ValueError("duplicate Church manifest identities")
    source_lines = source_text.splitlines()
    for name, case_id, line, signature in SOURCE_ROWS:
        row = by_id[case_id]
        expected_classification = "needs_int_provider" if name == "length" else "total"
        if (row["name"], row["line"], row["source_signature"], row["classification"]) != (
                name, line, signature, expected_classification):
            raise ValueError("changed source-derived operation: " + case_id)
        match = re.fullmatch(re.escape(name) + r"\s*(?:::|∷)\s*(.*)", source_lines[line - 1])
        if match is None or match.group(1) != signature:
            raise ValueError("source signature differs from manifest: " + case_id)
    return manifest_path, source_path, manifest, by_id


_MANIFEST_PATH, _SOURCE_PATH, _MANIFEST, _BY_ID = _load_source_manifest(HERE / "manifest.json")
HNUM = "(forall r. (r -> r) -> r -> r)"
LNUM = "(∀ R : Type, (R → R) → R → R)"
HASKELL_TYPES = {
    name: render_type(_BY_ID[case_id]["expanded_type"])
    for name, case_id, _, _ in SOURCE_ROWS
}
LEAN_TYPES = {
    name: render_type(_BY_ID[case_id]["expanded_type"], lean=True)
    for name, case_id, _, _ in SOURCE_ROWS
}
HASKELL_TYPES.update(numeralSuccessor=f"{HNUM} -> {HNUM}", numeralAdd=f"{HNUM} -> {HNUM} -> {HNUM}")
LEAN_TYPES.update(numeralSuccessor=f"{LNUM} → {LNUM}", numeralAdd=f"{LNUM} → {LNUM} → {LNUM}")


def hlist(a):
    return f"(forall r. ({a} -> r -> r) -> r -> r)"


def hmaybe(a):
    return f"(forall r. r -> ({a} -> r) -> r)"


def heither(a, b):
    return f"(forall r. ({a} -> r) -> ({b} -> r) -> r)"


# The only permitted nonempty search inventory. These are arithmetic
# primitives, not length or a target-specific fold. The replay definitions
# have exactly the same meanings as the definitions loaded for synthesis.
HASKELL_NUMERIC_PROVIDERS = [
    "numericZero :: Prelude.Int", "numericZero = 0",
    "numericSuccessor :: Prelude.Int -> Prelude.Int",
    "numericSuccessor n = n Prelude.+ 1",
]
LEAN_NUMERIC_PROVIDERS = [
    "def BehaviorExtendedNumeric.zero : Int := 0",
    "def BehaviorExtendedNumeric.successor (n : Int) : Int := n + 1",
]


def search_provider_inventory(operation):
    if operation not in OPERATIONS:
        raise KeyError(operation)
    if operation != "length":
        return {"haskell": [], "lean": [], "discovery": "off"}
    return {
        "haskell": [
            {"name": "numericZero", "type": "Int", "definition": "0"},
            {"name": "numericSuccessor", "type": "Int -> Int", "definition": "\\n -> n + 1"},
        ],
        "lean": [
            {"name": "BehaviorExtendedNumeric.zero", "type": "Int", "definition": "0"},
            {"name": "BehaviorExtendedNumeric.successor", "type": "Int → Int", "definition": "fun n => n + 1"},
        ],
        "discovery": "off; exact explicit provider allowlist only",
    }


REQUIRED_PROVIDERS = {operation: search_provider_inventory(operation) for operation in OPERATIONS}
OPERATION_PROVENANCE = {
    **{name: {
        "classification": "Church.hs_total_signature",
        "manifest_classification": _BY_ID[case_id]["classification"],
        "manifest_case_id": case_id, "source_line": line,
        "source_signature": signature, "acceptance_status": "specification_only_not_probed",
    } for name, case_id, line, signature in SOURCE_ROWS},
    **{name: dict(NUMERAL_PROVENANCE) for name in NUMERAL_OPERATIONS},
}


INPUTS = [list(xs) for n in range(4) for xs in itertools.product((-1, 0, 2), repeat=n)]
MAYBE_LIST_INPUTS = [list(xs) for n in range(4) for xs in itertools.product((None, -1, 2), repeat=n)]
NUMERALS = list(range(5))
OBSERVATIONS = {
    "foldr": 400, "foldl": 400, "length": 44, "fromMaybe": 18,
    "maybeToList": 7, "listToMaybe": 44, "catMaybes": 40,
    "squashMaybe": 8, "isLeft": 10, "maybeEither": 7,
    "either": 15, "numeralSuccessor": 15, "numeralAdd": 75,
}


def _hmaybe_value(value):
    return "Prelude.Nothing" if value is None else f"(Prelude.Just ({value}))"


_HHELPERS = [
    f"enc :: forall a. [a] -> {hlist('a')}",
    "enc xs step zero = Prelude.foldr step zero xs",
    f"dec :: forall a. {hlist('a')} -> [a]", "dec xs = xs (:) []",
    f"encMaybe :: forall a. Prelude.Maybe a -> {hmaybe('a')}",
    "encMaybe m zero some = Prelude.maybe zero some m",
    f"decMaybe :: forall a. {hmaybe('a')} -> Prelude.Maybe a",
    "decMaybe m = m Prelude.Nothing Prelude.Just",
    f"encEither :: forall a b. Prelude.Either a b -> {heither('a', 'b')}",
    "encEither e onLeft onRight = Prelude.either onLeft onRight e",
    f"decEither :: forall a b. {heither('a', 'b')} -> Prelude.Either a b",
    "decEither e = e Prelude.Left Prelude.Right",
    f"encMaybes :: forall a. [Prelude.Maybe a] -> {hlist(hmaybe('a'))}",
    "encMaybes xs step zero = Prelude.foldr (\\m rest -> step (encMaybe m) rest) zero xs",
    f"encNestedMaybe :: forall a. Prelude.Maybe (Prelude.Maybe a) -> {hmaybe(hmaybe('a'))}",
    "encNestedMaybe mm zero some = Prelude.maybe zero (\\m -> some (encMaybe m)) mm",
    f"encMaybeEither :: forall a b. Prelude.Either (Prelude.Maybe a) (Prelude.Maybe b) -> {heither(hmaybe('a'), hmaybe('b'))}",
    "encMaybeEither e onLeft onRight = Prelude.either (\\m -> onLeft (encMaybe m)) (\\m -> onRight (encMaybe m)) e",
    f"decMaybeEither :: forall a b. {hmaybe(heither('a', 'b'))} -> Prelude.Maybe (Prelude.Either a b)",
    "decMaybeEither m = m Prelude.Nothing (\\e -> Prelude.Just (decEither e))",
    f"encNum :: Prelude.Int -> {HNUM}",
    "encNum n step zero = Prelude.iterate step zero Prelude.!! n",
    f"decNum :: {HNUM} -> Prelude.Int", "decNum n = n (\\x -> x + 1) 0",
    "inputs :: [[Prelude.Int]]", "inputs = " + repr(INPUTS),
    "boolInputs :: [[Prelude.Bool]]", "boolInputs = [[],[Prelude.False],[Prelude.True],[Prelude.True,Prelude.False]]",
    "maybeInputs :: [Prelude.Maybe Prelude.Int]", "maybeInputs = [Prelude.Nothing,Prelude.Just (-1),Prelude.Just 0,Prelude.Just 2]",
    "maybeBools :: [Prelude.Maybe Prelude.Bool]", "maybeBools = [Prelude.Nothing,Prelude.Just Prelude.False,Prelude.Just Prelude.True]",
    "maybeLists :: [[Prelude.Maybe Prelude.Int]]",
    "maybeLists = [" + ",".join("[" + ",".join(map(_hmaybe_value, xs)) + "]" for xs in MAYBE_LIST_INPUTS) + "]",
    "nestedMaybes :: [Prelude.Maybe (Prelude.Maybe Prelude.Int)]",
    "nestedMaybes = Prelude.Nothing : Prelude.map Prelude.Just maybeInputs",
    "nestedBools :: [Prelude.Maybe (Prelude.Maybe Prelude.Bool)]",
    "nestedBools = [Prelude.Nothing,Prelude.Just Prelude.Nothing,Prelude.Just (Prelude.Just Prelude.True)]",
    "eitherInputs :: [Prelude.Either Prelude.Int Prelude.Bool]",
    "eitherInputs = Prelude.map Prelude.Left [-1,0,2] Prelude.++ Prelude.map Prelude.Right [Prelude.False,Prelude.True]",
    "maybeEitherInputs :: [Prelude.Either (Prelude.Maybe Prelude.Int) (Prelude.Maybe Prelude.Bool)]",
    "maybeEitherInputs = Prelude.map Prelude.Left maybeInputs Prelude.++ Prelude.map Prelude.Right maybeBools",
    "foldSteps :: [Prelude.Int -> Prelude.Int -> Prelude.Int]",
    "foldSteps = [(\\x y -> x - y),(\\x y -> 2*x + y),(\\x y -> x + 3*y)]",
]


def haskell_predicate(operation, candidate):
    """Closed ordinary Bool except for the exact fully typed candidate."""
    f = candidate
    bodies = {
        "length": f"Prelude.and [{f} (enc xs) == Prelude.length xs | xs <- inputs] && Prelude.and [{f} (enc xs) == Prelude.length xs | xs <- boolInputs]",
        "foldr": f"Prelude.and [{f} g z (enc xs) == Prelude.foldr g z xs | xs <- inputs, g <- foldSteps, z <- [-2,0,3]] && Prelude.and [{f} (:) [] (enc xs) == xs | xs <- inputs]",
        "foldl": f"Prelude.and [{f} g z (enc xs) == Prelude.foldl g z xs | xs <- inputs, g <- foldSteps, z <- [-2,0,3]] && Prelude.and [{f} (\\acc x -> x:acc) [] (enc xs) == Prelude.reverse xs | xs <- inputs]",
        "fromMaybe": f"Prelude.and [{f} d (encMaybe m) == Prelude.maybe d Prelude.id m | d <- [-2,0,3], m <- maybeInputs] && Prelude.and [{f} d (encMaybe m) == Prelude.maybe d Prelude.id m | d <- [Prelude.False,Prelude.True], m <- maybeBools]",
        "maybeToList": f"Prelude.and [dec ({f} (encMaybe m)) == Prelude.maybe [] (\\x -> [x]) m | m <- maybeInputs] && Prelude.and [dec ({f} (encMaybe m)) == Prelude.maybe [] (\\x -> [x]) m | m <- maybeBools]",
        "listToMaybe": f"Prelude.and [decMaybe ({f} (enc xs)) == (case xs of {{ [] -> Prelude.Nothing; x:_ -> Prelude.Just x }}) | xs <- inputs] && Prelude.and [decMaybe ({f} (enc xs)) == (case xs of {{ [] -> Prelude.Nothing; x:_ -> Prelude.Just x }}) | xs <- boolInputs]",
        "catMaybes": f"Prelude.and [dec ({f} (encMaybes xs)) == [x | Prelude.Just x <- xs] | xs <- maybeLists]",
        "squashMaybe": f"Prelude.and [decMaybe ({f} (encNestedMaybe mm)) == Prelude.maybe Prelude.Nothing Prelude.id mm | mm <- nestedMaybes] && Prelude.and [decMaybe ({f} (encNestedMaybe mm)) == Prelude.maybe Prelude.Nothing Prelude.id mm | mm <- nestedBools]",
        "isLeft": f"Prelude.and [{f} (encEither e) Prelude.True Prelude.False == Prelude.either (\\_ -> Prelude.True) (\\_ -> Prelude.False) e | e <- eitherInputs] && Prelude.and [{f} (encEither (Prelude.either Prelude.Right Prelude.Left e)) Prelude.True Prelude.False == Prelude.either (\\_ -> Prelude.False) (\\_ -> Prelude.True) e | e <- eitherInputs]",
        "maybeEither": f"Prelude.and [decMaybeEither ({f} (encMaybeEither e)) == Prelude.either (Prelude.fmap Prelude.Left) (Prelude.fmap Prelude.Right) e | e <- maybeEitherInputs]",
        "either": f"Prelude.and [{f} g h (encEither e) == Prelude.either g h e | e <- eitherInputs, g <- [(\\x -> x+7),(\\x -> 3*x)], h <- [(\\b -> if b then 19 else -4)]] && Prelude.and [{f} (\\x -> x >= 0) Prelude.not (encEither e) == Prelude.either (\\x -> x >= 0) Prelude.not e | e <- eitherInputs]",
        "numeralSuccessor": f"Prelude.and [decNum ({f} (encNum n)) == n+1 | n <- [0..4]] && Prelude.and [{f} (encNum n) Prelude.not b == encNum (n+1) Prelude.not b | n <- [0..4], b <- [Prelude.False,Prelude.True]]",
        "numeralAdd": f"Prelude.and [decNum ({f} (encNum n) (encNum m)) == n+m | n <- [0..4], m <- [0..4]] && Prelude.and [{f} (encNum n) (encNum m) Prelude.not b == encNum (n+m) Prelude.not b | n <- [0..4], m <- [0..4], b <- [Prelude.False,Prelude.True]]",
    }
    return "let { " + "; ".join(_HHELPERS) + " } in " + bodies[operation]


def lean_prelude():
    """Observation-only declarations, never discovered as search providers.

    Container parameters are universe-polymorphic so nested Church values
    (which live in Type 1) can be passed as elements. Result quantifiers remain
    Type exactly as in the expanded manifest target. Nested observations fold
    directly to small native data, avoiding an invalid Type-1-to-Type-0 cast.
    """
    lines = [
        "set_option linter.unusedVariables false",
        "universe u v",
        "abbrev BehaviorExtended.CList (A : Type u) := ∀ R : Type, (A → R → R) → R → R",
        "abbrev BehaviorExtended.CMaybe (A : Type u) := ∀ R : Type, R → (A → R) → R",
        "abbrev BehaviorExtended.CEither (A : Type u) (B : Type v) := ∀ R : Type, (A → R) → (B → R) → R",
        f"abbrev BehaviorExtended.CNum := {LNUM}",
        "def BehaviorExtended.enc {A : Type} (xs : List A) : BehaviorExtended.CList A := fun _ step zero => xs.foldr step zero",
        "def BehaviorExtended.dec {A : Type} (xs : BehaviorExtended.CList A) : List A := xs (List A) List.cons []",
        "def BehaviorExtended.encMaybe {A : Type} (m : Option A) : BehaviorExtended.CMaybe A := fun _ zero some => m.elim zero some",
        "def BehaviorExtended.decMaybe {A : Type} (m : BehaviorExtended.CMaybe A) : Option A := m (Option A) none some",
        "def BehaviorExtended.encEither {A B : Type} (e : Sum A B) : BehaviorExtended.CEither A B := fun _ onLeft onRight => e.elim onLeft onRight",
        "def BehaviorExtended.encMaybes {A : Type} (xs : List (Option A)) : BehaviorExtended.CList (BehaviorExtended.CMaybe A) := fun _ step zero => xs.foldr (fun m rest => step (BehaviorExtended.encMaybe m) rest) zero",
        "def BehaviorExtended.encNestedMaybe {A : Type} (mm : Option (Option A)) : BehaviorExtended.CMaybe (BehaviorExtended.CMaybe A) := fun _ zero some => mm.elim zero (fun m => some (BehaviorExtended.encMaybe m))",
        "def BehaviorExtended.encMaybeEither {A B : Type} (e : Sum (Option A) (Option B)) : BehaviorExtended.CEither (BehaviorExtended.CMaybe A) (BehaviorExtended.CMaybe B) := fun _ onLeft onRight => e.elim (fun m => onLeft (BehaviorExtended.encMaybe m)) (fun m => onRight (BehaviorExtended.encMaybe m))",
        "def BehaviorExtended.decMaybeEither {A B : Type} (m : BehaviorExtended.CMaybe (BehaviorExtended.CEither A B)) : Option (Sum A B) := m (Option (Sum A B)) none (fun e => some (e (Sum A B) Sum.inl Sum.inr))",
        "def BehaviorExtended.encNum (n : Nat) : BehaviorExtended.CNum := fun _ step zero => Nat.rec zero (fun _ acc => step acc) n",
        "def BehaviorExtended.decNum (n : BehaviorExtended.CNum) : Nat := n Nat Nat.succ 0",
        "def BehaviorExtended.inputs : List (List Int) := " + repr(INPUTS),
        "def BehaviorExtended.boolInputs : List (List Bool) := [[],[false],[true],[true,false]]",
        "def BehaviorExtended.maybeInputs : List (Option Int) := [none,some (-1),some 0,some 2]",
        "def BehaviorExtended.maybeBools : List (Option Bool) := [none,some false,some true]",
        "def BehaviorExtended.maybeLists : List (List (Option Int)) := [" +
        ",".join("[" + ",".join("none" if x is None else f"some ({x})" for x in xs) + "]" for xs in MAYBE_LIST_INPUTS) + "]",
        "def BehaviorExtended.nestedMaybes : List (Option (Option Int)) := none :: BehaviorExtended.maybeInputs.map some",
        "def BehaviorExtended.nestedBools : List (Option (Option Bool)) := [none,some none,some (some true)]",
        "def BehaviorExtended.eitherInputs : List (Sum Int Bool) := ([-1,0,2] : List Int).map Sum.inl ++ [false,true].map Sum.inr",
        "def BehaviorExtended.maybeEitherInputs : List (Sum (Option Int) (Option Bool)) := BehaviorExtended.maybeInputs.map Sum.inl ++ BehaviorExtended.maybeBools.map Sum.inr",
        "def BehaviorExtended.foldSteps : List (Int → Int → Int) := [(fun x y => x-y),(fun x y => 2*x+y),(fun x y => x+3*y)]",
    ]
    b = "BehaviorExtended."
    bodies = {
        "length": f"{b}inputs.all (fun xs => f Int ({b}enc xs) == Int.ofNat xs.length) && {b}boolInputs.all (fun xs => f Bool ({b}enc xs) == Int.ofNat xs.length)",
        "foldr": f"{b}inputs.all (fun xs => {b}foldSteps.all (fun g => ([-2,0,3] : List Int).all (fun z => f Int Int g z ({b}enc xs) == xs.foldr g z))) && {b}inputs.all (fun xs => f Int (List Int) List.cons [] ({b}enc xs) == xs)",
        "foldl": f"{b}inputs.all (fun xs => {b}foldSteps.all (fun g => ([-2,0,3] : List Int).all (fun z => f Int Int g z ({b}enc xs) == xs.foldl g z))) && {b}inputs.all (fun xs => f (List Int) Int (fun acc x => x :: acc) [] ({b}enc xs) == xs.reverse)",
        "fromMaybe": f"([-2,0,3] : List Int).all (fun d => {b}maybeInputs.all (fun m => f Int d ({b}encMaybe m) == m.getD d)) && [false,true].all (fun d => {b}maybeBools.all (fun m => f Bool d ({b}encMaybe m) == m.getD d))",
        "maybeToList": f"{b}maybeInputs.all (fun m => {b}dec (f Int ({b}encMaybe m)) == m.elim [] (fun x => [x])) && {b}maybeBools.all (fun m => {b}dec (f Bool ({b}encMaybe m)) == m.elim [] (fun x => [x]))",
        "listToMaybe": f"{b}inputs.all (fun xs => {b}decMaybe (f Int ({b}enc xs)) == xs.head?) && {b}boolInputs.all (fun xs => {b}decMaybe (f Bool ({b}enc xs)) == xs.head?)",
        "catMaybes": f"{b}maybeLists.all (fun xs => {b}dec (f Int ({b}encMaybes xs)) == xs.filterMap id)",
        "squashMaybe": f"{b}nestedMaybes.all (fun mm => {b}decMaybe (f Int ({b}encNestedMaybe mm)) == mm.elim none id) && {b}nestedBools.all (fun mm => {b}decMaybe (f Bool ({b}encNestedMaybe mm)) == mm.elim none id)",
        "isLeft": f"{b}eitherInputs.all (fun e => f Int Bool ({b}encEither e) Bool true false == e.elim (fun _ => true) (fun _ => false)) && {b}eitherInputs.all (fun e => f Bool Int ({b}encEither (e.elim Sum.inr Sum.inl)) Bool true false == e.elim (fun _ => false) (fun _ => true))",
        "maybeEither": f"{b}maybeEitherInputs.all (fun e => {b}decMaybeEither (f Int Bool ({b}encMaybeEither e)) == e.elim (fun m => m.map Sum.inl) (fun m => m.map Sum.inr))",
        "either": f"{b}eitherInputs.all (fun e => ([(fun x => x+7),(fun x => 3*x)] : List (Int → Int)).all (fun g => f Int Int Bool g (fun flag => if flag then 19 else -4) ({b}encEither e) == e.elim g (fun flag => if flag then 19 else -4))) && {b}eitherInputs.all (fun e => f Int Bool Bool (fun x => decide (x ≥ 0)) Bool.not ({b}encEither e) == e.elim (fun x => decide (x ≥ 0)) Bool.not)",
        "numeralSuccessor": f"([0,1,2,3,4] : List Nat).all (fun n => {b}decNum (f ({b}encNum n)) == n+1) && ([0,1,2,3,4] : List Nat).all (fun n => [false,true].all (fun flag => f ({b}encNum n) Bool Bool.not flag == {b}encNum (n+1) Bool Bool.not flag))",
        "numeralAdd": f"([0,1,2,3,4] : List Nat).all (fun n => ([0,1,2,3,4] : List Nat).all (fun m => {b}decNum (f ({b}encNum n) ({b}encNum m)) == n+m)) && ([0,1,2,3,4] : List Nat).all (fun n => ([0,1,2,3,4] : List Nat).all (fun m => [false,true].all (fun flag => f ({b}encNum n) ({b}encNum m) Bool Bool.not flag == {b}encNum (n+m) Bool Bool.not flag)))",
    }
    lines += [f"def {b}check_{op} (f : {LEAN_TYPES[op]}) : Bool := {bodies[op]}" for op in OPERATIONS]
    return lines


def lean_predicate(operation, candidate):
    return f"BehaviorExtended.check_{operation} {candidate} = true"


HASKELL_WITNESSES = {
    "foldr": "\\step zero xs -> xs step zero",
    "foldl": "\\step zero xs -> xs (\\x k acc -> k (step acc x)) (\\x -> x) zero",
    "length": "\\xs -> xs (\\_ n -> numericSuccessor n) numericZero",
    "fromMaybe": "\\zero m -> m zero (\\x -> x)",
    "maybeToList": "\\m step zero -> m zero (\\x -> step x zero)",
    "listToMaybe": "\\xs zero some -> xs (\\x _ -> some x) zero",
    "catMaybes": "\\xs step zero -> xs (\\m rest -> m rest (\\x -> step x rest)) zero",
    "squashMaybe": "\\mm zero some -> mm zero (\\m -> m zero some)",
    "isLeft": "\\e yes no -> e (\\_ -> yes) (\\_ -> no)",
    "maybeEither": "\\e zero some -> e (\\m -> m zero (\\x -> some (\\onLeft _ -> onLeft x))) (\\m -> m zero (\\y -> some (\\_ onRight -> onRight y)))",
    "either": "\\onLeft onRight e -> e onLeft onRight",
    "numeralSuccessor": "\\n step zero -> step (n step zero)",
    "numeralAdd": "\\n m step zero -> n step (m step zero)",
}
LEAN_WITNESSES = {
    "foldr": "fun _ _ step zero xs => xs _ step zero",
    "foldl": "fun B _ step zero xs => xs (B → B) (fun x k acc => k (step acc x)) (fun x => x) zero",
    "length": "fun _ xs => xs Int (fun _ n => BehaviorExtendedNumeric.successor n) BehaviorExtendedNumeric.zero",
    "fromMaybe": "fun A zero m => m A zero (fun x => x)",
    "maybeToList": "fun _ m R step zero => m R zero (fun x => step x zero)",
    "listToMaybe": "fun _ xs R zero some => xs R (fun x _ => some x) zero",
    "catMaybes": "fun _ xs R step zero => xs R (fun m rest => m R rest (fun x => step x rest)) zero",
    "squashMaybe": "fun _ mm R zero some => mm R zero (fun m => m R zero some)",
    "isLeft": "fun _ _ e R yes no => e R (fun _ => yes) (fun _ => no)",
    "maybeEither": "fun _ _ e R zero some => e R (fun m => m R zero (fun x => some (fun _ onLeft _ => onLeft x))) (fun m => m R zero (fun y => some (fun _ _ onRight => onRight y)))",
    "either": "fun _ C _ onLeft onRight e => e C onLeft onRight",
    "numeralSuccessor": "fun n R step zero => step (n R step zero)",
    "numeralAdd": "fun n m R step zero => n R step (m R step zero)",
}
_HASKELL_WRONG_ROWS = [
    ("foldr_seed", "foldr", "\\_ zero _ -> zero"),
    ("foldr_reversed", "foldr", "\\step zero xs -> xs (\\x k acc -> k (step x acc)) (\\x -> x) zero"),
    ("foldl_seed", "foldl", "\\_ zero _ -> zero"),
    ("foldl_reversed", "foldl", "\\step zero xs -> xs (\\x rest -> step rest x) zero"),
    ("length_zero", "length", "\\_ -> numericZero"),
    ("fromMaybe_default", "fromMaybe", "\\zero _ -> zero"),
    ("maybeToList_empty", "maybeToList", "\\_ _ zero -> zero"),
    ("listToMaybe_nothing", "listToMaybe", "\\_ zero _ -> zero"),
    ("catMaybes_empty", "catMaybes", "\\_ _ zero -> zero"),
    ("squashMaybe_nothing", "squashMaybe", "\\_ zero _ -> zero"),
    ("isLeft_negated", "isLeft", "\\e yes no -> e (\\_ -> no) (\\_ -> yes)"),
    ("maybeEither_nothing", "maybeEither", "\\_ zero _ -> zero"),
    ("successor_identity", "numeralSuccessor", "\\n -> n"),
    ("add_first", "numeralAdd", "\\n _ -> n"),
]
_LEAN_WRONG_ROWS = [
    ("foldr_seed", "foldr", "fun _ _ _ zero _ => zero"),
    ("foldr_reversed", "foldr", "fun _ B step zero xs => xs (B → B) (fun x k acc => k (step x acc)) (fun x => x) zero"),
    ("foldl_seed", "foldl", "fun _ _ _ zero _ => zero"),
    ("foldl_reversed", "foldl", "fun B _ step zero xs => xs B (fun x rest => step rest x) zero"),
    ("length_zero", "length", "fun _ _ => BehaviorExtendedNumeric.zero"),
    ("fromMaybe_default", "fromMaybe", "fun _ zero _ => zero"),
    ("maybeToList_empty", "maybeToList", "fun _ _ _ _ zero => zero"),
    ("listToMaybe_nothing", "listToMaybe", "fun _ _ _ zero _ => zero"),
    ("catMaybes_empty", "catMaybes", "fun _ _ _ _ zero => zero"),
    ("squashMaybe_nothing", "squashMaybe", "fun _ _ _ zero _ => zero"),
    ("isLeft_negated", "isLeft", "fun _ _ e R yes no => e R (fun _ => no) (fun _ => yes)"),
    ("maybeEither_nothing", "maybeEither", "fun _ _ _ _ zero _ => zero"),
    ("successor_identity", "numeralSuccessor", "fun n => n"),
    ("add_first", "numeralAdd", "fun n _ => n"),
]
HASKELL_WRONG = {
    operation: {label: term for label, op, term in _HASKELL_WRONG_ROWS if op == operation}
    for operation in OPERATIONS
}
LEAN_WRONG = {
    operation: {label: term for label, op, term in _LEAN_WRONG_ROWS if op == operation}
    for operation in OPERATIONS
}
CONTROL_KINDS = {
    **{label: "fully_typed_wrong_candidate" for label, _, _ in _HASKELL_WRONG_ROWS},
    "either_specialized_wrong_handler": "monomorphic_observation_adapter_not_a_polymorphic_candidate",
}


def haskell_control_source():
    """Replay-only declarations/assertions; compile under the complete types."""
    declarations = list(HASKELL_NUMERIC_PROVIDERS)
    assertions = []
    for operation, term in HASKELL_WITNESSES.items():
        name = "extendedWitness_" + operation
        declarations += [name + " :: " + HASKELL_TYPES[operation], name + " = " + term]
        assertions.append((name, haskell_predicate(operation, name)))
    for label, operation, term in _HASKELL_WRONG_ROWS:
        name = "extendedWrong_" + label
        declarations += [name + " :: " + HASKELL_TYPES[operation], name + " = " + term]
        assertions.append((name, "Prelude.not (" + haskell_predicate(operation, name) + ")"))
    # Total parametric either has no alternate wrong handler at its full type:
    # a left payload cannot be passed to the b -> c handler. Explicitly test a
    # specialization of the observation adapter instead of faking such a term.
    declarations += [
        f"extendedWrong_eitherMono :: (Prelude.Int -> Prelude.Int) -> (Prelude.Int -> Prelude.Int) -> {heither('Prelude.Int', 'Prelude.Int')} -> Prelude.Int",
        "extendedWrong_eitherMono onLeft _ e = e onLeft onLeft",
    ]
    assertions.append(("either_specialized_wrong_handler",
        "Prelude.not (extendedWrong_eitherMono (\\x -> x+7) (\\x -> 3*x) (\\_ onRight -> onRight 2) == (6 :: Prelude.Int))"))
    return declarations, assertions


def lean_control_source():
    """Replay-only definitions plus kernel-checkable pass/rejection theorems."""
    lines = list(LEAN_NUMERIC_PROVIDERS)
    declarations = ["BehaviorExtendedNumeric.zero", "BehaviorExtendedNumeric.successor"]
    for operation, term in LEAN_WITNESSES.items():
        name = "BehaviorExtendedControl.witness_" + operation
        proof = name + "_passes"
        lines += [f"def {name} : {LEAN_TYPES[operation]} := {term}",
                  f"theorem {proof} : {lean_predicate(operation, name)} := by decide"]
        declarations += [name, proof]
    for label, operation, term in _LEAN_WRONG_ROWS:
        name = "BehaviorExtendedControl.wrong_" + label
        proof = name + "_rejected"
        lines += [f"def {name} : {LEAN_TYPES[operation]} := {term}",
                  f"theorem {proof} : BehaviorExtended.check_{operation} {name} = false := by decide"]
        declarations += [name, proof]
    name = "BehaviorExtendedControl.either_specialized_wrong_handler"
    lines += [
        "def BehaviorExtendedControl.eitherMono (onLeft _ : Int → Int) (e : BehaviorExtended.CEither Int Int) : Int := e Int onLeft onLeft",
        f"theorem {name} : (BehaviorExtendedControl.eitherMono (fun x => x+7) (fun x => 3*x) (fun _ _ onRight => onRight 2) == (6 : Int)) = false := by decide",
    ]
    declarations += ["BehaviorExtendedControl.eitherMono", name]
    lines += [f"#print axioms {name}" for name in declarations]
    return lines, declarations


# A supplied element default is an additional argument after the existing
# leading forall binders, not a change to the original source signature. It
# applies only to the original failure branch; seeding a nonempty fold with
# the default would change its observations and is explicitly forbidden.
_PARTIAL_ROWS = (
    ("church_case_028", "head", 251, "List a -> a", "church0", "a"),
    ("church_case_031", "last", 264, "List a -> a", "church0", "a"),
    ("church_case_033", "at", 270, "Int -> List a -> a", "church0", "a"),
    ("church_case_038", "foldl1", 292, "_ -> List a -> a", "church0", "a"),
    ("church_case_039", "foldr1", 295, "_ -> List a -> a", "church0", "a"),
    ("church_case_093", "fromJust", 558, "Maybe a -> a", "church0", "a"),
    ("church_case_105", "fromLeft", 608, "Either a _ -> a", "church0", "a"),
    ("church_case_106", "fromRight", 611, "Either _ b -> b", "church0", "b"),
    ("church_case_177", "maximumBy", 1010, "LE a -> List a -> a", "church0", "a"),
    ("church_case_178", "maximumOn", 1015, "LE b -> _ -> List a -> a", "church1", "a"),
    ("church_case_179", "minimumBy", 1020, "LE a -> List a -> a", "church0", "a"),
    ("church_case_180", "minimumOn", 1025, "LE b -> _ -> List a -> a", "church1", "a"),
    ("church_case_181", "minMaxBy", 1030, "LE a -> List a -> a `Pair` a", "church0", "a"),
    ("church_case_238", "minmaxElement", 1377, "LE a -> List a -> a `Pair` a", "church0", "a"),
    ("church_case_248", "atKey", 1444, "(a -> c -> Bool) -> c -> Dict a b -> b", "church2", "b"),
    ("church_case_290", "reduce", 1669, "_ -> List a -> a", "church0", "a"),
    ("church_case_316", "maximum", 1776, "LE a -> List a -> a", "church0", "a"),
    ("church_case_317", "minimum", 1779, "LE a -> List a -> a", "church0", "a"),
    ("church_case_318", "minMax", 1782, "LE a -> List a -> a `Pair` a", "church0", "a"),
)
_PARTIAL_SEMANTICS = {
    "head": "First element; empty list returns the supplied element default.",
    "last": "Last element; empty list returns the supplied element default.",
    "at": "Zero-based indexing. Negative indices and indices at least the list length return the supplied default; valid indices select exactly that element.",
    "foldl1": "On a nonempty list, fold left from its first element with f accumulator element. Only an empty list returns the supplied default.",
    "foldr1": "On a nonempty list, fold right from its last element with f element accumulator. Only an empty list returns the supplied default.",
    "fromJust": "Just x returns x; Nothing returns the supplied element default.",
    "fromLeft": "Left x returns x; every Right payload returns the supplied left-element default.",
    "fromRight": "Right y returns y; every Left payload returns the supplied right-element default.",
    "maximumBy": "Greatest element under the supplied LE; retain the last equivalent maximum. Empty input returns the supplied element default.",
    "maximumOn": "Greatest projected key under the supplied LE; return the original element, retaining the last key tie. Empty input returns an element default, not a key default.",
    "minimumBy": "Least element under the supplied LE; retain the last equivalent minimum. Empty input returns the supplied element default.",
    "minimumOn": "Least projected key under the supplied LE; return the original element, retaining the last key tie. Empty input returns an element default, not a key default.",
    "minMaxBy": "Return the Church pair (minimum, maximum), retaining the last tie for both. Empty input returns the Church pair (default, default).",
    "minmaxElement": "Return the Church pair (minimum, maximum), retaining the FIRST minimum and LAST maximum on ties. Empty input returns the Church pair (default, default).",
    "atKey": "Inspect entries in dictionary order using the supplied relation as relation storedKey queryKey. Return the first matching value; no match, including an empty dictionary, returns the supplied value default.",
    "reduce": "Exactly the source alias of foldl1: left-associative reduction of a nonempty list from its first element; empty input returns the default.",
    "maximum": "Exactly the source alias of maximumBy, including last-maximum ties and the empty default.",
    "minimum": "Exactly the source alias of minimumBy, including last-minimum ties and the empty default.",
    "minMax": "Exactly the source alias of minMaxBy, including last ties for both components and (default, default) on empty input.",
}


def _add_supplied_default(source_type, default_type):
    if source_type["tag"] == "forall":
        return {**source_type, "body": _add_supplied_default(source_type["body"], default_type)}
    return {"tag": "arrow", "domain": default_type, "codomain": source_type}


def _partial_examples(operation):
    """JSON-ready exact expected native observations, proposed but not run.

    '$default' means the exact argument supplied to this case, not a fixed
    constant. Every applicable fixture is replayed with both listed defaults.
    Church result pairs are decoded before comparing these native values.
    """
    if operation in ("head", "last"):
        return {"defaults": [-99, 99], "fixtures": [
            {"list": [], "expected": "$default"},
            {"list": [-5], "expected": -5},
            {"list": [2, -3, 4], "expected": 2 if operation == "head" else 4},
        ]}
    if operation == "at":
        return {"defaults": [-99, 99], "fixtures": [
            {"index": n, "list": xs, "expected": expected}
            for n, xs, expected in [
                (-1, [], "$default"), (0, [], "$default"), (3, [], "$default"),
                (-1, [4, 7, 9], "$default"), (0, [4, 7, 9], 4),
                (1, [4, 7, 9], 7), (2, [4, 7, 9], 9),
                (3, [4, 7, 9], "$default"), (9, [4, 7, 9], "$default"),
            ]
        ]}
    if operation in ("foldl1", "foldr1", "reduce"):
        return {"defaults": [-99, 99], "combine": "x - y", "fixtures": [
            {"list": [], "expected": "$default"},
            {"list": [-5], "expected": -5},
            {"list": [8, 3, 1], "expected": 6 if operation == "foldr1" else 4},
        ], "restriction": "The default is not an accumulator seed for nonempty lists."}
    if operation in ("fromJust", "fromLeft", "fromRight"):
        successful, failed = {
            "fromJust": ("Just", "Nothing"),
            "fromLeft": ("Left", "Right"),
            "fromRight": ("Right", "Left"),
        }[operation]
        return {"defaults": [-99, 99], "fixtures": [
            {"constructor": successful, "payload": -5, "expected": -5},
            {"constructor": successful, "payload": 2, "expected": 2},
            {"constructor": failed, "payload": None if failed == "Nothing" else False, "expected": "$default"},
        ], "other_either_payload_type": None if operation == "fromJust" else "Bool"}
    if operation == "atKey":
        return {"defaults": [-99, 99], "fixtures": [
            {"relation": "stored == query", "query": 2, "entries": [], "expected": "$default"},
            {"relation": "stored == query", "query": 2, "entries": [[1, 10], [2, 20], [3, 30]], "expected": 20},
            {"relation": "stored == query", "query": 9, "entries": [[1, 10], [2, 20]], "expected": "$default"},
            {"relation": "stored <= query", "query": 2, "entries": [[1, 10], [2, 20], [3, 30]], "expected": 10},
        ]}
    items = [{"key": 2, "tag": 10}, {"key": 1, "tag": 11},
             {"key": 1, "tag": 12}, {"key": 2, "tag": 13}]
    pair_result = operation in ("minMaxBy", "minmaxElement", "minMax")
    minimum = items[1] if operation == "minmaxElement" else items[2]
    maximum = items[3]
    expected = ([minimum, maximum] if pair_result else
                minimum if operation in ("minimumBy", "minimumOn", "minimum") else maximum)
    singleton = {"key": 7, "tag": 20}
    return {
        "defaults": [{"key": -99, "tag": -1}, {"key": 99, "tag": -2}],
        "relation": ("project element.key, compare integer keys with <="
                     if operation in ("minimumOn", "maximumOn")
                     else "compare elements by element.key with <="),
        "fixtures": [
            {"list": [], "expected": ["$default", "$default"] if pair_result else "$default"},
            {"list": [singleton], "expected": [singleton, singleton] if pair_result else singleton},
            {"list": items, "expected": expected},
        ],
        "restriction": "Compare the full returned element including its tag; key-only equality would miss the source's tie rule.",
    }


PARTIAL_CASES = {}
for _case_id, _name, _line, _signature, _default_binder, _source_default in _PARTIAL_ROWS:
    _row = _BY_ID[_case_id]
    if (_row["name"], _row["line"], _row["source_signature"], _row["classification"],
            _row["default_type"]) != (
            _name, _line, _signature, "requires_partiality",
            {"tag": "name", "name": _default_binder}):
        raise ValueError("partial coverage provenance changed: " + _case_id)
    _defaulted = _add_supplied_default(_row["expanded_type"], _row["default_type"])
    PARTIAL_CASES[_case_id] = {
        "id": _case_id, "operation": _name, "source_line": _line,
        "source_signature": _signature, "resolved_signature": _row["resolved_signature"],
        "original_expanded_type": _row["expanded_type"],
        "manifest_default_type": _row["default_type"],
        "source_default_type": _source_default,
        "haskell_default_type": render_type(_row["default_type"]),
        "lean_default_type": render_type(_row["default_type"], lean=True),
        "proposed_haskell_target": render_type(_defaulted),
        "proposed_lean_target": render_type(_defaulted, lean=True),
        "default_argument_position": "first value argument after all leading type binders",
        "proposed_semantics": _PARTIAL_SEMANTICS[_name],
        "proposed_observations": _partial_examples(_name),
        "acceptance_status": "coverage_register_only_not_probed",
        "included_in_acceptance_operations": False,
        "missing_evidence": (
            "Int indices are opaque without numeric selector evidence. Zero/successor alone is insufficient. A future exact inventory must supply and independently check a negative-index test and a total nonnegative iteration/selection primitive."
            if _name == "at" else
            "No live synthesis, independent Haskell execution, or Lean kernel replay has been performed for these proposed defaulted observations."
        ),
    }
if set(PARTIAL_CASES) != {
        row["id"] for row in _MANIFEST["cases"] if row["classification"] == "requires_partiality"
        } or len(PARTIAL_CASES) != 19:
    raise ValueError("partial coverage must match all 19 manifest cases exactly")
PARTIAL_COVERAGE = PARTIAL_CASES


def source_provenance(manifest_path=HERE / "manifest.json"):
    """Return precise source/extension/provider/default boundaries for receipts."""
    manifest_path, source_path, manifest, by_id = _load_source_manifest(manifest_path)
    return {
        "specification_path": str(Path(__file__).resolve()),
        "specification_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "manifest_path": str(manifest_path),
        "manifest_sha256": hashlib.sha256(manifest_path.read_bytes()).hexdigest(),
        "church_source_path": str(source_path),
        "church_source_raw_sha256": hashlib.sha256(source_path.read_bytes()).hexdigest(),
        "church_source_canonical_lf_sha256": manifest["source_sha256"],
        "operations": {
            operation: {
                **OPERATION_PROVENANCE[operation],
                "haskell_target": HASKELL_TYPES[operation],
                "lean_target": LEAN_TYPES[operation],
                "required_providers": REQUIRED_PROVIDERS[operation],
                "finite_observation_count": OBSERVATIONS[operation],
                **({"expanded_type": by_id[SOURCE_CASE_IDS[operation]]["expanded_type"]}
                   if operation in SOURCE_CASE_IDS else {}),
            }
            for operation in OPERATIONS
        },
        "partial_cases": PARTIAL_CASES,
        "oracle_inventory_policy": "Observation adapters and controls are replay/predicate-only; they are never synthesis providers.",
        "validation_status": "specification_only; no engine, GHC, or Lean acceptance is asserted",
    }

"""Six Church-operation specifications shared by the two host-language runners.

Only types and oracle expressions enter synthesis commands. Known witnesses and
wrong implementations below are emitted exclusively into independent replay.
The Haskell where clauses are self-contained: oracle helpers never enter the
loaded declaration inventory. Lean oracle declarations require providers off.
"""
from __future__ import annotations

import itertools

OPERATIONS = ("not", "swap", "map", "append", "reverse", "filter")
SOURCE_LINES = dict(zip(OPERATIONS, (170, 195, 397, 391, 412, 406)))
SOURCE_SIGNATURES = {
    "not": "Bool -> Bool", "swap": "a `Pair` b -> b `Pair` a",
    "map": "_ -> List a -> List b", "append": "List a -> List a -> List a",
    "reverse": "List a -> List a", "filter": "(a -> Bool) -> List a -> List a",
}


def hlist(a):
    return f"(forall r. ({a} -> r -> r) -> r -> r)"


def hpair(a, b):
    return f"(forall r. ({a} -> {b} -> r) -> r)"


HBOOL = "(forall r. r -> r -> r)"
HASKELL_TYPES = {
    "not": f"{HBOOL} -> {HBOOL}",
    "swap": f"forall a b. {hpair('a', 'b')} -> {hpair('b', 'a')}",
    "map": f"forall a b. (a -> b) -> {hlist('a')} -> {hlist('b')}",
    "append": f"forall a. {hlist('a')} -> {hlist('a')} -> {hlist('a')}",
    "reverse": f"forall a. {hlist('a')} -> {hlist('a')}",
    "filter": f"forall a. (a -> {HBOOL}) -> {hlist('a')} -> {hlist('a')}",
}

LEAN_TYPES = {
    "not": "BehaviorChurch.CBool → BehaviorChurch.CBool",
    "swap": "∀ A B : Type, BehaviorChurch.CPair A B → BehaviorChurch.CPair B A",
    "map": "∀ A B : Type, (A → B) → BehaviorChurch.CList A → BehaviorChurch.CList B",
    "append": "∀ A : Type, BehaviorChurch.CList A → BehaviorChurch.CList A → BehaviorChurch.CList A",
    "reverse": "∀ A : Type, BehaviorChurch.CList A → BehaviorChurch.CList A",
    "filter": "∀ A : Type, (A → BehaviorChurch.CBool) → BehaviorChurch.CList A → BehaviorChurch.CList A",
}

# Forty lists for unary operations; thirteen lists produce 169 append pairs.
INPUTS = [list(xs) for n in range(4) for xs in itertools.product((-1, 0, 1), repeat=n)]
APPEND_INPUTS = [xs for xs in INPUTS if len(xs) <= 2]
OBSERVATIONS = {"not": 2, "swap": 15, "map": 200, "append": 169,
                "reverse": 40, "filter": 200}

_HHELPERS = [
    f"enc :: forall a. [a] -> {hlist('a')}",
    "enc xs step zero = Prelude.foldr step zero xs",
    f"dec :: forall a. {hlist('a')} -> [a]", "dec xs = xs (:) []",
    f"encBool :: Prelude.Bool -> {HBOOL}",
    "encBool b yes no = if b then yes else no",
    f"decBool :: {HBOOL} -> Prelude.Bool", "decBool b = b Prelude.True Prelude.False",
    f"encPair :: forall a b. (a, b) -> {hpair('a', 'b')}",
    "encPair (a, b) k = k a b",
    f"decPair :: forall a b. {hpair('a', 'b')} -> (a, b)", "decPair p = p (,)",
    "inputs :: [[Prelude.Int]]", "inputs = " + repr(INPUTS),
    "shortInputs :: [[Prelude.Int]]", "shortInputs = " + repr(APPEND_INPUTS),
    "transforms :: [Prelude.Int -> Prelude.Int]",
    "transforms = [(\\x -> x + 2), (\\x -> 2 * x - 1), Prelude.negate, (\\_ -> 7)]",
    "predicates :: [Prelude.Int -> Prelude.Bool]",
    "predicates = [(\\_ -> Prelude.True), (\\_ -> Prelude.False), Prelude.even, (\\x -> x < 0), (\\x -> x == 1)]",
]


def haskell_predicate(operation, candidate):
    """A closed HOST_BOOL except for the exact typed candidate binder."""
    f = candidate
    bodies = {
        "not": f"Prelude.and [decBool ({f} (encBool x)) == Prelude.not x | x <- [Prelude.False, Prelude.True]]",
        "swap": f"Prelude.and [decPair ({f} (encPair (x, y))) == (y, x) | x <- ([-1,0,2] :: [Prelude.Int]), y <- ([-1,0,2] :: [Prelude.Int])] && Prelude.and [decPair ({f} (encPair (x, y))) == (y, x) | x <- ([-1,0,2] :: [Prelude.Int]), y <- [Prelude.False, Prelude.True]]",
        "map": f"Prelude.and [dec ({f} g (enc xs)) == Prelude.map g xs | xs <- inputs, g <- transforms] && Prelude.and [dec ({f} (\\x -> x >= 0) (enc xs)) == Prelude.map (\\x -> x >= 0) xs | xs <- inputs]",
        "append": f"Prelude.and [dec ({f} (enc xs) (enc ys)) == xs Prelude.++ ys | xs <- shortInputs, ys <- shortInputs]",
        "reverse": f"Prelude.and [dec ({f} (enc xs)) == Prelude.reverse xs | xs <- inputs]",
        "filter": f"Prelude.and [dec ({f} (\\x -> encBool (p x)) (enc xs)) == Prelude.filter p xs | xs <- inputs, p <- predicates]",
    }
    return "let { " + "; ".join(_HHELPERS) + " } in " + bodies[operation]


def lean_prelude():
    """Computable, universe-zero observation adapters; no synthesized operation."""
    return [
        "set_option linter.unusedVariables false",
        "abbrev BehaviorChurch.CBool := ∀ R : Type, R → R → R",
        "abbrev BehaviorChurch.CPair (A B : Type) := ∀ R : Type, (A → B → R) → R",
        "abbrev BehaviorChurch.CList (A : Type) := ∀ R : Type, (A → R → R) → R → R",
        "def BehaviorChurch.enc {A : Type} (xs : List A) : BehaviorChurch.CList A := fun _ step zero => xs.foldr step zero",
        "def BehaviorChurch.dec {A : Type} (xs : BehaviorChurch.CList A) : List A := xs (List A) List.cons []",
        "def BehaviorChurch.encBool (b : Bool) : BehaviorChurch.CBool := fun _ yes no => if b then yes else no",
        "def BehaviorChurch.decBool (b : BehaviorChurch.CBool) : Bool := b Bool true false",
        "def BehaviorChurch.encPair {A B : Type} (p : A × B) : BehaviorChurch.CPair A B := fun _ k => k p.1 p.2",
        "def BehaviorChurch.decPair {A B : Type} (p : BehaviorChurch.CPair A B) : A × B := p (A × B) Prod.mk",
        "def BehaviorChurch.inputs : List (List Int) := " + repr(INPUTS),
        "def BehaviorChurch.shortInputs : List (List Int) := " + repr(APPEND_INPUTS),
        "def BehaviorChurch.transforms : List (Int → Int) := [(fun x => x + 2), (fun x => 2 * x - 1), (fun x => -x), (fun _ => 7)]",
        "def BehaviorChurch.predicates : List (Int → Bool) := [(fun _ => true), (fun _ => false), (fun x => decide (x % 2 = 0)), (fun x => decide (x < 0)), (fun x => decide (x = 1))]",
        f"def BehaviorChurch.check_not (f : {LEAN_TYPES['not']}) : Bool := [false, true].all (fun x => BehaviorChurch.decBool (f (BehaviorChurch.encBool x)) == !x)",
        f"def BehaviorChurch.check_swap (f : {LEAN_TYPES['swap']}) : Bool := ([-1,0,2] : List Int).all (fun x => ([-1,0,2] : List Int).all (fun y => BehaviorChurch.decPair (f Int Int (BehaviorChurch.encPair (x,y))) == (y,x))) && ([-1,0,2] : List Int).all (fun x => [false,true].all (fun y => BehaviorChurch.decPair (f Int Bool (BehaviorChurch.encPair (x,y))) == (y,x)))",
        f"def BehaviorChurch.check_map (f : {LEAN_TYPES['map']}) : Bool := BehaviorChurch.inputs.all (fun xs => BehaviorChurch.transforms.all (fun g => BehaviorChurch.dec (f Int Int g (BehaviorChurch.enc xs)) == xs.map g)) && BehaviorChurch.inputs.all (fun xs => BehaviorChurch.dec (f Int Bool (fun x => decide (x ≥ 0)) (BehaviorChurch.enc xs)) == xs.map (fun x => decide (x ≥ 0)))",
        f"def BehaviorChurch.check_append (f : {LEAN_TYPES['append']}) : Bool := BehaviorChurch.shortInputs.all (fun xs => BehaviorChurch.shortInputs.all (fun ys => BehaviorChurch.dec (f Int (BehaviorChurch.enc xs) (BehaviorChurch.enc ys)) == xs ++ ys))",
        f"def BehaviorChurch.check_reverse (f : {LEAN_TYPES['reverse']}) : Bool := BehaviorChurch.inputs.all (fun xs => BehaviorChurch.dec (f Int (BehaviorChurch.enc xs)) == xs.reverse)",
        f"def BehaviorChurch.check_filter (f : {LEAN_TYPES['filter']}) : Bool := BehaviorChurch.inputs.all (fun xs => BehaviorChurch.predicates.all (fun p => BehaviorChurch.dec (f Int (fun x => BehaviorChurch.encBool (p x)) (BehaviorChurch.enc xs)) == xs.filter p))",
    ]


def lean_predicate(operation, candidate):
    return f"BehaviorChurch.check_{operation} {candidate} = true"


# Independent oracle sanity controls. They must NEVER be copied into the live
# transcript or loaded environment. Witnesses show the oracle is satisfiable;
# wrong implementations show it discriminates more than type inhabitation.
HASKELL_WITNESSES = {
    "not": "\\b yes no -> b no yes",
    "swap": "\\p k -> p (\\x y -> k y x)",
    "map": "\\g xs step zero -> xs (\\x rest -> step (g x) rest) zero",
    "append": "\\xs ys step zero -> xs step (ys step zero)",
    "reverse": "\\xs step zero -> xs (\\x k rest -> k (step x rest)) (\\x -> x) zero",
    "filter": "\\p xs step zero -> xs (\\x rest -> p x (step x rest) rest) zero",
}
LEAN_WITNESSES = {
    "not": "fun b R yes no => b R no yes",
    "swap": "fun _ _ p R k => p R (fun x y => k y x)",
    "map": "fun _ _ g xs R step zero => xs R (fun x rest => step (g x) rest) zero",
    "append": "fun _ xs ys R step zero => xs R step (ys R step zero)",
    "reverse": "fun _ xs R step zero => xs (R → R) (fun x k rest => k (step x rest)) (fun x => x) zero",
    "filter": "fun _ p xs R step zero => xs R (fun x rest => p x R (step x rest) rest) zero",
}
HASKELL_WRONG = [
    ("not_identity", "not", "\\b -> b"),
    ("not_constant_false", "not", "\\_ _ no -> no"),
    ("map_empty", "map", "\\_ _ _ zero -> zero"),
    ("append_first", "append", "\\xs _ -> xs"),
    ("append_reversed", "append", "\\xs ys step zero -> ys step (xs step zero)"),
    ("reverse_identity", "reverse", "\\xs -> xs"),
    ("reverse_empty", "reverse", "\\_ _ zero -> zero"),
    ("filter_identity", "filter", "\\_ xs -> xs"),
    ("filter_empty", "filter", "\\_ _ _ zero -> zero"),
    ("filter_negated", "filter", "\\p xs step zero -> xs (\\x rest -> p x rest (step x rest)) zero"),
]
LEAN_WRONG = [
    ("not_identity", "not", "fun b => b"),
    ("not_constant_false", "not", "fun _ _ _ no => no"),
    ("map_empty", "map", "fun _ _ _ _ _ _ zero => zero"),
    ("append_first", "append", "fun _ xs _ => xs"),
    ("append_reversed", "append", "fun _ xs ys R step zero => ys R step (xs R step zero)"),
    ("reverse_identity", "reverse", "fun _ xs => xs"),
    ("reverse_empty", "reverse", "fun _ _ _ _ zero => zero"),
    ("filter_identity", "filter", "fun _ _ xs => xs"),
    ("filter_empty", "filter", "fun _ _ _ _ _ zero => zero"),
    ("filter_negated", "filter", "fun _ p xs R step zero => xs R (fun x rest => p x R rest (step x rest)) zero"),
]


def haskell_control_source():
    declarations, assertions = [], []
    for operation, term in HASKELL_WITNESSES.items():
        name = "oracleWitness_" + operation
        declarations += [name + " :: " + HASKELL_TYPES[operation], name + " = " + term]
        assertions.append((name, haskell_predicate(operation, name)))
    for label, operation, term in HASKELL_WRONG:
        name = "oracleWrong_" + label
        declarations += [name + " :: " + HASKELL_TYPES[operation], name + " = " + term]
        assertions.append((name, "Prelude.not (" + haskell_predicate(operation, name) + ")"))
    # Polymorphic total swap has no wrong parametric inhabitant. This control is
    # explicitly a monomorphic oracle-adapter check, not a candidate at its type.
    assertions.append(("swap_specialized_identity_rejected",
        "Prelude.not (Prelude.and [(x,y) == (y,x) | x <- ([-1,0,2] :: [Prelude.Int]), y <- [-1,0,2]])"))
    return declarations, assertions


def lean_control_source():
    lines, declarations = [], []
    for operation, term in LEAN_WITNESSES.items():
        name = "BehaviorControl.witness_" + operation
        proof = name + "_passes"
        lines += [f"def {name} : {LEAN_TYPES[operation]} := {term}",
                  f"theorem {proof} : {lean_predicate(operation, name)} := by decide",
                  f"#print axioms {name}", f"#print axioms {proof}"]
        declarations += [name, proof]
    for label, operation, term in LEAN_WRONG:
        name = "BehaviorControl.wrong_" + label
        proof = name + "_rejected"
        lines += [f"def {name} : {LEAN_TYPES[operation]} := {term}",
                  f"theorem {proof} : BehaviorChurch.check_{operation} {name} = false := by decide",
                  f"#print axioms {name}", f"#print axioms {proof}"]
        declarations += [name, proof]
    name = "BehaviorControl.swap_specialized_identity_rejected"
    lines += [f"theorem {name} : (([-1,0,2] : List Int).all (fun x => ([-1,0,2] : List Int).all (fun y => (x,y) == (y,x)))) = false := by decide",
              f"#print axioms {name}"]
    declarations.append(name)
    return lines, declarations

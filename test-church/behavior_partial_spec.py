"""Executable oracle specifications for all 19 supplied-default Church cases.

This module is a specification, not a synthesis or compiler acceptance receipt.
The original signatures remain classified requires_partiality. Each NEW target
inserts the documented element/value default as its first value argument after
all original leading type binders; no source binder is reordered or removed.
Int indexing remains Int indexing, including negative indices.

Runner API: OPERATIONS, SOURCE_CASE_IDS, PARTIAL_CASES, ORIGINAL_HASKELL_TYPES,
ORIGINAL_LEAN_TYPES, HASKELL_TYPES, LEAN_TYPES, FIXTURES, OBSERVATIONS,
REQUIRED_PROVIDERS, search_provider_inventory, search_provider_source,
haskell_predicate, lean_prelude, lean_predicate, HASKELL_WITNESSES/WRONG,
LEAN_WITNESSES/WRONG, and haskell_control_source/lean_control_source.
PROVIDER_WITNESSES in each language add a primitive-only at witness;
provider_control_source checks that primitive independently, and
CONTROL_COUNTEREXAMPLES identifies a separating observation for every control.
Predicates take the exact fully polymorphic candidate name. Haskell predicates
include their local observation helpers. Lean predicates require lean_prelude.
Control-source functions return (declarations, named_assertions/declarations);
their oracle helpers MUST be confined to independent compiler replay.

Discovery must be off. Only at permits one explicit generic Int eliminator.
Every observation helper, native-data decoder, comparator fixture, positive
witness, wrong control, and target implementation is excluded from search.
The passed comparator/relation/combiner is a target argument, not a provider.
All generated witness definitions are total on finite inputs and use explicit
defaults. No bottom, exception, unchecked Lean declaration, or inhabitance
instance is used. Finite probes do not prove full extensional correctness.

Running --prepare-only DIR writes reviewable sources and a coverage register;
it does not invoke a compiler, test runner, synthesis engine, or subprocess.
Importing this module only reads and validates source/manifest provenance.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
from pathlib import Path
import re

import behavior_extended_spec as extended
from behavior_runtime import render_type

HERE = Path(__file__).resolve().parent
REGISTER = extended.PARTIAL_CASES
OPERATIONS = tuple(row["operation"] for row in REGISTER.values())
SOURCE_CASE_IDS = {row["operation"]: case_id for case_id, row in REGISTER.items()}
SOURCE_LINES = {row["operation"]: row["source_line"] for row in REGISTER.values()}
SOURCE_SIGNATURES = {row["operation"]: row["source_signature"] for row in REGISTER.values()}
STATUS = "executable_specification_only_not_synthesized_or_compiler_replayed"
UNAVAILABLE_OPERATIONS = {}


def _with_default(source, default):
    if source["tag"] == "forall":
        return {**source, "body": _with_default(source["body"], default)}
    return {"tag": "arrow", "domain": copy.deepcopy(default), "codomain": copy.deepcopy(source)}


def _leading_binders(source):
    binders = []
    while source["tag"] == "forall":
        binders.extend(source["binders"])
        source = source["body"]
    return binders


_manifest = json.loads((HERE / "manifest.json").read_text(encoding="utf-8"))
_source = HERE.parent / _manifest["source"]
_source_text = _source.read_text(encoding="utf-8-sig")
SOURCE_SHA256 = hashlib.sha256(_source_text.encode("utf-8")).hexdigest()
if SOURCE_SHA256 != _manifest["source_sha256"] or SOURCE_SHA256 != extended.EXPECTED_SOURCE_SHA256:
    raise ValueError("Church source changed; review supplied-default specifications")
_rows = {row["id"]: row for row in _manifest["cases"]}
_partial_ids = {row["id"] for row in _manifest["cases"] if row["classification"] == "requires_partiality"}
if len(OPERATIONS) != 19 or len(set(OPERATIONS)) != 19 or set(REGISTER) != _partial_ids:
    raise ValueError("the supplied-default specification must retain all 19 manifest identities")

ORIGINAL_TYPES = {}
DEFAULTED_TYPES = {}
for _case_id, _registered in REGISTER.items():
    _row = _rows[_case_id]
    _op = _registered["operation"]
    if (_row["name"], _row["line"], _row["source_signature"], _row["expanded_type"],
        _row["default_type"]) != (
        _op, _registered["source_line"], _registered["source_signature"],
        _registered["original_expanded_type"], _registered["manifest_default_type"]
    ):
        raise ValueError("changed partial-case provenance: " + _case_id)
    if re.fullmatch(re.escape(_op) + r"\s*(?:::|∷)\s*" + re.escape(_row["source_signature"]),
                    _source_text.splitlines()[_row["line"] - 1]) is None:
        raise ValueError("partial-case source signature differs from manifest: " + _case_id)
    ORIGINAL_TYPES[_op] = copy.deepcopy(_row["expanded_type"])
    DEFAULTED_TYPES[_op] = _with_default(_row["expanded_type"], _row["default_type"])
    if (render_type(DEFAULTED_TYPES[_op]), render_type(DEFAULTED_TYPES[_op], lean=True)) != (
            _registered["proposed_haskell_target"], _registered["proposed_lean_target"]):
        raise ValueError("default insertion changed the registered target: " + _case_id)

ORIGINAL_HASKELL_TYPES = {op: render_type(ty) for op, ty in ORIGINAL_TYPES.items()}
ORIGINAL_LEAN_TYPES = {op: render_type(ty, lean=True) for op, ty in ORIGINAL_TYPES.items()}
HASKELL_TYPES = {op: render_type(ty) for op, ty in DEFAULTED_TYPES.items()}
LEAN_TYPES = {op: render_type(ty, lean=True) for op, ty in DEFAULTED_TYPES.items()}
DEFAULT_TYPES = {op: copy.deepcopy(REGISTER[SOURCE_CASE_IDS[op]]["manifest_default_type"])
                 for op in OPERATIONS}

# This is ordinary integer case analysis. It knows nothing about lists, Church
# encodings, indices, defaults, or target names. Positive predecessors are safe
# even for bounded Haskell Int: subtraction is never performed at minBound.
_INT_PROVIDER = {
    "haskell": [{
        "name": "partialIntCase",
        "type": "forall r. Int -> r -> r -> (Int -> r) -> r",
        "definition": r"\n negative zero positive -> if n < 0 then negative else if n == 0 then zero else positive (n - 1)",
    }],
    "lean": [{
        "name": "BehaviorPartialNumeric.intCase",
        "type": "∀ R : Type, Int → R → R → (Int → R) → R",
        "definition": "fun _ n negative zero positive => if n < 0 then negative else if n = 0 then zero else positive (n - 1)",
    }],
}


def search_provider_inventory(operation):
    if operation not in OPERATIONS:
        raise KeyError(operation)
    entries = _INT_PROVIDER if operation == "at" else {"haskell": [], "lean": []}
    return {
        **copy.deepcopy(entries), "discovery": "off; exact explicit allowlist only",
        "rationale": (
            "A generic negative/zero/positive-predecessor Int eliminator. A Church list can fold to Int -> a; no list selector or target implementation is supplied."
            if operation == "at" else
            "The target already supplies its heterogeneous relation as a Church Boolean selector; no numeric or lookup provider is needed."
            if operation == "atKey" else
            "The target already supplies its binary operation, and the Church list supplies iteration. No numeric or reduction provider is needed."
            if operation in ("foldl1", "foldr1", "reduce") else
            "Church eliminators and the original explicit target arguments suffice; no additional value provider is admitted."
        ),
    }


REQUIRED_PROVIDERS = {op: search_provider_inventory(op) for op in OPERATIONS}


def search_provider_source(operation, language):
    """Standalone declarations for exactly one query's permitted inventory."""
    if language not in ("haskell", "lean"):
        raise ValueError("language must be haskell or lean")
    providers = REQUIRED_PROVIDERS[operation][language]
    if language == "haskell":
        return [line for p in providers for line in (
            p["name"] + " :: " + p["type"], p["name"] + " = " + p["definition"])]
    if not providers:
        return []
    return ["namespace BehaviorPartialNumeric"] + [
        "def " + p["name"].split(".")[-1] + " : " + p["type"] + " := " + p["definition"]
        for p in providers
    ] + ["end BehaviorPartialNumeric"]


def _hp(a, b):
    return f"(forall r. ({a} -> {b} -> r) -> r)"


_H_OBSERVERS = [
    f"partialEnc :: forall a. [a] -> {extended.hlist('a')}",
    "partialEnc xs step zero = Prelude.foldr step zero xs",
    f"partialDec :: forall a. {extended.hlist('a')} -> [a]",
    "partialDec xs = xs (:) []",
    f"partialMaybe :: forall a. Prelude.Maybe a -> {extended.hmaybe('a')}",
    "partialMaybe m zero some = Prelude.maybe zero some m",
    f"partialEither :: forall a b. Prelude.Either a b -> {extended.heither('a', 'b')}",
    "partialEither e onLeft onRight = Prelude.either onLeft onRight e",
    "partialBool :: Prelude.Bool -> (forall r. r -> r -> r)",
    "partialBool flag yes no = if flag then yes else no",
    f"partialPair :: forall a b. (a,b) -> {_hp('a', 'b')}",
    "partialPair (x,y) pair = pair x y",
    f"partialDecPair :: forall a b. {_hp('a', 'b')} -> (a,b)",
    "partialDecPair pair = pair (,)",
    f"partialDict :: forall a b. [(a,b)] -> {extended.hlist(_hp('a', 'b'))}",
    "partialDict xs step zero = Prelude.foldr (\\(k,v) rest -> step (\\pair -> pair k v) rest) zero xs",
]
_L_OBSERVERS = [
    "namespace BehaviorPartial",
    "universe u v",
    "abbrev CList (A : Type u) := ∀ R : Type, (A → R → R) → R → R",
    "abbrev CMaybe (A : Type u) := ∀ R : Type, R → (A → R) → R",
    "abbrev CEither (A : Type u) (B : Type v) := ∀ R : Type, (A → R) → (B → R) → R",
    "abbrev CPair (A : Type u) (B : Type v) := ∀ R : Type, (A → B → R) → R",
    "abbrev CBool := ∀ R : Type, R → R → R",
    "def enc {A : Type} (xs : List A) : CList A := fun _ step zero => xs.foldr step zero",
    "def dec {A : Type} (xs : CList A) : List A := xs (List A) List.cons []",
    "def maybe {A : Type} (m : Option A) : CMaybe A := fun _ zero some => m.elim zero some",
    "def either {A B : Type} (e : Sum A B) : CEither A B := fun _ onLeft onRight => e.elim onLeft onRight",
    "def bool (flag : Bool) : CBool := fun _ yes no => if flag then yes else no",
    "def pair {A B : Type} (p : A × B) : CPair A B := fun _ k => k p.1 p.2",
    "def decPair {A B : Type} (p : CPair A B) : A × B := p (A × B) Prod.mk",
    "def dict {A B : Type} (xs : List (A × B)) : CList (CPair A B) := fun _ step zero => xs.foldr (fun p rest => step (fun _ k => k p.1 p.2) rest) zero",
    "end BehaviorPartial",
]

# Expected payloads are native, finite JSON data. Language encoders are used
# only to construct inputs and observe outputs, never to compute expectations.
INT = ("Prelude.Int", "Int")
BOOL = ("Prelude.Bool", "Bool")
STRING = ("Prelude.String", "String")
TAG = ("(Prelude.Int, Prelude.Int)", "(Int × Int)")
MIX = ("(Prelude.Int, Prelude.Bool)", "(Int × Bool)")
INTS = ("[Prelude.Int]", "(List Int)")


def _literal(value, language):
    if isinstance(value, bool):
        return ("Prelude.True" if value else "Prelude.False") if language == "haskell" else ("true" if value else "false")
    if isinstance(value, int):
        return "(" + str(value) + ")"
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, tuple):
        return "(" + ",".join(_literal(x, language) for x in value) + ")"
    if isinstance(value, list):
        return "[" + ",".join(_literal(x, language) for x in value) + "]"
    raise TypeError("unsupported native fixture literal: " + repr(value))


def _native(value, ty):
    return tuple("(" + _literal(value, lang) + (" :: " if lang == "haskell" else " : ")
                 + ty[index] + ")" for index, lang in enumerate(("haskell", "lean")))


def _church_list(xs, ty):
    native = _native(xs, ("[" + ty[0] + "]", "(List " + ty[1] + ")"))
    return ("(partialEnc " + native[0] + ")", "(BehaviorPartial.enc " + native[1] + ")")


FIXTURES = {op: [] for op in OPERATIONS}


def _observe(operation, label, types, arguments, expected, result_type, *, native, pair=False):
    row = {
        "id": operation + "_" + label,
        "source_case_id": SOURCE_CASE_IDS[operation],
        "haskell_type_arguments": [t[0] for t in types],
        "lean_type_arguments": [t[1] for t in types],
        "haskell_arguments": [arg[0] for arg in arguments],
        "lean_arguments": [arg[1] for arg in arguments],
        "expected_native": expected,
        "haskell_expected": _native(expected, result_type)[0],
        "lean_expected": _native(expected, result_type)[1],
        "decode": "church_pair" if pair else "identity",
        "native_inputs": native,
    }
    FIXTURES[operation].append(row)


_PAYLOADS = [
    ("int", INT, [-99, 99], [[], [-5], [2, -3, 4], [4, 7]]),
    ("bool", BOOL, [False, True], [[], [False], [True], [True, False], [False, True]]),
    ("mixed", MIX, [(-99, False), (99, True)], [[], [(1, False)], [(2, True), (-1, False)]]),
]
for _group, _ty, _defaults, _lists in _PAYLOADS:
    for _di, _default in enumerate(_defaults):
        for _xi, _xs in enumerate(_lists):
            for _op in ("head", "last"):
                _expected = _default if not _xs else _xs[0 if _op == "head" else -1]
                _observe(_op, f"{_group}_{_di}_{_xi}", [_ty],
                         [_native(_default, _ty), _church_list(_xs, _ty)], _expected, _ty,
                         native={"default": _default, "list": _xs})
            for _n in (-2, -1, 0, 1, 2, 3, 9):
                _expected = _xs[_n] if 0 <= _n < len(_xs) else _default
                _observe("at", f"{_group}_{_di}_{_xi}_{_n}".replace("-", "neg"), [_ty],
                         [_native(_default, _ty), _native(_n, INT), _church_list(_xs, _ty)],
                         _expected, _ty, native={"default": _default, "index": _n, "list": _xs})


def _fold_expected(xs, default, combine, right=False):
    if not xs:
        return default
    if right:
        value = xs[-1]
        for x in reversed(xs[:-1]):
            value = combine(x, value)
        return value
    value = xs[0]
    for x in xs[1:]:
        value = combine(value, x)
    return value


_FOLDS = [
    ("int", INT, [-99, 99], [[], [-5], [8, 3, 1], [2, -3, 4, 1]], [
        ("subtract", (r"(\x y -> x - y)", "(fun x y => x - y)"), lambda x, y: x - y),
        ("affine", (r"(\x y -> 2*x + y)", "(fun x y => 2*x + y)"), lambda x, y: 2*x + y),
    ]),
    ("bool", BOOL, [False, True], [[], [False], [True], [False, True, False]], [
        ("implication", (r"(\x y -> Prelude.not x || y)", "(fun x y => (!x) || y)"), lambda x, y: not x or y),
    ]),
    ("list_payload", INTS, [[-99], [99]], [[], [[1]], [[1], [2, 3], [4]]], [
        ("append", (r"(\x y -> x ++ y)", "(fun x y => x ++ y)"), lambda x, y: x + y),
        ("reverse_left", (r"(\x y -> Prelude.reverse x ++ y)", "(fun x y => x.reverse ++ y)"), lambda x, y: list(reversed(x)) + y),
    ]),
]
for _group, _ty, _defaults, _lists, _steps in _FOLDS:
    for _di, _default in enumerate(_defaults):
        for _xi, _xs in enumerate(_lists):
            for _step_name, _step, _combine in _steps:
                for _op in ("foldl1", "foldr1", "reduce"):
                    _observe(_op, f"{_group}_{_di}_{_xi}_{_step_name}", [_ty],
                             [_native(_default, _ty), _step, _church_list(_xs, _ty)],
                             _fold_expected(_xs, _default, _combine, _op == "foldr1"), _ty,
                             native={"default": _default, "list": _xs, "combine": _step_name})

for _group, _ty, _defaults, _lists in _PAYLOADS:
    _payloads = [None] + [xs[0] for xs in _lists if xs]
    for _di, _default in enumerate(_defaults):
        for _mi, _payload in enumerate(_payloads):
            _h = "Prelude.Nothing" if _payload is None else "(Prelude.Just " + _native(_payload, _ty)[0] + ")"
            _l = "none" if _payload is None else "(some " + _native(_payload, _ty)[1] + ")"
            _observe("fromJust", f"{_group}_{_di}_{_mi}", [_ty], [
                _native(_default, _ty), ("(partialMaybe (" + _h + "))", "(BehaviorPartial.maybe (" + _l + "))")
            ], _default if _payload is None else _payload, _ty,
                native={"default": _default, "option": _payload})

for _group, _result_ty, _other_ty, _defaults, _values, _others in [
    ("int_bool", INT, BOOL, [-99, 99], [-5, 2], [False, True]),
    ("bool_int", BOOL, INT, [False, True], [False, True], [-5, 2]),
    ("mixed_string", MIX, STRING, [(-99, False), (99, True)], [(1, False), (2, True)], ["", "other"]),
]:
    for _op in ("fromLeft", "fromRight"):
        for _di, _default in enumerate(_defaults):
            for _success in (False, True):
                for _vi, _payload in enumerate(_values if _success else _others):
                    _ty = _result_ty if _success else _other_ty
                    _left = _success == (_op == "fromLeft")
                    _hcon, _lcon = ("Prelude.Left", "Sum.inl") if _left else ("Prelude.Right", "Sum.inr")
                    _payload_expr = _native(_payload, _ty)
                    _arg = ("(partialEither (" + _hcon + " " + _payload_expr[0] + "))",
                            "(BehaviorPartial.either (" + _lcon + " " + _payload_expr[1] + "))")
                    # In BOTH source signatures the returned type is the first
                    # binder; fromRight's unused left payload is the second.
                    _observe(_op, f"{_group}_{_di}_{_success}_{_vi}", [_result_ty, _other_ty],
                             [_native(_default, _result_ty), _arg],
                             _payload if _success else _default, _result_ty,
                             native={"default": _default, "constructor": "Left" if _left else "Right", "payload": _payload})

_MAXIMUM = ("maximumBy", "maximumOn", "maximum")
_MINIMUM = ("minimumBy", "minimumOn", "minimum")
_PAIRS = ("minMaxBy", "minmaxElement", "minMax")
_EXTREMA = _MAXIMUM + _MINIMUM + _PAIRS
_RELATIONS = [
    ("ascending", ("(<=)", "(fun x y => decide (x ≤ y))"), lambda x, y: x <= y),
    ("descending", ("(>=)", "(fun x y => decide (x ≥ y))"), lambda x, y: x >= y),
    ("all_tied", (r"(\_ _ -> Prelude.True)", "(fun _ _ => true)"), lambda x, y: True),
]


def _extrema_expected(operation, xs, default, le, key):
    maximum = _fold_expected(xs, default, lambda x, y: y if le(key(x), key(y)) else x)
    minimum = _fold_expected(xs, default,
                             (lambda x, y: x if le(key(x), key(y)) else y)
                             if operation == "minmaxElement" else
                             (lambda x, y: y if le(key(y), key(x)) else x))
    if operation in _PAIRS:
        return (minimum, maximum)
    return minimum if operation in _MINIMUM else maximum


for _di, _default in enumerate([(-99, -1), (99, -2)]):
    for _xi, _xs in enumerate([
        [], [(7, 20)], [(2, 10), (1, 11), (1, 12), (2, 13)],
        [(1, 10), (1, 11), (1, 12)], [(3, 10), (-2, 11), (0, 12)],
    ]):
        for _relation_name, _relation, _le in _RELATIONS:
            for _op in _EXTREMA:
                _projected = _op in ("maximumOn", "minimumOn")
                _rel = (
                    r"(\x y -> partialBool (" + _relation[0] + r" x y))",
                    "(fun x y => BehaviorPartial.bool (" + _relation[1] + " x y))",
                ) if _projected else (
                    r"(\x y -> partialBool (" + _relation[0] + r" (Prelude.fst x) (Prelude.fst y)))",
                    "(fun x y => BehaviorPartial.bool (" + _relation[1] + " x.1 y.1))",
                )
                _args = [_native(_default, TAG), _rel]
                if _projected:
                    _args.append(("Prelude.fst", "(fun x => x.1)"))
                _args.append(_church_list(_xs, TAG))
                _pair = _op in _PAIRS
                _observe(_op, f"tag_{_di}_{_xi}_{_relation_name}",
                         [INT, TAG] if _projected else [TAG], _args,
                         _extrema_expected(_op, _xs, _default, _le, lambda x: x[0]),
                         ("(" + TAG[0] + "," + TAG[0] + ")", "(" + TAG[1] + " × " + TAG[1] + ")") if _pair else TAG,
                         native={"default": _default, "list": _xs, "relation": _relation_name, "projection": "first"},
                         pair=_pair)

# A second On specialization has Bool keys and Int elements, preventing an
# oracle/adapter from treating the projected key as the returned payload.
for _op in ("maximumOn", "minimumOn"):
    for _di, _default in enumerate([-99, 99]):
        for _xi, _xs in enumerate([[], [-4], [-2, 3, -1, 7], [2, 3, 4]]):
            _observe(_op, f"bool_key_{_di}_{_xi}", [BOOL, INT], [
                _native(_default, INT),
                (r"(\x y -> partialBool (Prelude.not x || y))", "(fun x y => BehaviorPartial.bool ((!x) || y))"),
                (r"(\x -> x >= 0)", "(fun x => decide (x ≥ 0))"), _church_list(_xs, INT),
            ], _extrema_expected(_op, _xs, _default, lambda x, y: not x or y, lambda x: x >= 0), INT,
                native={"default": _default, "list": _xs, "relation": "Boolean implication", "projection": "nonnegative"})

_DICTIONARIES = [
    ("int_int_int", INT, INT, INT, [-99, 99], [2, 9], [
        [], [(2, 20)], [(1, 10), (2, 20), (2, 21), (3, 30)],
    ], [
        ("equal", (r"(\k q -> partialBool (k == q))", "(fun k q => BehaviorPartial.bool (k == q))"), lambda k, q: k == q),
        ("less_equal", (r"(\k q -> partialBool (k <= q))", "(fun k q => BehaviorPartial.bool (decide (k ≤ q)))"), lambda k, q: k <= q),
    ]),
    ("int_bool_string", INT, BOOL, STRING, ["missing-a", "missing-b"], [False, True], [
        [], [(1, "one")], [(0, "zero"), (1, "first"), (1, "second"), (2, "two")],
    ], [
        ("query_flag", (r"(\k q -> partialBool (k == (if q then 1 else 0)))",
                        "(fun k q => BehaviorPartial.bool (k == (if q then 1 else 0)))"), lambda k, q: k == (1 if q else 0)),
    ]),
    ("string_int_bool", STRING, INT, BOOL, [False, True], [-1, 1, 3], [
        [], [("a", True)], [("a", False), ("b", True), ("long", False)],
    ], [
        ("key_length", (r"(\k q -> partialBool (Prelude.length k <= q))",
                        "(fun k q => BehaviorPartial.bool (decide (Int.ofNat k.length ≤ q)))"), lambda k, q: len(k) <= q),
    ]),
]
for _group, _key_ty, _query_ty, _value_ty, _defaults, _queries, _tables, _relations in _DICTIONARIES:
    for _di, _default in enumerate(_defaults):
        for _qi, _query in enumerate(_queries):
            for _ti, _table in enumerate(_tables):
                for _rel_name, _rel, _relation in _relations:
                    _expected = next((v for k, v in _table if _relation(k, _query)), _default)
                    _table_expr = _native(_table, (
                        "[(" + _key_ty[0] + "," + _value_ty[0] + ")]",
                        "(List (" + _key_ty[1] + " × " + _value_ty[1] + "))"))
                    _observe("atKey", f"{_group}_{_di}_{_qi}_{_ti}_{_rel_name}",
                             [_key_ty, _query_ty, _value_ty], [
                                 _native(_default, _value_ty), _rel, _native(_query, _query_ty),
                                 ("(partialDict " + _table_expr[0] + ")", "(BehaviorPartial.dict " + _table_expr[1] + ")"),
                             ], _expected, _value_ty,
                             native={"default": _default, "query": _query, "entries": _table, "relation": _rel_name})


def lean_conjunction(expressions, connective="&&"):
    """Keep every observation in order, with logarithmic conjunction depth.

    A long right-associated conjunction can exceed Lean's default recursion
    limit while elaborating `Decidable` or reducing an oracle proof. Changing
    only the binary association preserves all leaves and their evaluation order;
    it does not introduce an increased limit or a vacuous empty observation.
    """
    expressions = tuple(expressions)
    if not expressions or connective not in ("&&", "∧"):
        raise ValueError("expected nonempty Lean Bool or Prop conjunction")

    def build(start, stop):
        if stop - start == 1:
            return expressions[start]
        middle = (start + stop) // 2
        return "(" + build(start, middle) + " " + connective + " " + build(middle, stop) + ")"

    return build(0, len(expressions))


def _check_expression(operation, candidate, language):
    checks = []
    for row in FIXTURES[operation]:
        if language == "haskell":
            call = "(" + candidate + ")" + "".join(" @(" + ty + ")" for ty in row["haskell_type_arguments"])
        else:
            call = "(" + candidate + ")" + "".join(" (" + ty + ")" for ty in row["lean_type_arguments"])
        call += "".join(" (" + arg + ")" for arg in row[language + "_arguments"])
        observed = "(" + call + ")"
        if row["decode"] == "church_pair":
            observed = ("partialDecPair " if language == "haskell" else "BehaviorPartial.decPair ") + observed
        checks.append("(" + observed + " == " + row[language + "_expected"] + ")")
    return lean_conjunction(checks) if language == "lean" else " && ".join(checks)


def haskell_predicate(operation, candidate):
    """Closed native Bool except for candidate; requires TypeApplications."""
    return "let { " + "; ".join(_H_OBSERVERS) + " } in " + _check_expression(operation, candidate, "haskell")


def lean_prelude(operations=None):
    """Observation-only declarations; keep them out of provider discovery."""
    operations = tuple(OPERATIONS if operations is None else operations)
    return ["set_option linter.unusedVariables false"] + list(_L_OBSERVERS) + [
        f"def BehaviorPartial.check_{op} (f : {LEAN_TYPES[op]}) : Bool := " + _check_expression(op, "f", "lean")
        for op in operations
    ]


def lean_predicate(operation, candidate):
    if operation not in OPERATIONS:
        raise KeyError(operation)
    return f"BehaviorPartial.check_{operation} ({candidate}) = true"


# Independent total reference definitions over native lists. They are never
# provider candidates. Expected results above are concrete Python data, not
# evaluations of these Haskell/Lean definitions.
HASKELL_ORACLE_PRELUDE = [
    "partialOracleHead :: forall a. a -> [a] -> a",
    "partialOracleHead d xs = case xs of { [] -> d; x:_ -> x }",
    "partialOracleLast :: forall a. a -> [a] -> a",
    "partialOracleLast d xs = Prelude.foldl (\\_ x -> x) d xs",
    "partialOracleAt :: forall a. a -> Prelude.Int -> [a] -> a",
    "partialOracleAt d n xs = case xs of { [] -> d; x:rest -> if n < 0 then d else if n == 0 then x else partialOracleAt d (n-1) rest }",
    "partialOracleFoldL1 :: forall a. a -> (a -> a -> a) -> [a] -> a",
    "partialOracleFoldL1 d step xs = case xs of { [] -> d; x:rest -> Prelude.foldl step x rest }",
    "partialOracleFoldR1 :: forall a. a -> (a -> a -> a) -> [a] -> a",
    "partialOracleFoldR1 d step xs = Prelude.maybe d Prelude.id (Prelude.foldr (\\x rest -> Prelude.Just (Prelude.maybe x (step x) rest)) Prelude.Nothing xs)",
] + list(_H_OBSERVERS)
LEAN_ORACLE_PRELUDE = [
    "namespace BehaviorPartialOracle",
    "def head {A : Type} (d : A) (xs : List A) : A := match xs with | [] => d | x :: _ => x",
    "def last {A : Type} (d : A) (xs : List A) : A := xs.foldl (fun _ x => x) d",
    "def indexOr {A : Type} (d : A) (n : Int) : List A → A",
    "  | [] => d",
    "  | x :: xs => if n < 0 then d else if n = 0 then x else indexOr d (n - 1) xs",
    "def foldl1 {A : Type} (d : A) (step : A → A → A) (xs : List A) : A := match xs with | [] => d | x :: rest => rest.foldl step x",
    "def foldr1 {A : Type} (d : A) (step : A → A → A) (xs : List A) : A := (xs.foldr (fun x rest => some (rest.elim x (step x))) (none : Option A)).getD d",
    "end BehaviorPartialOracle",
]
HASKELL_WITNESSES = {
    "head": r"\d xs -> partialOracleHead d (partialDec xs)",
    "last": r"\d xs -> partialOracleLast d (partialDec xs)",
    "at": r"\d n xs -> partialOracleAt d n (partialDec xs)",
    "foldl1": r"\d step xs -> partialOracleFoldL1 d step (partialDec xs)",
    "foldr1": r"\d step xs -> partialOracleFoldR1 d step (partialDec xs)",
    "fromJust": r"\d m -> m d Prelude.id",
    "fromLeft": r"\d e -> e Prelude.id (\_ -> d)",
    "fromRight": r"\d e -> e (\_ -> d) Prelude.id",
    "atKey": r"\d rel key xs -> xs (\entry rest -> entry (\k v -> rel k key v rest)) d",
    "reduce": r"\d step xs -> partialOracleFoldL1 d step (partialDec xs)",
}
LEAN_WITNESSES = {
    "head": "fun A d xs => BehaviorPartialOracle.head d (BehaviorPartial.dec xs)",
    "last": "fun A d xs => BehaviorPartialOracle.last d (BehaviorPartial.dec xs)",
    "at": "fun A d n xs => BehaviorPartialOracle.indexOr d n (BehaviorPartial.dec xs)",
    "foldl1": "fun A d step xs => BehaviorPartialOracle.foldl1 d step (BehaviorPartial.dec xs)",
    "foldr1": "fun A d step xs => BehaviorPartialOracle.foldr1 d step (BehaviorPartial.dec xs)",
    "fromJust": "fun A d m => m A d id",
    "fromLeft": "fun A B d e => e A id (fun _ => d)",
    "fromRight": "fun B A d e => e B (fun _ => d) id",
    "atKey": "fun A C B d rel key xs => xs B (fun entry rest => entry B (fun k v => rel k key B v rest)) d",
    "reduce": "fun A d step xs => BehaviorPartialOracle.foldl1 d step (BehaviorPartial.dec xs)",
}


def _extreme_witness(operation, language, *, wrong_tie=False, swapped=False):
    projected = operation in ("maximumOn", "minimumOn")
    minimum_first = (operation == "minmaxElement") != wrong_tie
    if language == "haskell":
        args = r"\d le " + ("project " if projected else "") + "xs -> "
        key_x, key_y = ("(project x)", "(project y)") if projected else ("x", "y")
        maximum = rf"(\x y -> le {key_y} {key_x} x y)" if wrong_tie else rf"(\x y -> le {key_x} {key_y} y x)"
        minimum = rf"(\x y -> le {key_x} {key_y} x y)" if minimum_first else rf"(\x y -> le {key_y} {key_x} y x)"
        lo = "partialOracleFoldL1 d " + minimum + " (partialDec xs)"
        hi = "partialOracleFoldL1 d " + maximum + " (partialDec xs)"
        if operation in _PAIRS:
            return args + r"\pair -> pair (" + (hi if swapped else lo) + ") (" + (lo if swapped else hi) + ")"
        return args + (lo if operation in _MINIMUM else hi)
    args = "fun " + ("K A" if projected else "A") + " d le " + ("project " if projected else "") + "xs => "
    key_x, key_y = ("(project x)", "(project y)") if projected else ("x", "y")
    maximum = f"(fun x y => le {key_y} {key_x} A x y)" if wrong_tie else f"(fun x y => le {key_x} {key_y} A y x)"
    minimum = f"(fun x y => le {key_x} {key_y} A x y)" if minimum_first else f"(fun x y => le {key_y} {key_x} A y x)"
    lo = "BehaviorPartialOracle.foldl1 d " + minimum + " (BehaviorPartial.dec xs)"
    hi = "BehaviorPartialOracle.foldl1 d " + maximum + " (BehaviorPartial.dec xs)"
    if operation in _PAIRS:
        return args + "fun R pair => pair (" + (hi if swapped else lo) + ") (" + (lo if swapped else hi) + ")"
    return args + (lo if operation in _MINIMUM else hi)


for _op in _EXTREMA:
    HASKELL_WITNESSES[_op] = _extreme_witness(_op, "haskell")
    LEAN_WITNESSES[_op] = _extreme_witness(_op, "lean")

HASKELL_WRONG = {}
LEAN_WRONG = {}
for _op in OPERATIONS:
    _arity = 4 if _op == "atKey" or _op in ("maximumOn", "minimumOn") else (
        3 if _op in _EXTREMA or _op in ("at", "foldl1", "foldr1", "reduce") else 2)
    _ignored = " ".join("_" for _ in range(_arity - 1))
    _type_ignored = " ".join("_" for _ in _leading_binders(DEFAULTED_TYPES[_op]))
    HASKELL_WRONG[_op] = {"always_default": r"\d " + _ignored + " -> " + (r"\pair -> pair d d" if _op in _PAIRS else "d")}
    LEAN_WRONG[_op] = {"always_default": "fun " + _type_ignored + " d " + _ignored + " => " + ("fun _ pair => pair d d" if _op in _PAIRS else "d")}
for _op in ("head", "last"):
    _other = "last" if _op == "head" else "head"
    HASKELL_WRONG[_op]["wrong_end"] = HASKELL_WITNESSES[_other]
    LEAN_WRONG[_op]["wrong_end"] = LEAN_WITNESSES[_other]
HASKELL_WRONG["at"]["ignores_index"] = r"\d _ xs -> partialOracleHead d (partialDec xs)"
LEAN_WRONG["at"]["ignores_index"] = "fun A d _ xs => BehaviorPartialOracle.head d (BehaviorPartial.dec xs)"
for _op in ("foldl1", "foldr1", "reduce"):
    _other = "foldl1" if _op == "foldr1" else "foldr1"
    HASKELL_WRONG[_op]["wrong_association"] = HASKELL_WITNESSES[_other]
    LEAN_WRONG[_op]["wrong_association"] = LEAN_WITNESSES[_other]
    HASKELL_WRONG[_op]["default_is_seed"] = r"\d step xs -> Prelude.foldl step d (partialDec xs)"
    LEAN_WRONG[_op]["default_is_seed"] = "fun A d step xs => (BehaviorPartial.dec xs).foldl step d"
for _op in _EXTREMA:
    HASKELL_WRONG[_op]["wrong_tie"] = _extreme_witness(_op, "haskell", wrong_tie=True)
    LEAN_WRONG[_op]["wrong_tie"] = _extreme_witness(_op, "lean", wrong_tie=True)
    if _op in _PAIRS:
        HASKELL_WRONG[_op]["swapped_components"] = _extreme_witness(_op, "haskell", swapped=True)
        LEAN_WRONG[_op]["swapped_components"] = _extreme_witness(_op, "lean", swapped=True)
HASKELL_WRONG["atKey"]["last_matching_value"] = (
    r"\d rel key xs -> Prelude.maybe d Prelude.id "
    r"(xs (\entry rest -> case rest of { Prelude.Just v -> Prelude.Just v; "
    r"Prelude.Nothing -> entry (\k v -> rel k key (Prelude.Just v) Prelude.Nothing) }) Prelude.Nothing)"
)
LEAN_WRONG["atKey"]["last_matching_value"] = (
    "fun A C B d rel key xs => "
    "(xs (Option B) (fun entry rest => match rest with "
    "| some v => some v | none => entry (Option B) "
    "(fun k v => rel k key (Option B) (some v) none)) none).getD d"
)
WITNESS_NAMES = {op: {"haskell": "partialWitness_" + op, "lean": "BehaviorPartialControl.witness_" + op}
                 for op in OPERATIONS}
# A second at witness establishes that the exact proposed primitive inventory
# suffices mathematically: the existing Church fold builds an Int continuation.
# The primary witness independently uses native structural list recursion.
# Neither witness is a provider and neither establishes search reachability.
HASKELL_PROVIDER_WITNESSES = {
    "at": r"\d n xs -> xs (\x rest i -> partialIntCase i d x (\previous -> rest previous)) (\_ -> d) n",
}
LEAN_PROVIDER_WITNESSES = {
    "at": "fun A d n xs => xs (Int → A) (fun x rest i => BehaviorPartialNumeric.intCase A i d x (fun previous => rest previous)) (fun _ => d) n",
}
PROVIDER_WITNESS_NAMES = {
    "at": {"haskell": "partialPrimitiveWitness_at", "lean": "BehaviorPartialControl.primitiveWitness_at"},
}
WRONG_CONTROL_NAMES = {op: {label: {
    "haskell": "partialWrong_" + op + "_" + label,
    "lean": "BehaviorPartialControl.wrong_" + op + "_" + label,
} for label in HASKELL_WRONG[op]} for op in OPERATIONS}
OBSERVATIONS = {op: len(rows) for op, rows in FIXTURES.items()}


def _wrong_model(operation, label, row):
    """Independent native semantics used only to pin a separating fixture."""
    data = row["native_inputs"]
    default = data["default"]
    if label == "always_default":
        return (default, default) if operation in _PAIRS else default
    if label == "wrong_end":
        xs = data["list"]
        return default if not xs else xs[-1 if operation == "head" else 0]
    if label == "ignores_index":
        return data["list"][0] if data["list"] else default
    if label in ("wrong_association", "default_is_seed"):
        combine = {name: fn for _, _, _, _, steps in _FOLDS for name, _, fn in steps}[data["combine"]]
        if label == "wrong_association":
            return _fold_expected(data["list"], default, combine, operation != "foldr1")
        value = default
        for x in data["list"]:
            value = combine(value, x)
        return value
    if label in ("wrong_tie", "swapped_components"):
        le = ({name: fn for name, _, fn in _RELATIONS}[data["relation"]]
              if data["relation"] != "Boolean implication" else lambda x, y: not x or y)
        key = (lambda x: x[0]) if data["projection"] == "first" else (lambda x: x >= 0)
        if label == "swapped_components":
            lo, hi = _extrema_expected(operation, data["list"], default, le, key)
            return (hi, lo)
        hi = _fold_expected(data["list"], default, lambda x, y: x if le(key(y), key(x)) else y)
        lo = _fold_expected(data["list"], default,
                            (lambda x, y: y if le(key(y), key(x)) else x)
                            if operation == "minmaxElement" else
                            (lambda x, y: x if le(key(x), key(y)) else y))
        return (lo, hi) if operation in _PAIRS else lo if operation in _MINIMUM else hi
    if label == "last_matching_value":
        relation = {name: fn for _, _, _, _, _, _, _, rels in _DICTIONARIES for name, _, fn in rels}[data["relation"]]
        matches = [v for k, v in data["entries"] if relation(k, data["query"])]
        return matches[-1] if matches else default
    raise ValueError("no native control model: " + operation + "/" + label)


CONTROL_COUNTEREXAMPLES = {}
for _op in OPERATIONS:
    CONTROL_COUNTEREXAMPLES[_op] = {}
    for _label in HASKELL_WRONG[_op]:
        _separating = next((row for row in FIXTURES[_op]
                            if _wrong_model(_op, _label, row) != row["expected_native"]), None)
        if _separating is None:
            raise ValueError("wrong-control model is indistinguishable: " + _op + "/" + _label)
        CONTROL_COUNTEREXAMPLES[_op][_label] = {
            "fixture_id": _separating["id"],
            "expected_native": _separating["expected_native"],
            "wrong_native": _wrong_model(_op, _label, _separating),
            "status": "native_specification_counterexample; compiler replay still required",
        }


def provider_control_source(language):
    """Independent checks of the sole generic primitive; no target is loaded."""
    declarations = search_provider_source("at", language)
    if language == "haskell":
        assertions = [
            ("int_negative_zero_positive", "Prelude.and [" + ",".join(
                "(partialIntCase @Prelude.Int (" + str(n) + ") (-71) 23 (\\p -> p+101) == "
                + str(-71 if n < 0 else 23 if n == 0 else n + 100) + ")"
                for n in (-3, -1, 0, 1, 4)) + "]"),
            ("int_polymorphic_boolean", "Prelude.and [partialIntCase @Prelude.Bool (-1) Prelude.False Prelude.True (\\p -> p == 0) == Prelude.False, partialIntCase @Prelude.Bool 0 Prelude.False Prelude.True (\\p -> p == 0), partialIntCase @Prelude.Bool 1 Prelude.False Prelude.True (\\p -> p == 0), Prelude.not (partialIntCase @Prelude.Bool 2 Prelude.False Prelude.True (\\p -> p == 0))]"),
        ]
        return declarations, assertions
    if language != "lean":
        raise ValueError("language must be haskell or lean")
    declarations += [
        "theorem BehaviorPartialNumeric.integer_branches : " + " ∧ ".join(
            "(BehaviorPartialNumeric.intCase Int (" + str(n) + ") (-71) 23 (fun p => p+101) = "
            + str(-71 if n < 0 else 23 if n == 0 else n + 100) + ")"
            for n in (-3, -1, 0, 1, 4)) + " := by decide",
        "theorem BehaviorPartialNumeric.boolean_branches : (BehaviorPartialNumeric.intCase Bool (-1) false true (fun p => p == 0) = false) ∧ (BehaviorPartialNumeric.intCase Bool 0 false true (fun p => p == 0) = true) ∧ (BehaviorPartialNumeric.intCase Bool 1 false true (fun p => p == 0) = true) ∧ (BehaviorPartialNumeric.intCase Bool 2 false true (fun p => p == 0) = false) := by decide",
    ]
    names = ["BehaviorPartialNumeric.intCase", "BehaviorPartialNumeric.integer_branches",
             "BehaviorPartialNumeric.boolean_branches"]
    declarations += ["#print axioms " + name for name in names]
    return declarations, names


def haskell_control_source(operations=None):
    """Isolated replay declarations and (name, Bool expression) assertions."""
    operations = tuple(OPERATIONS if operations is None else operations)
    declarations = list(HASKELL_ORACLE_PRELUDE)
    if "at" in operations:
        declarations += search_provider_source("at", "haskell")
    assertions = []
    for op in operations:
        name = WITNESS_NAMES[op]["haskell"]
        declarations += [name + " :: " + HASKELL_TYPES[op], name + " = " + HASKELL_WITNESSES[op]]
        assertions.append((name, haskell_predicate(op, name)))
        if op in HASKELL_PROVIDER_WITNESSES:
            name = PROVIDER_WITNESS_NAMES[op]["haskell"]
            declarations += [name + " :: " + HASKELL_TYPES[op], name + " = " + HASKELL_PROVIDER_WITNESSES[op]]
            assertions.append((name, haskell_predicate(op, name)))
        for label, term in HASKELL_WRONG[op].items():
            name = WRONG_CONTROL_NAMES[op][label]["haskell"]
            declarations += [name + " :: " + HASKELL_TYPES[op], name + " = " + term]
            assertions.append((name, "Prelude.not (" + haskell_predicate(op, name) + ")"))
    return declarations, assertions


def lean_control_source(operations=None):
    """Requires lean_prelude(); returns source and names for axiom inventory."""
    operations = tuple(OPERATIONS if operations is None else operations)
    lines = search_provider_source("at", "lean") if "at" in operations else []
    lines += list(LEAN_ORACLE_PRELUDE) + ["namespace BehaviorPartialControl"]
    names = []
    for op in operations:
        local_name = "witness_" + op
        name = WITNESS_NAMES[op]["lean"]
        lines += [f"def {local_name} : {LEAN_TYPES[op]} := {LEAN_WITNESSES[op]}",
                  f"theorem {local_name}_passes : {lean_predicate(op, name)} := by decide"]
        names += [name, name + "_passes"]
        if op in LEAN_PROVIDER_WITNESSES:
            local_name = "primitiveWitness_" + op
            name = PROVIDER_WITNESS_NAMES[op]["lean"]
            lines += [f"def {local_name} : {LEAN_TYPES[op]} := {LEAN_PROVIDER_WITNESSES[op]}",
                      f"theorem {local_name}_passes : {lean_predicate(op, name)} := by decide"]
            names += [name, name + "_passes"]
        for label, term in LEAN_WRONG[op].items():
            local_name = "wrong_" + op + "_" + label
            name = WRONG_CONTROL_NAMES[op][label]["lean"]
            lines += [f"def {local_name} : {LEAN_TYPES[op]} := {term}",
                      f"theorem {local_name}_rejected : BehaviorPartial.check_{op} {name} = false := by decide"]
            names += [name, name + "_rejected"]
    lines += ["end BehaviorPartialControl"]
    # These commands report dependencies; they do not introduce axioms.
    lines += ["#print axioms " + name for name in names]
    return lines, names


PARTIAL_CASES = {
    case_id: {
        **copy.deepcopy(row),
        "haskell_original_target": ORIGINAL_HASKELL_TYPES[row["operation"]],
        "lean_original_target": ORIGINAL_LEAN_TYPES[row["operation"]],
        "defaulted_expanded_type": DEFAULTED_TYPES[row["operation"]],
        "haskell_target": HASKELL_TYPES[row["operation"]],
        "lean_target": LEAN_TYPES[row["operation"]],
        "original_binder_order": _leading_binders(ORIGINAL_TYPES[row["operation"]]),
        "acceptance_status": STATUS,
        "included_in_acceptance_operations": False,
        "included_in_executable_specification": True,
        "unavailable_reason": None,
        "observations": OBSERVATIONS[row["operation"]],
        "fixtures": FIXTURES[row["operation"]],
        "required_providers": REQUIRED_PROVIDERS[row["operation"]],
        "witness_names": WITNESS_NAMES[row["operation"]],
        "additional_primitive_witness": PROVIDER_WITNESS_NAMES.get(row["operation"]),
        "wrong_controls": WRONG_CONTROL_NAMES[row["operation"]],
        "wrong_control_counterexamples": CONTROL_COUNTEREXAMPLES[row["operation"]],
        "missing_evidence": "No live synthesis, independent GHC execution, or Lean kernel replay has yet been performed for this new specification.",
    }
    for case_id, row in REGISTER.items()
}


def source_provenance():
    return {
        "source": _manifest["source"], "source_sha256": SOURCE_SHA256,
        "manifest_sha256": hashlib.sha256((HERE / "manifest.json").read_bytes()).hexdigest(),
        "spec_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "extended_spec_sha256": hashlib.sha256(Path(extended.__file__).read_bytes()).hexdigest(),
        "operations": list(OPERATIONS), "status": STATUS,
        "unavailable_operations": UNAVAILABLE_OPERATIONS,
        "partial_cases": PARTIAL_CASES,
        "provider_boundary": "Exact allowlist only; observation, native oracle, witness, wrong-control, and target declarations are excluded from discovery and search.",
        "int_scope": "Source Int is preserved. Haskell Int is machine bounded; Lean Int is unbounded. The generic predecessor branch subtracts only from positive inputs. Shared fixtures use representable values.",
    }


def validate_specification():
    """Pure structural checks; does not invoke either language toolchain."""
    expected = set(OPERATIONS)
    for table in (HASKELL_TYPES, LEAN_TYPES, HASKELL_WITNESSES, LEAN_WITNESSES,
                  HASKELL_WRONG, LEAN_WRONG, FIXTURES, REQUIRED_PROVIDERS):
        if set(table) != expected:
            raise ValueError("an executable-specification table omits a registered operation")
    for op in OPERATIONS:
        if not FIXTURES[op] or set(HASKELL_WRONG[op]) != set(LEAN_WRONG[op]):
            raise ValueError("missing observations or mismatched wrong controls: " + op)
        if len({row["id"] for row in FIXTURES[op]}) != len(FIXTURES[op]):
            raise ValueError("duplicate observation identity: " + op)
        for row in FIXTURES[op]:
            if len(row["haskell_type_arguments"]) != len(_leading_binders(DEFAULTED_TYPES[op])):
                raise ValueError("an observation changed source binder arity: " + row["id"])
        for language in ("haskell", "lean"):
            providers = REQUIRED_PROVIDERS[op][language]
            if len(providers) != (1 if op == "at" else 0):
                raise ValueError("unexpected search provider: " + op)
    return {"operations": len(OPERATIONS), "observations": sum(OBSERVATIONS.values()),
            "positive_witnesses": len(HASKELL_WITNESSES) + len(HASKELL_PROVIDER_WITNESSES),
            "wrong_controls": sum(len(v) for v in HASKELL_WRONG.values()),
            "status": STATUS}


validate_specification()


def prepare_sources():
    """Return filenames/text without writing files or invoking subprocesses."""
    hdecls, assertions = haskell_control_source()
    hsource = "\n".join([
        "{-# LANGUAGE RankNTypes, ImpredicativeTypes, ScopedTypeVariables, TypeApplications #-}",
        "module Main where", "import Prelude", *hdecls,
        "main :: Prelude.IO ()",
        "main = Prelude.print [" + ",".join("(" + json.dumps(name) + ",(" + predicate + "))"
                                           for name, predicate in assertions) + "]",
    ]) + "\n"
    ldecls, _ = lean_control_source()
    hnumeric, numeric_assertions = provider_control_source("haskell")
    lnumeric, _ = provider_control_source("lean")
    return {
        "PartialControls.hs": hsource,
        "PartialControls.lean": "\n".join(lean_prelude() + ldecls) + "\n",
        "PartialNumeric.hs": "\n".join([
            "{-# LANGUAGE RankNTypes #-}", "module PartialNumeric where",
            *search_provider_source("at", "haskell")]) + "\n",
        "PartialNumeric.lean": "\n".join(search_provider_source("at", "lean")) + "\n",
        "PartialNumericControls.hs": "\n".join([
            "{-# LANGUAGE RankNTypes, TypeApplications #-}", "module Main where",
            "import Prelude", *hnumeric, "main :: Prelude.IO ()",
            "main = Prelude.print [" + ",".join("(" + json.dumps(name) + ",(" + assertion + "))"
                                               for name, assertion in numeric_assertions) + "]"]) + "\n",
        "PartialNumericControls.lean": "\n".join(lnumeric) + "\n",
        "partial-specification.json": json.dumps(source_provenance(), indent=2, ensure_ascii=False) + "\n",
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prepare-only", type=Path, required=True, metavar="EMPTY_DIRECTORY")
    args = parser.parse_args()
    output = args.prepare_only.resolve()
    if output.exists() and (not output.is_dir() or any(output.iterdir())):
        raise ValueError("choose an empty output directory; preserve earlier specification captures")
    output.mkdir(parents=True, exist_ok=True)
    for name, source in prepare_sources().items():
        (output / name).write_text(source, encoding="utf-8")
    print(json.dumps({**validate_specification(), "prepared": str(output)}, indent=2))


if __name__ == "__main__":
    main()

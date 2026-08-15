# Bounded Length counterexample simplification

Date: 2026-08-14

## Outcome

Djex now owns deterministic bounded simplification of independently validated
scalar and binary-product Length counterexamples.  The operation is pure: it
uses the checked problem retained by a query, emits no SMT-LIB, consumes no
solver status or model, and returns a new receipt only after concrete replay
and exact behavioral-problem association.

The public query entrances are:

- `simplifyLengthSMTLibQueryCounterexample`;
- `simplifyLengthSpinePairSMTLibQueryCounterexample`.

The evaluator siblings are `simplifyLengthProblemCounterexample` and
`simplifyLengthSpinePairProblemCounterexample`.

## Search contract

For source-ordered anchor inputs `a = [a0, ..., an]`, the search domain is the
componentwise dominated box:

```text
[0..a0] × ... × [0..an]
```

Assignments are visited lexicographically with the last input varying fastest.
The complete box must first fit the supplied `LengthInputBoxLimits`.  Fixed
precedence is:

1. checked problem width against the input-width limit;
2. exact anchor-input arity;
3. anchor values from left to right under `LengthEvaluationLimits`;
4. the saturating Cartesian-product assignment cap;
5. independent anchor replay against the exact problem;
6. lexicographic traversal through the existing input-box verifier;
7. exact query/problem association before an opaque query-owned receipt is
   released.

Width or Cartesian-product refusal returns `Right Nothing`.  This is ordinary
bounded unavailability.  `Right Nothing` also covers an admitted search whose
first violation is the anchor itself.  It deliberately carries no reason and
makes no minimality claim.

Arity or value rejection, a replay error or non-counterexample anchor, an
admitted traversal failure, an impossible positive completion, or an exact
association mismatch is a closed `Left`.  The scalar and product error
vocabularies remain nominally separate.

## Receipt authority

`Just` exists only when the final inputs differ strictly from the anchor.  The
opaque receipts are:

- `ValidatedLengthCounterexampleSimplification`;
- `ValidatedLengthSpinePairCounterexampleSimplification`.

Each receipt retains:

- its new domain-specific v1 verifier tag;
- the original source-ordered input vector;
- the exact number of search assignments inspected through and including the
  returned hit (the separate anchor replay is excluded);
- the fresh ordinary scalar or product counterexample receipt.

Convenience projections expose final inputs, recomputed result, provider/model
basis, and a strict-change predicate.  The predicate is always true for an
existing receipt because unchanged searches return `Nothing`.

The receipt proves only that Djex inspected the stated bounded prefix and
replayed the returned violation under the exact checked problem.  It does not
prove global minimality, source-language realization, totality, provider-law
implementations, universal correctness, or anything derived from Z3 status.

## Identity and compatibility

The checkpoint adds only the two simplification receipt schema tags.  It does
not change any existing:

- contract, provider inventory, session, candidate, concrete encoding, or
  complete problem fingerprint;
- scalar or product SMT query schema, bytes, symbols, or fingerprint;
- protocol plan, response, process, worker, query-run, live observation, or
  replay identity;
- public raw counterexample receipt.

Scalar and product query check/request bytes may remain structurally equal, but
their nominal domains, problems, simplification errors, evidence, and receipts
cannot cross-associate.

## Characterization

The focused suite pins:

- scalar and product strict reductions and exact mixed-radix inspected counts;
- unchanged and nullary anchors;
- width and product unavailability;
- productive arity/value and stale-anchor rejection;
- earlier admitted evaluation failures;
- provider-law basis retention;
- query-wrapper association and unchanged query bytes/fingerprints;
- API opacity, `NFData`, hidden constructors, and scalar/product nominal roles.

Leant consumes this API through an explicit programmatic policy.  Its separate
report records orchestration, MRU, presentation, and fallback semantics.

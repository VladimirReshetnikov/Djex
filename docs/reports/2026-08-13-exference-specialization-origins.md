# Exference specialization origins

Date: 2026-08-13

## Outcome

Exference's independent expression checker now retains a package-private
origin observation for a narrow class of visible type applications: complete,
bounded, explicitly selected leading-forall prefixes on direct globals whose
exact source scheme is present in the validated checker context.

Each retained origin records a candidate-local coordinate, its global owner,
the exact source scheme, and one step per source binder. Steps preserve source
order and contain the normalized source, selected, and result types plus
exactly the ordered source constraints which became unconditional at that
step. The checker publishes these records only after its existing rigid-scope,
constraint resolution, escaping-rigid, and residual-constraint gates all
succeed. Search routes are neither retained nor treated as provenance.

At this checkpoint no certificate was stamped into a term graph: every
`TypeApplicationWitness` still had `Nothing` in its certificate field and
Length had no new authority. The later Exference wiring and Length consumer
supersede that staging statement without widening this report's origin
eligibility. See the
[Exference wiring report](2026-08-13-exference-certificate-association-wiring.md)
and
[Length consumption report](2026-08-13-length-associated-provider-certificates.md).

## Eligibility boundary

An origin is retained only when all of the following are true:

- the visible spine has a syntactically direct global base;
- that global has an exact sidecar scheme in the checker context;
- the exact scheme is lexically closed;
- its complete leading telescope is nonempty and no wider than the shared
  six-argument provider-instantiation limit;
- the expression contains the complete source prefix; and
- every argument in that prefix is explicitly specified and closed.

Oversized, open, partial, inferred, local, arbitrary-function, compatibility
fallback, and returned-polytype suffix applications simply remain
origin-free. They are not new checker errors. A maximal direct-global spine is
processed once in source order, but only the original source telescope prefix
is annotated. For example, if `provider @Poly @Int` consumes the source
scheme's only binder with `@Poly`, the later `@Int` applies to the selected
polytype and does not inherit the source origin.

The telescope-width observation is productive: it uses the shared capped list
observer before the eligibility predicate checks source closure. This bounds
the origin-specific binder observation; ordinary checker validation and
zonking have already inspected the exact retained scheme independently. The
existing transactional checker alternative snapshots the complete `CheckState`;
tentative origin coordinates and records are rolled back with all other
allocations when preferred inference fails.

## Source-context semantics

Visible selection now decides whether another source binder follows by
inspecting the unsubstituted source body. This matches the structural
certificate replay and prevents a replacement-introduced forall from being
mistaken for continuation of the source telescope.

For `forall a. C a => a` selected with `forall b. b`, the source's `C` context
therefore activates immediately at source slot zero as `C (forall b. b)`. The
result remains the selected polytype, and any later visible application is an
unannotated suffix. Contexts owned inside the selected type remain inside that
type and never become source obligations.

All retained types and obligation arguments pass through the same fixed-point
substitution, rigid-alpha, and canonicalization function as the checked term.
No provisional checker metavariable crosses the evidence boundary.

## Authority deliberately withheld

Origin coordinates are candidate-local lookup coordinates only. A retained
origin does not prove any of the following:

- ownership by a prepared source declaration or inventory;
- kind correctness of a selected argument;
- identity of an instance or discharge derivation;
- association with a graph node or occurrence;
- permission to stamp a certificate; or
- permission to create a behavioral fingerprint.

The exact-scheme context proves structural agreement with the checker's flat
binding and the successful checker proves participation in ordinary
constraint resolution. At this checkpoint a later Length integration still
had to match the owner and exact scheme against its own prepared provider
inventory, build an atomic graph-occurrence association, and validate a
structural certificate plan. The linked follow-ups now perform that work for
the bounded provider-only case.

Both origin types and all observation functions remain absent from the public
`Language.Haskell.Djex` facade. Negative Template Haskell probes pin that API
boundary.

## Validation

The focused engine suite covers:

- exact two-slot direct-global retention and final normalization;
- context-free graphs retaining `Nothing` certificates;
- contextual activation with the unchanged contextual-graph absence;
- selected-polytype source activation, with and without a later suffix;
- partial, inferred, local, and compatibility fallback spines;
- open source schemes;
- the exact six-binder eligibility and seven-binder ineligibility boundary;
- unsuccessful constraint resolution producing the historical
  `ConstraintMismatch` before evidence exists;
- transactional rollback publishing one zero-based origin without a ghost
  coordinate after a failed preferred inference branch; and
- graph projection and candidate-key behavior from the prior checkpoint.

At this checkpoint, all 42 private engine tests, all 493 broader Exference
tests, and all 34 downstream API tests pass. The complete `cabal test all`
repository validation also passes.

## Next boundary

The next checkpoint constructed one opaque Exference-owned association
which contains the checked graph, structural certificate table, global owner,
base occurrence, and every visible node/occurrence/slot relation. It must seal
the entire source-prefix chain atomically and reject gaps, duplicates, suffix
inheritance, and detachable graph/table combinations. Only that associated
value can safely become an input to a private certificate-aware fingerprint.
Both that fingerprint and its first provider-only Length consumer are now
implemented in the follow-up reports linked above.

# Djinn typed-result seam

Date: 2026-08-11

## Outcome

Djinn now publishes the same opaque typed-candidate result shape as Exference
without manufacturing a shared typed graph from insufficient evidence.
`Language.Haskell.Djex.Djinn` exports `DjinnTypedCandidate`,
`DjinnTypedResult`, and typed counterparts of all four checked query runners.
The legacy runners are one-way `typedCandidateCompatibility` projections of
those typed paths.

Every current Djinn candidate reports
`DjinnTermGraphSourceTypingContextUnavailable`. This is a precise capability
boundary, not a search failure and not weakened logical evidence.

## Why graph construction remains unavailable

Djinn independently checks every raw LJT proof and retains a checker-owned
typed proof tree. That tree is exact in the LJT `Formula` vocabulary, but it is
not yet an exact shared-source typing derivation:

- structural datatype expansion has discarded the nominal source application;
- ordinary logical atoms no longer distinguish every source type role;
- legitimate discarded intermediate terms may retain correlated unresolved
  checker metavariables;
- the checked tree precedes external assumption-name restoration;
- provider-local proof symbols are rewritten after checking;
- implicit instantiation evidence is erased and selected evidence may become
  visible type applications;
- generated lowering refines patterns, simplifies cases and bindings, performs
  policy-sensitive eta contraction, and canonicalizes local spellings.

A graph built directly from that raw proof would not have trustworthy shared
types or an exact compatibility erasure. Inferring annotations from the final
generated clause would merely reconstruct authority after it had been erased.
Both approaches therefore fail closed.

## Retained association

`CheckedCandidateProof` still couples one raw proof with the evidence returned
by checking that exact occurrence. `ValidatedCandidate` carries the same
evidence beside the converted clause and ranking details through formula-plan
collection, eta-aware de-duplication, and stable sorting.

The private `projectValidatedResultWith` consumes each whole final association
and gives its projector a deterministic `Natural` candidate key. Keys are
allocated lazily with `zipWith [0..]` only after de-duplication and any
configured stable ranking. Future graph node and occurrence identities can
therefore derive from a final candidate key instead of colliding across sibling
plans or depending on discarded proof ordinals. Compatibility projection
ignores the key and never traverses proof evidence.

## Future source-type domain

The typed alias deliberately does not use the historical
`DjinnType = Type DjinnTypeVariable`. A behavioral candidate graph must retain
whether a source identity is flexible or a rigid skolem, so its reserved type
is:

```haskell
type DjinnTermGraphTypeVariable = Variable HSymbol
type DjinnTermGraphType = Type DjinnTermGraphTypeVariable
```

That is the domain expected by shared graph fingerprinting and by behavioral
root-opening checks. The alias does not claim that Djinn already has the
source-variable role plan needed to construct a value in that domain.

The first sound graph-producing follow-up should check the final
`FunctionClause` bidirectionally from retained source authority, start with a
deliberately small local/lambda/tuple/application fragment, seal under bounded
`sharedTypeStructure`, and require exact
`eraseTermGraphToFunctionClause` equality. Globals, visible applications,
constructor patterns, lets, cases, holes, and implicit polymorphic
specialization should remain explicit absences until their own authority is
retained.

## Nominal graph authority

`TypedCandidate` was already nominal in all four parameters, but that does not
protect a graph after `typedCandidateTermGraph` projects it. `TermGraph` is now
explicitly nominal in both its type and local-identity parameters. Downstream
code cannot use `coerce` to relabel a sealed graph into representationally equal
newtype domains before fingerprinting or behavioral sealing. Its derived
`Generic` instance was also removed: hiding the data constructor is not an
abstraction boundary when `GHC.Generics.to` can reconstruct the representation.
An explicit `NFData` implementation preserves deep evaluation without
publishing that construction path.

## Compatibility and validation

The refactor preserves the established failure order: all raw proofs are
checked before any proof is converted, complete associations are de-duplicated
and ranked, and only then is a result view selected. Typed graph absence is a
lazy per-candidate payload and does not force proof evidence or a candidate
tail.

Focused coverage establishes:

- exact typed-to-legacy result equality for the ordinary runner and the
  explicit empty candidate, assignment, and kinded-assignment entrances;
- unchanged logical evidence, operational completion, metadata, ordering, and
  generated compatibility clauses;
- explicit graph absence without forcing the checked sidecar or candidate
  tail;
- direct Core typed-to-legacy parity; and
- compile-fail `Coercible` probes for both `TermGraph` parameters plus a
  missing-`Generic` probe for constructor opacity.

This checkpoint unifies the engine result boundary. It does not yet make a
Djinn candidate eligible for Length interpretation, Z3 ranking, or any other
behavioral contract.

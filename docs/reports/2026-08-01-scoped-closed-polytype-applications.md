# Scoped closed-polytype applications — 2026-08-01

## Problem

A vacuous provider can need a visible type argument even when it is local:

```haskell
use
  :: (forall x. x -> x)
  -> (forall hidden. Token)
  -> Token
use _ provider = provider @(forall x. x -> x)
```

The provider's binder does not occur in `Token`, so result unification cannot
recover the selected type. Djex already retained this evidence for bounded
loaded Djinn schemes, and Exference already selected checked query candidates
for retained globals. Query-local Djinn axioms erased the choice, while
Exference's scoped-provider branch considered only explicit instance heads.

That asymmetry mattered to Haskell callbacks and to foreign frontends such as
Leant, where a polymorphic capability is often a local function parameter.

## Boundary

This change does not add general impredicative inference. A closed polytype is
eligible only when it already occurs in a proven proper-type position below an
arrow or tuple in the checked query. The type is treated as one opaque,
alpha-aware atom; neither backend invents or decomposes it.

For the query-selected Exference route:

- the provider is closed and has no direct context;
- every selected leading binder is absent from the residual body;
- at most four leading binders are selected;
- each candidate is a ground monotype or a complete lexically closed,
  context-free `forall`; and
- at most 32 candidate tuples are retained after alpha-aware deduplication.

The older explicit-instance route remains separate and monotype-only. Its
class environment does not carry the prepared kind witness needed to broaden
instance heads safely.

Djinn continues to use its existing bounded query vocabulary, per-scheme and
global axiom limits, and query-wide proof fuel. The complete instantiated body
and every specified argument are checked against the prepared environment's
kind assumptions.

## Djinn

Historical query-local instantiation axioms now receive the same checked
visible-argument constructor as loaded schemes. The logical axiom remains
unchanged. Only proof conversion gains metadata when a chosen binder is
vacuous.

The active structural and nominal axiom families carry those applications to
the shared generated term. Their private symbol namespaces are disjoint from
the loaded families, so merging the evidence maps cannot change lookup
precedence. Plain structural plans, formula ordering, candidate tuple order,
and all existing caps remain unchanged.

Inferable positions still use `@_` only when an earlier positional argument
must be exposed to reach a later vacuous binder. A checked closed choice,
including a quantified one, remains specified.

## Exference

The scoped-value visible branch now combines two finite sources in order:

1. monomorphic choices justified by explicit instance heads; and
2. the same checked query candidate pool already used by retained globals.

Ordinary fresh instantiation remains the first branch, so existing concise
terms keep their priority. Visible candidates are additive. The independent
expression checker consumes the explicit argument again and verifies the
result against the original provider annotation and query goal.

## Generated syntax

`VisibleTypeArgument` stores a specified type structurally. Quantified binders
receive stable lexical scope/slot identities, so alpha-renamed source types
compare and render alike. `visibleTypeArgumentClosedType` exposes the complete
closed type; the older `visibleTypeArgumentType` remains a monotype-only
compatibility projection.

Compiling `@(forall ...)` generally requires `TypeApplications`, `RankNTypes`,
`ImpredicativeTypes`, and—when the provider binder is ambiguous—
`AllowAmbiguousTypes`.

## Validation

Coverage includes:

- a Djinn query-local vacuous provider retaining the exact closed quantified
  argument;
- an Exference scoped provider carrying that argument through independent
  expression checking;
- a shared facade fixture that obtains the local application from both engines
  and compiles both generated definitions with GHC 9.12; and
- the complete Djinn, Exference, private-engine, shared-synthesis, facade, and
  downstream API suites.

## Remaining instance-evidence gap

Leant currently erases Lean instance binders when projecting providers and
does not serialize Lean's instance registry into Djex. A quantified type that
appears only in a Lean instance head is therefore not yet a candidate. The
sound follow-up is a bounded, checked caller-supplied candidate-evidence
channel, not an approximation that treats Lean dictionaries as Haskell class
declarations. The present change covers local and global providers when the
closed polytype is already supplied by the query.

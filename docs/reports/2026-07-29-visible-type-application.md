# Bounded visible type application — 2026-07-29

> **2026-08-01 follow-up.** The
> [scoped closed-polytype extension](2026-08-01-scoped-closed-polytype-applications.md)
> replaces the monotype-only shared argument invariant with a structurally
> closed, alpha-normal type and lets both engines retain bounded query-selected
> quantified applications for vacuous providers. The limits below describe
> this historical first slice.

## Outcome

Djex now has one backend-independent generated-expression node for visible
type application. Its argument is deliberately abstract and has only two
checked forms:

- `inferredVisibleTypeArgument`, rendered as `@_`; and
- a specified closed, variable-free, `forall`-free monotype constructed by
  `specifiedVisibleTypeArgument`, such as `@Int` or `@(Maybe Int)`.

The bound is a scope invariant, not only a rendering restriction. A generated
`FunctionClause` does not carry source type-variable binders, so admitting an
open argument such as `@a` would manufacture a name whose scope the shared tree
cannot prove. Excluding quantified arguments likewise keeps this feature from
becoming an unchecked impredicative-instantiation channel.

## Exference construction

Exference search uses the node in one evidence-directed provider branch. For a
scoped constrained provider with a leading rank-N scheme, a direct provider
constraint may be matched against an explicit instance head. The branch is
available only when that one ground head determines the complete nonempty
leading binder prefix and every ordered substitution image is a closed
monotype. Search then instantiates the complete prefix and emits the
corresponding left-associated application:

```haskell
provider @Int
```

Instantiated provider contexts remain ordinary scoped proof obligations; the
visible syntax does not bypass class resolution. A multi-binder prefix is
emitted as one visible application per binder, in binder order.

This is an additive branch. Exference retains fresh implicit per-use
instantiation, exact forwarding, and its existing shallow provider subsumption.
Only scoped providers participate in the visible construction; global binding
search retains its ordinary implicit behavior. The independent expression
checker consumes exactly one flexible leading binder per visible application
and also understands `@_`, but search itself emits only specified ground
arguments selected from explicit instance evidence.

## Shared rendering and compiler boundary

All generated-expression traversals preserve the new node, including scope
validation, substitution, simplification, alpha-equivalence, size accounting,
and local-name allocation. Text rendering and Haskell-src-exts conversion use
the same qualification policy as the surrounding expression. Compound type
arguments are parenthesized, so `Maybe Int` becomes `@(Maybe Int)` rather than
changing the parse.

Compiling output that contains the node requires `TypeApplications`. The
surrounding provider type will commonly also require `RankNTypes`; a contextual
scheme whose quantified variable appears only in its constraint may require
`AllowAmbiguousTypes`. These are requirements of the generated Haskell module,
not new search modes.

## Deliberate limits

This slice does not add general visible type application. The shared invariant
excludes open arguments such as `@a` and `forall`-bearing or otherwise
impredicative type arguments. Callers can construct any checked closed argument
around any generated expression, but Exference search selects only the ground
scoped-provider applications described above and never adds them to globals.
This slice also does not broaden ordinary higher-rank subsumption.

Djinn search gains no corresponding rule. Djinn can carry the shared generated
tree through backend-independent operations, but its historical `HExpr`
projection explicitly rejects visible type application instead of silently
dropping it. Djinn's existing bounded rank-N introduction, hypothesis
instantiation, and opaque-atom behavior remain unchanged.

The REPL source-expression inferencer also continues to reject caller-written
visible type application. The supported surface in this slice is checked
generated output plus the bounded Exference construction above.

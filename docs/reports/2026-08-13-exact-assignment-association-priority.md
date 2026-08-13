# Exact-assignment association priority

Date: 2026-08-13

## Outcome

Exference now tries a productive caller-supplied exact provider assignment in a
narrow leading visible-application lane before the ordinary compatibility use
of the same retained global. The ordinary use remains the immediate fallback,
followed by the established instance-head, query-derived, and scalar-candidate
visible lanes.

This ordering is an authority-retention requirement. Exact assignments retain
the complete checked provider scheme and selected arguments, so the independent
expression checker can attach an opaque certificate association to their
visible applications. The ordinary flattened binding can produce the same
rendered specialization without that association. A downstream renderer which
deduplicates exact spellings must see the associated candidate first; otherwise
it can retain the plain graph and irreversibly discard the checked authority
needed by Length.

## Compatibility and demand

The change is scoped to an exact-name assignment bucket whose checked replay
produces at least one complete specialization. The raw bucket is inspected
before the retained scheme or assignment worker:

- no bucket takes the exact historical `ordinary <|> remainingVisible` path;
- an unusable private raw bucket also falls back to that order; and
- a productive bucket runs `assignedVisible <|> ordinary <|>
  remainingVisible`.

The public assignment adapters already require an exact retained provider,
nonempty exact arity, a closed context-free scheme, closed specified arguments,
positional kinds, and a successful whole-specialization replay. The fallback is
nevertheless necessary because the package-private engine entrance accepts a
raw map used by adversarial tests and trusted adapters.

No assignment is donated to a sibling global or scoped value. Exact vector
order is preserved, no Cartesian product is introduced, and inferred,
instance-head, query-derived, and scalar-candidate visible choices retain their
relative ordering after the ordinary fallback. Empty public assignment input
also retains its previous demand and result order.

## Regression

The private engine test uses a scheme-backed direct global whose ordinary and
explicitly assigned forms have the same result type. It asserts that a
productive exact assignment yields the carrier observations
`[associated, plain]`, while an empty map and a deliberately unusable private
vector both remain plain-first. The Leant integration regression then observes
the slot-zero certificate on the first rendered polymorphic provider candidate,
confirms its public bare graph is not fingerprintable, and passes the retained
whole candidate into Length successfully.

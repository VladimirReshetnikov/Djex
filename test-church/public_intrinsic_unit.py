"""Public intrinsic-unit construction, exact GHC replay and False controls.

Reuse the public source runner's ordinary and named-where entrances. The
methodless fixture supplies no unit value or implementation of these queries;
the private engine regressions separately require a completely empty inventory.
"""
import public_kinded_source as public


public.CASES = [
    ('unit', '()', 'probe == ()'),
    ('local_consumer', 'forall r. (forall a. a -> r) -> r',
     r'probe @Int (\_ -> 37) == 37'),
    ('pair_argument', 'forall r. (((), ()) -> r) -> r',
     r'probe @Int (\((), ()) -> 53) == 53'),
]


if __name__ == '__main__':
    raise SystemExit(public.main())

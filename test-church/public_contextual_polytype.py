"""Public qualified-consumer selection with exact GHC replay and False controls.

The result forces a supplied forall, either directly or in a nominal wrapper.
The fixture defines data and a methodless class, with no reference solution.
Private engine tests additionally use an entirely constructor-free inventory.
"""
import public_kinded_source as public


public.FIXTURE = """module Fixture where
class Marker a
instance Marker Int
data Token = Token Int
data Box a = Box a
data Witness a r = Witness r a
"""
public.HEADER = public.HEADER.replace('NoPolyKinds', 'NoPolyKinds, AllowAmbiguousTypes')
POLY = '(forall b. b -> Fixture.Token)'
PREFIX = 'forall a r. Fixture.Marker a => (forall s. Fixture.Marker a => s -> Fixture.Witness s r) -> '
public.CASES = [
    ('qualified_polytype', PREFIX + POLY + ' -> Fixture.Witness ' + POLY + ' r',
     r'case probe @Int @Int (\x -> Fixture.Witness 7 x) (\_ -> Fixture.Token 37) of Fixture.Witness value payload -> value == 7 && (case payload True of Fixture.Token n -> n == 37)'),
    ('qualified_boxed_polytype', PREFIX + 'Fixture.Box ' + POLY + ' -> Fixture.Witness (Fixture.Box ' + POLY + ') r',
     r'case probe @Int @Int (\x -> Fixture.Witness 11 x) (Fixture.Box (\_ -> Fixture.Token 53)) of Fixture.Witness value (Fixture.Box payload) -> value == 11 && (case payload () of Fixture.Token n -> n == 53)'),
]


if __name__ == '__main__':
    raise SystemExit(public.main())

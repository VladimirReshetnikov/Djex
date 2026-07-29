module Djinn.Internal.Help (verboseHelp) where

import Data.Version (showVersion)
import Paths_djex (version)

verboseHelp :: String
verboseHelp = "\
\\n\
\\n\
\Djinn commands explained\n\
\========================\n\
\\n\
\<sym> ? <type>\n\
\  Try to find a function of the specified type.  Djinn knows about the\n\
\function type, tuples, Either, Maybe, (), and can be given new type\n\
\definitions.  (Djinn also knows about the empty type, Void, but this\n\
\is less useful.)  Further functions, type synonyms, and data types can\n\
\be added by using the commands below.  If a function can be found it\n\
\is printed in a style suitable for inclusion in a Haskell program.  If\n\
\no function can be found this will be reported as well.  Examples:\n\
\   Djinn> f ? a->a\n\
\   f :: a -> a\n\
\   f a = a\n\
\   Djinn> sel ? ((a,b),(c,d)) -> (b,c)\n\
\   sel :: ((a, b), (c, d)) -> (b, c)\n\
\   sel ((_, a), (b, _)) = (a, b)\n\
\   Djinn> cast ? a->b\n\
\   -- cast cannot be realized.\n\
\  Within its rank-1 propositional fragment, Djinn will always find a (total)\n\
\function if one exists.  (The worst case complexity is bad, but unlikely for\n\
\typical examples.)  Rank-N support is deliberately bounded; outside that\n\
\supported fragment Djinn reports inhabitation as undecided instead of claiming\n\
\that no function exists.\n\
\  When multiple implementations of the type exist, Djinn will only\n\
\give one of them.  Example:\n\
\  Djinn> f ? a->a->a\n\
\  f :: a -> a -> a\n\
\  f _ a = a\n\
\\n\
\\n\
\<sym> :: <type>\n\
\  Add a new function available for Djinn to construct the result.\n\
\Example:\n\
\   Djinn> type Int :: *\n\
\   Djinn> type Char :: *\n\
\   Djinn> foo :: Int -> Char\n\
\   Djinn> bar :: Char -> Bool\n\
\   Djinn> f ? Int -> Bool\n\
\   f :: Int -> Bool\n\
\   f a = bar (foo a)\n\
\  This feature is not as powerful as it first might seem.  Djinn does\n\
\*not* instantiate polymorphic functions.  It will only use the function\n\
\with exactly the given type.  Example:\n\
\   Djinn> cast :: a -> b\n\
\   Djinn> f ? c->d\n\
\   -- f cannot be realized.\n\
\\n\
\type <sym> <vars> = <type>\n\
\  Add a Haskell style type synonym.  Type synonyms are expanded before\n\
\Djinn starts looking for a realization.\n\
\  Example:\n\
\   Djinn> type Id a = a->a\n\
\   Djinn> f ? Id a\n\
\   f :: Id a\n\
\   f a = a\n\
\\n\
\type <sym> :: <kind>\n\
\  Add an abstract (uninterpreted) type of the given kind.\n\
\An uninterpreted type behaves like a type variable during deduction.\n\
\\n\
\data <sym> <vars> = <type>\n\
\  Add a Haskell style data type.\n\
\  Example:\n\
\   Djinn> data Foo a = C a a a\n\
\   Djinn> f ? a -> Foo a\n\
\   f :: a -> Foo a\n\
\   f a = C a a a\n\
\\n\
\data <sym> <vars>\n\
\  Add an empty type.\n\
\\n\
\class <sym> <vars> where <methods>\n\
\  Add a type class.  Example:\n\
\   class Ord a where compare :: a -> a -> Ordering\n\
\\n\
\\n\
\:clear\n\
\  Restore the initial environment and runtime settings.\n\
\\n\
\\n\
\:delete <sym>\n\
\  Remove a function, type, or class from the environment.\n\
\\n\
\\n\
\:environment\n\
\  List the current functions, types, and classes.\n\
\\n\
\\n\
\:help\n\
\  Show a short help message.\n\
\\n\
\\n\
\:load <file>\n\
\  Read and execute a file with commands.  The file may include Haskell\n\
\style -- comments.\n\
\\n\
\\n\
\:quit\n\
\  Quit Djinn.\n\
\\n\
\\n\
\:set <option>\n\
\  Set runtime options.\n\
\     +multi    show multiple solutions\n\
\               This will not show all solutions since there might be\n\
\               infinitely many.\n\
\     -multi    show one solution\n\
\     +sorted   sort solutions according to a heuristic criterion\n\
\     -sorted   do not sort solutions\n\
\     +debug    show the internal proof formula and proof term\n\
\     -debug    hide internal proof details\n\
\     cutoff=N  consider at most N solutions (N must be positive)\n\
\     budget=N  explore at most N proof-search steps; 0 (the default)\n\
\               means unlimited.  When a budget runs out before any\n\
\               proof is found, Djinn reports the result as undecided\n\
\               instead of claiming the type cannot be realized.\n\
\  The sorting heuristic first minimizes the fraction of unused bound\n\
\variables, then the total number of bound variables.\n\
\\n\
\:verbose-help\n\
\  Print this message.\n\
\\n\
\\n\
\Further examples\n\
\================\n\
\  $ djinn\n\
\  Welcome to Djinn version " ++ showVersion version ++ ".\n\
\  Type :h to get help.\n\
\\n\
\   -- return, bind, and callCC in the continuation monad\n\
\   Djinn> data CD r a = CD ((a -> r) -> r)\n\
\   Djinn> returnCD ? a -> CD r a\n\
\   returnCD :: a -> CD r a\n\
\   returnCD a = CD (\\ b -> b a)\n\
\\n\
\   Djinn> bindCD ? CD r a -> (a -> CD r b) -> CD r b\n\
\   bindCD :: CD r a -> (a -> CD r b) -> CD r b\n\
\   bindCD a b =\n\
\         case a of\n\
\         CD c -> CD (\\ d ->\n\
\                     c (\\ e ->\n\
\                        case b e of\n\
\                        CD f -> f d))\n\
\\n\
\   Djinn> callCCD ? ((a -> CD r b) -> CD r a) -> CD r a\n\
\   callCCD :: ((a -> CD r b) -> CD r a) -> CD r a\n\
\   callCCD a =\n\
\          CD (\\ b ->\n\
\              case a (\\ c -> CD (\\ _ -> b c)) of\n\
\              CD d -> d b)\n\
\\n\
\\n\
\   -- return and bind in the state monad\n\
\   Djinn> type S s a = (s -> (a, s))\n\
\   Djinn> returnS ? a -> S s a\n\
\   returnS :: a -> S s a\n\
\   returnS a b = (a, b)\n\
\   Djinn> bindS ? S s a -> (a -> S s b) -> S s b\n\
\   bindS :: S s a -> (a -> S s b) -> S s b\n\
\   bindS a b c =\n\
\        case a c of\n\
\        (d, e) -> b d e\n\
\\n\
\\n\
\  The function type may have a type class context, e.g.,\n\
\   Djinn> refl ? (Eq a) => a -> Bool\n\
\   refl :: (Eq a) => a -> Bool\n\
\   refl a = a == a\n\
\A context is simply interpreted as an additional (hidden) argument\n\
\that contains all the methods.  Again, there is no instantiation of\n\
\polymorphic functions, so classes where the methods are polymorphic\n\
\do not work as expected.\n\
\\n\
\It is also possible to query for an instance of a class, which is executed\n\
\as a query for each of the methods, e.g.,\n\
\  Djinn> ?instance Monad Maybe\n\
\  instance Monad Maybe where\n\
\     return = Just\n\
\     (>>=) a b =\n\
\          case a of\n\
\          Nothing -> Nothing\n\
\          Just c -> b c\n\
\\n\
\\n\
\Theory\n\
\======\n\
\  Djinn interprets a Haskell type as a logic formula using the\n\
\Curry-Howard isomorphism and then uses a decision procedure for\n\
\Intuitionistic Propositional Calculus.  This decision procedure is\n\
\based on Gentzen's LJ sequent calculus, but in a modified form, LJT,\n\
\that ensures termination.  This variation on LJ has a long history,\n\
\but the particular formulation used in Djinn is due to Roy Dyckhoff.\n\
\The decision procedure has been extended to generate a proof object\n\
\(i.e., a lambda term).  It is this lambda term (in normal form) that\n\
\constitutes the Haskell code.\n\
\  See http://www.dcs.st-and.ac.uk/~rd/publications/jsl57.pdf for more\n\
\on the exact method used by Djinn.\n\
\\n\
\  Since Djinn handles propositional calculus it also knows about the\n\
\absurd proposition, corresponding to the empty set.  This set is\n\
\sometimes called Void in Haskell.  Djinn emits its elimination rule as\n\
\an empty case; generated code using it requires the EmptyCase extension:\n\
\   eliminate value = case value of {}\n\
\  Using Void is of little use for programming, but can be interesting\n\
\for theorem proving.  Example, the double negation of the law of\n\
\excluded middle:\n\
\   Djinn> f ? Not (Not (Either x (Not x)))\n\
\   f :: Not (Not (Either x (Not x)))\n\
\   f a = case a (Right (\\b -> a (Left b))) of {}\n\
\  The Not type has the definition 'type Not x = x -> Void'.  The\n\
\regular version of the law of excluded middle cannot be proven, of\n\
\course.\n\
\   Djinn> f ? Either x (Not x)\n\
\   -- f cannot be realized.\n\
\"

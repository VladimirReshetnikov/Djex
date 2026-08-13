{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Representation of the checked typed-candidate association.
--
-- This module is Cabal-private.  Engine adapters may construct an association
-- only after independently checking both the compatibility candidate and its
-- typed graph result.  Public consumers receive the abstract type through
-- "Language.Haskell.Synthesis.TypedCandidate".
module Language.Haskell.Synthesis.Internal.TypedCandidate
  ( TypedCandidate
  , mkTypedCandidate
  , mkCertificateCapableTypedCandidate
  , mkCertificateAssociatedTypedCandidate
  , foldTypedCandidateGraph
  , typedCandidateCompatibility
  , typedQueryResultCompatibility
  , typedCandidateTermGraph
  ) where

import Control.DeepSeq (NFData (rnf))

import Language.Haskell.Synthesis.Internal.TypedGenerated.Certificate.Association
  ( CheckedTypeApplicationCertificateGraph
  , checkedTypeApplicationCertificateGraph
  )
import Language.Haskell.Synthesis.Query (QueryResult)
import Language.Haskell.Synthesis.Type (Type)
import Language.Haskell.Synthesis.TypedGenerated (TermGraph)

-- | One compatibility candidate paired with the result of retaining its
-- checked typed graph.
--
-- Both payloads are deliberately lazy.  Inspecting the compatibility value
-- must not construct a graph, while asking for one graph must not inspect a
-- sibling candidate or a later search batch.  Nominal roles prevent
-- downstream 'coerce' calls from changing any identity or evidence domain
-- while retaining the checked association.  Package-private engine adapters
-- remain responsible for pairing the exact compatibility candidate which was
-- checked; this representation does not prove that pairing.
data TypedCandidate failure ty local candidate = TypedCandidate
  candidate
  (TypedCandidateGraph failure ty local)

-- This sum remains hidden even from package-private consumers.  The exported
-- eliminator presents compatibility and the selected carrier together, while
-- the certificate atom itself continues to keep graph, table, and receipts
-- indivisible.  Trusted internal callers can still retain those observations
-- and construct another candidate, so the module grants no pairing proof.
data TypedCandidateGraph failure ty local where
  TypedCandidateGraphUnavailable
    :: failure
    -> TypedCandidateGraph failure ty local
  TypedCandidatePlainGraph
    :: TermGraph ty local
    -> TypedCandidateGraph failure ty local
  TypedCandidateCertificateGraph
    :: NFData variable
    => CheckedTypeApplicationCertificateGraph variable local
    -> TypedCandidateGraph failure (Type variable) local

type role TypedCandidate nominal nominal nominal nominal
type role TypedCandidateGraph nominal nominal nominal

instance
    ( Eq failure
    , Eq ty
    , Eq local
    , Eq candidate
    ) => Eq (TypedCandidate failure ty local candidate) where
  TypedCandidate leftCompatibility leftGraph ==
      TypedCandidate rightCompatibility rightGraph =
    leftCompatibility == rightCompatibility &&
      typedCandidateGraphProjection leftGraph ==
        typedCandidateGraphProjection rightGraph

instance
    ( Ord failure
    , Ord ty
    , Ord local
    , Ord candidate
    ) => Ord (TypedCandidate failure ty local candidate) where
  compare
      (TypedCandidate leftCompatibility leftGraph)
      (TypedCandidate rightCompatibility rightGraph) =
    case compare leftCompatibility rightCompatibility of
      EQ -> compare
        (typedCandidateGraphProjection leftGraph)
        (typedCandidateGraphProjection rightGraph)
      result -> result

instance
    ( Show failure
    , Show ty
    , Show local
    , Show candidate
    ) => Show (TypedCandidate failure ty local candidate) where
  showsPrec precedence (TypedCandidate compatibility graph) =
    showParen (precedence > 10) $
      showString "TypedCandidate " .
      showsPrec 11 compatibility .
      showChar ' ' .
      showsPrec 11 (typedCandidateGraphProjection graph)

instance
    ( NFData failure
    , NFData ty
    , NFData local
    , NFData candidate
    ) => NFData (TypedCandidate failure ty local candidate) where
  rnf (TypedCandidate compatibility graph) =
    rnf compatibility `seq` rnfTypedCandidateGraph graph

-- | Package one engine-checked compatibility candidate with its exact graph
-- availability result.  Kept private so the two projections cannot be
-- replaced independently by a downstream caller.
mkTypedCandidate
  :: candidate
  -> Either failure (TermGraph ty local)
  -> TypedCandidate failure ty local candidate
mkTypedCandidate compatibility graph =
  TypedCandidate compatibility $ case graph of
    Left failure -> TypedCandidateGraphUnavailable failure
    Right checkedGraph -> TypedCandidatePlainGraph checkedGraph

-- | Package one compatibility candidate with a lazy three-way graph result:
-- @Left failure@ is unavailable, @Right (Left graph)@ is a legacy plain graph,
-- and @Right (Right atom)@ is an opaque certificate-associated graph.
--
-- The complete nested result remains underneath the lazy private graph field.
-- An engine can therefore pass its availability decision directly without an
-- outer case which would be demanded by compatibility projection.
mkCertificateCapableTypedCandidate
  :: NFData variable
  => candidate
  -> Either
      failure
      (Either
        (TermGraph (Type variable) local)
        (CheckedTypeApplicationCertificateGraph variable local))
  -> TypedCandidate failure (Type variable) local candidate
mkCertificateCapableTypedCandidate compatibility graph =
  TypedCandidate compatibility $ case graph of
    Left failure -> TypedCandidateGraphUnavailable failure
    Right retained -> case retained of
      Left checkedGraph -> TypedCandidatePlainGraph checkedGraph
      Right checkedGraph -> TypedCandidateCertificateGraph checkedGraph

-- | Package one engine-checked compatibility candidate with either its
-- existing absence reason or an indivisible checked certificate graph.
--
-- The availability result is deliberately retained as a thunk.  Constructing
-- this value or projecting compatibility therefore does not inspect whether
-- certificate association succeeded.
mkCertificateAssociatedTypedCandidate
  :: NFData variable
  => candidate
  -> Either
      failure
      (CheckedTypeApplicationCertificateGraph variable local)
  -> TypedCandidate failure (Type variable) local candidate
mkCertificateAssociatedTypedCandidate compatibility graph =
  mkCertificateCapableTypedCandidate compatibility $ fmap Right graph

-- | Consume the hidden retention branch together with its compatibility
-- candidate.
--
-- Each callback receives that compatibility value and exactly one carrier.
-- The callbacks and every payload remain non-strict until selected by the
-- hidden outer branch.  This shape makes correct package-private use direct;
-- it does not prevent a trusted caller from retaining an observation or
-- constructing a differently paired 'TypedCandidate'.
foldTypedCandidateGraph
  :: (candidate -> failure -> result)
  -> (candidate -> TermGraph (Type variable) local -> result)
  -> ( candidate
       -> CheckedTypeApplicationCertificateGraph variable local
       -> result
     )
  -> TypedCandidate failure (Type variable) local candidate
  -> result
foldTypedCandidateGraph unavailable plain associated
    (TypedCandidate compatibility graph) = case graph of
  TypedCandidateGraphUnavailable failure ->
    unavailable compatibility failure
  TypedCandidatePlainGraph checkedGraph ->
    plain compatibility checkedGraph
  TypedCandidateCertificateGraph checkedGraph ->
    associated compatibility checkedGraph

-- | Recover the unchanged compatibility candidate.
typedCandidateCompatibility
  :: TypedCandidate failure ty local candidate
  -> candidate
typedCandidateCompatibility (TypedCandidate compatibility _) = compatibility

-- | Erase typed-candidate retention from one checked query result without
-- revalidating or changing its envelope.  The result's logical evidence,
-- operational progress, metadata, candidate cardinality, and ordering are
-- therefore unchanged.  Graph availability remains unobserved and candidate
-- tails stay lazy.
typedQueryResultCompatibility
  :: QueryResult metadata (TypedCandidate failure ty local candidate)
  -> QueryResult metadata candidate
typedQueryResultCompatibility = fmap typedCandidateCompatibility

-- | Recover either the explicit reason typed retention was unavailable or the
-- independently sealed typed graph.
typedCandidateTermGraph
  :: TypedCandidate failure ty local candidate
  -> Either failure (TermGraph ty local)
typedCandidateTermGraph (TypedCandidate _ graph) =
  typedCandidateGraphProjection graph

typedCandidateGraphProjection
  :: TypedCandidateGraph failure ty local
  -> Either failure (TermGraph ty local)
typedCandidateGraphProjection graph = case graph of
  TypedCandidateGraphUnavailable failure -> Left failure
  TypedCandidatePlainGraph checkedGraph -> Right checkedGraph
  TypedCandidateCertificateGraph checkedGraph ->
    Right $ checkedTypeApplicationCertificateGraph checkedGraph

rnfTypedCandidateGraph
  :: (NFData failure, NFData ty, NFData local)
  => TypedCandidateGraph failure ty local
  -> ()
rnfTypedCandidateGraph graph = case graph of
  TypedCandidateGraphUnavailable failure -> rnf failure
  TypedCandidatePlainGraph checkedGraph -> rnf checkedGraph
  TypedCandidateCertificateGraph checkedGraph -> rnf checkedGraph

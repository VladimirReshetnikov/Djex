-- | Bounded SMT-LIB response decoding for canonical Length queries.
--
-- The decoder implements the complete SMT-LIB 2.x lexical S-expression
-- surface privately, then accepts only the exact response shapes requested by
-- 'LengthSMTLibQuery': @sat@, @unsat@, @unknown@, and input-only @get-value@
-- valuations.  It understands comments, the four standard whitespace bytes,
-- doubled-quote strings, quoted symbols, and every standard atom category.
-- Response, nesting, node, token, and integer bounds are enforced before a
-- caller can retain decoded values.
--
-- Parsing is not process framing, query association, model validation, or
-- proof.  In particular, a parsed @unsat@ remains a heuristic solver report.
-- A parsed input valuation can yield model-relative counterexample evidence
-- only after 'validateLengthSMTLibCounterexample' independently recomputes the
-- candidate result against the exact retained problem.
module Language.Haskell.Synthesis.Semantic.Length.SMTLib.Response
  ( lengthSMTLibResponseSchemaTag
  , LengthSMTLibResponseLimitSource (..)
  , LengthSMTLibResponseLimits
  , LengthSMTLibResponseLimitField (..)
  , LengthSMTLibResponseLimitError (..)
  , mkLengthSMTLibResponseLimits
  , defaultLengthSMTLibResponseLimitSource
  , defaultLengthSMTLibResponseLimits
  , lengthSMTLibResponseByteLimit
  , lengthSMTLibResponseNestingDepthLimit
  , lengthSMTLibResponseNodeLimit
  , lengthSMTLibResponseTokenByteLimit
  , lengthSMTLibResponseIntegerBitLimit
  , SMTLibTokenPart (..)
  , SMTLibParseError (..)
  , LengthSMTLibResponseError (..)
  , parseLengthSMTLibCheckResponse
  , parseLengthSMTLibInputValueResponse
  ) where

import Language.Haskell.Synthesis.Internal.Semantic.Length.SMTLib.Response

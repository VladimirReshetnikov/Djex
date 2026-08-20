-- | Shared structural fields for canonical type fingerprints.
--
-- This private module deliberately stops at 'FingerprintField'.  The domain
-- which owns an identity still chooses its role, version, complete field
-- sequence, and byte bound before sealing a public 'Fingerprint'.
module Language.Haskell.Synthesis.Internal.Fingerprint.Type
  ( canonicalTypeFingerprintForm
  , typeFingerprintField
  , constraintFingerprintField
  , boxityFingerprintField
  , taggedFingerprintField
  , asciiFingerprintBytes
  ) where

import Data.Char (ord)
import Data.Word (Word8)

import Language.Haskell.Synthesis.Constraint (Constraint (..))
import Language.Haskell.Synthesis.Internal.Fingerprint
  ( FingerprintField (..)
  )
import Language.Haskell.Synthesis.Internal.Alpha (eraseVacuousForalls)
import Language.Haskell.Synthesis.Name (Boxity (..))
import Language.Haskell.Synthesis.Type (Type (..), canonicalizeType)

-- | Canonical structural form used before assigning type-variable slots.
-- Saturated arrows and tuples use their dedicated nodes, while a binderless,
-- context-free forall contributes no semantic scope and is erased throughout
-- the type.  This matches the equality policy of the shared checked type
-- structure without assigning any identity to free variables.
canonicalTypeFingerprintForm :: Type variable -> Type variable
canonicalTypeFingerprintForm = eraseVacuousForalls . canonicalizeType

-- | Encode one already-normalized shared type.  The caller owns variable
-- identity because closed schemes, query-wide free variables, and other
-- domains require different canonical variable policies.
typeFingerprintField
  :: (variable -> FingerprintField)
  -> Type variable
  -> FingerprintField
typeFingerprintField variableField source = case source of
  TypeVariable variable -> taggedFingerprintField "type-variable"
    [variableField variable]
  TypeConstructor name -> taggedFingerprintField "type-constructor"
    [FingerprintName name]
  TypeApplication function argument ->
    taggedFingerprintField "type-application"
      [ typeFingerprintField variableField function
      , typeFingerprintField variableField argument
      ]
  FunctionType parameter result -> taggedFingerprintField "function"
    [ typeFingerprintField variableField parameter
    , typeFingerprintField variableField result
    ]
  TupleType boxity fields -> taggedFingerprintField "tuple"
    [ boxityFingerprintField boxity
    , FingerprintSequence $ map (typeFingerprintField variableField) fields
    ]
  ForallType binders constraints body -> taggedFingerprintField "forall"
    [ FingerprintSequence $ map variableField binders
    , FingerprintSequence $
        map (constraintFingerprintField variableField) constraints
    , typeFingerprintField variableField body
    ]

-- | Encode one class constraint as a @constraint@-tagged field holding the
-- class name followed by the argument types in order, each encoded with
-- 'typeFingerprintField' under the caller's variable policy.
constraintFingerprintField
  :: (variable -> FingerprintField)
  -> Constraint (Type variable)
  -> FingerprintField
constraintFingerprintField variableField
    (Constraint className arguments) = taggedFingerprintField "constraint"
  [ FingerprintName className
  , FingerprintSequence $ map (typeFingerprintField variableField) arguments
  ]

-- | Encode tuple boxity as the ASCII byte field @boxed@ or @unboxed@.
boxityFingerprintField :: Boxity -> FingerprintField
boxityFingerprintField boxity = FingerprintBytes $ asciiFingerprintBytes $
  case boxity of
    Boxed -> "boxed"
    Unboxed -> "unboxed"

-- | Build a 'FingerprintTag' whose tag is the ASCII encoding of the given
-- string and whose payload is the given field sequence.
taggedFingerprintField :: String -> [FingerprintField] -> FingerprintField
taggedFingerprintField tag = FingerprintTag $ asciiFingerprintBytes tag

-- | Encode a string one byte per character by truncating each code point to
-- eight bits.  Intended for the ASCII tags and labels used in fingerprint
-- fields; non-ASCII characters are not preserved faithfully.
asciiFingerprintBytes :: String -> [Word8]
asciiFingerprintBytes = map $ fromIntegral . ord

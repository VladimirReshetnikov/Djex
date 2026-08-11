{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Private canonical construction for stable structural fingerprints.
--
-- Public consumers can inspect fingerprints through
-- "Language.Haskell.Synthesis.Fingerprint", but only trusted package modules
-- construct them.  The encoding is a complete structural key, not a digest:
-- equality never depends on accepting a hash collision.
module Language.Haskell.Synthesis.Internal.Fingerprint
  ( Fingerprint (..)
  , FingerprintBuilder (..)
  , FingerprintField (..)
  , FingerprintLimitError (..)
  , buildFingerprint
  , buildFingerprintWithin
  , fingerprintCanonicalBytes
  , fingerprintCode
  ) where

import Control.DeepSeq (NFData (rnf))
import Data.Char (intToDigit, ord)
import Data.List (genericLength)
import Data.Word (Word8)
import Numeric.Natural (Natural)

import Language.Haskell.Synthesis.Name
  ( Boxity (..)
  , LexicalClass (..)
  , ModuleName
  , Name
  , Occurrence (..)
  , SpecialName (..)
  , moduleNameSegments
  , nameModule
  , nameOccurrence
  )

-- | A complete canonical byte key for one phantom-typed identity domain.
--
-- The constructor is exported only from this private module.  The public
-- module keeps it opaque, preventing callers from attaching arbitrary bytes
-- to a semantic identity type.
newtype Fingerprint subject = Fingerprint [Word8]
  deriving (Eq, Ord)

-- The subject is semantically significant even though it has no runtime
-- representation.  A nominal role prevents downstream callers from using
-- 'coerce' to relabel a key from one identity domain as another.
type role Fingerprint nominal

instance Show (Fingerprint subject) where
  showsPrec precedence fingerprint = showParen (precedence > 10) $
    showString "Fingerprint " . shows (fingerprintCode fingerprint)

instance NFData (Fingerprint subject) where
  rnf (Fingerprint bytes) = rnf bytes

-- | Version, role, and ordered fields for one canonical identity.
--
-- The version describes the caller's semantic schema.  The role separates
-- identities which happen to contain the same fields, such as a contract and
-- an inventory.  Both are encoded inside the key.
data FingerprintBuilder subject = FingerprintBuilder
  { fingerprintBuilderVersion :: !Natural
  , fingerprintBuilderRole :: [Word8]
  , fingerprintBuilderFields :: [FingerprintField]
  }
  deriving (Eq, Ord, Show)

-- | Closed field vocabulary used by package-owned fingerprint schemas.
--
-- Every constructor has a distinct wire tag and a byte-length-prefixed
-- payload.  Sequences retain element boundaries because each nested field is
-- encoded independently.  'FingerprintName' inspects the validated
-- structural 'Name' representation instead of depending on presentation text
-- or its 'Show' instance.
data FingerprintField
  = FingerprintNatural !Natural
  | FingerprintBytes [Word8]
  | FingerprintSequence [FingerprintField]
  | FingerprintTag [Word8] [FingerprintField]
  | FingerprintName Name
  deriving (Eq, Ord, Show)

-- | A canonical fingerprint exceeded its caller-supplied byte budget.
--
-- The observed size is deliberately capped at @maximum + 1@.  Construction
-- only needs to distinguish a fitting encoding from an oversized one, and
-- stopping at that first excess keeps rejection productive for infinite byte
-- lists and cyclic field structures.
data FingerprintLimitError = FingerprintLimitExceeded
  { fingerprintMaximumBytes :: !Natural
  , fingerprintObservedBytesAtLeast :: !Natural
  }
  deriving (Eq, Ord, Show)

-- | Seal a complete builder into its collision-free canonical byte key.
buildFingerprint :: FingerprintBuilder subject -> Fingerprint subject
buildFingerprint builder = Fingerprint $
  fingerprintMagic
  ++ encodeField
      (FingerprintNatural $ fingerprintBuilderVersion builder)
  ++ encodeField
      (FingerprintBytes $ fingerprintBuilderRole builder)
  ++ encodeField
      (FingerprintSequence $ fingerprintBuilderFields builder)

-- | Seal a fingerprint only when its complete canonical encoding fits within
-- the given byte budget.
--
-- This is not implemented as @length . buildFingerprint@: the ordinary
-- encoding writes a field's payload length before its payload and therefore
-- must first traverse that payload.  The bounded encoder carries each
-- payload's exact byte count alongside its bytes, charges every eventual
-- output byte against the remaining budget, and stops following an infinite
-- byte list, field list, or cyclic nested field at the first excess.  This
-- bounds structural traversal by the encoded-byte budget; it does not claim a
-- constant-time bound for arithmetic on one already-constructed unbounded
-- 'Natural' or for inspection of one validated 'Name'.
buildFingerprintWithin
  :: Natural
  -> FingerprintBuilder subject
  -> Either FingerprintLimitError (Fingerprint subject)
buildFingerprintWithin maximumBytes builder = do
  (magic, _, afterMagic) <- retainBytes
    maximumBytes maximumBytes fingerprintMagic
  (version, _, afterVersion) <- encodeFieldWithin maximumBytes afterMagic
    $ FingerprintNatural $ fingerprintBuilderVersion builder
  (role, _, afterRole) <- encodeFieldWithin maximumBytes afterVersion
    $ FingerprintBytes $ fingerprintBuilderRole builder
  (fields, _, _) <- encodeFieldWithin maximumBytes afterRole
    $ FingerprintSequence $ fingerprintBuilderFields builder
  pure $ Fingerprint $ magic ++ version ++ role ++ fields

-- | Recover the exact canonical key bytes.
fingerprintCanonicalBytes :: Fingerprint subject -> [Word8]
fingerprintCanonicalBytes (Fingerprint bytes) = bytes

-- | Stable lowercase hexadecimal rendering of the complete key.
fingerprintCode :: Fingerprint subject -> String
fingerprintCode = concatMap renderByte . fingerprintCanonicalBytes
 where
  renderByte byte =
    [ intToDigit $ fromIntegral byte `div` 16
    , intToDigit $ fromIntegral byte `mod` 16
    ]

-- @DJEXFP@ followed by the canonical encoding-format version.  Semantic
-- schemas additionally carry their own version in 'FingerprintBuilder'.
fingerprintMagic :: [Word8]
fingerprintMagic = [0x44, 0x4a, 0x45, 0x58, 0x46, 0x50, 0x01]

encodeField :: FingerprintField -> [Word8]
encodeField field = case field of
  FingerprintNatural value -> sizedField 0x01 $ encodeNatural value
  FingerprintBytes bytes -> sizedField 0x02 bytes
  FingerprintSequence fields -> sizedField 0x03 $
    concatMap encodeField fields
  FingerprintTag tag fields -> sizedField 0x04 $
    encodeField (FingerprintBytes tag)
    ++ encodeField (FingerprintSequence fields)
  FingerprintName name -> sizedField 0x05 $
    encodeField $ FingerprintSequence $ nameFields name

-- Count output bytes before recursively entering a payload.  Although a
-- sized field emits its length before its payload, accounting for its tag,
-- then payload, then length is byte-count equivalent and makes a cyclic
-- payload consume the finite budget productively.
encodeFieldWithin
  :: Natural
  -> Natural
  -> FingerprintField
  -> Either FingerprintLimitError ([Word8], Natural, Natural)
encodeFieldWithin maximumBytes remaining field = case field of
  FingerprintNatural value ->
    sizedFieldWithin maximumBytes remaining 0x01
      $ \available -> encodeNaturalWithin maximumBytes available value
  FingerprintBytes bytes ->
    sizedFieldWithin maximumBytes remaining 0x02
      $ \available -> retainBytes maximumBytes available bytes
  FingerprintSequence fields ->
    sizedFieldWithin maximumBytes remaining 0x03
      $ \available -> encodeFieldsWithin maximumBytes available fields
  FingerprintTag tag fields ->
    sizedFieldWithin maximumBytes remaining 0x04 $ \afterTag -> do
      (encodedTag, encodedTagLength, afterEncodedTag) <-
        encodeFieldWithin maximumBytes afterTag $ FingerprintBytes tag
      (encodedFields, encodedFieldsLength, afterEncodedFields) <-
        encodeFieldWithin maximumBytes afterEncodedTag
          $ FingerprintSequence fields
      pure
        ( encodedTag ++ encodedFields
        , encodedTagLength + encodedFieldsLength
        , afterEncodedFields
        )
  FingerprintName name ->
    sizedFieldWithin maximumBytes remaining 0x05 $ \available ->
      encodeFieldWithin maximumBytes available
        $ FingerprintSequence $ nameFields name

sizedFieldWithin
  :: Natural
  -> Natural
  -> Word8
  -> (Natural -> Either FingerprintLimitError ([Word8], Natural, Natural))
  -> Either FingerprintLimitError ([Word8], Natural, Natural)
sizedFieldWithin maximumBytes remaining fieldTag encodePayload = do
  (_, tagLength, afterTag) <- retainBytes
    maximumBytes remaining [fieldTag]
  (payload, payloadLength, afterPayload) <- encodePayload afterTag
  (encodedLength, encodedLengthLength, afterLength) <- retainBytes
    maximumBytes afterPayload $ encodeVariableNatural payloadLength
  pure
    ( fieldTag : encodedLength ++ payload
    , tagLength + encodedLengthLength + payloadLength
    , afterLength
    )

encodeFieldsWithin
  :: Natural
  -> Natural
  -> [FingerprintField]
  -> Either FingerprintLimitError ([Word8], Natural, Natural)
encodeFieldsWithin _ remaining [] = Right ([], 0, remaining)
encodeFieldsWithin maximumBytes remaining (field : fields) = do
  (encodedField, encodedFieldLength, afterField) <-
    encodeFieldWithin maximumBytes remaining field
  (encodedFields, encodedFieldsLength, afterFields) <- encodeFieldsWithin
    maximumBytes afterField fields
  pure
    ( encodedField ++ encodedFields
    , encodedFieldLength + encodedFieldsLength
    , afterFields
    )

retainBytes
  :: Natural
  -> Natural
  -> [Word8]
  -> Either FingerprintLimitError ([Word8], Natural, Natural)
retainBytes _ remaining [] = Right ([], 0, remaining)
retainBytes maximumBytes 0 (_ : _) = Left FingerprintLimitExceeded
  { fingerprintMaximumBytes = maximumBytes
  , fingerprintObservedBytesAtLeast = maximumBytes + 1
  }
retainBytes maximumBytes remaining (byte : bytes) = do
  (retained, retainedLength, afterBytes) <- retainBytes
    maximumBytes (remaining - 1) bytes
  pure (byte : retained, retainedLength + 1, afterBytes)

-- Build the minimal big-endian magnitude by prepending each successively more
-- significant base-256 digit.  Unlike 'encodeNatural', this does not first
-- reverse a complete unbounded digit list: once the available output budget is
-- exhausted, one remaining non-zero quotient is enough to reject the value.
encodeNaturalWithin
  :: Natural
  -> Natural
  -> Natural
  -> Either FingerprintLimitError ([Word8], Natural, Natural)
encodeNaturalWithin maximumBytes remaining 0 =
  retainBytes maximumBytes remaining [0]
encodeNaturalWithin maximumBytes remaining value = go remaining value [] 0
 where
  go 0 _ _ _ = Left FingerprintLimitExceeded
    { fingerprintMaximumBytes = maximumBytes
    , fingerprintObservedBytesAtLeast = maximumBytes + 1
    }
  go available unencoded encoded encodedLength =
    let (quotient, remainder) = unencoded `quotRem` 256
        retained = fromIntegral remainder : encoded
        retainedLength = encodedLength + 1
        afterDigit = available - 1
    in if quotient == 0
        then Right (retained, retainedLength, afterDigit)
        else go afterDigit quotient retained retainedLength

sizedField :: Word8 -> [Word8] -> [Word8]
sizedField fieldTag payload =
  fieldTag : encodeVariableNatural (genericLength payload) ++ payload

-- Natural fields use a unique minimal big-endian magnitude.  In particular,
-- zero is one zero byte rather than an absent payload.
encodeNatural :: Natural -> [Word8]
encodeNatural 0 = [0]
encodeNatural value = reverse $ go value
 where
  go 0 = []
  go remaining =
    let (quotient, remainder) = remaining `quotRem` 256
    in fromIntegral remainder : go quotient

-- Canonical unsigned LEB128 for field byte lengths.  The high bit states that
-- another group follows; the final group is therefore unambiguous without a
-- machine-sized length.
encodeVariableNatural :: Natural -> [Word8]
encodeVariableNatural = go
 where
  go !remaining =
    let (quotient, remainder) = remaining `quotRem` 128
        byte = fromIntegral remainder
    in if quotient == 0
        then [byte]
        else (byte + 0x80) : go quotient

nameFields :: Name -> [FingerprintField]
nameFields name =
  [ qualifierField $ nameModule name
  , occurrenceField $ nameOccurrence name
  ]

qualifierField :: Maybe ModuleName -> FingerprintField
qualifierField Nothing = taggedAscii "unqualified" []
qualifierField (Just qualifier) = taggedAscii "qualified"
  [ FingerprintSequence $ map textField $ moduleNameSegments qualifier ]

occurrenceField :: Occurrence -> FingerprintField
occurrenceField occurrence = case occurrence of
  IdentifierOccurrence lexicalClass spelling -> taggedAscii "identifier"
    [lexicalClassField lexicalClass, textField spelling]
  OperatorOccurrence lexicalClass spelling -> taggedAscii "operator"
    [lexicalClassField lexicalClass, textField spelling]
  SpecialOccurrence special -> specialNameField special

lexicalClassField :: LexicalClass -> FingerprintField
lexicalClassField lexicalClass = case lexicalClass of
  VariableLike -> taggedAscii "variable-like" []
  ConstructorLike -> taggedAscii "constructor-like" []

specialNameField :: SpecialName -> FingerprintField
specialNameField special = case special of
  ListConstructor -> taggedAscii "list-constructor" []
  ConsConstructor -> taggedAscii "cons-constructor" []
  FunctionConstructor -> taggedAscii "function-constructor" []
  TupleConstructor boxity arity -> taggedAscii "tuple-constructor"
    [ boxityField boxity
    , FingerprintNatural $ fromIntegral arity
    ]

boxityField :: Boxity -> FingerprintField
boxityField boxity = case boxity of
  Boxed -> taggedAscii "boxed" []
  Unboxed -> taggedAscii "unboxed" []

-- Haskell names may contain non-ASCII identifier characters.  Encode source
-- text as Unicode code points rather than relying on a locale or an
-- implementation-specific String-to-byte conversion.
textField :: String -> FingerprintField
textField = FingerprintSequence
  . map (FingerprintNatural . fromIntegral . ord)

taggedAscii :: String -> [FingerprintField] -> FingerprintField
taggedAscii tag = FingerprintTag $ map (fromIntegral . ord) tag

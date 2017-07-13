module Data.ByteString.Internal
  ( Octet
  , Offset
  , Size
  , ByteString(..)
  , packBytes
  , packBytes'
  , unsafePackLenBytes
  , unsafeCreate
  , unsafeCreateUptoN
  , unsafeCreateUptoN'
  , create
  , createAndTrim
  , createUptoN
  , createUptoN'
  , createAndTrim'

  , mallocByteString
  , toBytesArray
  , fromBytesArray

  , memcpy
  , memcpyArr
  , memmove
  , memchr
  , memcmp
  , memset
  , subarray
  , findSubstring
  , elemIndexEnd

  , reverse
  , intersperse
  , foldlPtr
  , foldrPtr
  , concat

  , Ptr(..)
  , poke
  , pokeByteOff
  , peek
  , peekByteOff
  , plusPtr
  , minusPtr
  , nullPtr
  , newPtr
  , unsafePeek
  , unsafePeekByteOff

  , _isSpace
  , _isSpaceChar
  , assert
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Rec.Class (tailRecM2, tailRecM3, Step(..))

import Data.ArrayBuffer.Types (ArrayView, ArrayBuffer, Uint8, Uint8Array, ByteOffset, ByteLength)
import Data.Function.Uncurried as Fn
import Data.List (List(Nil), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | Type synonym indicating the value refers to a length of a buffer
type Size = Int

-- | Encode Pointer as Tuple of current offset and ArrayView
data Ptr a = Ptr ByteOffset (ArrayView a)

-- | ByteString
data ByteString = ByteString (Ptr Uint8) Offset Size

instance eqByteString :: Eq ByteString where
  eq a@(ByteString bs off len) b@(ByteString bs' off' len')
    | len /= len'                = false
    | otherwise                  = EQ == compare a b

instance ordByteString :: Ord ByteString where
  compare (ByteString _ _ 0) (ByteString _ _ 0) = EQ
  compare (ByteString p1 off1 len1) (ByteString p2 off2 len2) = unsafePerformEff $ do
    i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) (min len1 len2)
    pure $ case i `compare` 0 of
      EQ  -> len1 `compare` len2
      ox  -> ox

instance semigroupByteString :: Semigroup ByteString where
  append (ByteString _ _ 0) b                  = b
  append a                  (ByteString _ _ 0) = a
  append (ByteString b1 off1 len1) (ByteString b2 off2 len2) =
    unsafeCreate (len1+len2) \destptr1 ->
      let
        destptr2 = destptr1 `plusPtr` len1
      in
        memcpy destptr1 (b1 `plusPtr` off1) len1
      *> memcpy destptr2 (b2 `plusPtr` off2) len2

instance monoidByteString :: Monoid ByteString where
  mempty = ByteString nullPtr 0 0

instance showByteString :: Show ByteString where
  show (ByteString v s l) = "(ByteString " <> show v <> " " <> show s <> " " <> show l <> ")"

instance eqPtr :: Eq (Ptr a) where
  eq a b = EQ == compare a b

instance ordPtr :: Ord (Ptr a) where
  compare (Ptr a b) (Ptr c d)
    | a < c       = LT
    | a > c       = GT
    | otherwise   = compare 0 $ Fn.runFn2 _arrayViewCompare b d

instance showPtr :: Show (Ptr a) where
  show (Ptr n v) = "(Ptr " <> show n <> " " <> _printArrayView v <> " )"

newPtr :: forall a. ArrayView a -> Ptr a
newPtr = Ptr 0

nullPtr :: forall a. Ptr a
nullPtr = Ptr 0 _nullPtr

poke :: forall a eff. Ptr a -> Int -> Eff eff Unit
poke ptr = pokeByteOff ptr 0

peek :: forall a eff. Ptr a -> Eff eff (Maybe Int)
peek ptr = peekByteOff ptr 0

unsafePeek :: forall a eff. Ptr a -> Eff eff Int
unsafePeek ptr = unsafePeekByteOff ptr 0

pokeByteOff :: forall a eff. Ptr a -> ByteOffset -> Int -> Eff eff Unit
pokeByteOff (Ptr off av) n v = Fn.runFn3 _setAtOffset v (off + n) av

peekByteOff :: forall a eff. Ptr a -> ByteOffset -> Eff eff (Maybe Int)
peekByteOff (Ptr off av) n = Fn.runFn4 _getAtOffset Nothing Just (off + n) av

unsafePeekByteOff :: forall a eff. Ptr a -> ByteOffset -> Eff eff Int
unsafePeekByteOff (Ptr off av) n = Fn.runFn2 _unsafeGetAtOffset (off + n) av

plusPtr :: forall a. Ptr a -> Int -> Ptr a
plusPtr (Ptr n av) m = Ptr (n + m) av

minusPtr :: forall a. Ptr a -> Ptr a -> Int
minusPtr (Ptr n _) (Ptr m _) = n - m

packBytes :: List Octet -> ByteString
packBytes xs = unsafePackLenBytes (L.length xs) xs

packBytes' :: Array Octet -> ByteString
packBytes' xs = unsafePerformEff do
  Tuple pt len <- fromBytesArray xs
  pure $ ByteString pt 0 len

unsafePackLenBytes :: Size -> List Octet -> ByteString
unsafePackLenBytes size xs = unsafeCreate size (tailRecM2 go xs)
  where
    go Nil    _ = pure (Done unit)
    go (x:ys) p = do
      _ <- poke p x
      pure $ Loop { a: ys, b: p `plusPtr` 1 }

unsafeCreate :: forall eff. Size -> (Ptr Uint8 -> Eff eff Unit) -> ByteString
unsafeCreate size eff = unsafePerformEff $ create size eff

unsafeCreateUptoN :: forall eff. Size -> (Ptr Uint8 -> Eff eff Size) -> ByteString
unsafeCreateUptoN size eff = unsafePerformEff $ createUptoN size eff

unsafeCreateUptoN' :: forall a eff. Size -> (Ptr Uint8 -> Eff eff (Tuple Size a)) -> Tuple ByteString a
unsafeCreateUptoN' size eff = unsafePerformEff $ createUptoN' size eff

create :: forall eff. Size -> (Ptr Uint8 -> Eff eff Unit) -> Eff eff ByteString
create size k = do
  pt <- mallocByteString size
  _ <- k pt
  pure $ ByteString pt 0 size

createUptoN :: forall eff. Size -> (Ptr Uint8 -> Eff eff Size) -> Eff eff ByteString
createUptoN size k = do
  pt <- mallocByteString size
  len <- k pt
  pure $ ByteString pt 0 (min size len)

createUptoN' :: forall a eff. Size -> (Ptr Uint8 -> Eff eff (Tuple Size a)) -> Eff eff (Tuple ByteString a)
createUptoN' size k = do
  pt <- mallocByteString size
  Tuple len a <- k pt
  pure $ Tuple (ByteString pt 0 (min size len)) a

createAndTrim :: forall eff. Size -> (Ptr Uint8 -> Eff eff Int) -> Eff eff ByteString
createAndTrim size k = do
  pt <- mallocByteString size
  len <- k pt
  if len >= size
    then pure $ ByteString pt 0 size
    else create len \pt' -> memcpy pt' pt len

createAndTrim'
  :: forall eff a
   . Size
  -> (Ptr Uint8 -> Eff eff { offset :: Offset, size :: Size, value :: a })
  -> Eff eff (Tuple ByteString a)
createAndTrim' size' k = do
  pt <- mallocByteString size'
  { offset, size, value } <- k pt
  if size >= size'
    then pure $ Tuple (ByteString pt 0 size) value
    else do
      ps <- create size \pt' -> memcpy pt' (pt `plusPtr` offset) size
      pure $ Tuple ps value

mallocByteString :: forall eff. Size -> Eff eff (Ptr Uint8)
mallocByteString size = do
  b <- allocArrayBuffer size
  u8 <- Fn.runFn3 _newUint8Array 0 size b
  pure (newPtr u8)

fromBytesArray :: forall eff. Array Octet -> Eff eff (Tuple (Ptr Uint8) Size)
fromBytesArray xs = do
  pt <- _fromArray xs
  len <- _arrayViewLen pt
  pure $ Tuple (newPtr pt) len

toBytesArray :: forall eff. Ptr Uint8 -> Offset -> Size -> (Eff eff (Array Octet))
toBytesArray (Ptr n av) ofs size = Fn.runFn3 _toArray av (ofs + n) size

memcpy :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Size -> Eff eff Unit
memcpy (Ptr off1 av1) (Ptr off2 av2) n = Fn.runFn5 _memcpy av1 off1 av2 off2 n

memcpyArr :: forall eff. Ptr Uint8 -> Array Octet -> Eff eff Unit
memcpyArr (Ptr ofs buf) arr = Fn.runFn3 _memcpyArr buf ofs arr

memmove :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Size -> Eff eff Unit
memmove (Ptr off1 av1) (Ptr off2 av2) n = Fn.runFn5 _memmove av1 off1 av2 off2 n

memchr :: forall eff. Ptr Uint8 -> Octet -> Size -> Eff eff (Ptr Uint8)
memchr (Ptr off av) c n = do
  off' <- Fn.runFn4 _memchr av off c n
  pure $ if off' < 0 then nullPtr else Ptr off' av

memcmp :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Size -> Eff eff Int
memcmp (Ptr off1 av1) (Ptr off2 av2) n = Fn.runFn5 _memcmp av1 off1 av2 off2 n

memset :: forall eff. Ptr Uint8 -> Octet -> Size -> Eff eff Unit
memset (Ptr off av1) chr size = Fn.runFn4 _memset av1 off chr size

reverse :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Size -> Eff eff Unit
reverse (Ptr off1 av1) (Ptr off2 av2) n = Fn.runFn5 _reverse av1 off1 av2 off2 n

intersperse :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Octet -> Eff eff Unit
intersperse (Ptr off1 av1) (Ptr off2 av2) c =
  Fn.runFn5 _intersperse av1 off1 av2 off2 c

foldlPtr :: forall a. (a -> Octet -> a) -> a -> Ptr Uint8 -> a
foldlPtr f a (Ptr offs av) = Fn.runFn4 _foldl f a offs av

foldrPtr :: forall a. (Octet -> a -> a) -> a -> Ptr Uint8 -> a
foldrPtr f a (Ptr offs av) = Fn.runFn4 _foldr f a offs av

elemIndexEnd :: forall a. Int -> ArrayView a -> Maybe Int
elemIndexEnd x xs = Fn.runFn4 _findLastIndex Nothing Just (x == _) xs

-- | returns a new TypedArray on the same ArrayBuffer store
subarray
  :: forall a eff
   . ByteOffset
  -- ^ Zero-based index at which to begin extraction.
  -> ByteOffset
  -- ^ Zero-based index before which to end extraction. slice extracts up to but not including end.
  -> ArrayView a
  -- ^ The target typedarray to slice
  -> Eff eff (ArrayView a)
subarray start end ta = Fn.runFn3 _subarray start end ta

findSubstring :: ByteString -> ByteString -> Int
findSubstring (ByteString (Ptr a av) n l) (ByteString (Ptr b bv) m j) =
  Fn.runFn6 _findSubstring av (a + n) l bv (b + m) j

newUint8Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Uint8Array
newUint8Array ofs len ab = Fn.runFn3 _newUint8Array ofs len ab

concat :: List ByteString -> ByteString
concat xs = goLen0 xs xs
  where
    goLen0 _    Nil                      = mempty
    goLen0 bss0 (ByteString _ _ 0 : bss) = goLen0 bss0 bss
    goLen0 bss0 (bs           :bss)      = goLen1 bss0 bs bss

    goLen1 _    bs Nil                         = bs
    goLen1 bss0 bs (ByteString _ _ 0  :bss)    = goLen1 bss0 bs bss
    goLen1 bss0 bs (ByteString _ _ len:bss)    = goLen bss0 (len' + len) bss
      where ByteString _ _ len' = bs

    goLen bss0 total (ByteString _ _ len:bss) = goLen bss0 total' bss
      where total' = total + len
    goLen bss0 total Nil = unsafeCreate total $ \buf -> tailRecM3 goCopy bss0 buf 0

    goCopy Nil                  _ n            = pure (Done unit)
    goCopy (ByteString _  _   0  :bss) buf  n  = pure (Loop {a: bss, b: buf, c: n})
    goCopy (ByteString buf off len:bss) buf' n = do
      _ <- memcpy buf' (buf `plusPtr` off) len
      pure (Loop { a: bss, b: buf', c: n + len })

_isSpace :: Octet -> Boolean
_isSpace c =
  c == 0x20 ||
  c == 0x0A || -- LF, \n
  c == 0x09 || -- HT, \t
  c == 0x0C || -- FF, \f
  c == 0x0D || -- CR, \r
  c == 0x0B || -- VT, \v
  c == 0xA0    -- spotted by QC..

_isSpaceChar :: Char -> Boolean
_isSpaceChar c =
  c == ' '     ||
  c == '\t'    ||
  c == '\n'    ||
  c == '\r'    ||
  c == '\f'    ||
  c == '\v'    ||
  c == '\xa0'

foreign import _memcpy :: forall eff. Fn.Fn5 Uint8Array Offset Uint8Array Offset Size (Eff eff Unit)

foreign import _memcpyArr :: forall eff. Fn.Fn3 Uint8Array Offset (Array Octet) (Eff eff Unit)

foreign import  _memmove :: forall eff. Fn.Fn5 Uint8Array Offset Uint8Array Offset Size (Eff eff Unit)

foreign import _memchr :: forall eff. Fn.Fn4 Uint8Array Offset Octet Size (Eff eff Offset)

foreign import _memcmp :: forall eff. Fn.Fn5 Uint8Array Offset Uint8Array Offset Size (Eff eff Int)

foreign import _memset :: forall eff. Fn.Fn4 Uint8Array Offset Octet Size (Eff eff Unit)

foreign import _reverse :: forall eff. Fn.Fn5 Uint8Array Offset Uint8Array Offset Size (Eff eff Unit)

foreign import _fromArray :: forall eff. Array Octet -> Eff eff Uint8Array

foreign import _toArray :: forall eff. Fn.Fn3 Uint8Array Offset Size (Eff eff (Array Octet))

foreign import _intersperse :: forall eff. Fn.Fn5 Uint8Array Offset Uint8Array Offset Octet (Eff eff Unit)

foreign import _foldl :: forall a. Fn.Fn4 (a -> Octet -> a) a Offset Uint8Array a

foreign import _foldr :: forall a. Fn.Fn4 (Octet -> a -> a) a Offset Uint8Array a

foreign import _findSubstring :: Fn.Fn6 Uint8Array Offset Size Uint8Array Offset Size Int

foreign import assert :: forall a. Boolean -> a -> a

foreign import allocArrayBuffer :: forall eff. Int -> Eff eff ArrayBuffer

foreign import _newUint8Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Uint8Array)

foreign import _findLastIndex :: forall a. Fn.Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) (Int -> Boolean) (ArrayView a) (Maybe Int)

foreign import _subarray :: forall a eff. Fn.Fn3 ByteOffset ByteOffset (ArrayView a) (Eff eff (ArrayView a))

foreign import _arrayViewLen :: forall a eff. ArrayView a -> Eff eff Int

foreign import _setAtOffset :: forall a eff. Fn.Fn3 Int Int (ArrayView a) (Eff eff Unit)

foreign import _getAtOffset :: forall a eff. Fn.Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) ByteOffset (ArrayView a) (Eff eff (Maybe Int))

foreign import _unsafeGetAtOffset :: forall a eff. Fn.Fn2 ByteOffset (ArrayView a) (Eff eff Int)

foreign import _arrayViewCompare :: forall a. Fn.Fn2 (ArrayView a) (ArrayView a) Int

foreign import _nullPtr :: forall a. (ArrayView a)

foreign import _printArrayView :: forall a. ArrayView a -> String

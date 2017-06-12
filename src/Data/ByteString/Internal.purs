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
  , memmove
  , memchr
  , memcmp
  , memset
  , findSubstring

  , reverse
  , intersperse
  , foldlPtr
  , foldrPtr
  , concat

  , _isSpace
  , _isSpaceChar
  , assert
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Rec.Class (tailRecM2, tailRecM3, Step(..))

import Data.ArrayBuffer (allocArrayBuffer)
import Data.ArrayBuffer.TypedArray
  (Ptr(..), Uint8, Uint8Array, newUint8Array, length, nullPtr, poke, newPtr, plusPtr)
import Data.Function.Uncurried as Fn
import Data.List (List(Nil), (:))
import Data.List as L
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(..))

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | Type synonym indicating the value refers to a length of a buffer
type Size = Int

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
mallocByteString size = newPtr <$> (allocArrayBuffer size >>= newUint8Array 0 size)

fromBytesArray :: forall eff. Array Octet -> Eff eff (Tuple (Ptr Uint8) Size)
fromBytesArray xs = do
  pt <- _fromArray xs
  pure $ Tuple (newPtr pt) (length pt)

toBytesArray :: forall eff. Ptr Uint8 -> Offset -> Size -> (Eff eff (Array Octet))
toBytesArray (Ptr n av) ofs size = Fn.runFn3 _toArray av (ofs + n) size

memcpy :: forall eff. Ptr Uint8 -> Ptr Uint8 -> Size -> Eff eff Unit
memcpy (Ptr off1 av1) (Ptr off2 av2) n = Fn.runFn5 _memcpy av1 off1 av2 off2 n

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

findSubstring :: forall eff. ByteString -> ByteString -> Eff eff Int
findSubstring (ByteString (Ptr a av) n l) (ByteString (Ptr b bv) m j) =
  Fn.runFn6 _findSubstring av (a + n) l bv (b + m) j

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

foreign import _findSubstring :: forall eff. Fn.Fn6 Uint8Array Offset Size Uint8Array Offset Size (Eff eff Int)

foreign import assert :: forall a. Boolean -> a -> a

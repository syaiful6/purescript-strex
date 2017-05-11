module Data.ArrayBuffer.TypedArray
  ( TypedArrayProp
  , sizeOf
  , arrayBuffer
  , byteLength
  , byteOffset
  , length

  , setAtOffset
  , getAtOffset
  , set

  -- basic interface
  , copyWithin
  , slice
  , subarray
  , compareAV
  , printArrayView

  -- finding ix
  , elemIndex
  , elemIndexEnd
  , findIndex
  , findLastIndex
  --
  , newInt8Array
  , newInt16Array
  , newInt32Array
  , newUint8Array
  , newUint8ClampedArray
  , newUint16Array
  , newUint32Array

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

  , module Data.ArrayBuffer.Types
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.ArrayBuffer.Types
  ( ArrayBuffer, ByteOffset, ByteLength, ArrayView, Int8Array, Int16Array, Int32Array
  , Uint8Array, Uint16Array, Uint32Array, Uint8ClampedArray, Float32Array, Float64Array
  , Int8, Int16, Int32, Uint8, Uint16, Uint32, Uint8Clamped, Float32, Float64)

import Unsafe.Coerce (unsafeCoerce)

type TypedArrayProp =
  { buffer            :: ArrayBuffer
  , byteLength        :: ByteLength
  , byteOffset        :: ByteOffset
  , length            :: Int }

foreign import sizeOf :: forall a. ArrayView a -> Int

arrayBuffer :: forall a. ArrayView a -> ArrayBuffer
arrayBuffer = _.buffer <<< toTypedArrayProp

byteLength :: forall a. ArrayView a -> ByteLength
byteLength = _.byteLength <<< toTypedArrayProp

byteOffset :: forall a. ArrayView a -> ByteOffset
byteOffset = _.byteOffset <<< toTypedArrayProp

length :: forall a. ArrayView a -> Int
length = _.length <<< toTypedArrayProp

toTypedArrayProp :: forall a. ArrayView a -> TypedArrayProp
toTypedArrayProp = unsafeCoerce

getAtOffset
  :: forall a eff
   . Int
  -- ^ The offset to get the value from
  -> ArrayView a
  -- ^ The ArrayView we want to operate
  -> Eff eff (Maybe Int)
getAtOffset offset ta = Fn.runFn4 _getAtOffset Nothing Just offset ta

foreign import setAtOffset
  :: forall a eff
   . Int
  -- ^ The value we want to store
  -> Int
  -- ^ The offset of ArrayView we want to store the value
  -> ArrayView a
  -- ^ The ArrayView we want to operate
  -> Eff eff Unit

foreign import set
  :: forall eff a b
   . ArrayView a
  -- ^ The array from which to copy values.
  -> Int
  -- ^ The offset into the target array at which to begin writing values from the source array.
  -> ArrayView b
  -- ^ The target array
  -> Eff eff Unit

copyWithin
  :: forall a eff
   . ByteOffset
  -- ^ Target start index position where to copy the elements to.
  -> ByteOffset
  -- ^ Source start index position where to start copying elements from.
  -> ByteLength
  -- ^ Source end index position where to end copying elements from.
  -> ArrayView a
  -- ^ The TypedArray we want to modify
  -> Eff eff Unit
copyWithin target start end ta = Fn.runFn4 _copyWithin target start end ta

slice
  :: forall a eff
   . ByteOffset
  -- ^ Zero-based index at which to begin extraction.
  -> ByteOffset
  -- ^ Zero-based index before which to end extraction. slice extracts up to but not including end.
  -> ArrayView a
  -- ^ The target typedarray to slice
  -> Eff eff (ArrayView a)
slice start end ta = Fn.runFn3 _slice start end ta

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

compareAV :: forall a. ArrayView a -> ArrayView a -> Ordering
compareAV a b = compare 0 $ Fn.runFn2 _arrayViewCompare a b

elemIndex :: forall a. Int -> ArrayView a -> Maybe Int
elemIndex x = findIndex (_ == x)

elemIndexEnd :: forall a. Int -> ArrayView a -> Maybe Int
elemIndexEnd x = findLastIndex (_ == x)

findIndex :: forall a. (Int -> Boolean) -> ArrayView a -> Maybe Int
findIndex f xs = Fn.runFn4 _findIndex Nothing Just f xs

findLastIndex :: forall a. (Int -> Boolean) -> ArrayView a -> Maybe Int
findLastIndex f xs = Fn.runFn4 _findLastIndex Nothing Just f xs

foreign import printArrayView :: forall a. ArrayView a -> String

--------------------------------------------------------------------------------
-- Alloc new TypedArray --------------------------------------------------------
--------------------------------------------------------------------------------

newInt8Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Int8Array
newInt8Array ofs len ab = Fn.runFn3 _newInt8Array ofs len ab

newInt16Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Int16Array
newInt16Array ofs len ab = Fn.runFn3 _newInt16Array ofs len ab

newInt32Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Int32Array
newInt32Array ofs len ab = Fn.runFn3 _newInt32Array ofs len ab

newUint8Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Uint8Array
newUint8Array ofs len ab = Fn.runFn3 _newUint8Array ofs len ab

newUint16Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Uint16Array
newUint16Array ofs len ab = Fn.runFn3 _newUint16Array ofs len ab

newUint32Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Uint32Array
newUint32Array ofs len ab = Fn.runFn3 _newUint32Array ofs len ab

newUint8ClampedArray :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Uint8ClampedArray
newUint8ClampedArray ofs len ab = Fn.runFn3 _newUint8ClampedArray ofs len ab

newFloat32Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Float32Array
newFloat32Array ofs len ab = Fn.runFn3 _newFloat32Array ofs len ab

newFloat64Array :: forall eff. ByteOffset -> ByteLength -> ArrayBuffer -> Eff eff Float64Array
newFloat64Array ofs len ab = Fn.runFn3 _newFloat64Array ofs len ab

--------------------------------------------------------------------------------
-- Pointer ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | We encode pointer as pair of current offset and TypedArray
data Ptr a = Ptr ByteOffset (ArrayView a)

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
pokeByteOff (Ptr off av) n v =  setAtOffset v (off + n) av

peekByteOff :: forall a eff. Ptr a -> ByteOffset -> Eff eff (Maybe Int)
peekByteOff (Ptr off av) n = getAtOffset (off + n) av

unsafePeekByteOff :: forall a eff. Ptr a -> ByteOffset -> Eff eff Int
unsafePeekByteOff (Ptr off av) n = Fn.runFn2 _unsafeGetAtOffset (off + n) av

plusPtr :: forall a. Ptr a -> Int -> Ptr a
plusPtr (Ptr n av) m = Ptr (n + m) av

minusPtr :: forall a. Ptr a -> Ptr a -> Int
minusPtr (Ptr n _) (Ptr m _) = n - m

instance eqPtr :: Eq (Ptr a) where
  eq a b = EQ == compare a b

instance ordPtr :: Ord (Ptr a) where
  compare (Ptr a b) (Ptr c d)
    | a < c       = LT
    | a > c       = GT
    | otherwise   = compareAV b d

instance showPtr :: Show (Ptr a) where
  show (Ptr n v) = "(Ptr " <> show n <> " " <> printArrayView v <> " )"

--------------------------------------------------------------------------------
-- foreign ---------------------------------------------------------------------
--------------------------------------------------------------------------------

foreign import _newInt8Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Int8Array)

foreign import _newInt16Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Int16Array)

foreign import _newInt32Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Int32Array)

foreign import _newUint8Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Uint8Array)

foreign import _newUint16Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Uint16Array)

foreign import _newUint32Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Uint32Array)

foreign import _newUint8ClampedArray :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Uint8ClampedArray)

foreign import _newFloat32Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Float32Array)

foreign import _newFloat64Array :: forall eff. Fn.Fn3 ByteOffset ByteLength ArrayBuffer (Eff eff Float64Array)

foreign import _slice :: forall a eff. Fn.Fn3 ByteOffset ByteOffset (ArrayView a) (Eff eff (ArrayView a))

foreign import _subarray :: forall a eff. Fn.Fn3 ByteOffset ByteOffset (ArrayView a) (Eff eff (ArrayView a))

foreign import _copyWithin :: forall a eff. Fn.Fn4 ByteOffset ByteOffset ByteLength (ArrayView a) (Eff eff Unit)

foreign import _getAtOffset :: forall a eff. Fn.Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) ByteOffset (ArrayView a) (Eff eff (Maybe Int))

foreign import _unsafeGetAtOffset :: forall a eff. Fn.Fn2 ByteOffset (ArrayView a) (Eff eff Int)

foreign import _findIndex :: forall a. Fn.Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) (Int -> Boolean) (ArrayView a) (Maybe Int)

foreign import _findLastIndex :: forall a. Fn.Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) (Int -> Boolean) (ArrayView a) (Maybe Int)

foreign import _arrayViewCompare :: forall a. Fn.Fn2 (ArrayView a) (ArrayView a) Int

foreign import _nullPtr :: forall a. (ArrayView a)

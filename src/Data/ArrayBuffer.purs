module Data.ArrayBuffer
  ( allocArrayBuffer
  , byteLength
  , unsafeTransfer
  , slice
  , module Data.ArrayBuffer.Types
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

import Unsafe.Coerce (unsafeCoerce)

-- | creates a new ArrayBuffer of the given length in bytes.
foreign import allocArrayBuffer :: forall eff. Int -> Eff eff ArrayBuffer

-- | Returns the length (in bytes) of the ArrayBuffer. Fixed at construction time
byteLength :: ArrayBuffer -> ByteLength
byteLength = _.byteLength <<< unsafeCoerce

-- | This function considered unsafe because the array buffer passed here will be
-- | in detached state. It returns a new ArrayBuffer whose contents
-- | have been taken from the oldBuffer's data and then is either truncated or
-- | zero-extended by newByteLength. The ability to grow an ArrayBuffer without copying
-- | has the advantage of being much faster for large buffers (similar to realloc)
-- | The ability to detach an ArrayBuffer gives the developer explicit control over
-- | when the underlying memory is released.
unsafeTransfer
  :: forall eff
   . ByteLength
  -- ^ The byte length of the new ArrayBuffer object
  -> ArrayBuffer
  -- ^ An ArrayBuffer from which to transfer, this buffer will be in detached state
  -> Eff eff ArrayBuffer
unsafeTransfer bl ab = runFn2 _transferArrayBuffer bl ab

-- | returns a new ArrayBuffer whose contents are a copy of this ArrayBuffer's
-- | bytes from begin, inclusive, up to end, exclusive.
slice
  :: forall eff
   . ByteOffset
  -- ^ Zero-based byte index at which to begin slicing.
  -> ByteOffset
  -- ^ Byte index before which to end slicing
  -> ArrayBuffer
  -- ^ The ArrayBuffer to slice
  -> Eff eff ArrayBuffer
slice start end ab = runFn3 _slice start end ab

foreign import _transferArrayBuffer :: forall eff. Fn2 Int ArrayBuffer (Eff eff ArrayBuffer)

foreign import _slice :: forall eff. Fn3 Int Int ArrayBuffer (Eff eff ArrayBuffer)

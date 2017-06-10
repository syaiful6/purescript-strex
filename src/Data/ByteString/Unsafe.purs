module Data.ByteString.Unsafe
  ( unsafeHead
  , unsafeTail
  , unsafeInit
  , unsafeLast
  , unsafeIndex
  , unsafeTake
  , unsafeDrop
  ) where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ArrayBuffer.TypedArray (unsafePeekByteOff)
import Data.ByteString.Internal (ByteString(..), Octet, assert)


unsafeHead :: ByteString -> Octet
unsafeHead (ByteString p s l) = assert (l > 0) $
  unsafePerformEff $ unsafePeekByteOff p s

unsafeTail :: ByteString -> ByteString
unsafeTail (ByteString p s l) = assert (l > 0) $ ByteString p (s + 1) (l - 1)

unsafeInit :: ByteString -> ByteString
unsafeInit (ByteString p s l) = assert (l > 0) $ ByteString p s (l - 1)

unsafeLast :: ByteString -> Octet
unsafeLast (ByteString p s l) = assert (l > 0) $
  unsafePerformEff $ unsafePeekByteOff p ( s + l - 1)

unsafeIndex :: ByteString -> Int -> Octet
unsafeIndex (ByteString p s l) i = assert (i >= 0 && i < l) $
  unsafePerformEff $ unsafePeekByteOff p ( s + i )

unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (ByteString p s l) = assert (0 <= n && n <= l) $ ByteString p s n

unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (ByteString p s l) = assert (0 <= n && n <= l) $ ByteString p (s + n) (l - n)

module Data.ByteString.Internal
  ( ByteString(..), Octet, Offset, Size
  , unsafeCreate, unsafeFromArray, unsafeToBuffer, unsafeBufferSlice
  , create, fromArray, fromString, toBuffer
  , setAtOffset, bufferSlice, bufferCompare, copyBuffer, emptyBuf
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.Function.Uncurried as Fn
import Data.Monoid (class Monoid)

import Node.Buffer (BUFFER, Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)

import Unsafe.Coerce (unsafeCoerce)

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | Type synonym indicating the value refers to a length of a buffer
type Size = Int

-- |
data ByteString = ByteString Buffer Offset Size

instance eqByteString :: Eq ByteString where
  eq = eqBS

instance ordByteString :: Ord ByteString where
  compare = compareBS

instance semigroupByString :: Semigroup ByteString where
  append = appendBS

instance monoidByString :: Monoid ByteString where
  mempty = ByteString emptyBuf 0 0

instance showByteString :: Show ByteString where
  show s = "(Buffer unpack " <> show (unsafePerformEff $ Buffer.toArray $ unsafeToBuffer s) <> " )"

eqBS :: ByteString -> ByteString -> Boolean
eqBS a@(ByteString bs off len) b@(ByteString bs' off' len')
  | len /= len'                = false
  | otherwise                  = EQ == compareBS a b

compareBS :: ByteString -> ByteString -> Ordering
compareBS (ByteString _ _ 0) (ByteString _ _ 0)               = EQ
compareBS (ByteString bs1 of1 len1) (ByteString bs2 of2 len2) =
  compare 0 $ unsafePerformEff do
    bs1' <- Fn.runFn3 bufferSlice of1 len1 bs1
    bs2' <- Fn.runFn3 bufferSlice of2 len2 bs2
    Fn.runFn2 bufferCompare bs1' bs2'

appendBS :: ByteString -> ByteString -> ByteString
appendBS (ByteString _ _ 0) b                              = b
appendBS a                  (ByteString _ _ 0)             = a
appendBS (ByteString b1 of1 len1) (ByteString b2 of2 len2) =
  unsafeCreate (len1+len2) \buf -> do
    _ <- Fn.runFn5 copyBuffer of1 len1 b1 0 buf
    _ <- Fn.runFn5 copyBuffer of2 len2 b2 (len1 - of1) buf
    pure unit

unsafeCreate :: forall e. Size -> (Buffer -> Eff (buffer :: BUFFER | e) Unit) -> ByteString
unsafeCreate s f = unsafePerformEff $ create s f

unsafeFromArray :: Array Octet -> ByteString
unsafeFromArray = unsafePerformEff <<< fromArray

-- | convert a ByteString to Buffer
unsafeToBuffer :: ByteString -> Buffer
unsafeToBuffer = unsafePerformEff <<< toBuffer

unsafeBufferSlice :: Int -> Int -> Buffer -> Buffer
unsafeBufferSlice off len buf = unsafePerformEff $ Fn.runFn3 bufferSlice off len buf

create :: forall e. Size -> (Buffer -> Eff (buffer :: BUFFER | e) Unit) -> Eff (buffer :: BUFFER | e) ByteString
create size f = do
  buf <- Buffer.create size
  _ <- f buf
  pure $ ByteString buf 0 size

toBuffer :: forall e. ByteString -> Eff (buffer :: BUFFER | e) Buffer
toBuffer (ByteString src ofs len) = Fn.runFn3 bufferSlice ofs len src

fromArray :: forall e. Array Octet -> Eff (buffer :: BUFFER | e) ByteString
fromArray xs = do
  buf <- Buffer.fromArray xs
  size <- Buffer.size buf
  pure $ ByteString buf 0 size

fromString ::forall e. String -> Encoding -> Eff (buffer :: BUFFER | e) ByteString
fromString s enc = do
  buf <- Buffer.fromString s enc
  size <- Buffer.size buf
  pure $ ByteString buf 0 size

foreign import setAtOffset :: forall e. Fn.Fn3 Offset Octet Buffer (Eff (buffer :: BUFFER | e) Unit)

foreign import bufferCompare :: forall e. Fn.Fn2 Buffer Buffer (Eff (buffer :: BUFFER | e) Int)

foreign import bufferSlice :: forall e. Fn.Fn3 Int Int Buffer (Eff (buffer :: BUFFER | e) Buffer)

foreign import copyBuffer :: forall e. Fn.Fn5 Offset Offset Buffer Offset Buffer (Eff (buffer :: BUFFER | e) Int)

foreign import emptyBuf :: Buffer

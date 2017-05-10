module Data.ByteString where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString.Internal as B
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))

import Node.Buffer as Buffer
import Node.Encoding (Encoding)


-- | The empty 'ByteString'
empty :: B.ByteString
empty = B.ByteString B.emptyBuf 0 0

-- | A byte string with a single byte.
-- |
-- | Running time: `O(1)`
singleton :: B.Octet -> B.ByteString
singleton s = B.unsafeCreate 1 \b -> Fn.runFn3 B.setAtOffset 0 s b

-- | Convert an array of bytes to byte string
-- |
-- | Running time: `O(n)`
pack :: Array B.Octet -> B.ByteString
pack = B.unsafeFromArray

-- | Converts a 'ByteString' to an array of bytes
-- |
-- | Running time: `O(n)`
unpack :: B.ByteString -> Array B.Octet
unpack = unsafePerformEff <<< (Buffer.toArray <=< B.toBuffer)

-- | Test whether a ByteString is empty.
-- |
-- | Running time: `O(1)`
null :: B.ByteString -> Boolean
null (B.ByteString _ _ i) = i <= 0

-- | 'length' returns the length of a ByteString as an 'Int'
-- |
-- | Running time: `O(1)`
length :: B.ByteString -> Int
length (B.ByteString _ _ le) = le

-- | 'cons' is analogous to (:) for lists, but have different complexity
-- |
-- | Running time: `O(n)`
cons :: B.Octet -> B.ByteString -> B.ByteString
cons c (B.ByteString buf ofs len) = B.unsafeCreate (len + 1) \buf' -> do
  _ <- Buffer.write Buffer.UInt8 c 0 buf'
  _ <- Fn.runFn5 B.copyBuffer ofs len buf 1 buf'
  pure unit

-- | Append a byte to the end of a 'ByteString'
-- |
-- | Running time: `O(n)`
snoc :: B.ByteString -> B.Octet -> B.ByteString
snoc (B.ByteString buf ofs len) c = B.unsafeCreate (len + 1) \buf' -> do
  _ <- Fn.runFn5 B.copyBuffer ofs len buf 0 buf'
  _ <- Buffer.write Buffer.UInt8 c 0 buf'
  pure unit

-- | Extract the first element of a ByteString, if the ByteString is empty then
-- | it return Nothing, `Just Octed`
-- |
-- | Running time: `O(1)`
head :: B.ByteString -> Maybe B.Octet
head (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ unsafePerformEff $ Buffer.read Buffer.UInt8 s x

-- | Extract the elements after the head of a ByteString
-- |
-- | Running time: `O(n)`
tail :: B.ByteString -> Maybe B.ByteString
tail (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ B.ByteString x (s + 1) (l - 1)

-- | Extract the head and tail of a ByteString, returning Nothing if it is empty.
-- |
-- | Running time: `O(n)`
uncons :: B.ByteString -> Maybe { head :: B.Octet, tail :: B.ByteString }
uncons (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff do
  	  h <- Buffer.read Buffer.UInt8 s x
  	  pure $ Just $ { head: h, tail: B.ByteString x (s + 1) (l - 1) }

-- | Extract the last element of a ByteString
last :: B.ByteString -> Maybe B.Octet
last (B.ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ unsafePerformEff $ Buffer.read Buffer.UInt8 (s + l - 1) x

-- | Return all the elements of a 'ByteString' except the last one.
init :: B.ByteString -> Maybe B.ByteString
init bs@(B.ByteString x s l)
  | null bs   = Nothing
  | otherwise = Just $ B.ByteString x s (l - 1)

unsnoc :: B.ByteString -> Maybe { init :: B.ByteString, last :: B.Octet }
unsnoc bs@(B.ByteString x s l)
  | null bs    = Nothing
  | otherwise  = unsafePerformEff do
      lst <- Buffer.read Buffer.UInt8 (s + l - 1) x
      pure $ Just $ { init: B.ByteString x s (l - 1), last: lst }

fromString :: String -> Encoding -> B.ByteString
fromString s = unsafePerformEff <<< B.fromString s

toString :: B.ByteString -> Encoding -> String
toString bs enc = unsafePerformEff $ Buffer.toString enc =<< B.toBuffer bs

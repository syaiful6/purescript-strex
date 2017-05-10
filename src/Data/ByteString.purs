module Data.ByteString where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString.Internal as B
import Data.Function.Uncurried as Fn

import Node.Buffer as Buffer
import Node.Encoding (Encoding)

empty :: B.ByteString
empty = B.ByteString B.emptyBuf 0 0

singleton :: B.Octet -> B.ByteString
singleton s = B.unsafeCreate 1 \b -> Fn.runFn3 B.setAtOffset 0 s b

pack :: Array B.Octet -> B.ByteString
pack = B.unsafeFromArray

unpack :: B.ByteString -> Array B.Octet
unpack = unsafePerformEff <<< (Buffer.toArray <=< B.toBuffer)

null :: B.ByteString -> Boolean
null (B.ByteString _ _ i) = i <= 0

length :: B.ByteString -> Int
length (B.ByteString _ _ le) = le

fromString :: String -> Encoding -> B.ByteString
fromString s = unsafePerformEff <<< B.fromString s

toString :: B.ByteString -> Encoding -> String
toString bs enc = unsafePerformEff $ Buffer.toString enc =<< B.toBuffer bs

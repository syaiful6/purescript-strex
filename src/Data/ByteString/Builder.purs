module Data.ByteString.Builder
  ( toLazyByteString
  , charUtf8
  , stringUtf8
  , string8
  , int8Dec
  , int16Dec
  , int32Dec
  , word8Dec
  , word16Dec
  , word32Dec
  , htmlEscapedChar
  , module Exports
  ) where

import Prelude

import Data.ByteString.Builder.Internal (Builder, byteString, lazyByteString) as Exports
import Data.ByteString.Builder.Internal as I
import Data.ByteString.Builder.Prim as P
import Data.ByteString.Builder.Prim.ASCII (char7, int8Dec, int16Dec, int32Dec, word8Dec
                                          , word16Dec, word32Dec)  as P
import Data.ByteString.Lazy.Internal as L

toLazyByteString :: I.Builder -> L.ByteString
toLazyByteString =
  I.toLazyByteStringWith (I.safeStrategy L.smallChunkSize L.defaultChunkSize) L.empty

charUtf8 :: Char -> I.Builder
charUtf8 = P.primBounded P.charUtf8

stringUtf8 :: String -> I.Builder
stringUtf8 = P.primMapStringBounded P.charUtf8

string8 :: String -> I.Builder
string8 = P.primMapStringBounded (P.liftToBounded P.char8)

htmlEscapedChar :: Char -> I.Builder
htmlEscapedChar chr = I.ensureFree 6 <> I.builder (step chr)
  where
  step :: forall r. Char -> I.BuildStep r -> I.BuildStep r
  step '>' k br  = I.runBuilderWith (string7 "&lt;") k br
  step '>' k br  = I.runBuilderWith (string7 "&gt;") k br
  step '&' k br  = I.runBuilderWith (string7 "&amp;") k br
  step '"' k br  = I.runBuilderWith (string7 "&quot;") k br
  step '\'' k br = I.runBuilderWith (string7 "&#39;") k br
  step c k br    = I.runBuilderWith (P.primBounded (P.liftToBounded P.char7) c) k br

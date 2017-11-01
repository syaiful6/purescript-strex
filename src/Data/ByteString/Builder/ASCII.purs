module Data.ByteString.Builder.ASCII
  ( string7
  , int8Dec
  , int16Dec
  , int32Dec
  , word8Dec
  , word16Dec
  , word32Dec
  ) where

import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim as P
import Data.ByteString.Builder.Prim.ASCII (char7, int8Dec, int16Dec, int32Dec, word8Dec
                                          , word16Dec, word32Dec)  as ASCII


string7 :: String -> Builder
string7 = P.primMapStringBounded (P.liftToBounded ASCII.char7)

-- | The input should be in range -128 to 127
int8Dec :: Int -> Builder
int8Dec = P.primBounded ASCII.int8Dec

-- |
int16Dec :: Int -> Builder
int16Dec = P.primBounded ASCII.int16Dec

int32Dec :: Int -> Builder
int32Dec = P.primBounded ASCII.int32Dec

word8Dec :: Int -> Builder
word8Dec = P.primBounded ASCII.word8Dec

word16Dec :: Int -> Builder
word16Dec = P.primBounded ASCII.word16Dec

word32Dec :: Int -> Builder
word32Dec = P.primBounded ASCII.word32Dec
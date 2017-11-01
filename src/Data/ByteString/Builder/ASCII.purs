module Data.ByteString.Builder.ASCII
  ( string7
  , int8Dec
  , int16Dec
  , int32Dec
  , word8Dec
  , word16Dec
  , word32Dec
  ) where

import Prelude

import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim as P
import Data.ByteString.Builder.Prim.ASCII (char7, int8Dec, int16Dec, int32Dec, word8Dec
                                          , word16Dec, word32Dec)  as ASCII


string7 :: String -> Builder
string7 = P.primMapStringBounded (P.liftToBounded P.char7)

-- | The input should be in range -128 to 127
int8Dec :: BoundedPrim Int
int8Dec = P.primBounded int8Dec

-- |
int16Dec :: BoundedPrim Int
int16Dec = P.primBounded ASCII.int16Dec

int32Dec :: BoundedPrim Int
int32Dec = P.primBounded ASCII.int32Dec

word8Dec :: BoundedPrim Int
word8Dec = P.primBounded ASCII.word8Dec

word16Dec :: BoundedPrim Int
word16Dec = P.primBounded ASCII.word16Dec

word32Dec :: BoundedPrim Int
word32Dec = P.primBounded ASCII.word32Dec
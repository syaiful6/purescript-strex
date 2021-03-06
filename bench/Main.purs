module Bench.Main where

import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.Array as A
import Data.ByteString as B
import Data.ByteString.Builder.Internal (refinedEff)
import Data.ByteString.Builder.Prim as P
import Data.ByteString.Internal (mallocByteString)
import Data.String (length) as S
import Data.String.Unsafe (charAt) as S

import Performance.Minibench (bench)

benchIntPrim :: forall eff. String -> Int -> P.BoundedPrim Int -> Eff (console :: CONSOLE | eff) Unit
benchIntPrim name n bp
  | n <= 0    = pure unit
  | otherwise = do
      ptr <- mallocByteString (n * P.sizeBound bp)
      log ("Running " <> name)
      bench \_ -> unsafePerformEff (forE 0 n \n' -> refinedEff (void $ P.runBP bp n' ptr))

benchChrPrim :: forall eff. String -> String -> P.BoundedPrim Char -> Eff (console :: CONSOLE | eff) Unit
benchChrPrim name str bp = case S.length str of
  len | len <= 0  -> pure unit
      | otherwise -> do
          ptr <- mallocByteString (len * P.sizeBound bp)
          log ("Running " <> name)
          bench \_ -> unsafePerformEff (forE 0 len \i -> refinedEff (void $ P.runBP bp (S.charAt i str) ptr))

benchByteStringStrict :: forall eff. Eff (console :: CONSOLE | eff) Unit
benchByteStringStrict = do
  log "Bench pack"
  log "---------------\n"
  let packed = A.range 0 255
  bench \_ -> B.pack packed

  log "Benc Builder Primitive"
  log "---------------\n"
  benchIntPrim "uint8BE" 100 (P.liftToBounded P.uint8BE)
  log "---------------\n"
  benchIntPrim "int8BE" 100 (P.liftToBounded P.int8BE)
  log "---------------\n"
  benchChrPrim "char7" "Purescript ~ Aku isa mangan beling tanpa lara" (P.liftToBounded P.char7)
  log "---------------\n"
  benchChrPrim "charUtf8" "Purescript ~ ﻿काचं शक्नोम्यत्तुम् । नोपहिनस्ति माम् ॥" P.charUtf8

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
  benchByteStringStrict

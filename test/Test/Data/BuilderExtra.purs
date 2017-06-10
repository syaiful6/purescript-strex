module Test.Data.BuilderExtra where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Builder.Extra
  (BufferAllocStrategy, newByteStringBuilderRecv, allNewBuffersStrategy, refinedEff)
import Data.ByteString.Builder.Internal as BI
import Data.ByteString as S
import Data.ByteString.Lazy as L
import Data.Foldable (fold)
import Data.Tuple (Tuple(..))
import Data.List.Lazy as LZ
import Data.Maybe (maybe)

import Test.Unit.Assert (shouldEqual)
import Test.Unit (TestSuite, describe, it)


type TestEff eff = TestSuite
  ( console :: CONSOLE
  , random :: RANDOM
  , exception :: EXCEPTION
  | eff
  )

defaultStrategy :: BufferAllocStrategy
defaultStrategy = allNewBuffersStrategy (32 * 1024 - 16)

tester :: forall e. BufferAllocStrategy -> LZ.List Builder -> Eff e (LZ.List S.ByteString)
tester strat builders0 = do
  Tuple recv finish <- refinedEff $ newByteStringBuilderRecv strat
  let
    loop front xs0 = case LZ.step xs0 of
      LZ.Nil -> do
        mbs <- refinedEff finish
        pure $ front $ maybe LZ.nil pure mbs
      LZ.Cons bu bus -> do
        popper <- refinedEff $ recv bu
        let
          go front1 = do
            bs <- refinedEff $ popper
            if S.null bs then loop front1 bus else go (front1 <<< (bs `LZ.cons` _))
        go front
  loop id builders0

mainEx :: forall eff. TestEff eff
mainEx = do
  describe "ByteString Builder Extra" do
    it "indempontent with toLazyByteString" do
      let bss'     = [[0x74, 0xc3, 0xa9, 0x73, 0x74, 0x65], [0x64, 0x67, 0x55]]
          bss      = map S.pack bss'
          builders = map BI.byteString bss
          lbs      = toLazyByteString $ fold builders
      outBss <- liftEff $ tester defaultStrategy (LZ.fromFoldable builders)
      L.fromChunks outBss `shouldEqual` lbs

    it "works for strict bytestring insertion" do
      let
        bs = L.fromStrict $ S.pack [0x74, 0xc3, 0xa9, 0x73, 0x74, 0x65]
        builders = LZ.replicate 500 (BI.lazyByteStringInsert bs)
        lbs = toLazyByteString $ fold builders
      outBss <- liftEff $ tester defaultStrategy builders
      L.fromChunks outBss `shouldEqual` lbs

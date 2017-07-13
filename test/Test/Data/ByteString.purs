module Test.Data.ByteString where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))

import Test.Unit.Assert (shouldEqual)
import Test.Unit (TestSuite, describe, it)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf, sized)

import Partial.Unsafe (unsafePartial)

import Data.ByteString.Internal as BI
import Data.ByteString as B


type TestEff eff = TestSuite
  ( console :: CONSOLE
  , random :: RANDOM
  , exception :: EXCEPTION
  | eff
  )

newtype ByteStringTest = ByteStringTest BI.ByteString

derive instance newtypeByteStringTest :: Newtype ByteStringTest _
derive newtype instance eqByteStringTest :: Eq ByteStringTest
derive newtype instance ordByteStringTest :: Ord ByteStringTest

instance arbByteStringTest :: Arbitrary ByteStringTest where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- chooseInt 0 2
    pure $ ByteStringTest $ B.drop n bs

newtype OctetTest = OctetTest Int

derive instance newtypeOctetTest :: Newtype OctetTest _
derive newtype instance eqByteOctetTest :: Eq OctetTest
derive newtype instance ordByteOctetTest :: Ord OctetTest

instance arbOctetTest :: Arbitrary OctetTest where
  arbitrary = OctetTest <$> chooseInt 0 255

sizedByteString :: Int -> Gen B.ByteString
sizedByteString n = do
  m <- chooseInt 0 n
  map B.pack $ vectorOf m (chooseInt 0 255)

unOctetTest :: OctetTest -> Int
unOctetTest = unwrap

test_propertyIntersperse :: ByteStringTest -> Boolean
test_propertyIntersperse (ByteStringTest bts) =
  let sr = B.intersperse 0x2c bts
  in if B.length bts < 2
    then sr == bts
    else B.length sr == (2 * (B.length bts) - 1) && sr /= bts

test_propertyTakeByteString :: Int -> ByteStringTest -> Boolean
test_propertyTakeByteString n (ByteStringTest b)
  | n <= 0           = true
  | n >= B.length b  = b == B.take n b
  | otherwise        = B.length (B.take n b) == n

test_propertyBreakSubstring :: ByteStringTest -> OctetTest -> Boolean
test_propertyBreakSubstring (ByteStringTest bs) (OctetTest c) =
  B.break (_ == c) bs `eqRec` B.breakSubstring (B.singleton c) bs
  where
    eqRec s u = s.after == u.after && s.before == u.before

mainBS :: forall eff. TestEff eff
mainBS = do
  describe "Data.ByteString" do
    it "semigroup instance" do
      let bs1 = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          bs2 = B.singleton 32
          bs3 = B.singleton 98
          concatenated = bs1 <> bs2 <> bs3
      (bs1 <> (bs2 <> bs3)) `shouldEqual` ((bs1 <> bs2) <> bs3)

    it "correctly combine init & tail" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55]
          xs = unsafePartial $ fromJust $ B.tail bs
          ys = unsafePartial $ fromJust $ B.init xs
      ys `shouldEqual` (B.pack [0x72, 0x64, 0x67])
      (xs <> ys) `shouldEqual` B.pack [0x72, 0x64, 0x67, 0x55, 0x72, 0x64, 0x67]

    it "tail operation correctly extract the elements after the head" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55]
          xs = unsafePartial $ fromJust $ B.tail bs
          ys = unsafePartial $ fromJust $ B.tail xs
      xs `shouldEqual` (B.pack [0x72, 0x64, 0x67, 0x55])
      ys `shouldEqual` (B.pack [0x64, 0x67, 0x55])
      B.index xs 0 `shouldEqual` Just 0x72
      B.index ys 0 `shouldEqual` Just 0x64
      B.length xs `shouldEqual` 4

    it "elemIndex return first index of elem" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55, 0x64, 0x65]
          ix = B.elemIndex 0x64 bs
      ix `shouldEqual` Just 2

    it "elemIndexEnd return last index of elem" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55, 0x64, 0x65]
          ix = B.elemIndexEnd 0x64 bs
      ix `shouldEqual` Just 5

    it "findIndex return first index of elem" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55, 0x64, 0x65]
          ix = B.findIndex (_ == 0x64) bs
      ix `shouldEqual` Just 2

    it "findIndex return Nothing if predicate return false for all elem" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55, 0x64, 0x65]
          ix = B.findIndex (const false) bs
      ix `shouldEqual` Nothing

    it "snoc Append a byte to the end of a 'ByteString'" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55]
      B.snoc bs 0x2c `shouldEqual` B.pack [0x74, 0x72, 0x64, 0x67, 0x55, 0x2c]

    it "unfoldr should maintain order" do
      B.unfoldr stepUnfold 1 `shouldEqual` B.pack (A.range 1 5)

    it "Elem index" do
      let bs = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          xs = unsafePartial $ fromJust $ B.tail bs
      B.elemIndex 0xa9 xs `shouldEqual` Just 1

    it "isPrefixOf correctly implemented" do
      let bs = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          xs = B.take 3 bs
      B.isPrefixOf xs bs `shouldEqual` true

    it "can string prefix ByteString" do
      let bs = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          xs = B.take 3 bs
          ys = B.pack [0x73, 0xc3, 0xa9]
      B.stripPrefix xs bs `shouldEqual` Just (B.pack [0x73, 0x74])
      B.stripPrefix ys bs `shouldEqual` Nothing

    it "intersperse" do
      let bs = B.pack [0x74, 0x72, 0x64, 0x67, 0x55]
          is = B.intersperse 0x2c bs
      is `shouldEqual` B.pack [0x74, 0x2c, 0x72, 0x2c, 0x64, 0x2c, 0x67, 0x2c, 0x55]

    it "Break Substring" do
      let bs = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74, 0x65]
          xs = B.take 3 $ B.drop 2 bs
          pair = B.breakSubstring xs bs
      pair.before `shouldEqual` B.take 2 bs
      pair.after `shouldEqual` B.drop 2 bs

    it "property unpack, pack" do
      quickCheck \b ->
        let c = unOctetTest <$> b
        in B.unpack (B.pack c) == c
      quickCheck \(ByteStringTest b) -> B.pack (B.unpack b) == b

    it "property index" do
      quickCheck test_propertyTakeByteString

    it "property breakSubstring" do
      quickCheck test_propertyBreakSubstring

    it "property reverse" do
      quickCheck \(ByteStringTest b) -> B.reverse (B.reverse b) == b

    it "property intersperse" do
      quickCheck test_propertyIntersperse

stepUnfold :: Int -> Maybe (Tuple Int Int)
stepUnfold 6 = Nothing
stepUnfold n = Just (Tuple n (n + 1))

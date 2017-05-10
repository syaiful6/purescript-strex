module Test.Data.ByteString where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Newtype (class Newtype)

import Test.Unit.Assert (shouldEqual)
import Test.Unit (TestSuite, describe, it)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Data.AlphaNumString (AlphaNumString(..))

import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))

import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Data.ByteString.Internal as BI
import Data.ByteString as B

newtype ByteStringTest = ByteStringTest BI.ByteString

derive instance newtypeByteStringTest :: Newtype ByteStringTest _
derive newtype instance eqByteStringTest :: Eq ByteStringTest
derive newtype instance ordByteStringTest :: Ord ByteStringTest

instance arbByteStringTest :: Arbitrary ByteStringTest where
  arbitrary = ByteStringTest <<< B.fromString `flip` UTF8 <$> arbitrary

test_propertyLength :: ByteStringTest -> Boolean
test_propertyLength (ByteStringTest bts) =
  B.length bts == (unsafePerformEff (Buffer.size (BI.unsafeToBuffer bts)))

mainBS :: forall eff. TestSuite (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | eff)
mainBS = do
  describe "Data.ByteString" do
    it "semigroup instance" do
      let bs1 = B.pack [0x74, 0xc3, 0xa9, 0x73, 0x74]
          bs2 = B.singleton 32
          bs3 = B.singleton 98
          concatenated = bs1 <> bs2 <> bs3
      B.toString concatenated UTF8  `shouldEqual` "tést b"
      (bs1 <> (bs2 <> bs3)) `shouldEqual` ((bs1 <> bs2) <> bs3)

    it "property length" do
      liftEff $ quickCheck test_propertyLength

    it "property from string" do
      -- | todo seed all possible chars on UTF-8
      liftEff $ quickCheck \(AlphaNumString s) ->
        s == B.toString (B.fromString s UTF8) UTF8

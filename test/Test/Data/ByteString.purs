module Test.Data.ByteString where

import Prelude

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Data.Array as A
import Data.Char (fromCharCode)
import Data.Int (toNumber)
import Data.List.Lazy as LZ
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Data.Foldable as F

import Test.Unit.Assert (shouldEqual)
import Test.Unit (TestSuite, describe, it)
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (class Testable, Result(Success), test)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf, sized, perturbGen)

import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Data.ByteString.Internal as BI
import Data.ByteString as B
import Data.ByteString.Lazy as BL


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

newtype LazyByteStringTest = LazyByteStringTest BL.ByteString
derive instance newtypeLazyByteStringTest :: Newtype LazyByteStringTest _
derive newtype instance eqLazyByteStringTest :: Eq LazyByteStringTest
derive newtype instance ordLazyByteStringTest :: Ord LazyByteStringTest

instance arbLazyByteStringTest :: Arbitrary LazyByteStringTest where
  arbitrary = LazyByteStringTest <$> sized \n -> do
    numChunks <- chooseInt 0 n
    if numChunks == 0
      then pure BL.empty
      else
        map
          (BL.fromChunks <<< LZ.fromFoldable <<< A.filter (not <<< B.null)) $
          vectorOf numChunks (sizedByteString (n `div` numChunks))

newtype OctetTest = OctetTest Int

derive instance newtypeOctetTest :: Newtype OctetTest _
derive newtype instance eqByteOctetTest :: Eq OctetTest
derive newtype instance ordByteOctetTest :: Ord OctetTest

instance arbOctetTest :: Arbitrary OctetTest where
  arbitrary = OctetTest <$> chooseInt 0 255

instance coarbOctetTest :: Coarbitrary OctetTest where
  coarbitrary = perturbGen <<< toNumber <<< unOctetTest

class TestModel a b where
  testModel :: a -> b

instance testModelByStringLazy :: TestModel BL.ByteString B.ByteString where
  testModel ts = F.fold (BL.toChunks ts)

instance testModelByStringArrOcted :: TestModel B.ByteString (Array OctetTest) where
  testModel ts = unsafeCoerce (B.unpack ts) :: (Array OctetTest)

instance testModelByStringArrInt :: TestModel B.ByteString (Array Int) where
  testModel = B.unpack

instance testModelByteStringChars :: TestModel B.ByteString (Array Char) where
  testModel ts = map fromCharCode $ B.unpack ts

instance octetOctetTestModel :: TestModel OctetTest OctetTest where
  testModel = id

instance intintTestModel :: TestModel Int Int where
  testModel = id

instance charCharTestModel :: TestModel Char Char where
  testModel = id

instance boolBoolTestModel :: TestModel Boolean Boolean where
  testModel = id

class (Functor f, Functor g) <= NatTrans f g where
    eta :: forall a. f a -> g a

-- The transformation of the same type is identity
instance natTransArray2 :: NatTrans Array Array where eta = id
instance natTransIntInt :: NatTrans ((->) Int) ((->) Int) where eta = id
instance natTransCharChar :: NatTrans ((->) Char) ((->) Char) where eta = id
instance natTransOctetTest :: NatTrans ((->) OctetTest) ((->) OctetTest) where eta = id
instance natTransMaybeMaybe :: NatTrans Maybe Maybe where eta = id

instance natTransTupleTuple :: TestModel f g => NatTrans (Tuple f) (Tuple g) where
  eta (Tuple f a) = Tuple (testModel f) a

instance natTransModel :: (NatTrans m n, TestModel a b) => TestModel (m a) (n b) where
  testModel x = map testModel (eta x)

propertyIf :: forall a. Testable a => Boolean -> a -> Gen Result
propertyIf false _    = pure Success
propertyIf true a     = test a

infix 2 propertyIf as ==>

eq1 :: forall a b c d. Eq a => TestModel b a => TestModel c d => (c -> b) -> (d -> a) -> c -> Boolean
eq1 f g = \a ->
  testModel (f a) == g (testModel a)

eq2
  :: forall a b c d e f
   . Eq a => TestModel b a => TestModel c d => TestModel e f
  => (c -> e -> b)
  -> (d -> f -> a)
  -> c
  -> e
  -> Boolean
eq2 f g = \a b       ->
  testModel (f a b)       == g (testModel a) (testModel b)

eq3
  :: forall a b c d e f g h
   . Eq a => TestModel b a => TestModel c d => TestModel e f => TestModel g h
  => (c -> e -> g -> b)
  -> (d -> f -> h -> a)
  -> c
  -> e
  -> g
  -> Boolean
eq3 f g = \a b c     ->
  testModel (f a b c)     == g (testModel a) (testModel b) (testModel c)

sizedByteString :: Int -> Gen B.ByteString
sizedByteString n = do
  m <- chooseInt 0 n
  map B.pack $ vectorOf m (chooseInt 0 255)

unOctetTest :: OctetTest -> Int
unOctetTest = unwrap

unByteStringTest :: ByteStringTest -> B.ByteString
unByteStringTest = unwrap

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

prop_lengthPL :: ByteStringTest -> Boolean
prop_lengthPL bs = eq1 B.length (A.length :: Array Char -> Int) (unByteStringTest bs)

prop_nullBP :: LazyByteStringTest -> Boolean
prop_nullBP (LazyByteStringTest bs) = eq1 BL.null B.null bs

type X = Int
type O = OctetTest
type P = B.ByteString
type Z = BL.ByteString

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

    describe "ByteString <=> Lazy ByteString" do
      it "property null" do
        quickCheck prop_nullBP

      it "property length" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.length B.length bs

      it "property head" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.head B.head bs

      it "property tail" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.tail B.tail bs

      it "property last" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.last B.last bs

      it "property init" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.init B.init bs

      it "property reverse" do
        quickCheck \(LazyByteStringTest bs) ->
          eq1 BL.reverse B.reverse bs

      it "property foldl" do
        quickCheck \(LazyByteStringTest bs) f i ->
          eq3
            (BL.foldl :: (X -> X -> X) -> X -> Z -> X)
            (B.foldl :: (X -> X -> X) -> X -> P -> X)
            f
            i
            bs

      it "property foldr" do
        quickCheck \(LazyByteStringTest bs) f i ->
          eq3
            (BL.foldr :: (X -> X -> X) -> X -> Z -> X)
            (B.foldr :: (X -> X -> X) -> X -> P -> X)
            f
            i
            bs

    describe "ByteString <=> List" do
      it "property length" do
        quickCheck prop_lengthPL

      it "property null" do
        quickCheck \(ByteStringTest bs) -> eq1 B.null (A.null :: Array O -> Boolean) bs

      it "property head" do
        quickCheck \(ByteStringTest bs) ->
          eq1 (map OctetTest <<< B.head) (A.head :: Array O -> Maybe O) bs

      it "property tail" do
        quickCheck \(ByteStringTest bs) ->
          eq1 B.tail (A.tail :: Array O -> Maybe (Array O)) bs

      it "property last" do
        quickCheck \(ByteStringTest bs) ->
          eq1 (map OctetTest <<< B.last) (A.last :: Array O -> Maybe O) bs

      it "property init" do
        quickCheck \(ByteStringTest bs) ->
          eq1 B.init (A.init :: Array O -> Maybe (Array O)) bs

      it "property reverse" do
        quickCheck \(ByteStringTest bs) -> eq1 B.reverse (A.reverse :: Array O -> Array O) bs

      it "property any" do
        quickCheck \f (ByteStringTest bs) ->
          let
            anyOT :: (O -> Boolean) -> P -> Boolean
            anyOT g xa = B.any (g <<< OctetTest) xa
          in eq2 anyOT (F.any :: (O -> Boolean) -> Array O -> Boolean) f bs

      it "property all" do
        quickCheck \f (ByteStringTest bs) ->
          let
            allOT :: (O -> Boolean) -> P -> Boolean
            allOT g = B.all (g <<< OctetTest)
          in eq2 allOT (F.all :: (O -> Boolean) -> Array O -> Boolean) f bs

      it "prop cons" do
        quickCheck \i (ByteStringTest bs) ->
          eq2 (\(OctetTest s) xs -> B.cons s xs) (A.cons :: O -> Array O -> Array O) i bs

      it "property drop" do
        quickCheck \i (ByteStringTest bs) ->
          eq2 B.drop (A.drop :: X -> Array O -> Array O) i bs

      it "property dropWhile" do
        quickCheck \f (ByteStringTest bs) ->
          let dropWhile g = B.dropWhile (g <<< OctetTest)
          in eq2 dropWhile (A.dropWhile :: (O -> Boolean) -> Array O -> Array O) f bs

      it "property take" do
        quickCheck \i (ByteStringTest bs) ->
          eq2 B.take (A.take :: X -> Array O -> Array O) i bs

      it "property takeWhile" do
        quickCheck \f (ByteStringTest bs) ->
          let takeWhile g = B.takeWhile (g <<< OctetTest)
          in eq2 takeWhile (A.takeWhile :: (O -> Boolean) -> Array O -> Array O) f bs

      it "property elemIndex" do
        quickCheck \i (ByteStringTest bs) ->
          eq2 (B.elemIndex <<< unOctetTest) (A.elemIndex :: O -> Array O -> Maybe X) i bs

      it "property elemIndexEnd" do
        quickCheck \i (ByteStringTest bs) ->
          eq2 (B.elemIndexEnd <<< unOctetTest) (A.elemLastIndex :: O -> Array O -> Maybe X) i bs

      it "property findIndex" do
        quickCheck \f (ByteStringTest bs) ->
          let findIx g = B.findIndex (g <<< OctetTest)
          in eq2 findIx (A.findIndex :: (O -> Boolean) -> Array O -> Maybe Int) f bs

      it "property zip" do
        quickCheck \(ByteStringTest bs) (ByteStringTest cs) ->
          let zip xs ys = unsafeCoerce (A.fromFoldable (B.zip xs ys)) :: Array (Tuple O O)
          in eq2 zip (A.zip :: Array O -> Array O -> Array (Tuple O O)) bs cs

      it "property foldl" do
        quickCheck \f i (ByteStringTest bs) ->
          let reduce :: (X -> O -> X) -> X -> P -> X
              reduce g = B.foldl (\a o -> g a (OctetTest o))
          in eq3 reduce (F.foldl :: (X -> O -> X) -> X -> Array O -> X) f i bs

      it "property foldr" do
        quickCheck \f z (ByteStringTest bs) ->
          let reduceR :: (O -> X -> X) -> X -> P -> X
              reduceR g = B.foldr (\o a -> g (OctetTest o) a)
          in eq3 reduceR (F.foldr :: (O -> X -> X) -> X -> Array O -> X) f z bs

      it "property span" do
        quickCheck \f (ByteStringTest bs) ->
          let rtTuple { init, rest } = Tuple init rest
              baTuple { before, after } = Tuple before after
              bspan :: (O -> Boolean) -> P -> Tuple P P
              bspan g = baTuple <<< B.span (g <<< OctetTest)
              aspan :: (O -> Boolean) -> Array O -> Tuple (Array O) (Array O)
              aspan g = rtTuple <<< A.span g
          in eq2 bspan aspan f bs

stepUnfold :: Int -> Maybe (Tuple Int Int)
stepUnfold 6 = Nothing
stepUnfold n = Just (Tuple n (n + 1))

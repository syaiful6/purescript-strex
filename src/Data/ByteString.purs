{-
  This is port of ByteString's Haskell, implemented using `Uint8Array`. We choose
  `Uint8Array` rather than `Buffer` (Node.js), so this can be used on browser that support
  `TypedArray` interface. However it's easy to convert Uint8Array to Buffer because the implementation
  of Buffer on Node.js, eg. by pass the underlying ArrayBuffer, offset and length.
-}

module Data.ByteString
  ( empty
  , singleton
  , pack
  , unpack
  , null
  , length
  , head
  , tail
  , uncons
  , last
  , init
  , unsnoc
  , cons
  , snoc
  , index
  , elemIndex
  , elemIndexEnd
  , findIndex
  , isPrefixOf
  , stripPrefix
  , isSuffixOf
  , stripSuffix
  , take
  , drop
  , splitAt
  , takeWhile
  , dropWhile
  , break
  , breakSubstring
  , reverse
  , intersperse
  , foldl
  , foldr
  , foldMap
  , all
  , any
  , unfoldr
  , unfoldrN
  , replicate
  , copy
  , module Data.ByteString.Internal
  ) where

import Prelude

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Rec.Class (tailRecM3, Step(..))

import Data.ArrayBuffer.TypedArray (Ptr(..), nullPtr, plusPtr, minusPtr, poke, peek, peekByteOff)
import Data.ArrayBuffer.TypedArray as T
import Data.ByteString.Internal (ByteString(..), Octet)
import Data.ByteString.Internal as B
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromJust)
import Data.List (List(Nil), (:))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)


-- | The empty 'ByteString'
empty :: ByteString
empty = ByteString nullPtr 0 0

-- | A byte string with a single byte.
-- |
-- | Running time: `O(1)`
singleton :: Octet -> ByteString
singleton s = B.unsafeCreate 1 \p -> poke p s

-- | Convert an array of bytes to byte string
-- |
-- | Running time: `O(n)`
pack :: Array Octet -> ByteString
pack = B.packBytes'

-- | Converts a 'ByteString' to an array of bytes
-- |
-- | Running time: `O(n)`
unpack :: ByteString -> Array Octet
unpack (ByteString x o l) = unsafePerformEff $ B.toBytesArray x o l

--------------------------------------------------------------------------------
-- ByteString size -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether a ByteString is empty.
-- |
-- | Running time: `O(1)`
null :: ByteString -> Boolean
null (ByteString _ _ i) = i <= 0

-- | 'length' returns the length of a ByteString as an 'Int'
-- |
-- | Running time: `O(1)`
length :: ByteString -> Int
length (ByteString _ _ le) = le

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract the first element of a ByteString, if the ByteString is empty then
-- | it return Nothing, `Just Octed` if not
-- |
-- | Running time: `O(1)`
head :: ByteString -> Maybe Octet
head (ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff $ peekByteOff x s

-- | Extract the elements after the head of a ByteString
-- |
-- | Running time: `O(1)`
tail :: ByteString -> Maybe ByteString
tail (ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = Just $ ByteString x (s + 1) (l - 1)

-- | Extract the head and tail of a ByteString, returning Nothing if it is empty.
-- |
-- | Running time: `O(1)`
uncons :: ByteString -> Maybe { head :: Octet, tail :: ByteString }
uncons (ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff do
      h <- peekByteOff x s
      pure $
        { head: _
        , tail: ByteString x (s + 1) (l - 1)
        }
        <$>
        h

-- | Extract the last element of a ByteString
-- |
-- | Running time: `O(1)`
last :: ByteString -> Maybe Octet
last (ByteString x s l)
  | l <= 0    = Nothing
  | otherwise = unsafePerformEff $ peekByteOff x (s + l - 1)

-- | Return all the elements of a 'ByteString' except the last one.
-- |
-- | Running time: `O(1)`
init :: ByteString -> Maybe ByteString
init bs@(ByteString x s l)
  | null bs   = Nothing
  | otherwise = Just $ ByteString x s (l - 1)

-- | Extract the 'init' and 'last' of a ByteString, returning Nothing if it is empty.
-- |
-- | Running time: `O(1)`
unsnoc :: ByteString -> Maybe { init :: ByteString, last :: Octet }
unsnoc bs@(ByteString x s l)
  | null bs    = Nothing
  | otherwise  = unsafePerformEff do
      lst <- peekByteOff x (s + l - 1)
      pure $
        { init: ByteString x s (l - 1)
        , last: _
        }
        <$>
        lst

--------------------------------------------------------------------------------
-- Extending -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | 'cons' is analogous to (:) for lists, but have different complexity
-- |
-- | Running time: `O(n)`
cons :: Octet -> ByteString -> ByteString
cons c (ByteString x s l) = B.unsafeCreate (l + 1) \p -> do
  _ <- poke p c
  _ <- B.memcpy (p `plusPtr` 1) (x `plusPtr` s) l
  pure unit

-- | Append a byte to the end of a 'ByteString'
-- |
-- | Running time: `O(n)`
snoc :: ByteString -> Octet -> ByteString
snoc (ByteString x s l) c = B.unsafeCreate (l + 1) \p -> do
  _ <- B.memcpy p (x `plusPtr` s) l
  _ <- poke (p `plusPtr` l) c
  pure unit

--------------------------------------------------------------------------------
-- Indexing ByteStrings --------------------------------------------------------
--------------------------------------------------------------------------------

-- | 'ByteString' index (subscript) operator, 0 indexed.
-- |
-- | Running time: `O(1)`
index :: ByteString -> Int -> Maybe Octet
index (ByteString x s l) n
  | n < 0      = Nothing
  | n >= l     = Nothing
  | otherwise  = unsafePerformEff $ peekByteOff x (s + n)

-- | Returns the index of the first element in the given 'ByteString' which is
-- | equal to the query element, or 'Nothing' if there is no such element.
elemIndex :: Octet -> ByteString -> Maybe Int
elemIndex c (ByteString p s l) = unsafePerformEff do
  let p' = p `plusPtr` s
  q <- B.memchr p' c l
  pure $ if q == nullPtr then Nothing else Just $ q `minusPtr` p'

-- | Returns the index of the last element in the given 'ByteString' which is
-- | equal to the query element, or 'Nothing' if there is no such element.
elemIndexEnd :: Octet -> ByteString -> Maybe Int
elemIndexEnd c (ByteString (Ptr n x) s l) = unsafePerformEff do
  p <- T.subarray (n + s) (n + s + l) x
  pure $ T.elemIndexEnd c p

findIndex :: (Octet -> Boolean) -> ByteString -> Maybe Int
findIndex k (ByteString x s l) = go s x
  where
    go n p
      | n >= l    = Nothing
      | otherwise =
          let a = unsafePerformEff $ peek p
          in case a of
            Nothing -> Nothing
            Just b  -> if k b then Just n else go (n + 1) (p `plusPtr` 1)

--------------------------------------------------------------------------------
-- Sarching for substrings -----------------------------------------------------
--------------------------------------------------------------------------------

-- | Takes 2 ByteString and return true if the first is a prefix of the second.
-- |
-- | Running time: `O(n)`
isPrefixOf :: ByteString -> ByteString -> Boolean
isPrefixOf (ByteString x1 s1 l1) (ByteString x2 s2 l2)
  | l1 == 0   = true
  | l2 < l1   = false
  | otherwise = unsafePerformEff do
      i <- B.memcmp (x1 `plusPtr` s1) (x2 `plusPtr` s2) l1
      pure $ i == 0

-- | Takes two ByteStrings and returns 'Just' the remainder of the second if
-- | the first is its prefix, and otherwise Nothing
-- |
-- | Running time: `O(n)`
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix bs1@(ByteString _ _ l1) bs2
  | bs1 `isPrefixOf` bs2 = Just (drop l1 bs2)
  | otherwise            = Nothing

-- | Takes 2 ByteString and return true if the first is a suffix of the second.
-- |
-- | Running time: `O(n)`
isSuffixOf :: ByteString -> ByteString -> Boolean
isSuffixOf (ByteString x1 s1 l1) (ByteString x2 s2 l2)
 | l1 == 0   = true
 | l2 < l1   = false
 | otherwise = unsafePerformEff do
    i <- B.memcmp (x1 `plusPtr` s1) (x2 `plusPtr` s2 `plusPtr` (l2 - l1)) l1
    pure $ i == 0

-- | Takes two ByteStrings and returns 'Just' the remainder of the second if
-- | the first is its suffix, and otherwise Nothing
-- |
-- | Running time: `O(n)`
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix bs1@(ByteString _ _ l1) bs2@(ByteString _ _ l2)
   | bs1 `isSuffixOf` bs2 = Just (take (l2 - l1) bs2)
   | otherwise            = Nothing

-- -----------------------------------------------------------------------------
-- Substrings ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns the first `n` bytes of the ByteString.
take :: Int -> ByteString -> ByteString
take n bs@(ByteString x s l)
  | n <= 0    = empty
  | n >= l    = bs
  | otherwise = ByteString x s n

-- | Return the ByteString without the first `n` bytes
drop :: Int -> ByteString -> ByteString
drop n bs@(ByteString x s l)
  | n <= 0    = bs
  | n >= l    = empty
  | otherwise = ByteString x (s + n) (l - n)

splitAt :: Int -> ByteString -> { before :: ByteString, after :: ByteString }
splitAt n bs@(ByteString x s l)
  | n <= 0    = { before: empty, after: bs }
  | n >= l    = { before: bs, after: empty }
  | otherwise = { before: ByteString x s n, after: ByteString x (s + n) (l - n) }

-- | takeWhile', applied to a predicate @p@ and a ByteString @xs@, returns the longest
-- | prefix (possibly empty) of @xs@ of elements that satisfy @p@.
takeWhile :: (Octet -> Boolean) -> ByteString -> ByteString
takeWhile f ps = take (findIndexOrEnd (not <<< f) ps) ps

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Octet -> Boolean) -> ByteString -> ByteString
dropWhile f ps = drop (findIndexOrEnd (not <<< f) ps) ps

-- | `break`
break :: (Octet -> Boolean) -> ByteString -> { before :: ByteString, after :: ByteString }
break k ps = case findIndexOrEnd k ps of n -> { before: take n ps, after: drop n ps }

breakSubstring :: ByteString -> ByteString -> { before :: ByteString, after :: ByteString }
breakSubstring pat src = case length pat, compare (length pat) (length src) of
  0, _  -> { before: empty, after: src }
  1, _  -> break (_ == unsafePartial $ fromJust $ head pat) src
  _, GT -> { before: src, after: empty }
  _, _  -> unsafePerformEff do
    i <- B.findSubstring pat src
    pure $ if i < 0 then { before: empty, after: src } else splitAt i src

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

reverse :: ByteString -> ByteString
reverse (ByteString x s l) = B.unsafeCreate l \p -> B.reverse p (x `plusPtr` s) l

intersperse :: Octet -> ByteString -> ByteString
intersperse c ps@(ByteString buf ofs len)
  | len < 2   = ps
  | otherwise = B.unsafeCreate (2 * len - 1) $ \buf' ->
      B.intersperse buf' buf c

foldl :: forall a. (a -> Octet -> a) -> a -> ByteString -> a
foldl _ a (ByteString _ _ 0) = a
foldl f i (ByteString x n _) = B.foldlPtr f i (x `plusPtr` n)

foldr :: forall a. (Octet -> a -> a) -> a -> ByteString -> a
foldr _ a (ByteString _ _ 0) = a
foldr f i (ByteString x s _) = B.foldrPtr f i (x `plusPtr` s)

foldMap :: forall m. Monoid m => (Octet -> m) -> ByteString -> m
foldMap f = foldr (\x acc -> f x <> acc) mempty

all :: forall b. HeytingAlgebra b => (Octet -> b) -> ByteString -> b
all = alaF Conj foldMap

any :: forall b. HeytingAlgebra b => (Octet -> b) -> ByteString -> b
any = alaF Disj foldMap

copy :: ByteString -> ByteString
copy (ByteString x s l) = B.unsafeCreate l \buf' ->
  B.memcpy buf' (x `plusPtr` s) l

--------------------------------------------------------------------------------
-- Unfolds ByteString ----------------------------------------------------------
--------------------------------------------------------------------------------

unfoldr :: forall a. (a -> Maybe (Tuple Octet a)) -> a -> ByteString
unfoldr f = F.fold <<< go 32 64 Nil
  where
    go :: Int -> Int -> List ByteString -> a -> List ByteString
    go n n' memo x =
      case unfoldrN n f x of
        Tuple s Nothing   -> (F.foldl (flip (:)) Nil (s:memo))
        Tuple s (Just x') -> go n' (n+n') (s:memo) x'

-- | 'unfoldrN' builds a ByteString from a seed value.  However, the length of the
-- | result is limited by the first argument to 'unfoldrN'.
unfoldrN :: forall a. Int -> (a -> Maybe (Tuple Octet a)) -> a -> Tuple ByteString (Maybe a)
unfoldrN i f x0
  | i < 0     = Tuple empty (Just x0)
  | otherwise = unsafePerformEff $ B.createAndTrim' i $ \p -> tailRecM3 go p x0 0
      where
        go p x n
          | n == i    = pure (Done {offset: 0, size: n, value: Just x})
          | otherwise = case f x of
              Nothing          -> pure (Done {offset: 0, size: n, value: Nothing})
              Just (Tuple w x') -> do
                _ <- poke p w
                pure $ Loop {a: p `plusPtr` 1, b: x', c: n + 1 }

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- | the value of every element. The following holds:
replicate :: Int -> Octet -> ByteString
replicate w c
  | w <= 0    = empty
  | otherwise = B.unsafeCreate w \ptr -> B.memset ptr c w

--------------------------------------------------------------------------------
-- Internal Util ---------------------------------------------------------------
--------------------------------------------------------------------------------

findIndexOrEnd :: (Octet -> Boolean) -> ByteString -> Int
findIndexOrEnd k (ByteString x s l) = go s x
  where
    go n p
      | n >= l    = l
      | otherwise =
          let a = unsafePerformEff $ peek p
          in case a of
            Nothing -> l
            Just w  -> if k w then n else go (n + 1) (p `plusPtr` 1)

findFromEndUntil :: (Octet -> Boolean) -> ByteString -> Int
findFromEndUntil k ps@(ByteString x s l)
  | null ps     = 0
  | otherwise   = case last ps of
      Nothing -> 0
      Just x' -> if k x' then l else findFromEndUntil k (ByteString x s (l - 1))

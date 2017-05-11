module  Data.ArrayBuffer.DataView
 ( DataViewValue(..)
 , Endianness(..)
 , get
 , set
 , arrayBuffer
 , byteOffset
 , byteLength
 ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.ArrayBuffer.Types (ArrayBuffer, DataView)

import Unsafe.Coerce (unsafeCoerce)


data DataViewValue
  = Int8
  | Int16
  | Int32
  | Uint8
  | Uint16
  | Uint32
  | Float32
  | Float64

data Endianness
  = LittleEndian
  | BigEndian

derive instance eqDataViewValue :: Eq DataViewValue
derive instance ordDataViewValue :: Ord DataViewValue

derive instance eqEndianness :: Eq Endianness
derive instance ordEndianness :: Ord Endianness

instance showDataViewValue :: Show DataViewValue where
  show = case _ of
    Int8    -> "Int8"
    Int16   -> "Int16"
    Int32   -> "Int32"
    Uint8   -> "Uint8"
    Uint16  -> "Uint16"
    Uint32  -> "Uint32"
    Float32 -> "Float32"
    Float64 -> "Float64"

instance showEndianness :: Show Endianness where
  show LittleEndian = "LittleEndian"
  show BigEndian    = "BigEndian"

endiannesToBool :: Endianness -> Boolean
endiannesToBool LittleEndian = true
endiannesToBool BigEndian    = false

get :: forall eff. DataViewValue -> Endianness -> Int -> DataView -> Eff eff Int
get dV end offset dataview = getImpl (show dV) (endiannesToBool end) offset dataview

set
  :: forall eff
   . Int
  -- ^ The value to insert
  -> DataViewValue
  -> Endianness
  -> Int
  -- ^ offset
  -> DataView
  -> Eff eff Unit
set v dV end offset dataview = setImpl v (show dV) (endiannesToBool end) offset dataview

arrayBuffer :: DataView -> ArrayBuffer
arrayBuffer = _.buffer <<< unsafeCoerce

byteLength :: DataView -> Int
byteLength = _.byteLength <<< unsafeCoerce

byteOffset :: DataView-> Int
byteOffset = _.byteOffset <<< unsafeCoerce

foreign import newDataView
  :: forall eff
   . Int
  -- ^ The offset, in bytes, to the first byte in the specified buffer for the new view to reference.
  -> Int
  -- ^ The number of elements in the byte array.
  -> ArrayBuffer
  -- ^ The ArrayBuffer
  -> Eff eff DataView

foreign import getImpl :: forall eff. String -> Boolean -> Int -> DataView -> Eff eff Int

foreign import setImpl :: forall eff. Int -> String -> Boolean -> Int -> DataView -> Eff eff Unit

{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Plugin.Utils where

import Language.Haskell.TH.Syntax

import Control.Applicative (liftA3)

import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal -- (Pool, pooledMalloc, pooledMallocBytes, pooledNew, pooledNewArray, peekArray, pokeArray)
import Foreign.Storable (Storable(..))
import Foreign.Storable.Tuple

import Data.Int
import Data.Word
import Data.Complex

import Feldspar.Plugin.Marshal
import Feldspar.Vector (Vector, Vector1, Vector2)
import Feldspar.Matrix (Matrix)
import Feldspar.Core.Types (Index,Length,WordN(..),IntN(..))
import Feldspar.Core.Constructs (Data)

import qualified Feldspar.Compiler.Imperative.Representation as FCIR

import Debug.Trace

unData :: Type -> Q Type
unData (ConT c) | ''Index  == c = [t| WordN |]
                | ''Length == c = [t| WordN |]
unData (AppT (ConT c) x) | ''Data    == c = unData x
                         | ''Vector  == c = [t| [$(unData x)]   |]
                         | ''Vector1 == c = [t| [$(unData x)]   |]
                         | ''Vector2 == c = [t| [[$(unData x)]] |]
                         | ''Matrix  == c = [t| [[$(unData x)]] |]
unData (AppT t1 t2)                       = [t| $(unData t1) $(unData t2) |]
unData p                                  = return p

args :: Type -> [Type]
args (AppT (AppT ArrowT x) y) = x : args y
args _                        = []

result :: Type -> Type
result (AppT (AppT ArrowT _) y) = result y
result y                        = y

buildHaskellType :: Type -> Q Type
buildHaskellType (AppT (AppT ArrowT t) r) = [t| $(return t) -> $(buildHaskellType r) |]
buildHaskellType r                        = [t| IO $(return r) |]

buildCType :: Type -> Q Type
buildCType typ = [t| $(go typ) |]
  where
    go (AppT (AppT ArrowT t) r) = [t| $(mkPtr t)    -> $(go r) |]
    go r                        = [t| Ptr $(mkSA r) -> IO ()   |]

    mkPtr (AppT ListT t)               = [t| Ptr (SA $(mkSA t)) |]
    mkPtr (AppT (AppT (TupleT n) a) b) = [t| Ptr ($(mkSA a), $(mkSA b)) |]
    mkPtr t                            = return t

    mkSA (AppT ListT t) = [t| SA $(mkSA t) |]
    mkSA t              = return t


deriving instance Storable IntN
deriving instance Storable WordN

instance (RealFloat a, Storable a) => Storable (Complex a)
  where
    sizeOf    _ = 2 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr    = do
      [re,im] <- peekArray 2 (castPtr ptr :: Ptr a)
      return $ re :+ im
    poke ptr (re :+ im) = do
      pokeArray (castPtr ptr :: Ptr a) [re,im]

sizeFromType :: FCIR.Type -> Int
sizeFromType FCIR.VoidType = 0
sizeFromType FCIR.BoolType = sizeOf (undefined :: Bool)
sizeFromType FCIR.BitType  = 0
sizeFromType FCIR.FloatType = sizeOf (undefined :: Float)
sizeFromType (FCIR.NumType _ FCIR.S8)  = sizeOf (undefined :: Word8)
sizeFromType (FCIR.NumType _ FCIR.S16) = sizeOf (undefined :: Word16)
sizeFromType (FCIR.NumType _ FCIR.S32) = sizeOf (undefined :: Word32)
sizeFromType (FCIR.NumType _ FCIR.S64) = sizeOf (undefined :: Word64)
sizeFromType (FCIR.ComplexType t) = 2 * sizeFromType t
sizeFromType (FCIR.StructType xs)   = sum $ map (sizeFromType . snd) xs
-- sizeFromType (FCIR.ArrayType (FCIR.UndefinedLen) t) = error "sizeFromType: can not calculate size of array with unknown length"
-- sizeFromType (FCIR.ArrayType (FCIR.LiteralLen len) t) = len * sizeFromType t
sizeFromType typ = error $ "sizeFromType: missing implementation: " ++ show typ


class Storable a => Alloc a
  where
    allocate :: IO (Ptr a)
    allocate = malloc

instance Alloc Bool
instance Alloc IntN
instance Alloc Int8
instance Alloc Int16
instance Alloc Int32
instance Alloc Int64
instance Alloc WordN
instance Alloc Word8
instance Alloc Word16
instance Alloc Word32
instance Alloc Word64
instance Alloc Float

instance (RealFloat a, Alloc a) => Alloc (Complex a)
  where
    allocate = mallocBytes $ 2 * sizeOf (undefined :: a)

instance Alloc a => Alloc (SA a)
  where
    allocate = new $ SA nullPtr 0 0 0

instance (Alloc a, Alloc b) => Alloc (a,b)
  where
    allocate = do
      aPtr <- allocate
      bPtr <- allocate
      a' <- peek aPtr
      b' <- peek bPtr
      new (a',b')

mkMem :: [FCIR.Type] -> IO (Ptr (SA ()))
mkMem [] = return nullPtr
mkMem ms = do
    ms' <- mapM go ms
    buf <- newArray ms'
    let len = length ms
    let sa = SA buf (fromIntegral $ len) (-1) $ fromIntegral $ len * sizeOf (undefined :: SA ())
    ptr <- new sa
    return $ castPtr ptr
  where
    go :: FCIR.Type -> IO (SA ())
    go (FCIR.ArrayType (FCIR.UndefinedLen) typ)   = go (FCIR.ArrayType (FCIR.LiteralLen maxBound) typ)
    go (FCIR.ArrayType (FCIR.LiteralLen len) typ) = do
        let esize = fromIntegral $ sizeFromType typ
        let len'  = fromIntegral $ min len (1024 * 1024)
        buf <- mallocBytes $ fromIntegral $ len' * esize
        return $ SA (castPtr buf) len' esize $ fromIntegral $ len' * esize


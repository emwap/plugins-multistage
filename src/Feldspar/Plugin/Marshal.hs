{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Feldspar.Plugin.Marshal where

import Foreign.Ptr (Ptr)
import Foreign.Marshal (new, newArray, peekArray, pokeArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.Storable.Tuple ()
import Data.Int
import Data.Word
import Data.Complex
import Control.Applicative
import qualified Foreign.Storable.Record as Store

import Feldspar.Core.Types (IntN(..), WordN(..))


data SA a = SA { buf         :: Ptr a
               , elems       :: Int32
               , esize       :: Int32
               , bytes       :: Word32
               , initialized :: Word32
               }
  deriving (Eq, Show)

storeSA :: Storable a => Store.Dictionary (SA a)
storeSA = Store.run $ SA
    <$> Store.element buf
    <*> Store.element elems
    <*> Store.element esize
    <*> Store.element bytes
    <*> Store.element initialized

instance Storable a => Storable (SA a)
  where
    sizeOf    = Store.sizeOf    storeSA
    alignment = Store.alignment storeSA
    peek      = Store.peek      storeSA
    poke      = Store.poke      storeSA

deriving instance Storable IntN
deriving instance Storable WordN

storeComplex :: (RealFloat a, Storable a)
             => Store.Dictionary (Complex a)
storeComplex = Store.run $ (:+)
    <$> Store.element realPart
    <*> Store.element imagPart

instance (RealFloat a, Storable a) => Storable (Complex a)
  where
    sizeOf    = Store.sizeOf    storeComplex
    alignment = Store.alignment storeComplex
    peek      = Store.peek      storeComplex
    poke      = Store.poke      storeComplex



class Reference a
  where
    type Ref a :: *
    ref   :: a     -> Ref a
    deref :: Ref a -> IO a

instance Reference Bool
  where
    type Ref Bool = Bool
    ref   = id
    deref = return

instance Reference Int8
  where
    type Ref Int8 = Int8
    ref   = id
    deref = return

instance Reference Int16
  where
    type Ref Int16 = Int16
    ref   = id
    deref = return

instance Reference Int32
  where
    type Ref Int32 = Int32
    ref   = id
    deref = return

instance Reference Int64
  where
    type Ref Int64 = Int64
    ref   = id
    deref = return

instance Reference IntN
  where
    type Ref IntN = IntN
    ref   = id
    deref = return

instance Reference Word8
  where
    type Ref Word8 = Word8
    ref   = id
    deref = return

instance Reference Word16
  where
    type Ref Word16 = Word16
    ref   = id
    deref = return

instance Reference Word32
  where
    type Ref Word32 = Word32
    ref   = id
    deref = return

instance Reference Word64
  where
    type Ref Word64 = Word64
    ref   = id
    deref = return

instance Reference WordN
  where
    type Ref WordN = WordN
    ref   = id
    deref = return

instance Reference Float
  where
    type Ref Float = Float
    ref   = id
    deref = return

instance Storable a => Reference (Complex a)
  where
    type Ref (Complex a) = Complex a
    ref   = id
    deref = return

instance (Storable a) => Reference (SA a)
  where
    type Ref (SA a) = Ptr (SA a)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a,b) => Reference (a,b)
  where
    type Ref (a,b) = Ptr (a,b)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a,b,c) => Reference (a,b,c)
  where
    type Ref (a,b,c) = Ptr (a,b,c)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a, b, c, d) => Reference (a,b,c,d)
  where
    type Ref (a,b,c,d) = Ptr (a,b,c,d)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a, b, c, d, e) => Reference (a,b,c,d,e)
  where
    type Ref (a,b,c,d,e) = Ptr (a,b,c,d,e)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a, b, c, d, e, f) => Reference (a,b,c,d,e,f)
  where
    type Ref (a,b,c,d,e,f) = Ptr (a,b,c,d,e,f)
    ref a = unsafeLocalState $ new a
    deref = peek

instance Storable (a, b, c, d, e, f, g) => Reference (a,b,c,d,e,f,g)
  where
    type Ref (a,b,c,d,e,f,g) = Ptr (a,b,c,d,e,f,g)
    ref a = unsafeLocalState $ new a
    deref = peek


class Marshal a
  where
    type Rep a :: *
    to   :: a     -> Rep a
    from :: Rep a -> IO a

instance Marshal Bool
  where
    type Rep Bool = Bool
    to   = id
    from = return

instance Marshal Int8
  where
    type Rep Int8 = Int8
    to   = id
    from = return

instance Marshal Int16
  where
    type Rep Int16 = Int16
    to   = id
    from = return

instance Marshal Int32
  where
    type Rep Int32 = Int32
    to   = id
    from = return

instance Marshal Int64
  where
    type Rep Int64 = Int64
    to   = id
    from = return

instance Marshal IntN
  where
    type Rep IntN = IntN
    to   = id
    from = return

instance Marshal Word8
  where
    type Rep Word8 = Word8
    to   = id
    from = return

instance Marshal Word16
  where
    type Rep Word16 = Word16
    to   = id
    from = return

instance Marshal Word32
  where
    type Rep Word32 = Word32
    to   = id
    from = return

instance Marshal Word64
  where
    type Rep Word64 = Word64
    to   = id
    from = return

instance Marshal WordN
  where
    type Rep WordN = WordN
    to   = id
    from = return

instance Marshal Float
  where
    type Rep Float = Float
    to   = id
    from = return

instance Marshal a => Marshal (Complex a)
  where
    type Rep (Complex a) = Complex a
    to   = id
    from = return

instance (Storable (Rep a), Marshal a) => Marshal [a]
  where
    type Rep [a] = SA (Rep a)
    to xs = unsafeLocalState $ do
        let len   = fromIntegral $ length xs
        let esize = fromIntegral $ sizeOf (undefined :: Rep a)
        let ys    = map to xs
        buf <- newArray ys
        let sa = SA buf len esize (fromIntegral (len * esize)) 0x89abcdef
        return sa
    from sa@(SA buf len esize bytes _) =
        mapM from =<< peekArray (fromIntegral len) buf

instance (Marshal a, Marshal b) => Marshal (a,b)
  where
    type Rep (a,b) = (Rep a,Rep b)
    to (a,b) = (to a, to b)
    from (a,b) = (,) <$> from a <*> from b

instance ( Marshal a
         , Marshal b
         , Marshal c
         ) => Marshal (a,b,c)
  where
    type Rep (a,b,c) = (Rep a,Rep b,Rep c)
    to (a,b,c) = (to a, to b, to c)
    from (a,b,c) = (,,) <$> from a <*> from b <*> from c

instance ( Marshal a
         , Marshal b
         , Marshal c
         , Marshal d
         ) => Marshal (a,b,c,d)
  where
    type Rep (a,b,c,d) = (Rep a,Rep b,Rep c,Rep d)
    to (a,b,c,d) = (to a, to b, to c, to d)
    from (a,b,c,d) =
      (,,,) <$> from a <*> from b <*> from c <*> from d

instance ( Marshal a
         , Marshal b
         , Marshal c
         , Marshal d
         , Marshal e
         ) => Marshal (a,b,c,d,e)
  where
    type Rep (a,b,c,d,e) = (Rep a,Rep b,Rep c,Rep d,Rep e)
    to (a,b,c,d,e) = (to a, to b, to c, to d, to e)
    from (a,b,c,d,e) =
      (,,,,) <$> from a <*> from b <*> from c <*> from d <*> from e

instance ( Marshal a
         , Marshal b
         , Marshal c
         , Marshal d
         , Marshal e
         , Marshal f
         ) => Marshal (a,b,c,d,e,f)
  where
    type Rep (a,b,c,d,e,f) = (Rep a,Rep b,Rep c,Rep d,Rep e,Rep f)
    to (a,b,c,d,e,f) = (to a, to b, to c, to d, to e, to f)
    from (a,b,c,d,e,f) =
      (,,,,,) <$> from a <*> from b <*> from c <*> from d <*> from e <*> from f

instance ( Marshal a
         , Marshal b
         , Marshal c
         , Marshal d
         , Marshal e
         , Marshal f
         , Marshal g
         ) => Marshal (a,b,c,d,e,f,g)
  where
    type Rep (a,b,c,d,e,f,g) = (Rep a,Rep b,Rep c,Rep d,Rep e,Rep f,Rep g)
    to (a,b,c,d,e,f,g) = (to a, to b, to c, to d, to e, to f, to g)
    from (a,b,c,d,e,f,g) =
      (,,,,,,) <$> from a <*> from b <*> from c <*> from d <*> from e <*> from f <*> from g


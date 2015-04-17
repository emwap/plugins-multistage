{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic components
module System.Plugins.MultiStage
  (
  -- * Loading
    loadFunWithConfig
  , loadFunType

  -- * Configuration
  , Config(..)
  , defaultConfig

  -- * Calling Convention
  , CallConv(..)
  , buildType
  , applyTF
  , expandTF

  -- * Marshaling
  , pack
  , unpack
  , Reference(..)
  , Marshal(..)
  )
where

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Desugar

import Data.Int
import Data.Word
import Data.Maybe (mapMaybe)
import Control.Applicative

import Foreign.Ptr
import Foreign.Marshal (new)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable

-- | Configuration parameters for the function loader
data Config = Config { declWorker   :: Config -> Name -> Name -> [Name] -> Type -> [DecQ]
                     , builder      :: Config -> Name -> Q Body
                     , worker       :: Name -> [Name] -> Q Body
                     , typeFromName :: Name -> Q Type
                     , mkHSig       :: Type -> Q Type
                     , mkCSig       :: Type -> Q Type
                     , prefix       :: String
                     , wdir         :: String
                     , opts         :: [String]
                     , safety       :: Safety
                     }

defaultConfig :: Config
defaultConfig = Config { declWorker   = declareWorker
                       , builder      = noBuilder
                       , worker       = noWorker
                       , typeFromName = loadFunType
                       , mkHSig       = return
                       , mkCSig       = return
                       , prefix       = "c_"
                       , wdir         = "tmp"
                       , opts         = []
                       , safety       = unsafe
                       }

noBuilder :: Config -> Name -> Q Body
noBuilder _ _ = normalB [| return nullPtr |]

noWorker :: Name -> [Name] -> Q Body
noWorker fun as = normalB $ appsE $ map varE $ fun:as

-- | Generic function compiler and loader
loadFunWithConfig :: Config -> [Name] -> Q [Dec]
loadFunWithConfig conf@Config{..} names = concat <$> mapM go names
  where
    go name = do
      typ <- typeFromName name
      let base    = nameBase name
      let cname   = mkName $ prefix ++ base
      let wname   = mkName $ prefix ++ base ++ "_worker"
      let args    = [mkName $ 'v' : show i | i <- [1..(arity typ)]]
      sequence $  declWorker conf wname name args typ
               ++ declareWrapper cname wname args typ

    arity :: Type -> Int
    arity (AppT (AppT ArrowT _) r) = 1 + arity r
    arity _                        = 0

-- | Extract the type of the supplied function name
loadFunType :: Name -> Q Type
loadFunType name = do
  info <- reify name
  case info of
    (VarI _ t _ _) -> return t
    _ -> error $ unwords ["loadFun:",show (nameBase name)
                         ,"is not a function:",show info]

declareWorker :: Config -> Name -> Name -> [Name] -> Type -> [DecQ]
declareWorker conf@Config{..} wname name as typ =
    [ declareImport conf factory csig
    , sigD bname $ appT [t|Ptr|] csig
    , funD bname [clause [] (builder conf name) []]
    , sigD rname csig
    , funD rname [clause [] (normalB [|$(varE factory) $ castPtrToFunPtr $(varE bname)|]) []]
    , sigD wname hsig
    , funD wname [clause (map varP as) (worker rname as) []]
    ]
  where
    base    = nameBase name
    bname   = mkName $ prefix ++ base ++ "_builder"
    factory = mkName $ prefix ++ base ++ "_factory"
    rname   = mkName $ prefix ++ base ++ "_raw"
    hsig    = mkHSig typ
    csig    = mkCSig typ

declareWrapper :: Name -> Name -> [Name] -> Type -> [DecQ]
declareWrapper cname wname as typ =
    [ sigD cname (return typ)
    , funD cname [clause (map varP as) (wrapper wname as) [] ]
    ]

declareImport :: Config -> Name -> TypeQ -> DecQ
declareImport Config{..} name csig =
    forImpD cCall safety "dynamic" name [t|FunPtr $(csig) -> $(csig)|]

wrapper :: Name -> [Name] -> Q Body
wrapper workername args = normalB
    [|unsafeLocalState $(appsE $ map varE $ workername : args) |]


-- | The Calling Convention specifies how a type should be converted
data CallConv = CallConv { arg :: Type -> Q Type
                           -- ^ Convert an argument
                         , res :: Type -> Q Type
                           -- ^ Convert the result
                         }

-- | Convert a type using the supplied calling convention
buildType :: CallConv -> Type -> Q Type
buildType CallConv{..} typ = go typ >>= expandTF
  where
    go (AppT (AppT ArrowT t) r) = arg t `arrT` go r
    go r                        = res r

    arrT t = appT (appT arrowT t)

-- | Apply a type family
applyTF :: Name -> Type -> Q Type
applyTF tf = expandTF . AppT (ConT tf)

-- | Expand type families
expandTF :: Type -> Q Type
expandTF t = sweeten <$> (desugar t >>= expandType)

-- | Pack a value into its runtime representation
--
-- > pack a = to a >>= ref
--
pack :: (Reference (Rep a), Marshal a) => a -> IO (Ref (Rep a))
pack a = to a >>= ref

-- | Unpack a value from its runtime representation
--
-- > unpack a = deref a >>= from
--
unpack :: (Reference (Rep a), Marshal a) => Ref (Rep a) -> IO a
unpack a = deref a >>= from

-- | Optionally make a refrence of a value
class Reference a
  where
    -- | The type of a referenced value
    type Ref a :: *

    -- | Convert to a referenced value
    ref         ::                a -> IO (Ref a)
    default ref :: (a ~ Ref a) => a -> IO (Ref a)
    {-# INLINE ref #-}
    ref = return

    -- | Convert from a referenced value
    -- In the IO monad to allow @peek@ing through the reference.
    deref         ::                Ref a -> IO a
    default deref :: (a ~ Ref a) => Ref a -> IO a
    {-# INLINE deref #-}
    deref = return

instance Reference Bool        where type Ref Bool        = Bool
instance Reference Int8        where type Ref Int8        = Int8
instance Reference Int16       where type Ref Int16       = Int16
instance Reference Int32       where type Ref Int32       = Int32
instance Reference Int64       where type Ref Int64       = Int64
instance Reference Word8       where type Ref Word8       = Word8
instance Reference Word16      where type Ref Word16      = Word16
instance Reference Word32      where type Ref Word32      = Word32
instance Reference Word64      where type Ref Word64      = Word64
instance Reference Float       where type Ref Float       = Float
instance Reference Double      where type Ref Double      = Double

-- | Convert between Haskell and representation types
class Marshal a
  where
    type Rep a :: *

    to         ::                a -> IO (Rep a)
    default to :: (a ~ Rep a) => a -> IO (Rep a)
    {-# INLINE to #-}
    to = return

    from         ::                Rep a -> IO a
    default from :: (a ~ Rep a) => Rep a -> IO a
    {-# INLINE from #-}
    from = return

instance Marshal Bool        where type Rep Bool        = Bool
instance Marshal Int8        where type Rep Int8        = Int8
instance Marshal Int16       where type Rep Int16       = Int16
instance Marshal Int32       where type Rep Int32       = Int32
instance Marshal Int64       where type Rep Int64       = Int64
instance Marshal Word8       where type Rep Word8       = Word8
instance Marshal Word16      where type Rep Word16      = Word16
instance Marshal Word32      where type Rep Word32      = Word32
instance Marshal Word64      where type Rep Word64      = Word64
instance Marshal Float       where type Rep Float       = Float
instance Marshal Double      where type Rep Double      = Double


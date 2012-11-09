{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Plugin.Utils
  ( unData
  , buildHaskellType
  , buildCType
  )
  where

import Language.Haskell.TH.Syntax

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Feldspar.Plugin.Marshal
import Feldspar.Vector (Vector, Vector1, Vector2)
import Feldspar.Matrix (Matrix)
import Feldspar.Core.Types (Index,Length,WordN(..),IntN(..))
import Feldspar.Core.Constructs (Data)

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


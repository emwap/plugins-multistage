{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Plugin.Utils
  ( rewriteType
  , buildHaskellType
  , buildCType
  )
  where


import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns (expandSyns)

import Foreign.Ptr (Ptr)

import Language.Syntactic.Sugar (Syntactic(..))
import Feldspar.Plugin.Marshal (Reference(..), Marshal(..))

import Control.Monad ((>=>))

rewriteType :: Type -> Q Type
rewriteType =   return
            >=> expandSyns
            >=> rewriteSyntactic

rewriteSyntactic :: Type -> Q Type
rewriteSyntactic = go
  where
    go t@(AppT c@(ConT _) x) = do
      inst <- isInstance ''Syntactic [t]
      if inst
        then [t| $(conT ''Internal) $(return t) |]
        else [t| $(return c) $(go x) |]
    go (AppT t1 t2) = [t| $(go t1) $(go t2) |]
    go t = return t

buildHaskellType :: Type -> Q Type
buildHaskellType = go
  where
    go (AppT (AppT ArrowT t) r) = [t| $(return t) -> $(go r) |]
    go r                        = [t| IO $(return r) |]

buildCType :: Type -> Q Type
buildCType = go
  where
    go (AppT (AppT ArrowT t) r) = [t| Ref (Rep $(return t)) -> $(go r) |]
    go r                        = [t| Ptr (Rep $(return r)) -> IO ()   |]


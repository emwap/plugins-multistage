{-# LANGUAGE CPP #-}
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
import Language.Haskell.TH.ExpandSyns

import Foreign.Ptr (Ptr)

import Language.Syntactic.Sugar (Syntactic(..))
import Feldspar.Plugin.Marshal (Reference(..), Marshal(..))

import Control.Monad ((<=<))

rewriteType :: Type -> Q Type
rewriteType =   return
            <=< rewriteSyntactic
            <=< expandSyns

rewriteSyntactic :: Type -> Q Type
rewriteSyntactic = expandFam ''Internal <=< go
  where
    go t@(AppT c@(ConT _) x) = do
      inst <- isInstance ''Syntactic [t]
      if inst
        then [t| Internal $(return t) |]
        else [t| $(return c) $(go x) |]
    go (AppT t1 t2) = [t| $(go t1) $(go t2) |]
    go t = return t

buildHaskellType :: Type -> Q Type
buildHaskellType = go
  where
    go (AppT (AppT ArrowT t) r) = [t| $(return t) -> $(go r) |]
    go r                        = [t| IO $(return r) |]

buildCType :: Type -> Q Type
buildCType = expandFam ''Ref <=< expandFam ''Rep <=< go
  where
    go :: Type -> Q Type
    go (AppT (AppT ArrowT t) r) = [t|      Ref (Rep $(return t))  -> $(go r) |]
    go r                        = [t| Ptr (Ref (Rep $(return r))) -> IO ()   |]

expandFam :: Name -> Type -> Q Type
expandFam name = go
  where
    go (AppT (AppT ArrowT t) r) = [t| $(go t) -> $(go r) |]
    go (AppT t1@(ConT n) t2) | n == name = do
        decs <- reifyInstances name [t2]
        case map projInst decs of
          [Just (_, [AppT p1 (VarT pv1)], pt2)]
              | AppT p2 et <- t2, p1 == p2 -> go $ substInType (pv1,et) pt2
          [Just (_, [pattern], value)]
              | pattern == value           -> return value
          _                                -> appT (return t1) (go t2)
    go (AppT t1 t2)   = appT (go t1) (go t2)
    go t              = return t

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
    projInst (TySynInstD name (TySynEqn patterns typ)) = Just (name,patterns,typ)
#else
    projInst (TySynInstD name patterns typ)            = Just (name,patterns,typ)
#endif
    projInst _ = Nothing


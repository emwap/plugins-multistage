{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Type Rewriting machinery
module Feldspar.Plugin.TypeRewrite
  ( buildType
  , CallConv(..)
  , expandTF
  )
  where

import Data.Maybe (mapMaybe)

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns (substInType)

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

-- | Expand type families
expandTF :: Type -> Q Type
expandTF = down
  where
    down :: Type -> Q Type
    down (AppT t1 t2) = appT (down t1) (down t2) >>= up
    down t            = up t

    up :: Type -> Q Type
    up t@(AppT (ConT fam) t1) = do
      info <- reify fam
      case info of
        FamilyI{} -> do
          is <- reifyInstances fam [t1]
          case mapMaybe projInst is of
            [(AppT p1 (VarT pv1),pt2)]
                | AppT p2 et <- t1
                , p1 == p2 -> down $ substInType (pv1,et) pt2
            [(p1,value)]
                | p1 == value -> up value
            _ -> return t
        _ -> return t
    up (AppT t1 t2) = appT (return t1) (return t2)
    up t = return t

projInst :: Dec -> Maybe (Type, Type)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
projInst (TySynInstD _ (TySynEqn [pattern] typ)) = Just (pattern,typ)
#else
projInst (TySynInstD _ [pattern] typ)            = Just (pattern,typ)
#endif
projInst _ = Nothing


{-# LANGUAGE CPP #-}

-- | Type Rewriting machinery
module Language.Haskell.TH.TypeRewrite
  ( applyTF
  , expandTF
  )
  where

import Data.Maybe (mapMaybe)

import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns (expandSyns,substInType)

-- | Apply a type family
-- Walk the type and apply the type family to every element that is an
-- instance of @tf@
applyTF :: Name -> Type -> Q Type
applyTF tf typ = expandSyns typ >>= go
  where
    go t@(AppT c@(ConT _) x) = do
      inst <- isInstance tf [t]
      if inst
        then appT (conT tf)  (return t)
        else appT (return c) (go x)
    go (AppT t1 t2) = appT (go t1) (go t2)
    go t = return t

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
            _ -> do
              reportWarning $ unwords ["uncaught",show fam,show is]
              return t
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


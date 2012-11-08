{-# LANGUAGE TemplateHaskell #-}

module Feldspar.Plugin.Generic
  ( loadFunWithConfig
  , loadFunType
  , Config(..)
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)

import Foreign.Marshal.Unsafe (unsafeLocalState)

data Config = Config { declWorker   :: Name -> Name -> [Name] -> Type -> [DecQ]
                     , typeFromName :: Name -> Q Type
                     , prefix       :: String
                     }

loadFunWithConfig :: Config -> Name -> Q [Dec]
loadFunWithConfig conf name = do
    typ <- (typeFromName conf) name
    let base    = nameBase name
    let cname   = mkName $ (prefix conf) ++ base
    let wname   = mkName $ (prefix conf) ++ base ++ "_worker"
    let as      = [mkName $ "v" ++ show i | i <- [1..(arity typ)]]
    sequenceQ $  ((declWorker conf) wname name as typ)
              ++ (declareWrapper cname wname as typ)
  where
    arity :: Type -> Int
    arity (AppT (AppT ArrowT t) r) = 1 + arity r
    arity r                        = 0

loadFunType :: Name -> Q Type
loadFunType name = do
  info <- reify name
  case info of
    (VarI _ t _ _) -> return t
    otherwise -> error ("loadFun: " ++ (show $ nameBase name) ++ " is not a function")

declareWrapper :: Name -> Name -> [Name] -> Type -> [DecQ]
declareWrapper cname wname as typ =
    [ sigD cname $ return typ
    , funD cname $ [clause (map varP as) (wrapper wname as) [] ]
    ]

wrapper :: Name -> [Name] -> Q Body
wrapper workername as = normalB
    [|unsafeLocalState $ $(appsE $ map varE $ workername : as) |]



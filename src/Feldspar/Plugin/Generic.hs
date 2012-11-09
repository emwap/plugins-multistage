{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
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

data Config = Config { declWorker   :: Name -> Name -> [Name] -> Type -> [String] -> [DecQ]
                     , typeFromName :: Name -> Q Type
                     , prefix       :: String
                     , opts         :: [String]
                     }

loadFunWithConfig :: Config -> Name -> Q [Dec]
loadFunWithConfig Config{..} name = do
    typ <- typeFromName name
    let base    = nameBase name
    let cname   = mkName $ prefix ++ base
    let wname   = mkName $ prefix ++ base ++ "_worker"
    let args    = [mkName $ 'v' : show i | i <- [1..(arity typ)]]
    sequenceQ $  declWorker wname name args typ opts
              ++ declareWrapper cname wname args typ
  where
    arity :: Type -> Int
    arity (AppT (AppT ArrowT _) r) = 1 + arity r
    arity _                        = 0

loadFunType :: Name -> Q Type
loadFunType name = do
  info <- reify name
  case info of
    (VarI _ t _ _) -> return t
    _              -> error ("loadFun: " ++ show (nameBase name) ++ " is not a function")

declareWrapper :: Name -> Name -> [Name] -> Type -> [DecQ]
declareWrapper cname wname as typ =
    [ sigD cname (return typ)
    , funD cname [clause (map varP as) (wrapper wname as) [] ]
    ]

wrapper :: Name -> [Name] -> Q Body
wrapper workername args = normalB
    [|unsafeLocalState $ $(appsE $ map varE $ workername : args) |]



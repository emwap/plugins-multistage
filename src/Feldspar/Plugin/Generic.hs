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
import Foreign.Marshal.Unsafe (unsafeLocalState)

-- | Configuration parameters for the function loader
data Config = Config { declWorker   :: Config -> Name -> Name -> [Name] -> Type -> [DecQ]
                     , typeFromName :: Name -> Q Type
                     , prefix       :: String
                     , wdir         :: String
                     , opts         :: [String]
                     }

-- | Generic function compiler and loader
loadFunWithConfig :: Config -> Name -> Q [Dec]
loadFunWithConfig conf@Config{..} name = do
    typ <- typeFromName name
    let base    = nameBase name
    let cname   = mkName $ prefix ++ base
    let wname   = mkName $ prefix ++ base ++ "_worker"
    let args    = [mkName $ 'v' : show i | i <- [1..(arity typ)]]
    sequence $  declWorker conf wname name args typ
             ++ declareWrapper cname wname args typ
  where
    arity :: Type -> Int
    arity (AppT (AppT ArrowT _) r) = 1 + arity r
    arity _                        = 0

-- | Extract the type of the supplied function name
loadFunType :: Name -> Q Type
loadFunType name = do
  info <- reify name
  case info of
    (VarI _ t _ _) -> return t
    _              -> error ("loadFun: " ++ show (nameBase name) ++ " is not a function: " ++ show info)

declareWrapper :: Name -> Name -> [Name] -> Type -> [DecQ]
declareWrapper cname wname as typ =
    [ sigD cname (return typ)
    , funD cname [clause (map varP as) (wrapper wname as) [] ]
    ]

wrapper :: Name -> [Name] -> Q Body
wrapper workername args = normalB
    [|unsafeLocalState $(appsE $ map varE $ workername : args) |]



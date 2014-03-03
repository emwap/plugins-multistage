{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.TypeRewrite

import Foreign.Ptr
import Foreign.Marshal.Unsafe (unsafeLocalState)

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
                       }

noBuilder :: Config -> Name -> Q Body
noBuilder _ _ = normalB [| return nullPtr |]

noWorker :: Name -> [Name] -> Q Body
noWorker fun as = normalB $ appsE $ map varE $ fun:as

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
    _ -> error $ unwords ["loadFun:",show (nameBase name)
                         ,"is not a function:",show info]

declareWorker :: Config -> Name -> Name -> [Name] -> Type -> [DecQ]
declareWorker conf@Config{..} wname name as typ =
    [ declareImport factory csig
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

declareImport :: Name -> TypeQ -> DecQ
declareImport name csig =
    forImpD cCall safe "dynamic" name [t|FunPtr $(csig) -> $(csig)|]

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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Feldspar.Plugin where

import Feldspar.Plugin.Generic
import Feldspar.Plugin.Utils
import Feldspar.Plugin.Marshal

import System.Plugins

import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Marshal (alloca,pokeArray)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)

import Control.Monad ((>=>), when, unless)

import Language.Haskell.TH

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Process (readProcessWithExitCode)
import System.Info (os)


-- Feldspar specific
import Feldspar.Compiler
import Feldspar.Compiler.Backend.C.Library (encodeFunctionName)

defaultConfig :: Config
defaultConfig = Config { declWorker   = declareWorker
                       , typeFromName = loadFunType >=> rewriteType
                       , prefix       = "c_"
                       , wdir         = "tmp"
                       , opts         = [ -- "-optc -DLOG"
                                        ]
                       }

loadFun :: Name -> Q [Dec]
loadFun = loadFunWithConfig defaultConfig


declareImport :: Name -> TypeQ -> DecQ
declareImport name csig =
    forImpD cCall safe "dynamic" name [t|FunPtr $(csig) -> $(csig)|]

declareWorker :: Config -> Name -> Name -> [Name] -> Type -> [DecQ]
declareWorker conf@Config{..} wname name as typ =
    [ declareImport factory csig
    , sigD bname [t| Ptr $(csig) |]
    , funD bname [clause [] (builder conf name) []]
    , sigD wname hsig
    , funD wname [clause (map varP as) (worker bname factory as csig) []]
    ]
  where
    base    = nameBase name
    bname   = mkName $ prefix ++ base ++ "_builder"
    factory = mkName $ prefix ++ base ++ "_factory"
    hsig    = buildHaskellType typ
    csig    = buildCType typ

worker :: Name -> Name -> [Name] -> Q Type -> Q Body
worker bname factory as csig = normalB
    [|do
        let fun = $(varE factory) $ castPtrToFunPtr $(varE bname)
        calloca $ \outPtr -> do
          $(appE (appsE ([|fun|] : map toRef as)) [|outPtr|])
          from =<< deref =<< peek outPtr
    |]
  where
    toRef name = [| ref $ to $(varE name) |]

calloca :: forall a b. Storable a => (Ptr a -> IO b) -> IO b
calloca f = do
    alloca $ \ptr -> do
      pokeArray (castPtr ptr) $ replicate (sizeOf ptr) (0::Word8)
      f ptr

builder :: Config -> Name -> Q Body
builder Config{..} fun = normalB
    [|unsafeLocalState $ do
        createDirectoryIfMissing True wdir
        $(varE 'compile) $(varE fun) basename base defaultOptions
        compileAndLoad basename opts
        lookupSymbol symbol
    |]
  where
    base     = nameBase fun
    basename = wdir ++ "/" ++ base
    symbol   = ldprefix ++ encodeFunctionName base
    ldprefix = case os of
                 "darwin" -> "_"
                 _        -> ""

compileAndLoad :: String -> [String] -> IO ()
compileAndLoad name opts = do
    let cname = name ++ ".c"
    let oname = name ++ ".o"
    exists <- doesFileExist oname
    when exists $ removeFile oname
    compileC cname oname opts
    initLinker
    _ <- loadRawObject oname
    resolveObjs $ error $ "Symbols in " ++ oname ++ " could not be resolved"

compileC :: String -> String -> [String] -> IO ()
compileC srcfile objfile opts = do
    let args = [ "-package feldspar-compiler"
               , "-optc -std=c99"
               , "-optc -Wall"
               , "-w"
               , "-c"
               ]
    (_,stdout,stderr) <- readProcessWithExitCode "ghc" (args ++ opts ++ ["-o",objfile,srcfile]) ""
    let output = stdout ++ stderr
    unless (null output) $ putStrLn output

lookupSymbol :: String -> IO (Ptr a)
lookupSymbol symbol = do
    mptr <- withCString symbol _lookupSymbol
    when (mptr == nullPtr) $ error $ "Symbol " ++ symbol ++ " not found"
    return mptr

foreign import ccall safe "lookupSymbol"
    _lookupSymbol :: CString -> IO (Ptr a)


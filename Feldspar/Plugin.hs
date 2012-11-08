{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feldspar.Plugin where

import Feldspar.Plugin.Generic
import Feldspar.Plugin.Utils
import Feldspar.Plugin.Marshal

import Debug.Trace

import System.Plugins

import Foreign.Ptr
import Foreign.Marshal (alloca)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(peek))
import Foreign.C.String (CString, withCString)

import Control.Monad ((>=>), when, unless)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Process (readProcessWithExitCode)


-- Feldspar specific
import Feldspar.Compiler.Internal (icompileWithInfos)
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Backend.C.Library (fixFunctionName)

defaultConfig = Config { declWorker   = declareWorker
                       , typeFromName = loadFunType >=> unData
                       , prefix       = "c_"
                       , opts         = [ "-package feldspar-compiler"
                                        , "-optc -std=c99"
                                        , "-c"
                                        , "-optc -Wall"
                                        , "-w"
                                        ]
                       }

loadFun = loadFunWithConfig defaultConfig


declareImport :: Name -> TypeQ -> DecQ
declareImport name typ = forImpD cCall safe "dynamic" name factoryType
  where
    factoryType = [t|FunPtr $(typ) -> $(typ)|]

declareWorker :: Name -> Name -> [Name] -> Type -> [String] -> [DecQ]
declareWorker wname name as typ opts =
    [ declareImport factory csig
    , funD bname [clause [] (builder name opts) []]
    , sigD wname hsig
    , funD wname [clause (varsP as) (worker bname factory as csig) []]
    ]
  where
    base    = nameBase name
    bname   = mkName $ "c_" ++ base ++ "_builder"
    factory = mkName $ "c_" ++ base ++ "_factory"
    varsP   = map varP
    hsig    = buildHaskellType typ
    csig    = buildCType typ

worker :: Name -> Name -> [Name] -> Q Type -> Q Body
worker bname factory as csig = normalB
    [|do
        let ptr               = $(varE bname)
        let funptr            = castPtrToFunPtr ptr :: FunPtr $(csig)
        let fun               = $(varE factory) funptr
        alloca $ \outPtr -> do
          $(appE (appsE ([|fun|] : map toRef as)) [|outPtr|])
          res <- peek outPtr
          from res
    |]
  where
    toRef name = appE (varE 'ref) $ appE (varE 'to) $ varE name

builder :: Name -> [String] -> Q Body
builder fun opts = let base = nameBase fun
                    in normalB
  [|unsafeLocalState $ do
      let wdir = "tmp"
      createDirectoryIfMissing True wdir
      let basename  = wdir ++ "/" ++ base
      let hfilename = basename ++ ".h"
      let cfilename = basename ++ ".c"
      let ofilename = basename ++ ".o"
      let pname     = fixFunctionName base
      let result    = $(varE 'icompileWithInfos) $(varE fun) base defaultOptions
      let header    = sctccrHeader result
      let source    = sctccrSource result
      writeFile hfilename $ sourceCode header
      writeFile cfilename $ unlines [ "#include \"" ++ base ++ ".h\"" -- TODO this should really be done by the compiler
                                    , sourceCode source
                                    ]
      compileAndLoad cfilename ofilename opts
      mptr <- withCString ("_" ++ pname) lookupSymbol
      when (mptr == nullPtr) $ error $ "Symbol " ++ pname ++ " not found"
      return mptr
  |]

compileAndLoad :: String -> String -> [String] -> IO ()
compileAndLoad cname oname opts = do
    initLinker
    exists <- doesFileExist oname
    when exists $ removeFile oname
    compileC cname oname opts
    loadRawObject oname
    resolveObjs $ error $ "Symbols in " ++ oname ++ " could not be resolved"

compileC :: String -> String -> [String] -> IO ()
compileC srcfile objfile opts = do
  (excode,stdout,stderr) <- readProcessWithExitCode "ghc" (opts ++ [srcfile]) ""
  let output = stdout ++ stderr
  unless (null output) $ putStrLn output

foreign import ccall safe "lookupSymbol"
   lookupSymbol :: CString -> IO (Ptr a)


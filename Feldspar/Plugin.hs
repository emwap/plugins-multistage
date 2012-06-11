{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Feldspar.Plugin where

import Feldspar.Plugin.Generic
import Feldspar.Plugin.Utils
import Feldspar.Plugin.Marshal

import Debug.Trace

import BasicTypes (succeeded)
import ObjLink (lookupSymbol, loadObj, resolveObjs, initObjLinker)

import Foreign.Ptr
import Foreign.Marshal ( Pool, withPool, unsafeLocalState)
import Foreign.Storable (Storable(peek))

import Control.Monad ((>=>), when, unless)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Process (readProcessWithExitCode)


-- Feldspar specific
import Feldspar.Compiler (icompileWithInfos)
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Backend.C.Library (fixFunctionName)

defaultConfig = Config { declWorker   = declareWorker
                       , typeFromName = loadFunType >=> unData
                       , prefix       = "c_"
                       }

loadFun = loadFunWithConfig defaultConfig


declareImport :: Name -> TypeQ -> DecQ
declareImport name typ = forImpD cCall safe "dynamic" name factoryType
  where
    factoryType = [t|FunPtr $(typ) -> $(typ)|]

declareWorker :: Name -> Name -> [Name] -> Type -> [DecQ]
declareWorker wname name as typ =
    [ declareImport factory csig
    , funD bname [clause [] (builder name) []]
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
        outPtr  <- allocate
        $(appE (appsE ([|fun|] : map toRef as)) [|outPtr|])
        res <- peek outPtr
        from res
    |]
  where
    toRef name = appE (varE 'ref) $ appE (varE 'to) $ varE name

builder :: Name -> Q Body
builder fun = let base = nameBase fun
               in normalB
  [|unsafeLocalState $ do
      let wdir = "tmp"
      createDirectoryIfMissing True wdir
      let basename  = wdir ++ "/" ++ base
      let hfilename = basename ++ ".h"
      let cfilename = basename ++ ".c"
      let ofilename = basename ++ ".o"
      let pname     = fixFunctionName base
      let result    = icompileWithInfos $(varE fun) base defaultOptions
      let header    = sctccrHeader result
      let source    = sctccrSource result
      writeFile hfilename $ sourceCode header
      writeFile cfilename $ unlines [ "#include \"" ++ base ++ ".h\"" -- TODO this should really be done by the compiler
                                    , sourceCode source
                                    ]
      compileAndLoad cfilename ofilename
      mptr <- lookupSymbol pname
      case mptr of
        Just p  -> return p
        Nothing -> error $ "Symbol " ++ pname ++ " not found"
  |]

compileAndLoad :: String -> String -> IO ()
compileAndLoad cname oname = do
    exists <- doesFileExist oname
    when exists $ removeFile oname
    compileC cname oname
    loadObj oname
    res <- resolveObjs
    unless (succeeded res) $ error $ "Symbols in " ++ oname ++ " could not be resolved"

compileC srcfile objfile = do
  (excode,stdout,stderr) <- readProcessWithExitCode "ghc"
    ["-package feldspar-compiler", "-optc -std=c99", "-c", "-optc -Wall", "-w", srcfile] ""
  let output = stdout ++ stderr
  unless (null output) $ putStrLn output


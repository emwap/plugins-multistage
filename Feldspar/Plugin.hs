--
-- Copyright (c) 2012, Anders Persson
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright notice, 
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the author nor the names of contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

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


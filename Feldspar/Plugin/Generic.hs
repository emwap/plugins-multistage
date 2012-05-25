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

module Feldspar.Plugin.Generic
  ( loadFunWithConfig
  , loadFunType
  , Config(..)
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (sequenceQ)

import Foreign.Marshal (unsafeLocalState)

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



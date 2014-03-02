{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Type rewriting for Feldspar programs
module Feldspar.Compiler.CallConv
  ( rewriteType
  , expandTF
  , buildHaskellType
  , buildCType
  )
  where


import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns (expandSyns)

import Foreign.Ptr (Ptr)

import Feldspar (Syntactic(..))
import Feldspar.Plugin.TypeRewrite
import Foreign.Marshal.Class (Reference(..), Marshal(..))

-- | Normalize the type (expand type synonyms and type families)
rewriteType :: Type -> Q Type
rewriteType t = rewriteSyntactic t >>= expandTF

rewriteSyntactic :: Type -> Q Type
rewriteSyntactic typ = expandSyns typ >>= go
  where
    go t@(AppT c@(ConT _) x) = do
      inst <- isInstance ''Syntactic [t]
      if inst
        then appT [t|Internal|] (return t)
        else appT (return c) (go x)
    go (AppT t1 t2) = appT (go t1) (go t2)
    go t = return t

haskellCC :: CallConv
haskellCC = CallConv { arg  = return
                     , res  = appT [t|IO|] . return
                     }

feldsparCC :: CallConv
feldsparCC = CallConv { arg = \t -> [t| $(conv t) |]
                      , res = \t -> [t| Ptr $(conv t) -> IO () |]
                      }
  where
    conv t = [t| Ref (Rep $(return t)) |]

-- | Construct the corresponding Haskell type of a foreign Feldspar
-- function
--
-- > prog1 :: Data Index -> Vector1 Index
-- >
-- > sigD (mkName "h_prog1") $ loadFunType 'prog1 >>= rewriteType >>= buildHaskellType
--
-- becomes
--
-- > h_prog1 :: Index -> IO [Index]
--
buildHaskellType :: Type -> Q Type
buildHaskellType = buildType haskellCC

-- | Construct the corresponding C type of a compiled Feldspar function
--
-- > sigD (mkName "c_prog1_fun") $ loadFunType 'prog1 >>= rewriteType
--                                                    >>= buildCType
--
-- becomes
--
-- > c_prog1_fun :: Word32 -> Ptr (SA Word32) -> IO ()
--
buildCType :: Type -> Q Type
buildCType typ = buildType feldsparCC typ


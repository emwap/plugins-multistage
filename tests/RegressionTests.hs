{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import Data.Word

import System.Plugins.MultiStage

prog0 :: Word32 -> Word32 -> Word32
prog0 = (+)

loadFunWithConfig defaultConfig{ wdir="tests",builder=defaultBuilder } ['prog0 ]
loadFunWithConfig defaultConfig{ wdir="tests",builder=defaultBuilder,suffix="_suf"} ['prog0 ]

prop_prog0 :: Word32 -> Word32 -> Property
prop_prog0 x y = prog0 x y === c_prog0 x y

prop_prog0_suf :: Word32 -> Word32 -> Property
prop_prog0_suf x y = prog0 x y === c_prog0_suf x y

-- prog1 :: Vector1 Index -> Vector1 Index
-- prog1 = id

-- loadFunWithConfig defaultConfig ['prog1 ]

-- prog2 :: Vector1 Index -> PushVector1 Index
-- prog2 v = let pv = toPush v in pv PV.++ pv

-- loadFun ['prog2 ]

-- prop_prog1 :: Property
-- prop_prog1 = eval prog1 === c_prog1

-- prop_prog2 :: NonEmptyList WordN -> Property
-- prop_prog2 (NonEmpty xs) = eval prog2 xs === c_prog2 xs

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests

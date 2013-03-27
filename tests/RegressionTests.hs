{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Feldspar
import Feldspar.Vector
import Feldspar.Vector.Push as PV
import Feldspar.Plugin


prog0 :: Data Index -> Data Index -> Data Index
prog0 = (+)

loadFun 'prog0

prog1 :: Vector1 Index -> Vector1 Index
prog1 = id

loadFun 'prog1

prog2 :: Vector1 Index -> PushVector1 Index
prog2 v = let pv = toPush v in pv PV.++ pv

loadFun 'prog2

prop_prog0 = eval prog0 === c_prog0
prop_prog1 = eval prog1 === c_prog1
prop_prog2 (NonEmpty xs) = eval prog2 xs === c_prog2 xs


tests = $(testGroupGenerator)

main = defaultMain [tests]

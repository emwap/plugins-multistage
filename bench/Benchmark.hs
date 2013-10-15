{-# LANGUAGE TemplateHaskell #-}

import Feldspar
import Feldspar.Vector
import Feldspar.Plugin
import Feldspar.Algorithm.CRC

import Criterion.Main

testdata :: [Word8]
testdata = Prelude.take 1024 $ cycle [1,2,3,4]

naive :: Vector1 Word8 -> Data Word16
naive = crcNaive 0x8005 0

normal :: Vector1 Word8 -> Data Word16
normal v = share (makeCrcTable 0x8005) $ \t -> crcNormal t 0 v

h_naive = eval naive
loadFun 'naive

h_normal = eval normal
loadFun 'normal

main = defaultMain
    [ bgroup "warmup"
        [ bench "c_navie"  $ nf c_naive [1,2,3,4]
        , bench "c_normal" $ nf c_normal [1,2,3,4]
        ]
    , bgroup "naive"
        [ bench "h_naive" $ nf h_naive testdata
        , bench "c_naive" $ nf c_naive testdata
        ]
    , bgroup "normal"
        [ bench "h_normal" $ nf h_normal testdata
        , bench "c_normal" $ nf c_normal testdata
        ]
    ]


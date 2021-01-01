{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

import Data.Text hiding (unpack)
import Data.Text.IO (readFile)

import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (readFile)

import Language.LEF.Parser



main :: IO ()
main = defaultMain lefs


lefs :: TestTree
lefs = testGroup "LEF"
  [ ibms
  ]


ibms :: TestTree
ibms = testGroup "ibm-place2"
  [ ibm_place2 "ibm01" ibm01_lef
  , ibm_place2 "ibm02" ibm02_lef
  , ibm_place2 "ibm07" ibm07_lef
  , ibm_place2 "ibm08" ibm08_lef
  , ibm_place2 "ibm09" ibm09_lef
  , ibm_place2 "ibm10" ibm10_lef
  , ibm_place2 "ibm11" ibm11_lef
  , ibm_place2 "ibm12" ibm12_lef
  ]


ibm_place2 :: String -> IO Text -> TestTree
ibm_place2 s k = testCase s $ either (fail . show) (void . pure) . parseLEF =<< k



ibm01_lef, ibm02_lef, ibm07_lef, ibm08_lef, ibm09_lef, ibm10_lef, ibm11_lef, ibm12_lef :: IO Text
ibm01_lef = readFile "sample/ibm-place2/ibm01/ibm01.lef"
ibm02_lef = readFile "sample/ibm-place2/ibm02/ibm02-5l.lef"
ibm07_lef = readFile "sample/ibm-place2/ibm07/ibm07-5l.lef"
ibm08_lef = readFile "sample/ibm-place2/ibm08/ibm08-5l.lef"
ibm09_lef = readFile "sample/ibm-place2/ibm09/ibm09-5l.lef"
ibm10_lef = readFile "sample/ibm-place2/ibm10/ibm10-5l.lef"
ibm11_lef = readFile "sample/ibm-place2/ibm11/ibm11-5l.lef"
ibm12_lef = readFile "sample/ibm-place2/ibm12/ibm12-5l.lef"


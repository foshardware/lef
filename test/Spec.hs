{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

import Data.FileEmbed
import Data.Text hiding (unpack)
import Data.Text.Encoding

import Test.Tasty
import Test.Tasty.HUnit

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


ibm_place2 :: String -> Text -> TestTree
ibm_place2 s = testCase s . either (fail . show) (void . pure) . parseLEF



ibm01_lef, ibm02_lef, ibm07_lef, ibm08_lef, ibm09_lef, ibm10_lef, ibm11_lef, ibm12_lef :: Text
ibm01_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm01/ibm01.lef")
ibm02_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm02/ibm02-5l.lef")
ibm07_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm07/ibm07-5l.lef")
ibm08_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm08/ibm08-5l.lef")
ibm09_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm09/ibm09-5l.lef")
ibm10_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm10/ibm10-5l.lef")
ibm11_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm11/ibm11-5l.lef")
ibm12_lef = decodeUtf8 $(embedFile "sample/ibm-place2/ibm12/ibm12-5l.lef")


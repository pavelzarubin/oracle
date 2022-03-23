module Main where

import Spec.Trace
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "oracle tests"
      [ startUpdateGetTest,
        badUpdateTest,
        getNotExistOracleTest
      ]

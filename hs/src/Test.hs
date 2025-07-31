{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test () where

import Main (app, server)
import Rapid
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai as Test

-- $> main
main :: IO ()
main =
  rapid 0 \r ->
    restart r "http" server

tasty :: TestTree -> IO ()
tasty action =
  bracket
    (hGetBuffering stdout)
    (hSetBuffering stdout)
    (const $ defaultMain action)

-- $> tasty tests
tests ::TestTree
tests = testGroup "Tasty.Wai Tests"
  [ testWai app "Hello World" do
      resp <- Test.get "/"
      assertStatus 200 resp
      assertBodyContains "Welcome" resp
  , testWai app "Not found" do
      resp <- Test.get "/notfound"
      assertStatus 404 resp
      assertBodyContains "Not found" resp
  ]

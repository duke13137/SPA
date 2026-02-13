{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test (tasty, testRoute, testDB) where
import Main hiding (main)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai hiding (Session)
import Test.Tasty.Wai qualified as Test

import Data.IntMap.Strict qualified as IntMap
import Database
import Hasql.Pool qualified as Pool
import Hasql.Session
import Hasql.Statement
import Hasql.TH
import System.IO.Unsafe (unsafePerformIO)

import Web.Hyperbole qualified as H

tasty :: TestTree -> IO ()
tasty action =
  bracket
    (hGetBuffering stdout)
    (hSetBuffering stdout)
    (const $ defaultMain action)

-- $> tasty testRoute
testRoute :: TestTree
testRoute = testGroup "Tasty.Wai Tests"
  [ testWai mockApp "Hello World" do
      resp <- Test.get "/"
      assertStatus 200 resp
      assertBodyContains "Welcome" resp
  , testWai mockApp "Not found" do
      resp <- Test.get "/notfound"
      assertStatus 404 resp
      assertBodyContains "Not found" resp
  ]

mockApp :: H.Application
mockApp = unsafePerformIO do
  todos <- newTVarIO IntMap.empty
  nextId <- newTVarIO (1 :: Int)
  pure $ app todos nextId
{-# NOINLINE mockApp #-}

-- $> tasty testDB
testDB :: TestTree
testDB = testGroup "Database Tests"
  [ testCase "runDB: sumAndDivMod 3 8 3" do
      result <- runDB (sumAndDivModSession 3 8 3)
      result @?= Right (Right (3, 2))
  , testCase "withPool: sumAndDivMod 30 8 3" do
      result <- withPool \pool ->
        Pool.use pool (sumAndDivModSession 30 8 3)
      result @?= Right (12, 2)
  ]

sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  sumOfAAndB <- statement (a, b) sumStatement
  statement (sumOfAAndB, c) divModStatement

sumStatement :: Statement (Int64, Int64) Int64
sumStatement =
  [singletonStatement|
    select ($1 :: int8 + $2 :: int8) :: int8
  |]

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement =
  [singletonStatement|
    select
      (($1 :: int8) / ($2 :: int8)) :: int8,
      (($1 :: int8) % ($2 :: int8)) :: int8
  |]


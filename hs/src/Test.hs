{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2024               #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Test (tasty, testRoute, testDB) where

import Data.List (isInfixOf)
import Database
import Hasql.TH
import Network.HTTP.Types.Method (StdMethod (..))
import Network.Wai
import Network.Wai.Test qualified as WaiTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai hiding (Session, head)
import Test.Tasty.Wai qualified as Test

import Main (app)
import Todo

tasty :: TestTree -> IO ()
tasty action =
  bracket
    (hGetBuffering stdout)
    (hSetBuffering stdout)
    (const $ defaultMain action)

-- $> tasty testRoute
testRoute :: TestTree
testRoute = withResource acquirePool releasePool \getPool ->
  testGroup "Tasty.Wai Tests"
  [ testWai (appWithPool getPool) "Hello World" do
      resp <- Test.get "/"
      assertStatus 200 resp
      assertBodyContains "Welcome" resp
  , testWai (appWithPool getPool) "Not found" do
      resp <- Test.get "/notfound"
      assertStatus 404 resp
      assertBodyContains "Not found" resp
  , testWai (appWithPool getPool) "GET /todos" do
      resp <- Test.get "/todos"
      assertStatus 200 resp
      assertBodyContains "Todos" resp
      assertBodyContains "What needs to be done?" resp
  , testWai (appWithPool getPool) "UI Todo CRUD" do
      pool <- liftIO getPool
      _ <- liftIO $ runDb pool truncateTodosSession

      -- 1. Create
      _ <- Test.postWithHeaders "/todos" "title=Buy+milk" [("Content-Type", "application/x-www-form-urlencoded")]
      respList <- Test.get "/todos/list"
      assertStatus 200 respList
      assertBodyContains "Buy milk" respList

      -- Get the inserted task ID
      Right ts <- liftIO $ runDb pool getTodosSession
      let firstId = case ts of
                      (t:_) -> t.id
                      _     -> error "Expected list with elements"
      let idStr = encodeUtf8 (show firstId :: Text)

      -- 2. Edit Form
      let editPath = "/todos/" <> idStr <> "/edit"
      respEdit <- Test.get editPath
      assertStatus 200 respEdit
      assertBodyContains "Buy milk" respEdit
      assertBodyContains "Save" respEdit

      -- 3. Update
      let updatePath = "/todos/" <> idStr
      respUpdate <- Test.srequest $ Test.buildRequestWithHeaders PUT updatePath "title=Buy+water" [("Content-Type", "application/x-www-form-urlencoded")]
      assertStatus 200 respUpdate
      assertBodyContains "Buy water" respUpdate

      -- 4. Delete
      let deletePath = "/todos/" <> idStr
      respDelete <- Test.srequest $ Test.buildRequestWithHeaders DELETE deletePath "" []
      assertStatus 200 respDelete

      -- Verify deletion
      respList2 <- Test.get "/todos/list"
      assertStatus 200 respList2

      -- 5. Create multiple, toggle, search, clear
      _ <- Test.postWithHeaders "/todos" "title=Task+A" [("Content-Type", "application/x-www-form-urlencoded")]
      _ <- Test.postWithHeaders "/todos" "title=Task+B" [("Content-Type", "application/x-www-form-urlencoded")]
      _ <- Test.postWithHeaders "/todos" "title=Task+C" [("Content-Type", "application/x-www-form-urlencoded")]

      -- Search for "Task A"
      respSearch <- Test.get "/todos/list?search=Task+A"
      assertStatus 200 respSearch
      assertBodyContains "Task A" respSearch
      let bodyStr = decodeUtf8 (simpleBody respSearch)
      _ <- liftIO $ assertBool "should not contain Task B" $ not $ "Task B" `isInfixOf` bodyStr

      -- Get all IDs and toggle first two
      Right allTodos <- liftIO $ runDb pool getTodosSession
      let allIds = map (.id) allTodos
      case allIds of
        (a:b:_) -> do
          let aStr = encodeUtf8 (show a :: Text)
          let bStr = encodeUtf8 (show b :: Text)
          _ <- Test.srequest $ Test.buildRequestWithHeaders PATCH ("/todos/" <> aStr) "" []
          _ <- Test.srequest $ Test.buildRequestWithHeaders PATCH ("/todos/" <> bStr) "" []
          pass
        _ -> liftIO $ assertFailure "Expected at least 2 todos"

      -- Verify 2 completed in DB
      Right toggled <- liftIO $ runDb pool getTodosSession
      let doneCount = length $ filter (.completed) toggled
      liftIO $ doneCount @?= 2

      -- Filter active
      respActive <- Test.get "/todos/list?filter=active"
      assertStatus 200 respActive
      assertBodyContains "Task C" respActive

      -- Clear completed
      respClear <- Test.postWithHeaders "/todos/clear" "" [("Content-Type", "application/x-www-form-urlencoded")]
      assertStatus 200 respClear

      -- Verify only 1 incomplete remains
      Right remaining <- liftIO $ runDb pool getTodosSession
      liftIO $ length remaining @?= 1
  ]

appWithPool :: IO Pool -> Application
appWithPool getPool req respond = do
  pool <- getPool
  app pool req respond

truncateTodosSession :: Session ()
truncateTodosSession =
  statement ()
    [resultlessStatement|
      delete from todos
    |]

-- $> tasty testDB
testDB :: TestTree
testDB = withResource acquirePool releasePool $ \getPool ->
  testGroup "Database Tests"
    [ testCase "Todo CRUD" do
        pool <- getPool
        -- clear before test
        _ <- runDb pool truncateTodosSession

        -- Insert
        _ <- runDb pool (addTodoSession "Test Task 1")
        _ <- runDb pool (addTodoSession "Test Task 2")

        -- Verify Insert
        todos1 <- runDb pool getTodosSession
        case todos1 of
          Right ts -> do
             length ts @?= 2
             case ts of
                (firstTodo:_) -> firstTodo.title @?= "Test Task 1"
                _             -> assertFailure "Expected list with elements"
          Left err -> assertFailure $ "DB Error: " ++ show err

        -- Complete a task
        let todos1' = fromRight [] todos1
        case todos1' of
            (firstTodo:_) -> do
                _ <- runDb pool (toggleTodoSession firstTodo.id)

                todos2 <- runDb pool getTodosSession
                case todos2 of
                    Right ts2 -> case ts2 of
                        (t2:_) -> t2.completed @?= True
                        _      -> assertFailure "Expected list"
                    Left err -> assertFailure $ "DB Error: " ++ show err
            _ -> pass

        -- Clear completed
        _ <- runDb pool clearCompletedSession

        todos3 <- runDb pool getTodosSession
        case todos3 of
          Right ts -> length ts @?= 1
          Left err -> assertFailure $ "DB Error: " ++ show err
    ]

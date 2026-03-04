{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module HyperTodo where

import Prelude

import Web.Atomic.CSS hiding (All)
import Web.Hyperbole as H

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T

import Effectful.State.Dynamic qualified as State

data Todo = Todo { title :: T.Text, completed :: Bool }
  deriving (Generic, Show, Read)

data TodoApp = TodoApp
  deriving (Generic, Show, Read)

instance H.ViewId TodoApp where
  type ViewState TodoApp = IntMap.IntMap Todo

hyperTodo :: H.Page es '[TodoApp]
hyperTodo = do
  pure $ H.hyperState TodoApp IntMap.empty todoView

todoView :: H.View TodoApp ()
todoView = do
  items <- H.viewState
  let matched = IntMap.toList items
      activeCount = IntMap.size $ IntMap.filter (not . (.completed)) items
      completedCount = IntMap.size $ IntMap.filter (.completed) items
      activeCountText = show activeCount <> if activeCount == 1 then " item left" else " items left"
  H.div do
    H.form (AddTodo "") do
      H.input "title" & H.placeholder "What needs to be done?"
      H.submit "Add"

    H.ul do
      forM_ matched \(i, todo) -> H.li do
        H.button (ToggleTodo i) do
          if todo.completed then "☑" else "☐"
        H.text todo.title
        H.button (DeleteTodo i) "✕"

    H.text activeCountText

    when (completedCount > 0) $
      H.button ClearCompletedTodo $ H.text "Clear completed"

instance H.HyperView TodoApp es where
  data Action TodoApp
    = AddTodo T.Text
    | ToggleTodo Int
    | DeleteTodo Int
    | ClearCompletedTodo
    deriving (Generic, Show, Read, H.ViewAction)

  update (AddTodo title) = do
    items <- State.get @(IntMap.IntMap Todo)
    let trimmed = T.strip title
        nextTodoId = maybe 1 ((+ 1) . fst) (IntMap.lookupMax items)
    when (not (T.null trimmed)) do
      let newItems = IntMap.insert nextTodoId (Todo trimmed False) items
      State.put newItems
    pure todoView

  update (ToggleTodo i) = do
    items <- State.get @(IntMap.IntMap Todo)
    let newItems = IntMap.adjust (\t -> Todo t.title (not t.completed)) i items
    State.put newItems
    pure todoView

  update (DeleteTodo i) = do
    items <- State.get @(IntMap.IntMap Todo)
    let newItems = IntMap.delete i items
    State.put newItems
    pure todoView

  update ClearCompletedTodo = do
    items <- State.get @(IntMap.IntMap Todo)
    let newItems = IntMap.filter (not . (.completed)) items
    State.put newItems
    pure todoView

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
module Main where

import Optics
import Prelude

import Debug.Breakpoint
import Rapid

import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.UrlMap qualified as Wai
import Web.Twain as Twain

import Web.Atomic.CSS
import Web.Hyperbole as H

import Data.IntMap.Strict qualified as IntMap

import Colog
import Database
import Htmx

-- Todo types
data Todo = Todo { title :: Text, completed :: Bool }
type Todos = TVar (IntMap Todo)
type NextId = TVar Int

-- $> main
main :: IO ()
main = do
  todos <- newTVarIO IntMap.empty
  nextId <- newTVarIO (1 :: Int)
  rapid 0 \r -> restart r "server" $
    Wai.run 8080 (app todos nextId)

app :: Todos -> NextId -> Application
app todos nextId = Wai.mapUrls $
        Wai.mount "hyper" hyperApp
    <|> Wai.mountRoot (foldr ($) (Twain.notFound page404) (routes todos nextId))

hyperApp :: Application
hyperApp  = H.liveApp H.quickStartDocument (H.runPage hyperView)

routes :: Todos -> NextId -> [Middleware]
routes todos nextId =
  [ Twain.get "/" index
  , Twain.get "/hello/:name" echoName
  ] <> todoRoutes todos nextId

htmx :: Html () -> Html ()
htmx body = [hsx|
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>My Simple HTML Page</title>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">
      <script src="https://cdn.jsdelivr.net/npm/htmx.org@4.0.0-alpha6/dist/htmx.js"></script>
    </head>
    <body>
      <main class="container">
      {body}
      </main>
    </body>
  </html>
|]

logger :: MonadIO m => LoggerT Message m a -> m a
logger = usingLoggerT $ cmap fmtMessage logTextStdout

render :: Html () -> ResponderM a
render = Twain.send . Twain.html . renderBS

index :: ResponderM a
index = render $ htmx [hsx|
    <button hx-get="/hello/world" hx-target="#hello">
        Welcome
    </button>
    <h1 id="hello"></h1>
|]

echoName :: ResponderM a
echoName = logger do
  who <- lift $ Twain.param @Text "name"
  logInfo $ "name: " <> who
  -- breakpointM
  lift $ render [hsx|
    <h1 id="hello">Hello, {who}!</h1>
  |]

page404 :: ResponderM a
page404 = send $ Twain.html "<h1>Not found...</h1>"

-- Todo routes
todoRoutes :: Todos -> NextId -> [Middleware]
todoRoutes todos nextId =
  [ Twain.get    "/todos"       (getTodosPage todos)
  , Twain.get    "/todos/list"  (getTodoListPartial todos)
  , Twain.post   "/todos"       (addTodo todos nextId)
  , Twain.post   "/todos/clear" (clearCompleted todos)
  , Twain.patch  "/todos/:id"   (toggleTodo todos)
  , Twain.delete "/todos/:id"   (deleteTodo todos)
  ]

-- Todo handlers
getTodosPage :: Todos -> ResponderM a
getTodosPage todos = do
  items <- liftIO $ readTVarIO todos
  render $ todoPage items "all"

getTodoListPartial :: Todos -> ResponderM a
getTodoListPartial todos = do
  items <- liftIO $ readTVarIO todos
  filter_ <- Twain.paramMaybe @Text "filter"
  render $ todoListSection items (fromMaybe "all" filter_)

addTodo :: Todos -> NextId -> ResponderM a
addTodo todos nextId = do
  title' <- Twain.param @Text "title"
  liftIO $ atomically do
    i <- readTVar nextId
    modifyTVar' nextId (+ 1)
    modifyTVar' todos (IntMap.insert i (Todo title' False))
  items <- liftIO $ readTVarIO todos
  render do
    todoListSection items "all"
    [hsx|<input id="todo-input" name="title" placeholder="What needs to be done?" required autofocus hx-swap-oob="true">|]

toggleTodo :: Todos -> ResponderM a
toggleTodo todos = do
  i <- Twain.param @Int "id"
  liftIO $ atomically $
    modifyTVar' todos (IntMap.adjust (\t -> Todo t.title (not t.completed)) i)
  items <- liftIO $ readTVarIO todos
  render $ todoListSection items "all"

deleteTodo :: Todos -> ResponderM a
deleteTodo todos = do
  i <- Twain.param @Int "id"
  liftIO $ atomically $
    modifyTVar' todos (IntMap.delete i)
  items <- liftIO $ readTVarIO todos
  render $ todoListSection items "all"

clearCompleted :: Todos -> ResponderM a
clearCompleted todos = do
  liftIO $ atomically $
    modifyTVar' todos (IntMap.filter (not . (.completed)))
  items <- liftIO $ readTVarIO todos
  render $ todoListSection items "all"

-- Todo views
todoPage :: IntMap Todo -> Text -> Html ()
todoPage items filterBy = htmx [hsx|
  <article>
    <h1>Todos</h1>
    <form hx-post="/todos" hx-target="#todo-list">
      <fieldset role="group">
        <input id="todo-input" name="title" placeholder="What needs to be done?" required>
        <button type="submit">Add</button>
      </fieldset>
    </form>
    <nav>
      <ul>
        <li><a href="#" hx-get="/todos/list?filter=all" hx-target="#todo-list">All</a></li>
        <li><a href="#" hx-get="/todos/list?filter=active" hx-target="#todo-list">Active</a></li>
        <li><a href="#" hx-get="/todos/list?filter=completed" hx-target="#todo-list">Completed</a></li>
      </ul>
    </nav>
    {todoListSection items filterBy}
  </article>
|]

todoListSection :: IntMap Todo -> Text -> Html ()
todoListSection items filterBy = [hsx|
  <section id="todo-list">
    <ul style="list-style: none; padding: 0;">
      {mapM_ (uncurry todoItem) filterby}
    </ul>
    <footer style="display: flex; justify-content: space-between; align-items: center;">
      <small>{activeCountText}</small>
      {clearButton}
    </footer>
  </section>
|]
  where
    filterby = case filterBy of
      "active"    -> IntMap.toList $ IntMap.filter (not . (.completed)) items
      "completed" -> IntMap.toList $ IntMap.filter (.completed) items
      _           -> IntMap.toList items
    activeCount = IntMap.size $ IntMap.filter (not . (.completed)) items
    activeCountText :: Text
    activeCountText = show activeCount <> " item" <> (if activeCount == 1 then "" else "s") <> " left"
    completedCount = IntMap.size $ IntMap.filter (.completed) items
    completedCountText :: Text
    completedCountText = show completedCount
    clearButton :: Html ()
    clearButton
      | completedCount > 0 = [hsx|
          <button class="outline" hx-post="/todos/clear" hx-target="#todo-list">
            Clear completed ({completedCountText})
          </button>
        |]
      | otherwise = mempty

todoItem :: Int -> Todo -> Html ()
todoItem i todo = [hsx|
  <li style="display: flex; align-items: center; gap: 0.5rem; padding: 0.5rem 0; border-bottom: 1px solid var(--pico-muted-border-color);">
    {completed}
    {titleHtml}
    <button class="outline secondary" hx-delete={deletePath} hx-target="#todo-list" hx-confirm="Delete this todo?"
            style="margin-left: auto; width: auto; padding: 0.25rem 0.5rem; margin-bottom: 0;">
      ✕
    </button>
  </li>
|]
  where
    patchPath = "/todos/" <> show i :: Text
    deletePath = "/todos/" <> show i :: Text
    completed :: Html ()
    completed
      | todo.completed = [hsx|<input type="checkbox" checked hx-patch={patchPath} hx-target="#todo-list" style="margin-bottom: 0;">|]
      | otherwise      = [hsx|<input type="checkbox" hx-patch={patchPath} hx-target="#todo-list" style="margin-bottom: 0;">|]
    titleHtml :: Html ()
    titleHtml
      | todo.completed = [hsx|<s style="opacity: 0.5;">{todo.title}</s>|]
      | otherwise      = [hsx|<span>{todo.title}</span>|]

hyperView :: Page es '[Event]
hyperView = do
  pure $ do
    hyper Event1 $ hyperButton "Hello"
    hyper Event2 $ hyperButton "World!"

data Event = Event1 | Event2
  deriving (Generic, ViewId)

instance HyperView Event es where
  data Action Event
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    pure $ hyperButton new

hyperButton :: Text -> View Event ()
hyperButton msg = do
  button (Louder msg) (H.text msg)

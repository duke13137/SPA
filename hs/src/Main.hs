{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2024               #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

import Optics
import Prelude hiding (Handler)

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T

import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp qualified as Wai
import Network.Wai.UrlMap qualified as Wai
import Servant.API
import Servant.Server
import Web.Twain as Twain

import Rapid

import Colog
import Database
import Htmx

-- $> main
main :: IO ()
main = do
  todos  <- newTVarIO IntMap.empty
  nextId <- newTVarIO (1 :: Int)
  bracket acquirePool releasePool \pool ->
    rapid 0 \r -> restart r "server" $
      Wai.run 8080 (app pool todos nextId)

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

type HelloAPI = "hello" :> Capture "name" Text :> Get '[HTML] (Html ())

helloServer :: Pool -> Server HelloAPI
helloServer = helloHandler

helloHandler :: Pool -> Text -> Handler (Html ())
helloHandler _pool name = pure [hsx|<h1 id="hello">Hello, {name}!</h1>|]

htmxApp :: Pool -> Application
htmxApp pool = serve (Proxy @HelloAPI) (helloServer pool)

-- Todo types
data Todo = Todo { title :: Text, completed :: Bool }
type Todos  = TVar (IntMap Todo)
type NextId = TVar Int

app :: Pool -> Todos -> NextId -> Application
app pool todos nextId = Wai.mapUrls $
        Wai.mount "htmx" (htmxApp pool)
    <|> Wai.mountRoot (foldr ($) (Twain.notFound page404) (routes pool todos nextId))

routes :: Pool -> Todos -> NextId -> [Middleware]
routes pool todos nextId =
  [ Twain.get "/" index
  ] <> todoRoutes pool todos nextId

htmx :: Html () -> Html ()
htmx body = [hsx|
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>My Simple HTML Page</title>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css">
      <script defer src="https://cdn.jsdelivr.net/npm/htmx.org@next"></script>
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
index  = render $ htmx [hsx|
    <button hx-get="/htmx/hello/world" hx-target="#hello">
        Welcome
    </button>
    <h1 id="hello"></h1>
|]

page404 :: ResponderM a
page404 = send $ Twain.html "<h1>Not found...</h1>"

-- Todo routes
todoRoutes :: Pool -> Todos -> NextId -> [Middleware]
todoRoutes pool todos nextId =
  [ Twain.get    "/todos"       (getTodosPage pool todos)
  , Twain.get    "/todos/list"  (getTodoListPartial pool todos)
  , Twain.post   "/todos"       (addTodo pool todos nextId)
  , Twain.post   "/todos/clear" (clearCompleted pool todos)
  , Twain.patch  "/todos/:id"   (toggleTodo pool todos)
  , Twain.delete "/todos/:id"   (deleteTodo pool todos)
  ]

-- Todo handlers
getTodosPage :: Pool -> Todos -> ResponderM a
getTodosPage _pool todos = do
  items <- readTVarIO todos
  render $ todoPage items "all"

getTodoListPartial :: Pool -> Todos -> ResponderM a
getTodoListPartial _pool todos = do
  items   <- readTVarIO todos
  filter_ <- Twain.paramMaybe @Text "filter"
  search_ <- Twain.paramMaybe @Text "search"
  render $ todoListSection items (fromMaybe "" search_) (fromMaybe "all" filter_)

addTodo :: Pool -> Todos -> NextId -> ResponderM a
addTodo _pool todos nextId = do
  title' <- Twain.param @Text "title"
  items <- atomically do
    i <- readTVar nextId
    modifyTVar' nextId (+ 1)
    modifyTVar' todos (IntMap.insert i (Todo title' False))
    readTVar todos
  render do
    todoListSection items "" "all"
    [hsx|<input id="todo-input" name="title" placeholder="What needs to be done?" required autofocus hx-swap-oob="true">|]

toggleTodo :: Pool -> Todos -> ResponderM a
toggleTodo _pool todos = do
  i <- Twain.param @Int "id"
  items <- atomically $ do
    modifyTVar' todos (IntMap.adjust (\t -> Todo t.title (not t.completed)) i)
    readTVar todos
  render $ todoListSection items "" "all"

deleteTodo :: Pool -> Todos -> ResponderM a
deleteTodo _pool todos = do
  i <- Twain.param @Int "id"
  items <- atomically $ do
    modifyTVar' todos (IntMap.delete i)
    readTVar todos
  render $ todoListSection items "" "all"

clearCompleted :: Pool -> Todos -> ResponderM a
clearCompleted _pool todos = do
  items <- atomically $ do
    modifyTVar' todos (IntMap.filter (not . (.completed)))
    readTVar todos
  render $ todoListSection items "" "all"

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
    <input type="search" name="search"
           hx-get="/todos/list" hx-trigger="input changed delay:300ms, search"
           hx-target="#todo-list" placeholder="Search todos...">
    <nav>
      <ul>
        <li><a href="#" hx-get="/todos/list?filter=all" hx-target="#todo-list">All</a></li>
        <li><a href="#" hx-get="/todos/list?filter=active" hx-target="#todo-list">Active</a></li>
        <li><a href="#" hx-get="/todos/list?filter=completed" hx-target="#todo-list">Completed</a></li>
      </ul>
    </nav>
    {todoListSection items "" filterBy}
  </article>
|]

todoListSection :: IntMap Todo -> Text -> Text -> Html ()
todoListSection items searchQ filterBy = [hsx|
  <section id="todo-list">
    <ul style="list-style: none; padding: 0;">
      {mapM_ (uncurry todoItem) matched}
    </ul>
    <footer style="display: flex; justify-content: space-between; align-items: center;">
      <small>{activeCountText}</small>
      {clearButton}
    </footer>
  </section>
|]
  where
    searched
      | T.null searchQ = items
      | otherwise      = IntMap.filter (\t -> T.toLower searchQ `T.isInfixOf` T.toLower t.title) items
    matched = case filterBy of
      "active"    -> IntMap.toList $ IntMap.filter (not . (.completed)) searched
      "completed" -> IntMap.toList $ IntMap.filter (.completed) searched
      _           -> IntMap.toList searched
    activeCount = IntMap.size $ IntMap.filter (not . (.completed)) searched
    activeCountText :: Text
    activeCountText = show activeCount <> " item" <> (if activeCount == 1 then "" else "s") <> " left"
    completedCount = IntMap.size $ IntMap.filter (.completed) searched
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
    patchPath  = "/todos/" <> show i :: Text
    deletePath = "/todos/" <> show i :: Text
    completed :: Html ()
    completed
      | todo.completed = [hsx|<input type="checkbox" checked hx-patch={patchPath} hx-target="#todo-list" style="margin-bottom: 0;">|]
      | otherwise      = [hsx|<input type="checkbox" hx-patch={patchPath} hx-target="#todo-list" style="margin-bottom: 0;">|]
    titleHtml :: Html ()
    titleHtml
      | todo.completed = [hsx|<s style="opacity: 0.5;">{todo.title}</s>|]
      | otherwise      = [hsx|<span>{todo.title}</span>|]

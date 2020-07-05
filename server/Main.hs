{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Common
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Servant
import Servant ((:<|>)(..), (:>))
import qualified System.IO as IO

import Control.Concurrent
import qualified Miso
import Miso (View)

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3012..."
  state <- newMVar 0
  Wai.run 3012 $ Wai.logStdout $ compress $ app state
  where
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def {Wai.gzipFiles = Wai.GzipCompress}

app :: MVar Int -> Wai.Application
app state = do
  Servant.serve
    (Proxy @ServerAPI)
    (static :<|> serverHandlers :<|> modelDataForRoot :<|> modifyCounter :<|>
     Servant.Tagged page404)
  where
    static :: Servant.Server StaticAPI
    static = Servant.serveDirectoryFileServer "static"
    serverHandlers :: Servant.Server ServerRoutes
    serverHandlers = homeServer :<|> flippedServer
    -- Alternative type:
    -- Servant.Server (ToServerRoutes Home HtmlPage Action)
    -- Handles the route for the home page, rendering Common.homeView.
    homeServer :: Servant.Handler (HtmlPage (View Action))
    homeServer = do
      counter <- liftIO $ readMVar state
      let md = initialModelData counter
      pure $ HtmlPage md $ viewModel $ initialModel homeLink md
    -- Alternative type:
    -- Servant.Server (ToServerRoutes Common.Flipped HtmlPage Common.Action)
    -- Renders the /flipped page.
    flippedServer :: Servant.Handler (HtmlPage (View Action))
    flippedServer = do
      counter <- liftIO $ readMVar state
      let md = initialModelData counter
      pure $ HtmlPage md $ viewModel $ initialModel flippedLink md
    modelDataForRoot :: Servant.Handler ModelData
    modelDataForRoot = do
      counter <- liftIO $ readMVar state
      return $ initialModelData counter
    modifyCounter :: Int -> Servant.Handler Int
    modifyCounter d = liftIO $ modifyMVar state $ \v -> return (v + d, v + d)
    -- The 404 page is a Wai application because the endpoint is Raw.
    -- It just renders the page404View and sends it to the client.
    page404 :: Wai.Application
    page404 _ respond =
      respond $
      Wai.responseLBS HTTP.status404 [("Content-Type", "text/html")] $
      L.renderBS $ L.toHtml page404View

-- | Represents the top level Html code. Its value represents the body of the
-- page.
data HtmlPage a =
  HtmlPage ModelData a
  deriving (Show, Eq)

instance L.ToHtml a => L.ToHtml (HtmlPage a) where
  toHtmlRaw = L.toHtml
  toHtml (HtmlPage modelData x) = do
    L.doctype_
    L.head_ $ do
      L.title_ "Miso isomorphic example"
      L.meta_ [L.charset_ "utf-8"]
      L.with
        (L.script_ mempty)
        [ L.makeAttribute "src" "/static/all.js"
        , L.makeAttribute "async" mempty
        , L.makeAttribute "defer" mempty
        ]
      L.script_
        ("window.modelData = \"" <>
         (Text.replace "\"" "\\\"" $
          Text.replace "\\" "\\\\" $
          Text.decodeUtf8 $ BSL.toStrict $ encode modelData) <>
         "\"")
    L.body_ (L.toHtml x)

-- Converts the ClientRoutes (which are a servant tree of routes leading to
-- some `View action`) to lead to `Get '[Html] (HtmlPage (View Common.Action))`
type ServerRoutes = Miso.ToServerRoutes ViewRoutes HtmlPage Action

-- The server serves static files besides the ServerRoutes, among which is the
-- javascript file of the client.
type ServerAPI
   = StaticAPI :<|> (ServerRoutes :<|> ModelDataForRoot :<|> ModifyCounter :<|> Servant.Raw -- This will show the 404 page for any unknown route
                     )

type StaticAPI = "static" :> Servant.Raw

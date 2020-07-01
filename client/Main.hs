{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Control.Lens ((+=), (-=), (.=), (^.), makeLenses)
import Data.Proxy (Proxy(..))
import qualified Servant.API as Servant
import Servant.API ((:<|>)(..))
import qualified Servant.Links as Servant
import qualified Miso
import Miso (App(..), View)
import qualified Miso.String as Miso
import qualified Network.URI as Network
import Servant.Client.JSaddle
import Data.Maybe
import Data.Aeson (decode)
-- | JSAddle import
#ifndef __GHCJS__
import qualified Clay as C
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Language.Javascript.JSaddle.Warp as JSaddle
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Proxy hiding (Proxy)
import qualified Network.HTTP.Proxy as Proxy
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
import Network.WebSockets (defaultConnectionOptions)
#endif

#ifndef __GHCJS__
runApp :: Miso.JSM (Network.URI -> App Model a) -> IO ()
runApp app = do
  proxyApp <- httpProxyApp proxySettings <$> newManager defaultManagerSettings
  let cssBS = TL.encodeUtf8 $ TL.replace "'" "\\'" $ "// css"
      extraJs =
        "(function () { let s = document.createElement('style'); s.innerHTML = '" <>
        cssBS <>
        "'; document.getElementsByTagName('head')[0].appendChild(s); })();"
  jsaddleApp <-
    JSaddle.jsaddleOr defaultConnectionOptions (f >> Miso.syncPoint) $
    JSaddle.jsaddleAppWithJsOr (jsaddleJs False <> extraJs) proxyApp
  Warp.runSettings settings jsaddleApp
  where
    settings = Warp.setPort 8088 (Warp.setTimeout 3600 Warp.defaultSettings)
    proxySettings =
      defaultProxySettings {proxyPort = 3002, proxyRequestModifier = rewrite}
    path req = "http://localhost:3002" <> Proxy.requestPath req
    rewrite req = return $ Right req {Proxy.requestPath = path req}
    f = do
      uri <- Miso.getCurrentURI
      a <- app
      Miso.startApp $ a uri

getModelData :: Miso.JSM ModelData
getModelData = do
  eMD <- runClientM' getModelDataForRoot
  return $ case eMD of
    Right md -> md
    Left _ -> initialModelData 0
#else
runApp :: IO (Network.URI -> App Model a) -> IO ()
runApp app = Miso.miso =<< app

foreign import javascript unsafe "$r = window['modelData']" getModelDataFFI
  :: IO Miso.MisoString

getModelData :: IO ModelData
getModelData =
  fromMaybe (error "Couldn't decode model data") . decode . Miso.fromMisoString <$>
  getModelDataFFI
#endif

main :: IO ()
main = do
  runApp $ do
    md <- getModelData
    return $ \currentURI ->
      App
        { initialAction = NoOp
        , model = initialModel currentURI md
        , update = Miso.fromTransition . updateModel
        , view = viewModel
        , events = Miso.defaultEvents
        , subs = [Miso.uriSub Common.HandleURIChange]
        , mountPoint = Nothing
        , logLevel = Miso.DebugPrerender
        }
updateModel :: Action -> Miso.Transition Action Model ()
updateModel action =
  case action of
    NoOp -> pure ()
    AddOne -> sendModify 1
    SubtractOne -> sendModify (-1)
    ModifyCounterDone counterValue' -> modelData . counterValue .= counterValue'
    ChangeURI uri ->
      Miso.scheduleIO $ do
        Miso.pushURI uri
        pure NoOp
    HandleURIChange uri' -> uri .= uri'
  where
  sendModify d =
    Miso.scheduleIO $ do
      er <- runClientM' $ modifyCounter d
      case er of
        Right newCounter -> return $ ModifyCounterDone newCounter
        Left _ -> return NoOp

getModelDataForRoot :: ClientM ModelData
getModelDataForRoot = client (Proxy @ModelDataForRoot)

modifyCounter :: Int -> ClientM Int
modifyCounter = client (Proxy @ModifyCounter)

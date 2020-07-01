{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Common where

import Control.Lens
import Data.Aeson
import Data.Proxy (Proxy(..))
import GHC.Generics
import qualified Miso
import Miso (View)
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network
import Servant.API
import qualified Servant.Links as Servant

data Model =
  Model
    { _uri :: !Network.URI
    , _modelData :: !ModelData
    }
  deriving (Eq, Show)

data ModelData =
  ModelData
    { _counterValue :: !Int
    }
  deriving (Eq, Show, Generic)

instance ToJSON ModelData

instance FromJSON ModelData

initialModel :: Network.URI -> ModelData -> Model
initialModel uri md = Model {_uri = uri, _modelData = md}

initialModelData :: Int -> ModelData
initialModelData c = ModelData {_counterValue = c}

data Action
  = NoOp
  | AddOne
  | SubtractOne
  | ModifyCounterDone Int
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show, Eq)

-- Holds a servant route tree of `View action`
type ViewRoutes = Home :<|> Flipped

-- Home route, contains two buttons and a field
type Home = View Action

-- Flipped route, same as Home, but with the buttons flipped
type Flipped = "flipped" :> View Action

type ModelDataForRoot = "model" :> Get '[ JSON] ModelData

type ModifyCounter = "modify" :> Capture "d" Int :> Post '[ JSON] Int

makeLenses ''Model

makeLenses ''ModelData

-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Action
viewModel m =
  case Miso.runRoute (Proxy @ViewRoutes) viewTree _uri m of
    Left _routingError -> page404View
    Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree :: (Model -> View Action) :<|> (Model -> View Action)
viewTree = homeView :<|> flippedView

-- View function of the Home route
homeView :: Model -> View Action
homeView m =
  div_
    []
    [ div_
        []
        [ button_ [onClick SubtractOne] [text "-"]
        , text $ Miso.ms $ show $ _counterValue md
        , button_ [onClick AddOne] [text "+"]
        ]
    , button_ [onClick $ ChangeURI flippedLink] [text "Go to /flipped"]
    ]
  where
    md = _modelData m

-- View function of the Home route
flippedView :: Model -> View Action
flippedView m =
  div_
    []
    [ div_
        []
        [ button_ [onClick AddOne] [text "+"]
        , text $ Miso.ms $ show $ _counterValue md
        , button_ [onClick SubtractOne] [text "-"]
        ]
    , button_ [onClick $ ChangeURI homeLink] [text "Go to /"]
    ]
  where
    md = _modelData m

page404View :: View Action
page404View = text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink = Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)

-- Network.URI that points to the flipped route
flippedLink :: Network.URI
flippedLink =
  Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Flipped)

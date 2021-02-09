--
-- WEB / Server
--

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Server (
    runServer
  ) where


import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant 
  ( Server
  , serve
  , Proxy (..)
  , (:<|>)(..)
  , serveDirectoryWebApp
  )

import Data.Assets (Assets)
import Config (Config)
import Web.API (API)
import qualified Web.Handler as Handler (lessonPage)



api :: Proxy API
api = Proxy


server :: Config -> Assets -> Server API
server _ assets = Handler.lessonPage assets
      :<|> serveDirectoryWebApp "dist-web/"


app :: Config -> Assets -> Application
app config assets = serve api $ server config assets


runServer :: Config -> Assets -> IO ()
runServer config assets = do
  run config.port $ app config assets


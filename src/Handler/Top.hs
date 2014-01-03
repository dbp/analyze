{-# LANGUAGE OverloadedStrings #-}

module Handler.Top (routes) where


import           Data.ByteString (ByteString)
import           Snap.Snaplet.Heist
import           Snap.Snaplet
import           Snap.Core
------------------------------------------------------------------------------
import           Application
import           Handler.Auth (authRoutes)
import           Handler.Sites (sitesRoutes)
import           Helpers.Auth (withAccount)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("", ifTop $ indexHandler)
         , ("/site", withAccount sitesRoutes)
         , ("/auth", authRoutes)
         , ("", notFoundHandler)
         ]

indexHandler :: AppHandler ()
indexHandler = render "index"

notFoundHandler :: AppHandler ()
notFoundHandler = do
  modifyResponse (setResponseCode 404)
  render "not_found"

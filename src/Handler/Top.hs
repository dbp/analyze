{-# LANGUAGE OverloadedStrings #-}

module Handler.Top (routes) where


import           Data.Monoid
import           Data.ByteString (ByteString)
import           Snap.Snaplet.Heist
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Core
import           Snap.Util.FileServe
import           Heist
------------------------------------------------------------------------------
import           Application
import           Handler.Auth (authRoutes)
import           Handler.Sites (sitesRoutes)
import           Handler.Submit (submitRoutes)
import           State.Sites (getUserSites)
import           Splices.Sites (sitesSplice)
import           Helpers.Auth (withAccount)

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("", ifTop indexHandler)
         , ("/site", withAccount sitesRoutes)
         , ("/auth", authRoutes)
         , ("/submit", submitRoutes)
         , ("/static", serveDirectory "static")
         , ("", notFoundHandler)
         ]

indexHandler :: AppHandler ()
indexHandler = do
  u <- with auth currentUser
  s <- case u of
         Nothing -> return mempty
         Just user -> do
           sites <- getUserSites user
           return ("sites" ## sitesSplice sites)
  renderWithSplices "index" s

notFoundHandler :: AppHandler ()
notFoundHandler = do
  modifyResponse (setResponseCode 404)
  render "not_found"

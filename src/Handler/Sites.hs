{-# LANGUAGE OverloadedStrings #-}

module Handler.Sites (sitesRoutes) where

import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Calendar
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Heist
import qualified Heist.Interpreted as I
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Digestive.Heist
-----------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.Text
import           Helpers.Forms
import           State.Sites
import           State.Accounts

sitesRoutes :: Account -> AppHandler ()
sitesRoutes account = route $ map (\x -> (fst x, snd x account))
                      [ ("new", newSiteHandler)
                      , (":id", siteHandler)
                      ]

sitePath :: Int -> ByteString
sitePath id' = B.concat [rootUrl, "/site/", B8.pack $ show id']

data NewSiteData = NewSiteData Text Text Day Text Text
newSiteForm :: Form Text AppHandler NewSiteData
newSiteForm = NewSiteData
              <$> "name" .: nameForm Nothing
              <*> "url" .: urlForm Nothing
              <*> "start_date" .: dateForm Nothing
              <*> "user_link_pattern" .: linkPatternForm Nothing
              <*> "issue_link_pattern" .: linkPatternForm Nothing

newSiteHandler :: Account -> AppHandler ()
newSiteHandler account = do
  r <- runForm "new_site" newSiteForm
  case r of
    (v, Nothing) -> renderWithSplices "sites/new" (digestiveSplices v)
    (_, Just (NewSiteData name url start_date user_pattern issue_pattern)) -> do
      mid <- newSite (Site (-1) name url (UTCTime start_date 0) user_pattern issue_pattern)
      case mid of
        Nothing -> displayError "Could not create a new site." Nothing
        Just id' ->
          -- TODO(dbp 2014-01-03): Add user to site.
          redirect $ sitePath id'

siteHandler :: Account -> AppHandler ()
siteHandler account = undefined

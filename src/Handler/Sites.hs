{-# LANGUAGE OverloadedStrings #-}

module Handler.Sites (sitesRoutes) where

import           Control.Applicative
import           Control.Monad.Trans
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
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)
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
import           Helpers.Auth
import           State.Sites
import           State.Accounts
import           Splices.Sites

sitesRoutes :: Account -> AppHandler ()
sitesRoutes account = route $ map (\x -> (fst x, snd x account))
                      [ ("/new", newSiteHandler)
                      , ("/:id", siteHandler)
                      ]

sitePath :: Int -> ByteString
sitePath id' = B.concat [rootUrl, "/site/", B8.pack $ show id']

data SiteData = SiteData Text Text Day Text Text
siteForm :: Maybe Site -> Form Text AppHandler SiteData
siteForm m = SiteData
             <$> "name" .: nameForm (fmap siteName m)
             <*> "url" .: urlForm (fmap siteUrl m)
             <*> "start_date" .: dateForm (fmap (utctDay . siteStartDate) m)
             <*> "user_link_pattern" .: linkPatternForm (fmap siteUserLinkPattern m)
             <*> "issue_link_pattern" .: linkPatternForm (fmap siteIssueLinkPattern m)

newSiteHandler :: Account -> AppHandler ()
newSiteHandler account = do
  r <- runForm "new_site" (siteForm Nothing)
  case r of
    (v, Nothing) -> renderWithSplices "sites/new" (digestiveSplices v)
    (_, Just (SiteData name url start_date user_pattern issue_pattern)) -> do
      let site = Site (-1) name url (UTCTime start_date 0) user_pattern issue_pattern
      mid <- newSite site
      case mid of
        Nothing -> displayError "Could not create a new site." Nothing
        Just id' -> do
          addSiteUser (site { siteId = id' }) account
          redirect $ sitePath id'

siteHandler :: Account -> AppHandler ()
siteHandler account = do
  mid <- getParam "id"
  case fmap (read.B8.unpack) mid of
    Nothing -> pass
    Just id' -> do
      msite <- getSite id'
      case msite of
        Nothing -> pass
        Just site -> do
          authed <- isSiteUser site account
          case authed of
            False -> loginRedirect
            True ->
              route [ ("", ifTop $ showSiteHandler account site)
                    , ("/edit", editSiteHandler account site)
                    , ("/token/new", newTokenHandler account site)
                    , ("/day/:day", dayHandler account site)
                    ]

showSiteHandler :: Account -> Site -> AppHandler ()
showSiteHandler account site = do
  tokens <- getTokens site
  days <- getDaysWithVisits site
  renderWithSplices "sites/show" (siteSplice site
                                  <> ("tokens" ## tokensSplice tokens)
                                  <> ("days" ## daysWithVisitsSplice days))

editSiteHandler :: Account -> Site -> AppHandler ()
editSiteHandler account site = do
  r <- runForm "edit_site" (siteForm (Just site))
  case r of
    (v, Nothing) -> renderWithSplices "sites/edit" (digestiveSplices v)
    (_, Just (SiteData name url start_date user_pattern issue_pattern)) -> do
      updateSite (site { siteName = name, siteUrl = url, siteStartDate = (UTCTime start_date 0)
                       , siteUserLinkPattern = user_pattern, siteIssueLinkPattern = issue_pattern })
      redirect $ sitePath (siteId site)

newTokenHandler :: Account -> Site -> AppHandler ()
newTokenHandler account site = do
  newToken site
  redirect $ sitePath (siteId site)

dayHandler :: Account -> Site -> AppHandler ()
dayHandler account site = do
  md <- getParam "day"
  case (fmap B8.unpack md) >>= (parseTime defaultTimeLocale "%F") of
    Nothing -> pass
    Just d -> do
      vs <- getDaysVisits site d
      renderWithSplices "sites/day/show" (siteSplice site
                                         <> ("days" ## dayVisitsSplice vs))

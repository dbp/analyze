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
import           Helpers.Misc
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

errorPath :: ErrorSummary -> ByteString
errorPath e = B.concat [rootUrl, "/site/", B8.pack $ show $ errorSiteId e, "/error/"
                       , B8.pack $ show $ errorId e]

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
                    , ("/error/:id", errorHandler account site)
                    ]

showSiteHandler :: Account -> Site -> AppHandler ()
showSiteHandler account site = do
  tokens <- getTokens site
  days <- getDaysWithVisits site
  errs <- getSiteErrors site
  exs <- mapM getLastExample errs
  renderWithSplices "sites/show" (siteSplice site
                                  <> ("tokens" ## tokensSplice tokens)
                                  <> ("days" ## daysWithVisitsSplice days)
                                  <> ("errors" ## errorsLastExSplice (zip errs exs)))

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

errorHandler :: Account -> Site -> AppHandler ()
errorHandler account site = do
  mi <- getParam "id"
  case (fmap B8.unpack mi) >>= readSafe of
    Nothing -> pass
    Just i -> do
      me <- getErrorById site i
      case me of
        Nothing -> pass
        Just e ->
          route [("", ifTop (showErrorHandler site e))
                ,("/resolve", toggleErrorHandler site e)
                ,("/issue", issueErrorHandler site e)]

showErrorHandler :: Site -> ErrorSummary -> AppHandler ()
showErrorHandler site e = do
  exs <- getErrorExamples e
  renderWithSplices "sites/error/show" (do "site" ## I.runChildrenWith (siteSplice site)
                                           errorSplices e
                                           "examples" ## examplesSplices exs)

toggleErrorHandler :: Site -> ErrorSummary -> AppHandler ()
toggleErrorHandler site e = do
    r <- toggle (errorResolved e)
    updateErrorSummary (siteId site) (e { errorResolved = r})
    redirect (errorPath e)
  where toggle (Just _) = return Nothing
        toggle Nothing = fmap Just $ liftIO getCurrentTime

issueErrorHandler :: Site -> ErrorSummary -> AppHandler ()
issueErrorHandler site e = do
   i <- getParam "issue"
   let e' = case i of
              Nothing -> e { errorIssueId = Nothing }
              Just issue_id -> e { errorIssueId = Just (T.decodeUtf8 issue_id) }
   updateErrorSummary (siteId site) e'
   redirect (errorPath e')

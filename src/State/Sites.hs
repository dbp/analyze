{-# LANGUAGE OverloadedStrings #-}

module State.Sites where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import           Data.Maybe (isNothing)
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth (AuthUser(..), UserId(..))
import           Data.Time.Clock
import           Data.Time.Calendar
------------------------------------------------------------------------------
import           Application
import           Helpers.Text
import           Helpers.Errors
import           Helpers.State
import           State.Accounts

data Site = Site { siteId :: Int
                 , siteName :: Text
                 , siteUrl :: Text
                 , siteStartDate :: UTCTime
                 , siteUserLinkPattern :: Text
                 , siteIssueLinkPattern :: Text
                 } deriving (Eq, Show)

instance FromRow Site where
  fromRow = Site <$> field <*> field <*> field
                 <*> field <*> field <*> field

data SiteToken = SiteToken { tokenText :: Text
                           , tokenInvalidated :: Maybe UTCTime
                           , tokenCreated :: UTCTime
                           , tokenSiteId :: Int
                           }
instance FromRow SiteToken where
  fromRow = SiteToken <$> field <*> field <*> field <*> field

data SiteVisit = SiteVisit { visitId :: Int
                           , visitSiteId :: Int
                           , visitUrl :: Text
                           , visitRenderTime :: Double
                           , visitTime :: UTCTime
                           }
instance FromRow SiteVisit where
  fromRow = SiteVisit <$> field <*> field <*> field <*> field <*> field


data SiteError = SiteError { errorId :: Int
                           , errorSiteId :: Int
                           , errorUrl :: Text
                           , errorMessage :: Text
                           , errorUid :: Maybe Text
                           , errorTime :: UTCTime
                           }
instance FromRow SiteError where
  fromRow = SiteError <$> field <*> field <*> field <*> field <*> field <*> field

data DayVisit = DayVisit { dayDay :: Day
                         , daySiteId :: Int
                         , dayUrl :: Maybe Text
                         , dayHits :: Int
                         , dayMaxTime :: Double
                         , dayMinTime :: Double
                         , dayAvgTime :: Double
                         , dayVarTime :: Double
                         }
instance FromRow DayVisit where
  fromRow = DayVisit <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field

clearSites :: AppHandler ()
clearSites = void $ execute_ "delete from sites"

countSites :: AppHandler Int
countSites = numberQuery' "select count(*) from sites"

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ n u s ulp ilp) = do
  r <- idQuery "insert into sites (name, url, start_date, user_link_pattern, issue_link_pattern) values (?,?,?,?,?) returning id" (n, u, s, ulp, ilp)
  if isNothing r then registerError "newSite: Could not create new site." (Just u) else return ()
  return r

getSite :: Int -> AppHandler (Maybe Site)
getSite id' = singleQuery "select id, name, url, start_date, user_link_pattern, issue_link_pattern from sites where id = ?" (Only id')

updateSite :: Site -> AppHandler ()
updateSite (Site i n u s ulp ilp) = void $ execute "update sites set name = ?, url = ?, start_date = ?, user_link_pattern = ?, issue_link_pattern = ? where id = ?" (n, u, s, ulp, ilp, i)

addSiteUser :: Site -> Account -> AppHandler ()
addSiteUser site account =
  void $ execute "insert into site_users (site_id, user_id) values (?,?)" (siteId site, accountId account)

isSiteUser :: Site -> Account -> AppHandler Bool
isSiteUser site account = do
  fmap (/= 0) $ numberQuery "select count(*) from site_users where site_id = ? and user_id = ?" (siteId site, accountId account)

newToken :: Site -> AppHandler (Maybe SiteToken)
newToken site = do
  now <- liftIO getCurrentTime
  res <- query "insert into tokens (site_id, created) values (?, ?) returning token" ((siteId site), now)
  case res of
    [[t]] -> return (Just $ SiteToken t Nothing now (siteId site))
    _ -> return Nothing

getTokens :: Site -> AppHandler [SiteToken]
getTokens site = query "select token, invalidated, created, site_id from tokens where site_id = ?" (Only $ siteId site)

getToken :: Text -> AppHandler (Maybe SiteToken)
getToken t = singleQuery "select token, invalidated, created, site_id from tokens where token = ?" (Only t)

invalidateToken :: SiteToken -> AppHandler ()
invalidateToken token = void $ execute "update tokens set invalidated = now() where token = ? and site_id = ?" (tokenText token, tokenSiteId token)

clearVisitsQueue :: AppHandler ()
clearVisitsQueue = void $ execute_ "delete from visits_queue"

siteVisitsQueue :: Site -> AppHandler [SiteVisit]
siteVisitsQueue site = query "select id, site_id, url, render_time, time from visits_queue where site_id = ?" (Only $ siteId site)

newSiteVisit :: SiteVisit -> AppHandler (Maybe Int)
newSiteVisit (SiteVisit _ s u r _) = do
  r <- idQuery "insert into visits_queue (site_id, url, render_time) values (?,?,?) returning id" (s, u, r)
  when (isNothing r) (registerError "newSiteVisit: Could not create new site visit." (Just $ tshow (s, u)))
  return r

getVisits :: Int -> AppHandler [SiteVisit]
getVisits n = query "select id, site_id, url, render_time, time from visits_queue order by id asc limit ?" (Only n)

getDayVisit :: Int -> Day -> Text -> AppHandler (Maybe DayVisit)
getDayVisit site_id day url = singleQuery "select day, site_id, url, hits, max_time, min_time, avg_time, var_time from day_visits where site_id = ? and day = ? and url = ?" (site_id, day, url)

newDayVisit :: DayVisit -> AppHandler ()
newDayVisit (DayVisit d s u h mx mn avg var) =
  void $ execute "insert into day_visits (day, site_id, url, hits, max_time, min_time, avg_time, var_time) values (?,?,?,?,?,?,?,?)" (d, s, u, h, mx, mn, avg, var)


updateDayVisit :: DayVisit -> AppHandler ()
updateDayVisit (DayVisit d s u h mx mn avg var) =
  void $ execute "update day_visits set hits = ?, max_time = ?, min_time = ?, avg_time = ?, var_time = ? where day = ?, site_id = ?, url = ?" (h, mx, mn, avg, var, d, s, u)

clearErrorsQueue :: AppHandler ()
clearErrorsQueue = void $ execute_ "delete from errors_queue"

siteErrorsQueue :: Site -> AppHandler [SiteError]
siteErrorsQueue site = query "select id, site_id, url, message, user_id, time from errors_queue where site_id = ?" (Only $ siteId site)

newSiteError :: SiteError -> AppHandler (Maybe Int)
newSiteError (SiteError _ s u m uid _) = do
  r <- idQuery "insert into errors_queue (site_id, url, message, user_id) values (?,?,?,?) returning id" (s, u, m, uid)
  when (isNothing r) (registerError "newSiteError: Could not create new site visit." (Just $ tshow (s, u)))
  return r

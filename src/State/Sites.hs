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
                           } deriving (Eq, Show)
instance FromRow SiteToken where
  fromRow = SiteToken <$> field <*> field <*> field <*> field

data SiteVisit = SiteVisit { visitId :: Int
                           , visitSiteId :: Int
                           , visitUrl :: Text
                           , visitRenderTime :: Double
                           , visitTime :: UTCTime
                           , visitMethod :: Text
                           } deriving (Eq, Show)
instance FromRow SiteVisit where
  fromRow = SiteVisit <$> field <*> field <*> field <*> field <*> field <*> field


data SiteError = SiteError { serrorId :: Int
                           , serrorSiteId :: Int
                           , serrorUrl :: Text
                           , serrorMessage :: Text
                           , serrorUid :: Maybe Text
                           , serrorTime :: UTCTime
                           } deriving (Eq, Show)
instance FromRow SiteError where
  fromRow = SiteError <$> field <*> field <*> field <*> field <*> field <*> field

data DayVisit = DayVisit { dayDay :: Day
                         , daySiteId :: Int
                         , dayUrl :: Maybe Text
                         , dayMethod :: Text
                         , dayHits :: Int
                         , dayMaxTime :: Double
                         , dayMinTime :: Double
                         , dayAvgTime :: Double
                         , dayVarTime :: Double
                         } deriving (Eq, Show)
instance FromRow DayVisit where
  fromRow = DayVisit <$> field <*> field <*> field <*> field
                      <*> field <*> field <*> field <*> field <*> field

data ErrorSummary = ErrorSummary { errorId :: Int
                                 , errorSiteId :: Int
                                 , errorMessage :: Text
                                 , errorResolved :: Maybe UTCTime
                                 , errorCreated :: UTCTime
                                 , errorIssueId :: Maybe Text
                                 } deriving (Eq, Show)

instance FromRow ErrorSummary where
  fromRow = ErrorSummary <$> field <*> field <*> field
                         <*> field <*> field <*> field

data ErrorExample = ErrorExample { errorExampleId :: Int
                                 , errorExampleErrorId :: Int
                                 , errorExampleUrl :: Text
                                 , errorTime :: UTCTime
                                 , errorExampleUid :: Maybe Text
                                 } deriving (Eq, Show)
instance FromRow ErrorExample where
  fromRow = ErrorExample <$> field <*> field <*> field <*> field <*> field

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

getUserSites :: AuthUser -> AppHandler [Site]
getUserSites u =
  case userId u of
    Nothing -> return []
    Just uid -> query "select id, name, url, start_date, user_link_pattern, issue_link_pattern from sites join site_users on site_id = id where user_id = ?" (Only uid)

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
siteVisitsQueue site = query "select id, site_id, url, render_time, time, method from visits_queue where site_id = ?" (Only $ siteId site)

newSiteVisit :: SiteVisit -> AppHandler (Maybe Int)
newSiteVisit (SiteVisit _ s u r _ m) = do
  r <- idQuery "insert into visits_queue (site_id, url, render_time, method) values (?,?,?,?) returning id" (s, u, r, m)
  when (isNothing r) (registerError "newSiteVisit: Could not create new site visit." (Just $ tshow (s, u)))
  return r

getMarkVisits :: Int -> AppHandler [SiteVisit]
getMarkVisits n = query "update visits_queue set processing = true where id in (select id from visits_queue where processing = false order by id asc limit ?) and processing = false returning id, site_id, url, render_time, time, method" (Only n)

deleteVisitQueueItem :: Int -> AppHandler ()
deleteVisitQueueItem i = void $ execute "delete from visits_queue where id = ?" (Only i)


getDayVisit :: Int -> Day -> Text -> Text -> AppHandler (Maybe DayVisit)
getDayVisit site_id day url meth = singleQuery "select day, site_id, url, method, hits, max_time, min_time, avg_time, var_time from day_visits where site_id = ? and day = ? and url = ? and method = ?" (site_id, day, url, meth)

newDayVisit :: DayVisit -> AppHandler ()
newDayVisit (DayVisit d s u m h mx mn avg var) =
  void $ execute "insert into day_visits (day, site_id, url, method, hits, max_time, min_time, avg_time, var_time) values (?,?,?,?,?,?,?,?,?)" (d, s, u, m, h, mx, mn, avg, var)


updateDayVisit :: DayVisit -> AppHandler ()
updateDayVisit (DayVisit d s u m h mx mn avg var) =
  void $ execute "update day_visits set hits = ?, max_time = ?, min_time = ?, avg_time = ?, var_time = ? where day = ? and site_id = ? and url = ? and method = ?" (h, mx, mn, avg, var, d, s, u, m)

siteDayVisits :: Site -> AppHandler [DayVisit]
siteDayVisits s = query "select day, site_id, url, method, hits, max_time, min_time, avg_time, var_time from day_visits where site_id = ?" (Only $ siteId s)

getDaysVisits :: Site -> Day -> AppHandler [DayVisit]
getDaysVisits s d = query "select day, site_id, url, method, hits, max_time, min_time, avg_time, var_time from day_visits where site_id = ? and day = ?" (siteId s, d)

getDaysWithVisits :: Site -> AppHandler [Day]
getDaysWithVisits s = fmap (map head) $ query "select distinct day from day_visits where site_id = ? order by day desc" (Only (siteId s))

clearErrorsQueue :: AppHandler ()
clearErrorsQueue = void $ execute_ "delete from errors_queue"

clearErrors :: AppHandler ()
clearErrors = void $ execute_ "delete from errors"

siteErrorsQueue :: Site -> AppHandler [SiteError]
siteErrorsQueue site = query "select id, site_id, url, message, user_id, time from errors_queue where site_id = ?" (Only $ siteId site)

countErrorExamples :: AppHandler Int
countErrorExamples = numberQuery' "select count(*) from errors_examples"

newSiteError :: SiteError -> AppHandler (Maybe Int)
newSiteError (SiteError _ s u m uid _) = do
  r <- idQuery "insert into errors_queue (site_id, url, message, user_id) values (?,?,?,?) returning id" (s, u, m, uid)
  when (isNothing r) (registerError "newSiteError: Could not create new site visit." (Just $ tshow (s, u)))
  return r

getMarkErrors :: Int -> AppHandler [SiteError]
getMarkErrors n = query "update errors_queue set processing = true where id in (select id from errors_queue where processing = false order by id asc limit ?) and processing = false returning id, site_id, url, message, user_id, time" (Only n)

getErrorByMessage :: Text -> Int -> AppHandler (Maybe ErrorSummary)
getErrorByMessage m si = singleQuery "select id, site_id, message, resolved, created, issue_id from errors where message = ? and site_id = ?" (m, si)

getSiteErrors :: Site -> AppHandler [ErrorSummary]
getSiteErrors s = query "select id, site_id, message, resolved, created, issue_id from errors where site_id = ?" (Only (siteId s))

getErrorById :: Site -> Int -> AppHandler (Maybe ErrorSummary)
getErrorById s i = singleQuery "select id, site_id, message, resolved, created, issue_id from errors where id = ? and site_id = ?" (i, siteId s)

newErrorSummary :: ErrorSummary -> AppHandler (Maybe Int)
newErrorSummary (ErrorSummary _ s m r c iid) = do
  r <- idQuery "insert into errors (site_id, message, resolved, issue_id) values (?,?,?,?) returning id" (s, m, r, iid)
  when (isNothing r) (registerError "newErrorSummary: Could not create new error summary." (Just $ tshow (s, m)))
  return r

newErrorExample :: ErrorExample -> AppHandler (Maybe Int)
newErrorExample (ErrorExample _ e url time uid) = do
  r <- idQuery "insert into errors_examples (error_id, url, time, user_id) values (?,?,?,?) returning id" (e, url, time, uid)
  when (isNothing r) (registerError "newErrorExample: Could not create new error example." (Just $ tshow (e, url, time, uid)))
  return r

getErrorExamples :: ErrorSummary -> AppHandler [ErrorExample]
getErrorExamples e = query "select id, error_id, url, time, user_id from errors_examples where error_id = ? order by time desc" (Only (errorId e))

getLastExample :: ErrorSummary -> AppHandler (Maybe ErrorExample)
getLastExample e = singleQuery "select id, error_id, url, time, user_id from errors_examples where error_id = ? order by time desc limit 1" (Only (errorId e))

deleteErrorQueueItem :: Int -> AppHandler ()
deleteErrorQueueItem i = void $ execute "delete from errors_queue where id = ?" (Only i)

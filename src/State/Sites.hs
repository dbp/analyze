{-# LANGUAGE OverloadedStrings #-}

module State.Sites where

import           Control.Applicative
import           Control.Monad.Trans (liftIO)
import           Data.Text (Text)
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth (AuthUser(..), UserId(..))
import           Data.Time.Clock
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

clearSites :: AppHandler ()
clearSites = void $ execute_ "delete from sites"

countSites :: AppHandler Int
countSites = numberQuery' "select count(*) from sites"

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ n u s ulp ilp) = do
  res <- query "insert into sites (name, url, start_date, user_link_pattern, issue_link_pattern) values (?,?,?,?,?) returning id" (n, u, s, ulp, ilp) :: AppHandler [[Int]]
  case res of
    ((x:_):_)-> return (Just x)
    _ -> registerError "newSite: Could not create new site." (Just u) >> return Nothing


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

invalidateToken :: SiteToken -> AppHandler ()
invalidateToken token = void $ execute "update tokens set invalidated = now() where token = ? and site_id = ?" (tokenText token, tokenSiteId token)

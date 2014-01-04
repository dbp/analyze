{-# LANGUAGE OverloadedStrings #-}

module State.Sites where

import           Control.Applicative
import           Data.Text (Text)
import           Snap.Snaplet.PostgresqlSimple
import           Data.Time.Clock
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.State

data Site = Site { siteId :: Int
                 , siteName :: Text
                 , siteUrl :: Text
                 , siteStartDate :: UTCTime
                 , siteUserLinkPattern :: Text
                 , siteIssueLinkPattern :: Text
                 }

instance FromRow Site where
  fromRow = Site <$> field <*> field <*> field
                 <*> field <*> field <*> field

clearSites :: AppHandler ()
clearSites = execute_ "delete from sites" >> return ()

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

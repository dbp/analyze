{-# LANGUAGE OverloadedStrings #-}

module State.Sites where


import           Data.Text (Text)
import           Snap.Snaplet.PostgresqlSimple
import           Data.Time.Clock
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors

data Site = Site { siteId :: Int
                 , siteName :: Text
                 , siteUrl :: Text
                 , siteStartDate :: UTCTime
                 , siteUserLinkPattern :: Text
                 , siteIssueLinkPattern :: Text
                 }

clearSites :: AppHandler ()
clearSites = query_ "delete from sites" >> return ()

countSites :: AppHandelr Int
countSites = numberQuery' "select count(*) from sites"

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ n u s ulp ilp) = do
  res <- query "insert into sites (name, url, start_date, user_link_pattern, issue_link_pattern) values (?,?,?,?,?,?) returning id" (n, u, s, ulp, ilp) :: AppHandler [[Int]]
  case res of
    ((x:_):_)-> return (Just x)
    _ -> registerError "newSite: Could not create new site." (Just u) >> return Nothing
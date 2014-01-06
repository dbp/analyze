{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Control.Monad.Trans.State (get, put)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Time.Calendar
import Data.Time.Clock
import System.Random (randomIO)

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Testing

import Application
import Site
import Helpers.Text
import Handler.Top
import State.Accounts
import State.Sites


main :: IO ()
main = runSnapTests (route routes) app $ do
  tlog "Running tests..."
  tlog "/ success"
  tsucceeds (tget "/")
  tlog "/foo/bar not found"
  tnotfound (tget "/foo/bar")
  tlog "/auth/new_user success"
  tsucceeds (tget "/auth/new_user")

  tlog "/auth/new_user creates a new account"
  tcleanup clearAccounts $
    tchanges (+1) countAccounts (tpost "/auth/new_user"
                                [ ("new_user.name", "Jane")
                                , ("new_user.email", "jdoe@c.com")
                                , ("new_user.password", "foobar")])

  tlog "/site/new redirect without login"
  tredirects (tget "/site/new")

  tcleanup clearAccounts $ twithUser $ do
    tlog "/site/new success with login"
    tsucceeds (tget "/site/new")
    tlog "/site/new creates a new site"
    tchanges (+1) countSites
     (tpost "/site/new" [
         ("new_site.name", "Acme")
         , ("new_site.url", "http://acme.com")
         , ("new_site.start_date.year", "2014")
         , ("new_site.start_date.month", "1")
         , ("new_site.start_date.day", "3")
         , ("new_site.user_link_pattern", "http://acme.com/user/*")
         , ("new_site.issue_link_pattern", "http://acme.com/issue/*")
         ])

  site_id <- fmap fromJust $ teval (newSite (Site (-1) "Some Site" "http://acme.com"
                                               (UTCTime (fromGregorian 2014 1 1) 0) "" ""))
  let site_url = B.append "/site/" (T.encodeUtf8 $ tshow site_id)
  tlog "/site/:id redirect without login"
  tredirects (tget site_url)
  tcleanup clearAccounts $ tcleanup clearSites $ twithUser $ do
    tlog "/site/:id success with login"
    tsucceeds (tget site_url)
    tlog "/site/:id has site name in response"
    tresponds (tget site_url) "Some Site"


-- App level helpers

-- Authentication
twithUser :: SnapTesting App a -> SnapTesting App a
twithUser act = do
  (site, app) <- get
  put ((withUser site), app)
  res <- act
  put (site, app)
  return res

-- reasonably likely to be unique
generateEmail :: IO Text
generateEmail = do
  int <- randomIO :: IO Int
  return $ T.pack $ (show $ abs int) ++ "@test.com"

generateName :: IO Text
generateName = do
  int <- randomIO :: IO Int
  return $ T.pack $ "Person #" ++ (show $ abs int)

withUser :: AppHandler a -> AppHandler a
withUser hndlr = do
  em <- liftIO generateEmail
  name <- liftIO generateName
  res <- with auth $ createUser em "password"
  case res of
    -- NOTE(dbp 2014-01-03): These are bad errors, but I'm not sure how the types let us do better.
    Left failure -> do
      liftIO $ putStrLn "Could not create user"
      hndlr
    Right au -> do
      newAccount (Account (fromJust $ userId au) name False)
      with auth $ forceLogin au
      hndlr

-- withAdmin :: AppHandler a -> AppHandler a
-- withAdmin hndlr = do
--   em <- liftIO generateEmail
--   name <- liftIO generateName
--   res <- with auth $ createUser em "password"
--   case res of
--     Left failure ->  do
--       liftIO $ putStrLn "Could not create admin"
--       hndlr
--     Right au -> do
--       newAccount (Account (fromJust $ userId au) name True)
--       with auth $ forceLogin au
--       hndlr

-- withLogin :: Text -> AppHandler a -> AppHandler a
-- withLogin id' hndlr = do
--   user <- with auth $ withBackend $ \r -> liftIO $ (lookupByUserId r (UserId id'))
--   case user of
--     Nothing ->  do
--       liftIO $ putStrLn "Could not find user"
--       hndlr
--     Just user' -> do
--       with auth $ forceLogin user'
--       hndlr

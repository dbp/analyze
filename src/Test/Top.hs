{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import Data.Maybe (fromJust)
import Data.Text (Text)
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
  name "/ success" $
    succeeds (get "/")
  name "/foo/bar not found" $
    notfound (get "/foo/bar")
  name "/auth/new_user success" $
    succeeds (get "/auth/new_user")

  name "/auth/new_user creates a new account" $
    cleanup clearAccounts $
    changes (+1) countAccounts (post "/auth/new_user"
                                [ ("new_user.name", "Jane")
                                , ("new_user.email", "jdoe@c.com")
                                , ("new_user.password", "foobar")])

  name "/site/new redirect without login" $
    redirects (get "/site/new")

  cleanup clearAccounts $ withUser $ do
    name "/new success with login" $
      succeeds (get "/site/new")
    name "/site/new creates a new site" $
      changes (+1) countSites
       (post "/site/new" [
         ("new_site.name", "Acme")
         , ("new_site.url", "http://acme.com")
         , ("new_site.start_date.year", "2014")
         , ("new_site.start_date.month", "1")
         , ("new_site.start_date.day", "3")
         , ("new_site.user_link_pattern", "http://acme.com/user/*")
         , ("new_site.issue_link_pattern", "http://acme.com/issue/*")
         ])

  site_id <- fmap fromJust $ eval (newSite (Site (-1) "Some Site" "http://acme.com"
                                            (UTCTime (fromGregorian 2014 1 1) 0) "" ""))
  let site_url = B.append "/site/" (T.encodeUtf8 $ tshow site_id)
  name "/site/:id redirect without login" $
    redirects (get site_url)
  cleanup clearAccounts $ cleanup clearSites $ withUser $ do
    name "/site/:id success with login" $
      succeeds (get site_url)
    name "/site/:id has site name in response" $
      responds (get site_url) "Some Site"


-- App level helpers

-- Authentication
withUser :: SnapTesting App a -> SnapTesting App a
withUser = modifySite addRandomUser

-- reasonably likely to be unique
generateEmail :: IO Text
generateEmail = do
  int <- randomIO :: IO Int
  return $ T.pack $ (show $ abs int) ++ "@test.com"

generateName :: IO Text
generateName = do
  int <- randomIO :: IO Int
  return $ T.pack $ "Person #" ++ (show $ abs int)

addRandomUser :: AppHandler a -> AppHandler a
addRandomUser hndlr = do
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

{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
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
    changes (+1) countAccounts (post "/auth/new_user" $ params
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
       (post "/site/new" $ params [
         ("new_site.name", "Acme")
         , ("new_site.url", "http://acme.com")
         , ("new_site.start_date.year", "2014")
         , ("new_site.start_date.month", "1")
         , ("new_site.start_date.day", "3")
         , ("new_site.user_link_pattern", "http://acme.com/user/*")
         , ("new_site.issue_link_pattern", "http://acme.com/issue/*")
         ])

  let site = (Site (-1) "Some Site" "http://acme.com"
                                            (UTCTime (fromGregorian 2014 1 1) 0)
                                            "http://acme.com/user/*"
                                            "http://acme.com/issue/*")
  site_id <- fmap fromJust $ eval (newSite site)
  (account, user) <- fmap fromJust $ eval getRandomUser
  eval (addSiteUser (site { siteId = site_id }) account)
  -- for use in forms
  let site_params = params [ ("edit_site.name", "Some Site")
                           , ("edit_site.url", "http://acme.com")
                           , ("edit_site.start_date.year", "2014")
                           , ("edit_site.start_date.month", "1")
                           , ("edit_site.start_date.day", "1")
                           , ("edit_site.user_link_pattern", "http://acme.com/user/*")
                           , ("edit_site.issue_link_pattern", "http://acme.com/issue/*")
                           ]
  let site_url = B.append "/site/" (T.encodeUtf8 $ tshow site_id)
  name "/site/:id redirect without login" $
    redirects (get site_url)
  name "/site/:id redirects without right user" $
    withUser $ redirects (get site_url)
  cleanup clearAccounts $ cleanup clearSites $ loginAs user $ do
    name "/site/:id success with right login" $
      succeeds (get site_url)
    name "/site/:id has site name in response" $
      responds (get site_url) "Some Site"
    name "/site/:id/edit displays a page with a form on it" $
      responds (get $ B.append site_url "/edit") "<form"
    name "/site/:id/edit post changes url" $
      changes (const "http://newacme.com")
        (fmap (siteUrl.fromJust) $ getSite site_id)
        (post (B.append site_url "/edit") $ M.union (params [("edit_site.url", "http://newacme.com")])
                                                    site_params)
    name "/site/:id/edit redirects" $
      redirects (post (B.append site_url "/edit") site_params)


-- App level helpers

-- Authentication
withUser :: SnapTesting App a -> SnapTesting App a
withUser = modifySite $ \site -> do
  (_, au) <- fmap fromJust getRandomUser
  with auth $ forceLogin au
  site

loginAs :: AuthUser -> SnapTesting App a -> SnapTesting App a
loginAs au = modifySite $ \site -> do
  with auth $ forceLogin au
  site

-- reasonably likely to be unique
generateEmail :: IO Text
generateEmail = do
  int <- randomIO :: IO Int
  return $ T.pack $ (show $ abs int) ++ "@test.com"

generateName :: IO Text
generateName = do
  int <- randomIO :: IO Int
  return $ T.pack $ "Person #" ++ (show $ abs int)

getRandomUser :: AppHandler (Maybe (Account, AuthUser))
getRandomUser = do
  em <- liftIO generateEmail
  name <- liftIO generateName
  res <- with auth $ createUser em "password"
  case res of
    -- NOTE(dbp 2014-01-03): These are bad errors, but I'm not sure how the types let us do better.
    Left failure -> do
      liftIO $ putStrLn "Could not create user"
      return Nothing
    Right au -> do
      let account = Account (fromJust $ userId au) name False
      newAccount account
      return (Just (account, au))

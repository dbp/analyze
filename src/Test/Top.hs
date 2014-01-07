{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Time.Calendar
import Data.Time.Clock
import System.Random (randomIO)
import System.Process (system)

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
main = runSnapTests [consoleReport, desktopReport] (route routes) app $ do
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

  let site' = (Site (-1) "Some Site" "http://acme.com"
                                            (UTCTime (fromGregorian 2014 1 1) 0)
                                            "http://acme.com/user/*"
                                            "http://acme.com/issue/*")
  site_id <- fmap fromJust $ eval (newSite site')
  let site = site' { siteId = site_id }
  (account, user) <- fmap fromJust $ eval getRandomUser
  eval (addSiteUser site account)
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
  withUser $ do
    name "/site/:id redirects without right user" $
      redirects (get site_url)
    name "/site/:id/edit redirects without right user" $
      redirects (get $ B.append site_url "/edit")

  cleanup clearAccounts $ cleanup clearSites $ loginAs user $ do
    name "/site/:id success with right login" $
      succeeds (get site_url)
    name "/site/:id has site name in response" $
      contains (get site_url) "Some Site"
    name "/site/:id/edit displays a page with a form on it" $
      contains (get $ B.append site_url "/edit") "<form"
    name "/site/:id/edit post changes url" $
      changes (const "http://newacme.com")
        (fmap (siteUrl.fromJust) $ getSite site_id)
        (post (B.append site_url "/edit") $ M.union (params [("edit_site.url", "http://newacme.com")])
                                                    site_params)
    name "/site/:id/edit redirects" $
      redirects (post (B.append site_url "/edit") site_params)

    token <- fmap fromJust $ eval (newToken site)
    name "/site/:id shows any tokens" $
      contains (get site_url) (tokenText token)
    eval (invalidateToken token)
    name "/site/:id should not show invalidated tokens" $
      notcontains (get site_url) (tokenText token)
    let new_token_link = B.append site_url "/token/new"
    name "/site/:id should have a link to create a new token" $
      contains (get site_url) (T.decodeUtf8 new_token_link)

    name "/site/:id/token/new should create a new token for site" $
      changes (+1) (fmap length (getTokens site)) (get new_token_link)

    name "/submit/visit should 404 without token" $
      notfound (post "/submit/visit" $ params [("url", "/foo"), ("render", "100")])
    name "/submit/visit should 404 without url" $
      notfound (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                              , ("render", "100")])
    name "/submit/visit should 404 without render" $
      notfound (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                              , ("url", "/foo")])
    name "/submit/visit should create a new entry in visits_queue" $
      changes (+1) (fmap length (siteVisitsQueue site))
        (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                       , ("url", "/foo")
                                       , ("render", "100")])
-- App level helpers
desktopReport :: ReportGenerator
desktopReport res = do
  let (passed, total) = count res
  case passed == total of
    True ->
      void $ system $ "notify-send -u low -t 1000 'All Tests Passing' 'All " ++ (show total) ++ " tests passed.'"
    False ->
      void $ system $ "notify-send -u normal -t 1000 'Some Tests Failing' '" ++ (show (total - passed)) ++ " out of " ++ (show total) ++ " tests failed.'"
 where count [] = (0, 0)
       count (ResultName _ children : xs) = count (children ++ xs)
       count (ResultPass _ : xs) = let (p, t) = count xs
                                   in (1 + p, 1 + t)
       count (ResultFail _ : xs) = let (p, t) = count xs
                                   in (p, 1 + t)

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

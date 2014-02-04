{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import System.Random (randomIO)

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Test.BDD

import Application
import Site
import Helpers.Text
import Handler.Top
import State.Accounts
import State.Sites
import Worker hiding (main)

main :: IO ()
main = do
  runSnapTests [consoleReport, linuxDesktopReport] (route routes) app $ do
  name "online variance" $
    quickCheck varianceProp
  name "/" $ do
    name "success" $
      succeeds (get "/")
    name "shows links to sites" $
      cleanup clearAccounts $ cleanup clearSites $ do
        (ac, au) <- fmap fromJust (eval getRandomUser)
        loginAs au $ do
          let site' = (Site (-1) "Some Site" "http://acme.com"
                                                   (UTCTime (fromGregorian 2014 1 1) 0)
                                                   "http://acme.com/user/*"
                                                   "http://acme.com/issue/*")
          site_id <- fmap fromJust $ eval (newSite site')
          let site = site' { siteId = site_id }
          let site_url = B.append "/site/" (T.encodeUtf8 $ tshow site_id)
          notcontains (get "/") (T.decodeUtf8 site_url)
          eval (addSiteUser site ac)
          contains (get "/") (T.decodeUtf8 site_url)
    name "shows a new site link" $ do
      withUser $ contains (get "/") "/site/new"
  name "/foo/bar not found" $
    notfound (get "/foo/bar")
  name "/auth/new_user" $ do
    name "success" $
      succeeds (get "/auth/new_user")
    name "creates a new account" $
      cleanup clearAccounts $
      changes (+1) countAccounts (post "/auth/new_user" $ params
                                  [ ("new_user.name", "Jane")
                                  , ("new_user.email", "jdoe@c.com")
                                  , ("new_user.password", "foobar")])

  name "/site/new" $ do
    name "redirect without login" $
      redirects (get "/site/new")
    cleanup clearAccounts $ withUser $ do
      name "success with login" $
        succeeds (get "/site/new")
      name "creates a new site" $
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
  token <- fmap fromJust $ eval (newToken site)
  cleanup clearAccounts $ cleanup clearSites $ name "/site/:id" $ do
    name "redirect without login" $
      redirects (get site_url)
    withUser $ do
      name "redirects without right user" $
        redirects (get site_url)
      name "/edit redirects without right user" $
        redirects (get $ B.append site_url "/edit")


    loginAs user $ do
      name "success with right login" $
        succeeds (get site_url)
      name "has site name in response" $
        contains (get site_url) "Some Site"
      name "/edit displays a page with a form on it" $
        contains (get $ B.append site_url "/edit") "<form"
      name "/edit post changes url" $
        changes (const "http://newacme.com")
          (fmap (siteUrl.fromJust) $ getSite site_id)
          (post (B.append site_url "/edit") $ M.union (params [("edit_site.url", "http://newacme.com")])
                                                      site_params)
      name "/edit redirects" $
        redirects (post (B.append site_url "/edit") site_params)

      name " shows any tokens" $
        contains (get site_url) (tokenText token)
      eval (invalidateToken token)
      name " should not show invalidated tokens" $
        notcontains (get site_url) (tokenText token)
      let new_token_link = B.append site_url "/token/new"
      name " should have a link to create a new token" $
        contains (get site_url) (T.decodeUtf8 new_token_link)

      name "/token/new should create a new token for site" $
        changes (+1) (fmap length (getTokens site)) (get new_token_link)

    name "/submit/visit" $ do
      name "should 404 without token" $
        notfound (post "/submit/visit" $ params [("url", "/foo"), ("render", "100"), ("method", "get")])
      name "should 404 without url" $
        notfound (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                                , ("render", "100")
                                                , ("method", "get")])
      name "should 404 without render" $
        notfound (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                                , ("url", "/foo")
                                                , ("method", "get")])
      name "should 404 without method" $
        notfound (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                                , ("url", "/foo")
                                                , ("render", "100")])
      name "should 404 with invalid token" $
        notfound (post "/submit/visit" $ params [("token", "BLAH")
                                                , ("url", "/foo")
                                                , ("render", "100")
                                                , ("method", "get")])
      name "should create a new entry in visits_queue" $
        changes (+1) (fmap length (siteVisitsQueue site))
          (post "/submit/visit" $ params [("token", T.encodeUtf8 $ tokenText token)
                                         , ("url", "/foo")
                                         , ("render", "100")
                                         , ("method", "get")])
      name "processVisits should result in a day visit, and empty queue" $ do
        changes' (\(a,b) -> (a - 1, b + 1)) ((\a b -> (length a, length b))
                                             <$> (siteVisitsQueue site)
                                             <*> (siteDayVisits site))
          (eval $ do vis <- getMarkVisits 1
                     processVisits vis)
        equals 1 (fmap length (siteDayVisits site))
        equals 1 (fmap length (getDaysWithVisits site))
      today <- liftIO getCurrentTime
      loginAs user $ name "day visits" $ do
        let day_url = B.append site_url (B.append "/day/" (B8.pack (formatTime defaultTimeLocale "%F" today)))
        name "show site should show link to day of visit" $
          contains (get site_url) (T.decodeUtf8 day_url)
        name "day url should exist" $ do
          succeeds (get day_url)
    name "/submit/error" $ do
       name "should 404 without token" $
         notfound (post "/submit/error" $ params [("url", "/foo"), ("message", "Maybe.fromJust")])
       name "should 404 without url" $
         notfound (post "/submit/error" $ params [("token", T.encodeUtf8 $ tokenText token), ("message", "Maybe.fromJust")])
       name "should 404 without message" $
         notfound (post "/submit/error" $ params [("token", T.encodeUtf8 $ tokenText token), ("url", "/foo")])
       name "should 404 with invalid token" $
         notfound (post "/submit/visit" $ params [("token", "BLAH")
                                                 , ("url", "/foo")
                                                 , ("message", "Maybe.fromJust")
                                                 , ("method", "get")])
       name "should create a new entry in errors_queue" $
         changes (+1) (fmap length (siteErrorsQueue site))
           (post "/submit/error" $ params [("token", T.encodeUtf8 $ tokenText token)
                                          , ("url", "/foo")
                                          , ("message", "Maybe.fromJust: Nothing")
                                          , ("uid", "1")])
       name "processErrors should result in a new error, and empty queue" $ do
          changes' (\(a,b,c) -> (a - 1, b + 1, c + 1)) ((\a b c -> (length a, length b, c))
                                                        <$> (siteErrorsQueue site)
                                                        <*> (getSiteErrors site)
                                                        <*> countErrorExamples)
           (eval $ do errs <- getMarkErrors 1
                      processErrors errs)
          equals 1 (fmap length (getSiteErrors site))
       loginAs user $ name "error summaries" $ do
         er <- fmap head (eval $ getSiteErrors site)
         let er_url = B.append site_url (B.append "/error/" (B8.pack (show (errorId er))))
         name "show site should show link to error summary" $
           contains (get site_url) (T.decodeUtf8 er_url)
         name "error url should exist" $ do
           succeeds (get er_url)
         name "toggling error should result in resolved changing" $ do
           redirectsto (post (B.append er_url "/resolve") $ params []) (T.decodeUtf8 er_url)
           equals (Just True) (fmap (fmap (isJust . errorResolved)) (getErrorById site (errorId er)))
           redirectsto (post (B.append er_url "/resolve") $ params []) (T.decodeUtf8 er_url)
           equals (Just False) (fmap (fmap (isJust . errorResolved)) (getErrorById site (errorId er)))
         name "posting to issue should set issue id" $ do
           redirectsto (post (B.append er_url "/issue") $ params [("issue", "1")])
                       (T.decodeUtf8 er_url)
           equals (Just (Just "1")) (fmap (fmap errorIssueId) (getErrorById site (errorId er)))
         name "posting to issue without id should clear it" $ do
           redirectsto (post (B.append er_url "/issue") $ params [])
                       (T.decodeUtf8 er_url)
           equals (Just Nothing) (fmap (fmap errorIssueId) (getErrorById site (errorId er)))
         name "submitting a resolved error should mark it as unresolved"  $ do
           now <- liftIO getCurrentTime
           eval (updateErrorSummary (siteId site) (er { errorResolved = Just now}))
           succeeds (post "/submit/error" $ params [("token", T.encodeUtf8 $ tokenText token)
                                                    , ("url", "/foo")
                                                    , ("message", "Maybe.fromJust: Nothing")
                                                    , ("uid", "1")])
           eval (do errs <- getMarkErrors 1
                    processErrors errs)
           equals (Just Nothing) (fmap (fmap errorResolved) (getErrorById site (errorId er)))


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

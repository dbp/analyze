{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (bracket)
import System.Random (randomIO)
import Snap.Core
import Test.HUnit
import Control.Monad.Trans
import Snap.Test hiding (runHandler, evalHandler)
import Snap.Snaplet
import Snap.Snaplet.Test
import Snap.Snaplet.Auth
import Application
import Site
import Handler.Top
import State.Accounts
import State.Sites

main :: IO ()
main = do
  putStrLn "Running tests..."
  putStrLn "/ success"
  run (get' "/") site assertSuccess
  putStrLn "/foo/bar not found"
  run (get' "/foo/bar") site assert404

  putStrLn "/auth/new_user success"
  run (get' "/auth/new_user") site assertSuccess
  putStrLn "/auth/new_user creates a new account"
  cleanup clearAccounts $
    changes (+1) countAccounts (post' "/auth/new_user"
                                (M.fromList [ ("new_user.name", ["Jane"])
                                            , ("new_user.email", ["jdoe@c.com"])
                                            , ("new_user.password", ["foobar"])]))
  putStrLn "/site/new redirect without login"
  run (get' "/site/new") site assertRedirect
  putStrLn "/site/new success with login"
  run (get' "/site/new") (withUser site) assertSuccess
  putStrLn "/site/new creates a new site"
  cleanup clearAccounts $ cleanup clearSites $
    changes' (withUser site) (+1) countSites
    (post' "/site/new" (M.fromList [
                            ("new_site.name", ["Acme"])
                            , ("new_site.url", ["http://acme.com"])
                            , ("new_site.start_date.year", ["2014"])
                            , ("new_site.start_date.month", ["1"])
                            , ("new_site.start_date.day", ["3"])
                            , ("new_site.user_link_pattern", ["http://acme.com/user/*"])
                            , ("new_site.issue_link_pattern", ["http://acme.com/issue/*"])
                            ]))

-- Helpers follow
site :: AppHandler ()
site = route routes

run :: RequestBuilder IO () -> AppHandler a -> (Response -> Assertion) -> IO ()
run req hndlr asrt = do
  res <- runHandler Nothing req hndlr app
  case res of
    Left err -> assertFailure (show err)
    Right response -> asrt response

get' = (flip get) mempty
post' url params = postUrlEncoded url params

changes :: (Show a, Eq a) => (a -> a) -> AppHandler a -> RequestBuilder IO () -> IO ()
changes = changes' site

changes' :: (Show a, Eq a) => AppHandler () -> (a -> a) -> AppHandler a
            -> RequestBuilder IO () -> IO ()
changes' hndlr delta measure req = do
  before <- eval' measure
  runHandler (Just "test") req hndlr app
  after <- eval' measure
  assertEqual "Expected value to change" (delta before) after

eval' hndlr = fmap (either (error. T.unpack) id) $ evalHandler (Just "test") (get' "") hndlr app

cleanup :: AppHandler () -> IO () -> IO ()
cleanup cu act = bracket (return ()) (\_ -> runHandler (Just "test") (get' "") cu app) (const act)
                 >> return ()

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

withAdmin :: AppHandler a -> AppHandler a
withAdmin hndlr = do
  em <- liftIO generateEmail
  name <- liftIO generateName
  res <- with auth $ createUser em "password"
  case res of
    Left failure ->  do
      liftIO $ putStrLn "Could not create admin"
      hndlr
    Right au -> do
      newAccount (Account (fromJust $ userId au) name True)
      with auth $ forceLogin au
      hndlr

withLogin :: Text -> AppHandler a -> AppHandler a
withLogin id' hndlr = do
  user <- with auth $ withBackend $ \r -> liftIO $ (lookupByUserId r (UserId id'))
  case user of
    Nothing ->  do
      liftIO $ putStrLn "Could not find user"
      hndlr
    Just user' -> do
      with auth $ forceLogin user'
      hndlr

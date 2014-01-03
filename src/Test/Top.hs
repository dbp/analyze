{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomIO)
import Snap.Core
import Test.HUnit
import Snap.Test hiding (runHandler, evalHandler)
import Snap.Snaplet.Test
import Snap.Snaplet.Auth
import Application
import Site
import Handler.Top
import State.Accounts

main :: IO ()
main = do
  run (get' "/") site assertSuccess
  run (get' "/foo/bar") site assert404
  cleanup clearAccounts $
    changes (+1) countAccounts (post' "/new_user" (M.fromList [ ("new_user.name", ["Jane"])
                                                              , ("new_user.email", ["jdoe@c.com"])
                                                              , ("new_user.password", ["foobar"])]))

  cleanup clearSites $
    changes (+1) countSites (post' /)

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
changes delta measure req = do
  before <- eval' measure
  runHandler (Just "test") req site app
  after <- eval' measure
  assertEqual "Expected value to change" (delta before) after

eval' hndlr = fmap (either (error. T.unpack) id) $ evalHandler (Just "test") (get' "") hndlr app

cleanup :: AppHandler () -> IO () -> IO ()
cleanup cu act = do
  act
  runHandler (Just "test") (get' "") cu app
  return ()

-- reasonably likely to be unique
generateEmail :: IO Text
generateEmail = do
  int <- randomIO :: IO Int64
  return $ T.pack $ (show int) ++ "@test.com"

withUser :: AppHandler a -> AppHandler a
withUser hndlr = do
  em <- liftIO generateEmail
  res <- with auth $ createUser em "password"
  case res of
    -- NOTE(dbp 2014-01-03): These are bad errors, but I'm not sure how the types let us do better.
    Left failure -> hndlr
    Right au -> do
      newAccount (Account (unUid $ userId au) name False)
      with auth $ forceLogin au
      hndlr

withAdmin :: AppHandler a -> AppHandler a
withAdmin hndlr = do
  em <- liftIO generateEmail
  res <- with auth $ createUser em "password"
  case res of
    Left failure -> hndlr
    Right au -> do
      newAccount (Account (unUid $ userId au) name True)
      with auth $ forceLogin au
      hndlr

withLogin :: Text -> AppHandler a -> AppHandler a
withLogin id' hndlr = do
  user <- with auth $ withBackend $ lookupByUserId (UserId id')
  case user of
    Nothing -> hndlr
    Just user' -> do
      with auth $ forceLogin user'
      hndlr

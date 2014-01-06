{-# LANGUAGE OverloadedStrings #-}

module Test.Top where

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Time.Calendar
import Data.Time.Clock
import Control.Exception (bracket)
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Random (randomIO)
import Snap.Core
import Test.HUnit
import Control.Monad.Trans
import qualified Snap.Test as Test hiding (runHandler, evalHandler)
import Snap.Snaplet
import qualified Snap.Snaplet.Test as Test
import Snap.Snaplet.Auth
import Application
import Site
import Helpers.Text
import Handler.Top
import State.Accounts
import State.Sites

type SnapTesting a = StateT (AppHandler ()) IO a
type TestRequest = Test.RequestBuilder IO ()

runSnapTests :: SnapTesting () -> IO ()
runSnapTests = (flip evalStateT (route routes))

tlog :: String -> SnapTesting ()
tlog = lift . putStrLn

main :: IO ()
main = runSnapTests $ do
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
  tcleanup clearAccounts $ twithUser $ do
    tlog "/site/:id success with login"
    tsucceeds (tget site_url)
    tlog "/site/:id has site name in response"
    tresponds (tget site_url) "Some Site"

-- Requests
tget :: ByteString -> TestRequest
tget s = (Test.get s mempty)

tpost :: ByteString -> [(ByteString, ByteString)] -> TestRequest
tpost url params = Test.postUrlEncoded url (M.fromList $ map (\x -> (fst x, [snd x])) params)

-- Assertions
tsucceeds :: TestRequest -> SnapTesting ()
tsucceeds req = do
  site <- get
  lift $ run req site Test.assertSuccess

tnotfound :: TestRequest -> SnapTesting ()
tnotfound req = do
  site <- get
  lift $ run req site Test.assert404

tredirects :: TestRequest -> SnapTesting ()
tredirects req = do
  site <- get
  lift $ run req site Test.assertRedirect

tchanges :: (Show a, Eq a) => (a -> a) -> AppHandler a -> TestRequest -> SnapTesting ()
tchanges delta measure req = do
  site <- get
  before <- teval measure
  lift $ Test.runHandler (Just "test") req site app
  after <- teval measure
  lift $ assertEqual "Expected value to change" (delta before) after

tresponds :: TestRequest -> Text -> SnapTesting ()
tresponds req mtch = do
  site <- get
  lift $ run req site (Test.assertBodyContains (T.encodeUtf8 mtch))

-- Authentication
twithUser :: SnapTesting a -> SnapTesting a
twithUser act = do
  site <- get
  put (withUser site)
  res <- act
  put site
  return res

-- Clean up
tcleanup :: AppHandler () -> SnapTesting () -> SnapTesting ()
tcleanup cu act = do
  act
  lift $ Test.runHandler (Just "test") (tget "") cu app
  return ()

-- Arbitrary code
teval :: AppHandler a -> SnapTesting a
teval act =
  lift $ fmap (either (error. T.unpack) id) $ Test.evalHandler (Just "test") (tget "") act app


run :: Test.RequestBuilder IO () -> AppHandler a -> (Response -> Assertion) -> IO ()
run req hndlr asrt = do
  res <- Test.runHandler (Just "test") req hndlr app
  case res of
    Left err -> assertFailure (show err)
    Right response -> asrt response

-- App level helpers

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

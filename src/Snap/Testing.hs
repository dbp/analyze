{-# LANGUAGE OverloadedStrings #-}

module Snap.Testing where

import Data.Map (fromList)
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid (mempty)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Snap.Core (Response)
import Snap.Snaplet (Handler, SnapletInit)
import Snap.Test (RequestBuilder,
                  assertSuccess, assert404, assertRedirect, assertBodyContains)
import qualified Snap.Test as Test
import Snap.Snaplet.Test (runHandler, evalHandler)
import Test.HUnit (Assertion, assertEqual, assertFailure)

-- Basic Types
type SnapTesting b a = StateT (Handler b b (), SnapletInit b b) IO a
type TestRequest = RequestBuilder IO ()

runSnapTests :: Handler b b () -> SnapletInit b b -> SnapTesting b () -> IO ()
runSnapTests site app = flip evalStateT (site, app)

-- Logging
tlog :: String -> SnapTesting b ()
tlog = lift . putStrLn

-- Requests
tget :: ByteString -> TestRequest
tget = flip Test.get mempty

tpost :: ByteString -> [(ByteString, ByteString)] -> TestRequest
tpost url params = Test.postUrlEncoded url (fromList $ map (\x -> (fst x, [snd x])) params)

-- Assertions
tsucceeds :: TestRequest -> SnapTesting b ()
tsucceeds req = run req assertSuccess

tnotfound :: TestRequest -> SnapTesting b ()
tnotfound req = run req assert404

tredirects :: TestRequest -> SnapTesting b ()
tredirects req = run req assertRedirect

tchanges :: (Show a, Eq a) => (a -> a) -> Handler b b a -> TestRequest -> SnapTesting b ()
tchanges delta measure req = do
  (site, app) <- get
  before <- teval measure
  _ <- lift $ runHandler (Just "test") req site app
  after <- teval measure
  lift $ assertEqual "Expected value to change" (delta before) after

tresponds :: TestRequest -> Text -> SnapTesting b ()
tresponds req mtch = run req (assertBodyContains (encodeUtf8 mtch))

-- Clean up
tcleanup :: Handler b b () -> SnapTesting b () -> SnapTesting b ()
tcleanup cu act = do
  act
  (_, app) <- get
  _ <- lift $ runHandler (Just "test") (tget "") cu app
  return ()

-- Arbitrary code
teval :: Handler b b a -> SnapTesting b a
teval act = do
  (_, app) <- get
  lift $ fmap (either (error. unpack) id) $ evalHandler (Just "test") (tget "") act app


run :: TestRequest ->  (Response -> Assertion) -> SnapTesting b ()
run req asrt = do
  (site, app) <- get
  res <- lift $ runHandler (Just "test") req site app
  case res of
    Left err -> lift $ assertFailure (show err)
    Right response -> lift $ asrt response

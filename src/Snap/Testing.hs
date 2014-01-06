{-# LANGUAGE OverloadedStrings #-}

module Snap.Testing
       ( SnapTesting
       , TestRequest
       , runSnapTests
       , name
       , get
       , post
       , succeeds
       , notfound
       , redirects
       , changes
       , responds
       , cleanup
       , eval
       ) where

import Data.Map (fromList)
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid (mempty)
import Control.Monad.Trans
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as S (get, put)
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
name :: String -> SnapTesting b () -> SnapTesting b ()
name s a = do
  lift $ putStrLn s
  a

-- Requests
get :: ByteString -> TestRequest
get = flip Test.get mempty

post :: ByteString -> [(ByteString, ByteString)] -> TestRequest
post url params = Test.postUrlEncoded url (fromList $ map (\x -> (fst x, [snd x])) params)

-- Assertions
succeeds :: TestRequest -> SnapTesting b ()
succeeds req = run req assertSuccess

notfound :: TestRequest -> SnapTesting b ()
notfound req = run req assert404

redirects :: TestRequest -> SnapTesting b ()
redirects req = run req assertRedirect

changes :: (Show a, Eq a) => (a -> a) -> Handler b b a -> TestRequest -> SnapTesting b ()
changes delta measure req = do
  (site, app) <- S.get
  before <- eval measure
  _ <- lift $ runHandler (Just "test") req site app
  after <- eval measure
  lift $ assertEqual "Expected value to change" (delta before) after

responds :: TestRequest -> Text -> SnapTesting b ()
responds req mtch = run req (assertBodyContains (encodeUtf8 mtch))

-- Clean up
cleanup :: Handler b b () -> SnapTesting b () -> SnapTesting b ()
cleanup cu act = do
  act
  (_, app) <- S.get
  _ <- lift $ runHandler (Just "test") (get "") cu app
  return ()

-- Arbitrary code
eval :: Handler b b a -> SnapTesting b a
eval act = do
  (_, app) <- S.get
  lift $ fmap (either (error. unpack) id) $ evalHandler (Just "test") (get "") act app


run :: TestRequest ->  (Response -> Assertion) -> SnapTesting b ()
run req asrt = do
  (site, app) <- S.get
  res <- lift $ runHandler (Just "test") req site app
  case res of
    Left err -> lift $ assertFailure (show err)
    Right response -> lift $ asrt response

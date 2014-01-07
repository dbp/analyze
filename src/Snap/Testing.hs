{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Snap.Testing
       ( SnapTesting
       , TestRequest
       , ReportGenerator
       , TestResult(..)
       , runSnapTests
       , consoleReport
       , name
       , get
       , post
       , params
       , succeeds
       , notfound
       , redirects
       , changes
       , contains
       , notcontains
       , cleanup
       , eval
       , modifySite
       ) where

import           Data.Map (Map, fromList)
import           Data.ByteString (ByteString)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T (append)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Monoid (mempty)
import           Data.Maybe (fromJust)
import           Control.Monad (liftM, zipWithM)
import           Control.Monad.Trans
import           Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as S (get, put)
import           Control.Monad.Trans.Writer (WriterT(..), tell)
import           Control.Exception (SomeException, catch)
import           Snap.Core (Response(..), getHeader)
import           Snap.Snaplet (Handler, SnapletInit)
import           Snap.Test (RequestBuilder, getResponseBody)
import qualified Snap.Test as Test
import           Snap.Snaplet.Test (runHandler, evalHandler)
import           Text.Regex.Posix ((=~))

-- Basic Types
type SnapTesting b a = WriterT [TestLog] (StateT (Handler b b (), SnapletInit b b) IO) a
type TestRequest = RequestBuilder IO ()
type ReportGenerator = [TestResult] -> IO ()
data TestResult = ResultName Text [TestResult] | ResultPass Text | ResultFail Text
-- TestLog is the flat datastructure that will be turned into the TestResult tree
data TestLog = NameStart Text | NameEnd | TestPass Text | TestFail Text deriving Show

runSnapTests :: [ReportGenerator] -> Handler b b () -> SnapletInit b b -> SnapTesting b () -> IO ()
runSnapTests rgs site app tests = do
  testlog <- liftM snd $ evalStateT (runWriterT tests) (site, app)
  let res = fst $ buildResult [] testlog
  _ <- zipWithM ($) rgs (repeat res)
  return ()

buildResult :: [TestResult] -> [TestLog] -> ([TestResult], [TestLog])
buildResult acc [] = (acc, [])
buildResult acc ((NameStart nm):xs) =
  let (cur, rest) = buildResult [] xs in
  buildResult (acc ++ [(ResultName nm cur)]) rest
buildResult acc (NameEnd:xs) = (acc, xs)
buildResult acc ((TestPass desc):xs) = buildResult (acc ++ [ResultPass desc]) xs
buildResult acc ((TestFail desc):xs) = buildResult (acc ++ [ResultFail desc]) xs

-- Report generators
consoleReport :: ReportGenerator
consoleReport = cg 0
  where cg _ [] = return ()
        cg indent (ResultName n children : xs) = do
          fmt indent n
          cg (indent + 2) children
          cg indent xs
        cg indent (ResultPass n : xs) = do
          fmt indent (T.append "PASSED " n)
          cg indent xs
        cg indent (ResultFail n : xs) = do
          fmt indent (T.append "FAILED: " n)
          cg indent xs
        fmt indent t = putStrLn $ replicate indent ' ' ++ unpack t

-- Logging
name :: Text -> SnapTesting b () -> SnapTesting b ()
name s a = do
  tell [NameStart s]
  a
  tell [NameEnd]

-- Requests
get :: ByteString -> TestRequest
get = flip Test.get mempty

post :: ByteString -> Map ByteString [ByteString] -> TestRequest
post = Test.postUrlEncoded

params :: [(ByteString, ByteString)] -> Map ByteString [ByteString]
params = fromList . map (\x -> (fst x, [snd x]))

-- Assertions
succeeds :: TestRequest -> SnapTesting b ()
succeeds req = run req testSuccess

notfound :: TestRequest -> SnapTesting b ()
notfound req = run req test404

redirects :: TestRequest -> SnapTesting b ()
redirects req = run req testRedirect

changes :: (Show a, Eq a) => (a -> a) -> Handler b b a -> TestRequest -> SnapTesting b ()
changes delta measure req = do
  (site, app) <- lift S.get
  before <- eval measure
  _ <- liftIO $ runHandlerSafe req site app
  after <- eval measure
  res <- testEqual "Expected value to change" (delta before) after
  tell [res]

contains :: TestRequest -> Text -> SnapTesting b ()
contains req mtch = run req (testBodyContains (encodeUtf8 mtch))

notcontains :: TestRequest -> Text -> SnapTesting b ()
notcontains req mtch = run req (testBodyNotContains (encodeUtf8 mtch))

-- Clean up
cleanup :: Handler b b () -> SnapTesting b () -> SnapTesting b ()
cleanup cu act = do
  act
  (_, app) <- lift S.get
  _ <- liftIO $ runHandlerSafe (get "") cu app
  return ()

-- Arbitrary code
eval :: Handler b b a -> SnapTesting b a
eval act = do
  (_, app) <- lift S.get
  liftIO $ fmap (either (error. unpack) id) $ evalHandlerSafe act app


-- Helpers to build app specific functionality
modifySite :: (Handler b b () -> Handler b b ()) -> SnapTesting b a -> SnapTesting b a
modifySite f act = do
  (site, app) <- lift S.get
  lift $ S.put (f site, app)
  res <- act
  lift $ S.put (site, app)
  return res

-- Private helpers
runHandlerSafe :: TestRequest -> Handler b b v -> SnapletInit b b -> IO (Either Text Response)
runHandlerSafe req site app =
  catch (runHandler (Just "test") req site app) (\(e::SomeException) -> return $ Left (pack $ show e))

evalHandlerSafe :: Handler b b v -> SnapletInit b b -> IO (Either Text v)
evalHandlerSafe act app =
  catch (evalHandler (Just "test") (get "") act app) (\(e::SomeException) -> return $ Left (pack $ show e))


run :: TestRequest -> (Response -> SnapTesting b TestLog) -> SnapTesting b ()
run req asrt = do
  (site, app) <- lift S.get
  res <- liftIO $ runHandlerSafe req site app
  case res of
    Left err -> tell [TestFail $ T.append "Handler returned an error: " err]
    Right response -> do
      testlog <- asrt response
      tell [testlog]

-- Low level matchers - these parallel HUnit assertions in Snap.Test

testEqual :: (Eq a, Show a) => Text -> a -> a -> SnapTesting b TestLog
testEqual msg a b = return $ if a == b then TestPass "" else TestFail msg

testBool :: Text -> Bool -> SnapTesting b TestLog
testBool msg b = return $ if b then TestPass "" else TestFail msg

testSuccess :: Response -> SnapTesting b TestLog
testSuccess rsp = testEqual message 200 status
  where
    message = pack $ "Expected success (200) but got (" ++ (show status) ++ ")"
    status  = rspStatus rsp

test404 :: Response -> SnapTesting b TestLog
test404 rsp = testEqual message 404 status
  where
    message = pack $ "Expected Not Found (404) but got (" ++ (show status) ++ ")"
    status = rspStatus rsp

testRedirectTo :: ByteString
                  -> Response
                  -> SnapTesting b TestLog
testRedirectTo uri rsp = do
    testRedirect rsp
    testEqual message uri rspUri
  where
    rspUri = fromJust $ getHeader "Location" rsp
    message = pack $ "Expected redirect to " ++ show uri
              ++ " but got redirected to "
              ++ show rspUri ++ " instead"

testRedirect :: Response -> SnapTesting b TestLog
testRedirect rsp = testBool message (300 <= status && status <= 399)
  where
    message = pack $ "Expected redirect but got status code ("
              ++ show status ++ ")"
    status  = rspStatus rsp


containsGen :: (Bool -> Bool) -> Text -> ByteString -> Response -> SnapTesting b TestLog
containsGen b message match rsp =
  do
    body <- liftIO $ getResponseBody rsp
    return $ if (b (body =~ match)) then TestPass "" else TestFail message
  where
    message = pack $ "Expected body to not match regexp \"" ++ show match
              ++ "\", but did"

testBodyContains :: ByteString  -- ^ Regexp that will match the body content
                -> Response
                -> SnapTesting b TestLog
testBodyContains match = containsGen id message match
  where
    message = pack $ "Expected body to match regexp \"" ++ show match
              ++ "\", but didn't"


testBodyNotContains :: ByteString  -- ^ Regexp that will match the body content
                   -> Response
                   -> SnapTesting b TestLog
testBodyNotContains match = containsGen not message match
  where
    message = pack $ "Expected body to not match regexp \"" ++ show match
              ++ "\", but did"

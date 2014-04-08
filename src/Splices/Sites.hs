{-# LANGUAGE OverloadedStrings #-}

module Splices.Sites where

import           Snap.Snaplet.Heist
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
import           Data.Time.Calendar
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Monoid
import           Snap.Core
---------------------------------------
import           Application
import           State.Sites
import           Helpers.Text

fillInSplice :: T.Text -> I.Splice AppHandler
fillInSplice template = do
  n <- getParamNode
  let mi =  X.getAttribute "id" n
  case mi of
    Nothing -> return []
    Just i -> do
      let nw = T.replace "*" i template
      return [X.TextNode nw]

sitesSplice :: [Site] -> I.Splice AppHandler
sitesSplice = I.mapSplices (I.runChildrenWith . siteSplice)

siteSplice :: Site -> Splices (I.Splice AppHandler)
siteSplice (Site id' name url start_date user_link issue_link) = do
  "id" ## I.textSplice $ T.pack $ show id'
  "name" ## I.textSplice name
  "url" ## I.textSplice url
  "start-date" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F" start_date
  "user-link" ## fillInSplice user_link
  "issue-link" ## fillInSplice issue_link


tokensSplice :: [SiteToken] -> I.Splice AppHandler
tokensSplice = I.mapSplices (I.runChildrenWith . tokenSplice)

tokenSplice :: SiteToken -> Splices (I.Splice AppHandler)
tokenSplice (SiteToken token invalidated created site_id) = do
  "token" ## I.textSplice token
  "if-invalidated" ## ifISplice (isJust invalidated)
  "not-invalidated" ## ifISplice (isNothing invalidated)
  "created" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F" created

daysWithVisitsSplice :: [Day] -> I.Splice AppHandler
daysWithVisitsSplice = I.mapSplices (I.runChildrenWith . (\d -> "formatted" ## I.textSplice (T.pack $ formatTime defaultTimeLocale "%F" d)))


dayVisitsSplice :: [DayVisit] -> I.Splice AppHandler
dayVisitsSplice = I.mapSplices (I.runChildrenWith . dayVisitSplice)

dayVisitSplice :: DayVisit -> Splices (I.Splice AppHandler)
dayVisitSplice (DayVisit day si url meth hits mx mn avg var) = do
  "date" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F" day
  "site-id" ## I.textSplice $ tshow si
  "url" ## I.textSplice $ fromMaybe "NONE" url
  "url-encoded" ## I.textSplice $ maybe "NONE" (T.decodeUtf8 . urlEncode . T.encodeUtf8) url
  "method" ## I.textSplice meth
  "hits" ## I.textSplice $ tshow hits
  "max" ## I.textSplice $ tshow' 4 mx
  "min" ## I.textSplice $ tshow' 4 mn
  "avg" ## I.textSplice $ tshow' 4 avg
  "var" ## I.textSplice $ tshow' 4 var


errorsLastExSplice :: [(ErrorSummary, Maybe ErrorExample)] -> I.Splice AppHandler
errorsLastExSplice = I.mapSplices (I.runChildrenWith . errorLastExSplices)
  where errorLastExSplices (e, mx) =
          (errorSplices e) <> (maybe mempty (("example" ##) . I.runChildrenWith . exampleSplices)) mx

errorsSplice :: [ErrorSummary] -> I.Splice AppHandler
errorsSplice = I.mapSplices (I.runChildrenWith . errorSplices)

errorSplices :: ErrorSummary -> Splices (I.Splice AppHandler)
errorSplices (ErrorSummary i si m r c ii) = do
  "id" ## I.textSplice (tshow i)
  "site-id" ## I.textSplice (tshow si)
  "message" ## I.textSplice m
  "is-resolved" ## ifISplice (isJust r)
  "not-resolved" ## ifISplice (isNothing r)
  "resolved" ## I.textSplice  $ maybe "" (T.pack . formatTime defaultTimeLocale "%F %T") r
  "created" ## I.textSplice $ T.pack $ formatTime defaultTimeLocale "%F %T" c
  "issue-id" ## I.textSplice (fromMaybe "" ii)
  "has-issue-id" ## ifISplice (isJust ii)
  "no-issue-id" ## ifISplice (not $ isJust ii)

examplesSplices :: [ErrorExample] -> I.Splice AppHandler
examplesSplices = I.mapSplices (I.runChildrenWith . exampleSplices)

exampleSplices :: ErrorExample -> Splices (I.Splice AppHandler)
exampleSplices (ErrorExample i ei u t ui) = do
  "id" ## I.textSplice (tshow i)
  "error-id" ## I.textSplice (tshow ei)
  "url" ## I.textSplice u
  "time" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F %T" t
  "user-id" ## I.textSplice (fromMaybe "" ui)
  "has-user-id" ## ifISplice (isJust ui)

{-# LANGUAGE OverloadedStrings #-}

module Splices.Sites where

import           Snap.Snaplet.Heist
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I
import           Data.Time.Calendar
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import           Data.Maybe
---------------------------------------
import           Application
import           State.Sites
import           Helpers.Text

sitesSplice :: [Site] -> I.Splice AppHandler
sitesSplice = I.mapSplices (I.runChildrenWith . siteSplice)

siteSplice :: Site -> Splices (I.Splice AppHandler)
siteSplice (Site id' name url start_date user_link issue_link) = do
  "id" ## I.textSplice $ T.pack $ show id'
  "name" ## I.textSplice name
  "url" ## I.textSplice url
  "start_date" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F" start_date


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
dayVisitSplice (DayVisit day si url hits mx mn avg var) = do
  "date" ## I.textSplice  $ T.pack $ formatTime defaultTimeLocale "%F" day
  "site-id" ## I.textSplice $ tshow si
  "url" ## I.textSplice $ fromMaybe "NONE" url
  "hits" ## I.textSplice $ tshow hits
  "max" ## I.textSplice $ tshow' 4 mx
  "min" ## I.textSplice $ tshow' 4 mn
  "avg" ## I.textSplice $ tshow' 4 avg
  "var" ## I.textSplice $ tshow' 4 var

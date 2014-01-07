{-# LANGUAGE OverloadedStrings #-}

module Splices.Sites where

import           Snap.Snaplet.Heist
import           Heist
import           Heist.Splices
import qualified Heist.Interpreted as I
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import           Data.Maybe
---------------------------------------
import           Application
import           State.Sites

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

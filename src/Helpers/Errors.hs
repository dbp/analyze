{-# LANGUAGE OverloadedStrings #-}

module Helpers.Errors where

import           Heist
import qualified Heist.Interpreted as I
import           Data.Text (Text)
import           Snap.Snaplet.Heist
import           Application

-- record an error on the backend, no user facing result.
registerError :: Text -> Maybe Text -> AppHandler ()
registerError err extra = return ()

-- display an error to the user, recording only if priv is Just.
displayError :: Text -> Maybe Text -> AppHandler ()
displayError err priv = renderWithSplices "errors/display" ("message" ## I.textSplice err)

-- display an error to user and record it on the backend.
failureError :: Text -> Maybe Text -> AppHandler ()
failureError err extra = return ()

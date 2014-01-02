{-# LANGUAGE OverloadedStrings #-}

module Handler.Top
       (notFoundHandler)
       where

import Snap.Snaplet.Heist
import Application

notFoundHandler :: AppHandler ()
notFoundHandler = render "not_found"

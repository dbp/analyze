{-# LANGUAGE OverloadedStrings #-}

module Helpers.Auth where

import Snap.Snaplet
import Snap.Core
import Snap.Snaplet.Auth
import qualified Data.ByteString as B

import Application
import State.Accounts
import Helpers.Errors

withAccount :: (Account -> AppHandler ()) -> AppHandler ()
withAccount hndlr = do
  au <- with auth currentUser
  case au of
    Nothing -> loginRedirect
    Just user ->
      case (userId user) of
        Nothing -> failureError "No id on user account." Nothing
        Just uid -> do
          ac <- getAccount uid
          case ac of
            Nothing -> failureError "No account for user." (Just $ unUid uid)
            Just account -> hndlr account


loginRedirect :: AppHandler ()
loginRedirect = do
  url <- fmap rqURI getRequest
  redirect $ B.concat [rootUrl, "/auth/login", "?redirect=", urlEncode url]

{-# LANGUAGE OverloadedStrings #-}

module Handler.Auth
       (authRoutes)
       where

import           Control.Applicative
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Maybe
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Heist
import qualified Heist.Interpreted as I
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Digestive.Heist
-----------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.Text
import           Helpers.Forms
import           State.Accounts

authRoutes :: AppHandler ()
authRoutes = route [ ("/login", with auth handleLoginSubmit)
                   , ("/logout", with auth handleLogout)
                   , ("/new_user", handleNewUser)
                   ]

handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (do r <- getParam "redirect"
                                          case r of
                                            Nothing -> redirect "/"
                                            Just url -> redirect url)
  where
    err = Just "Unknown user or password"


handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

data NewUserData = NewUserData Text Text Text
newUserForm :: Form Text AppHandler NewUserData
newUserForm = NewUserData
  <$> "name" .: nameForm Nothing
  <*> "email" .: emailForm Nothing
  <*> "password" .: passwordForm

handleNewUser :: AppHandler ()
handleNewUser = do
  r <- runForm "new_user" newUserForm
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (NewUserData name email password)) -> do
      res <- with auth $ createUser email (T.encodeUtf8 password)
      case res of
        Left failure -> render' v  (Just (tshow failure))
        Right user ->
          case userId user of
            Nothing -> render' v (Just "Error A1: Could not create account.")
            Just uid -> do
              newAccount (Account uid name False)
              redirect "/"
 where render' v msg = renderWithSplices "auth/new_user"
                       (digestiveSplices v <>
                        ("message" ## I.textSplice (fromMaybe "" msg)))

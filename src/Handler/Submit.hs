{-# LANGUAGE OverloadedStrings #-}

module Handler.Submit (submitRoutes) where


import           Control.Applicative
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Calendar
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
import           Helpers.Auth
import           Helpers.Misc
import           State.Sites

submitRoutes :: AppHandler ()
submitRoutes  = route
                [ ("/visit", visitHandler)
                ]

visitHandler :: AppHandler ()
visitHandler = do
  mt <- getParam "token"
  mu <- getParam "url"
  mr <- getParam "render"
  case (,,) <$> mt <*> mu <*> ((fmap B8.unpack mr) >>= readSafe) of
    Nothing -> pass
    Just (t, u, r) -> do
      mtoken <- getToken (T.decodeUtf8 t)
      case mtoken of
        Nothing -> pass
        Just token -> do
          newSiteVisit (SiteVisit (-1) (tokenSiteId token) (T.decodeUtf8 u) r (UTCTime (fromGregorian 0 0 0) 0))
          return ()

{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Monoid
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Heist
import           Heist.Splices.BindStrict
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Application
import           Handler.Top (routes)

rebindSplice :: I.Splice AppHandler
rebindSplice = do
  node <- getParamNode
  let attrs = do o <- X.getAttribute "old" node
                 n <- X.getAttribute "new" node
                 return (o, n)
  case attrs of
    Nothing -> return []
    Just (old, new) -> do
      st <- getHS
      let spl = I.lookupSplice old st
      case spl of
        Nothing -> return []
        Just splice -> do
           modifyHS $ I.bindSplice new splice
           return []

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An analyzer application" Nothing $ do
    let defaultHeistConfig = mempty { hcLoadTimeSplices = defaultLoadTimeSplices
                                    , hcInterpretedSplices = do
                                        "rebind" ## rebindSplice
                                        bindStrictTag ## bindStrictImpl }
    h <- nestSnaplet "" heist $ heistInit' "templates" defaultHeistConfig
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d

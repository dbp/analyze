{-# LANGUAGE OverloadedStrings #-}

module State.Accounts where

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth (UserId(..))
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.State


data Account = Account { accountId :: UserId
                       , accountName :: Text
                       , accountAdmin :: Bool
                       }

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field

instance ToField UserId where
  toField (UserId uid) = toField uid

-- copied from snaplet-postgresql-simple, since it isn't exported
buildUid :: Int -> UserId
buildUid = UserId . T.pack . show
instance FromField UserId where
    fromField f v = buildUid <$> fromField f v


clearAccounts :: AppHandler ()
clearAccounts = execute_ "delete from accounts" >> execute_ "delete from snap_auth_user" >> return ()

countAccounts :: AppHandler Int
countAccounts = numberQuery' "select count(*) from accounts"

newAccount :: Account -> AppHandler ()
newAccount (Account i n a) = do
  res <- query "insert into accounts (id, name, admin) values (?,?,?) returning name" (i, n, a) :: AppHandler [[Text]]
  case res of
    (x:_)-> return ()
    _ -> registerError "newAccount: Could not create new account." (Just $ unUid i)



getAccount :: UserId -> AppHandler (Maybe Account)
getAccount id' = singleQuery "select id, name, admin from accounts where id = ?" (Only id')

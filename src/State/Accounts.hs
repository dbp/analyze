{-# LANGUAGE OverloadedStrings #-}

module State.Accounts where

import           Control.Applicative
import           Data.Text (Text)
import           Data.Maybe
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.State

data Account = Account { accountId :: Text
                       , accountName :: Text
                       , accountAdmin :: Bool
                       }

instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field

clearAccounts :: AppHandler ()
clearAccounts = execute_ "delete from accounts" >> return ()

countAccounts :: AppHandler Int
countAccounts = numberQuery' "select count(*) from accounts"

newAccount :: Account -> AppHandler ()
newAccount (Account i n a) = do
  res <- query "insert into accounts (id, name, admin) values (?,?,?) returning name" (i, n, a) :: AppHandler [[Text]]
  case res of
    (x:_)-> return ()
    _ -> registerError "newAccount: Could not create new account." (Just i)



getAccount :: Text -> AppHandler (Maybe Account)
getAccount id' = singleQuery "select id, name, admin from accounts where id = ?" (Only id')

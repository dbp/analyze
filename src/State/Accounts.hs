{-# LANGUAGE OverloadedStrings #-}

module State.Accounts where


import           Data.ByteString (ByteString)
import           Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors

data Account = Account { accountId :: Int
                       , accountName :: Text
                       , accountAdmin :: Bool
                       }

newAccount :: Account -> AppHandler ()
newAccount (Account i n a) = do
  res <- query "insert into accounts (id, name, admin) values (?,?,?) returning id" (i, n, a)
  case res of
    [] -> registerError "newAccount: Could not create new account."
    _ -> return ()

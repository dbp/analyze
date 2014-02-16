{-# LANGUAGE OverloadedStrings #-}

module State.Email where

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Auth (UserId(..))
------------------------------------------------------------------------------
import           Application
import           Helpers.Errors
import           Helpers.State

sendEmail :: Text -> Text -> Text -> AppHandler ()
sendEmail to subj body = void $ execute "insert into amazon_email_queue (to_addr, from_addr, from_name, subject, body) values (?, 'dbp@dbpmail.net', 'Analyze by Position Studios', ?, ?)" (to, subj, body)

{-# LANGUAGE OverloadedStrings #-}

module Helpers.State
       ( singleQuery
       , singleQuery'
       , numberQuery
       , numberQuery'
       , void
       ) where

import Control.Monad (void)
import Data.Maybe
import Snap.Snaplet.PostgresqlSimple

singleQuery :: (HasPostgres m, Functor m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
singleQuery stmt attrs = fmap listToMaybe $ query stmt attrs

singleQuery' :: (HasPostgres m, Functor m, FromRow r) => Query -> m (Maybe r)
singleQuery' stmt = fmap listToMaybe $ query_ stmt

numberQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m Int
numberQuery q attrs = fmap (head.fromJust) $ singleQuery q attrs

numberQuery' :: (HasPostgres m, Functor m) => Query -> m Int
numberQuery' q = fmap (head.fromJust) $ singleQuery' q

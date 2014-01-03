{-# LANGUAGE OverloadedStrings #-}

module Helpers.State where

import Data.Maybe
import Snap.Snaplet.PostgresqlSimple

singleQuery :: (HasPostgres m, Functor m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
singleQuery stmt attrs = fmap listToMaybe $ query stmt attrs

singleQuery' :: (HasPostgres m, Functor m, FromRow r) => Query -> m (Maybe r)
singleQuery' stmt = fmap listToMaybe $ query_ stmt

numberQuery' :: (HasPostgres m, Functor m) => Query -> m Int
numberQuery' q = fmap (head.fromJust) $ singleQuery' q

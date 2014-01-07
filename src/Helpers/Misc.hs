module Helpers.Misc where

import Data.Maybe (listToMaybe)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b
trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c


readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

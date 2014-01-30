module Helpers.Text where

import qualified Data.Text as T
import           Data.Text (Text)

tshow :: Show a => a -> Text
tshow = T.pack . show

tshow' :: Show a => Int -> a -> Text
tshow' prec = T.pack . (take prec) . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

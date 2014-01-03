{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Helpers.Forms where

import Control.Applicative
import Text.Digestive
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Calendar

import Application
import Helpers.Text
import Helpers.Misc

emailForm :: Maybe Text -> Form Text AppHandler Text
emailForm email = check "Not a valid email address." (\e -> T.isInfixOf "@" e) $
                  check "Must not be blank" tNotNull
                  (text email)

passwordForm :: Form Text AppHandler Text
passwordForm = check "Must not be blank" tNotNull (text Nothing)

nameForm :: Maybe Text -> Form Text AppHandler Text
nameForm name = check "Must not be blank" tNotNull (text name)

urlForm :: Maybe Text -> Form Text AppHandler Text
urlForm url = check "Not a valid url. Needs http:// or similar." (\u -> T.isInfixOf "://" u) $
              check "Must not be blank" tNotNull (text url)

dateForm :: Maybe Day -> Form Text AppHandler Day
dateForm date = validate checkDate $ (,,)
                <$> "year" .: choice (map (\x -> (x, tshow x)) [2014..2024])
                                     (fmap (fst3.toGregorian) date)
                <*> "month" .: choice [ (1, "January")
                                     , (2, "Februrary")
                                     , (3, "March")
                                     , (4, "April")
                                     , (5, "May")
                                     , (6, "June")
                                     , (7, "July")
                                     , (8, "August")
                                     , (9, "September")
                                     , (10, "October")
                                     , (11, "November")
                                     , (12, "December")
                                     ] (fmap (snd3.toGregorian) date)
                <*> "day" .: choice (map (\x -> (x, tshow x)) [1..31])
                                    (fmap (trd3.toGregorian) date)
  where checkDate (y, m, d) =
          case fromGregorianValid y m d of
            Nothing -> Error "Is not a valid date."
            Just day -> Success day

linkPatternForm :: Maybe Text -> Form Text AppHandler Text
linkPatternForm pattern = check "Must have * somewhere in the link" (\u -> T.isInfixOf "*" u) $
                          urlForm pattern

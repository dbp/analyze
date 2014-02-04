{-# LANGUAGE OverloadedStrings #-}

module Worker where

import qualified Data.Map as M (empty)
import System.Environment (getArgs)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.List (partition)
import Data.Maybe

import Snap.Test (get)
import Snap.Snaplet.Test (evalHandler)

import Application
import Site (app)

import State.Sites

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then putStrLn "usage: runghc Worker.hs --[production|devel|test]" else do
    let env = dropWhile (== '-') (head args)
    void $ evalHandler (Just env) (get "" M.empty) process app

process :: AppHandler ()
process = do
  visits <- getMarkVisits 1000
  processVisits visits
  errors <- getMarkErrors 1000
  processErrors errors
  liftIO $ threadDelay 1000000
  process

processVisits :: [SiteVisit] -> AppHandler ()
processVisits [] = return ()
processVisits (v:vs) = do
  let day = utctDay $ visitTime v
  let url = visitUrl v
  let meth = visitMethod v
  let (dvs, rest) = partition (\x -> utctDay (visitTime x) == day && visitUrl x == url && visitMethod x == meth) (v:vs)
  let times = map visitRenderTime dvs
  mdv <- getDayVisit (visitSiteId v) day url meth
  case mdv of
    Nothing ->
      newDayVisit (DayVisit day (visitSiteId v) (Just url) meth (length times) (maximum times)
                   (minimum times) (average times) (variance times))
    Just dv -> do -- calculate variance and average
       let n = dayHits dv + length dvs
       updateDayVisit $ dv { dayHits = n, dayMaxTime = max (dayMaxTime dv) (maximum times)
                           , dayMinTime = min (dayMinTime dv) (minimum times)
                           , dayAvgTime = onlineAverage (dayAvgTime dv) (dayHits dv) times
                           , dayVarTime = onlineVariance (dayVarTime dv)
                                                         (dayAvgTime dv)
                                                         (dayHits dv)
                                                         times}
  mapM_ (deleteVisitQueueItem . visitId) dvs
  deleteVisitQueueItem (visitId v)
  processVisits rest

processErrors :: [SiteError] -> AppHandler ()
processErrors [] = return ()
processErrors ((SiteError i si url msg uid tm):es) = do
  me <- getErrorByMessage msg si
  case me of
    Nothing -> do
      mei <- newErrorSummary (ErrorSummary (-1) si msg Nothing (UTCTime (fromGregorian 0 0 0) 0) Nothing)
      case mei of
        Nothing -> return ()
        Just ei ->
          void $ newErrorExample (ErrorExample (-1) ei url tm uid)
    Just e -> do
      if isJust (errorResolved e)
         then updateErrorSummary si (e { errorResolved = Nothing})
         else return ()
      void $ newErrorExample (ErrorExample (-1) (errorId e) url tm uid)
  deleteErrorQueueItem i
  processErrors es


average :: [Double] -> Double
average l = sum l / fromIntegral (length l)

onlineAverage :: Double -> Int -> [Double] -> Double
onlineAverage old n l = (old * fromIntegral n + sum l) / fromIntegral (length l + n)

variance :: [Double] -> Double
variance l =
  if null l then 0 else
  let u = average l in
  sum (map ((**2) . flip (-) u) l) / fromIntegral (length l)


onlineVariance :: Double -> Double -> Int -> [Double] -> Double
onlineVariance oldv _oldm _oldn [] = oldv
onlineVariance _oldv _oldm 0 (x:xs) = onlineVariance' x 0 1 xs
onlineVariance oldv oldm n xs = onlineVariance' oldm (oldv * fromIntegral n) n xs

-- This algorithm due to Welford, 1962 (source: Knuth, TAOCP, vol 2, pg 232)
onlineVariance' :: Double -> Double -> Int -> [Double] -> Double
onlineVariance' _m s n [] = if n < 2 then 0 else s / fromIntegral n
onlineVariance' m s n (x:xs) =
  let n' = n + 1 in
  let m' = m + (x - m) / fromIntegral n' in
  let s' = s + (x - m)*(x - m') in
  onlineVariance' m' s' n' xs

varianceProp :: [Double] -> Bool
varianceProp nms = abs (variance nms - onlineVariance 0 0 0 nms) < 0.02

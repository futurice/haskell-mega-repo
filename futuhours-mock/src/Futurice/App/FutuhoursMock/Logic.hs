{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    fillProjects,
    ) where

import Futurice.App.FutuhoursMock.Types
import Futurice.Prelude
import Prelude ()

import Data.Vector.Lens (vector)
import Futurice.App.FutuhoursMock.MockData
import System.IO.Unsafe (unsafePerformIO)
import System.Random (getStdRandom, randomR, randomRIO)
import Test.QuickCheck (arbitrary, sample')
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map


projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint _ctx = do
    projects <- sample' arbitrary
    pure $ projects ^. vector

userEndpoint :: Ctx -> IO (User)
userEndpoint _ctx = do
    -- TODO: how to generate unique values per request?
    let _userBalance = unsafePerformIO $ (getStdRandom (randomR (-10,40)) :: IO Float)
    let _userHolidaysLeft = unsafePerformIO $ randomRIO (0, 24)
    let _userUtilizationRate = unsafePerformIO $ (getStdRandom (randomR (0,100)) :: IO Float)
    pure $ User
        { _userFirstName="Test"
        , _userLastName="User"
        , _userBalance=_userBalance
        , _userHolidaysLeft=_userHolidaysLeft
        , _userUtilizationRate=_userUtilizationRate
        , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
        }

hoursEndpoint :: Ctx -> IO (HoursResponse)
hoursEndpoint _ctx = do
  -- :> QueryParam "start-date" String
  -- :> QueryParam "end-date" String
  let hd = HoursDay { _dayHolidayName=Just "mock",
                      _dayHours=8,
                      _dayEntries=[],
                      _dayClosed=False}
  let hm = HoursMonth { _monthHours=8,
                        _monthUtilizationRate=100,
                        _monthDays=Map.fromList [("day-format", [hd])] }
  pure $ HoursResponse {_hoursResponseProjects=[],
                        _hoursResponseMonths=Map.fromList [("month-format", [hm])],
                        _hoursResponseDefaultWorkHours=7.5}

fillProjects :: IO [Project]
fillProjects = do
  ps' <- flip traverse projects $ \p -> do
    ts' <- flip traverse (p ^.. projectTasks . traverse) $ \t -> do
      now <- getCurrentTime
      hrs <- randomRIO (1, 7) :: IO Int
      let t'' = case (fromMaybe Nothing $ t ^? taskLatestEntry) of
                  Just x -> Just $ x { _latestEntryDate = Just now
                                     , _latestEntryHours = Just $ (fromIntegral hrs)*0.5 }
                  Nothing -> Nothing
      hrsRemaining <- case (_projectId p /= _projectId internalProject && _projectId p /= _projectId absenceProject) of
                          True -> do
                            x <- randomRIO (-10, 20) :: IO Float
                            pure $ Just x
                          False -> pure $ Nothing
      pure $ t { _taskLatestEntry=t'', _taskHoursRemaining=hrsRemaining }
    pure $ p { _projectTasks=ts'}
  pure ps'

entryEndpoint :: Ctx -> IO ([Int])
entryEndpoint _ctx = do
  pure $ [1]

entryIdEndpoint :: Ctx -> Int -> IO ([Int])
entryIdEndpoint _ctx _id = do
  pure $ [1]

entryDeleteEndpoint :: Ctx -> Int -> IO ([Int])
entryDeleteEndpoint _ctx _id = do
  pure $ [1]

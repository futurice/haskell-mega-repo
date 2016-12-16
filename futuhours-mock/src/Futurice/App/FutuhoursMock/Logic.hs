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
    mkEntryEndPoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    fillProjects,
    parseDayFormat,
    parseMonthFormat,
    daysFD,
    monthForDay,
    dayInMonth,
    genMonths,
    ) where

import Futurice.App.FutuhoursMock.Types
import Futurice.Prelude
import Prelude ()

import Data.IORef
import Data.Vector.Lens (vector)
import Data.Text (pack, unpack)
import Futurice.App.FutuhoursMock.MockData
import System.Random (getStdRandom, randomR, randomRIO)
import Test.QuickCheck (arbitrary, sample')
import Data.Time (getCurrentTime)
import Data.Time.Format (parseTimeOrError, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Time.Calendar (diffDays, addDays, toGregorian, fromGregorian)
import Data.Maybe (fromJust)
import Servant
import qualified PlanMill as PM
import qualified Data.Map.Strict as Map


projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint _ctx = do
    p <- sample' arbitrary
    pure $ p ^. vector

userEndpoint :: Ctx -> IO (User)
userEndpoint _ctx = do
  -- GET /user
    -- TODO: how to generate unique values per request?
    _userBalance <- getStdRandom (randomR (-10,40)) :: IO Float
    _userHolidaysLeft <- randomRIO (0, 24)
    _userUtilizationRate <- getStdRandom (randomR (0,100)) :: IO Float
    pure $ User
        { _userFirstName="Test"
        , _userLastName="User"
        , _userBalance=_userBalance
        , _userHolidaysLeft=_userHolidaysLeft
        , _userUtilizationRate=_userUtilizationRate
        , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
        }

parseDayFormat :: Text -> Day
parseDayFormat x = utctDay $ (parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (unpack x) :: UTCTime)

parseMonthFormat :: Text -> Day
parseMonthFormat x = utctDay $ (parseTimeOrError False defaultTimeLocale "%Y-%m" (unpack x) :: UTCTime)

hoursEndpoint
  :: Ctx
  -> Maybe Day
  -> Maybe Day
  -> ExceptT ServantErr IO HoursResponse
hoursEndpoint _ctx sd ed = do
  -- GET /hours
  let duration = diffDays (fromJust sd) (fromJust ed)
  let d = daysFD duration $ (fromJust sd)
  months <- liftIO $ genMonths d
  p <- liftIO $ fillProjects
  return $ HoursResponse { _hoursResponseProjects=p
                         , _hoursResponseMonths=months
                         , _hoursResponseDefaultWorkHours=7.5
                         }

daysFD :: Integer -> Day -> [Day]
daysFD duration start = [addDays i start | i <- [0..duration]]

monthForDay :: Day -> String
monthForDay x = formatTime defaultTimeLocale "%Y-%m" x

dayInMonth :: String -> Day -> Bool
dayInMonth m d = (m == formatTime defaultTimeLocale "%Y-%m" d)

type Counter = Int -> IO Int

makeCounter :: IO Counter
makeCounter = do
  r <- newIORef 0
  return $ \i -> do
    modifyIORef' r (+i)
    readIORef r

plusOne :: Counter -> IO Int
plusOne cnt = do
  val <- cnt 1
  pure $ val

mkGo :: Counter -> IO (HoursDay)
mkGo cnt = do
  newval <- plusOne cnt
  pure $ days !! mod (newval) (length days)

genMonths :: [Day] -> IO (Map.Map Text HoursMonth)
genMonths ds = do
  cnt <- makeCounter
  ms <- flip traverse ds $ \mm -> do
    let m = monthForDay mm
    let days' = [(d, mkGo cnt) | d<-ds, dayInMonth m d]
    hrs <- randomRIO (0, 150) :: IO Int
    utz <- getStdRandom (randomR (0,100)) :: IO Float
    days'' <- for days' $ \(d', d) -> do
        let (ym',mm',_) = toGregorian mm
        let (_,_,dd') = toGregorian d'
        fsd <- d
        pure $ (pack $ show $ fromGregorian ym' mm' dd', fsd)
    pure $ (pack m, HoursMonth
                    { _monthHours=(fromIntegral hrs)*0.5
                    , _monthUtilizationRate=utz
                    , _monthDays=Map.fromList days''})
  pure $ Map.fromList ms

fillProjects :: IO [Project]
fillProjects = do
  ps' <- flip traverse projects $ \p -> do
    ts' <- flip traverse (p ^.. projectTasks . traverse) $ \t -> do
      now <- getCurrentTime
      hrs <- randomRIO (1, 7) :: IO Int
      let t'' = case (fromMaybe Nothing $ t ^? taskLatestEntry) of
                  Just x -> Just $ x { _latestEntryDate = Just (utctDay now)
                                     , _latestEntryHours = Just $ (fromIntegral hrs)*0.5 }
                  Nothing -> Nothing
      hrsRemaining <- case (_projectId p /= _projectId internalProject && _projectId p /= _projectId absenceProject) of
                          True -> do
                            x <- randomRIO (-10, 20) :: IO Float
                            pure $ Just x
                          False -> pure $ Nothing
      pure $ t { _taskLatestEntry=t'', _taskHoursRemaining=hrsRemaining }
    pure $ p { _projectTasks=ts'}
  pure ps'

mkEntryEndPoint :: EntryUpdate -> IO EntryUpdateResponse
mkEntryEndPoint req = do
  p <- liftIO $ fillProjects
  let date = _euDate req
  usrB <- liftIO $ getStdRandom (randomR (-10, 40))
  usrHL <- liftIO $ randomRIO (0, 24)
  usrUTZ <- liftIO $ getStdRandom (randomR (0,100))
  newEntryId <- liftIO $ getStdRandom (randomR (0, 100))
  let md = HoursDayUpdate
            { _hoursDayUpdateHolidayName=Nothing
            , _hoursDayUpdateHours=_euHours req
            , _hoursDayUpdateEntry=Just Entry
                          { _entryId=PM.Ident newEntryId
                          , _entryProjectId=_euProjectId req
                          , _entryTaskId=_euTaskId req
                          , _entryDescription=_euDescription req
                          , _entryHours=_euHours req
                          , _entryClosed=fromMaybe False (_euClosed req) -- TODO: is Maybe open or closed?
                          }
            }
  let d = Map.fromList [(pack $ formatTime defaultTimeLocale "%Y-%m-%d" date, [md])]
  let mm = HoursMonthUpdate
            { _hoursMonthUpdateHours=75
            , _hoursMonthUpdateUtilizationRate=70
            , _hoursMonthUpdateDays=d}
  let months = Map.fromList [(pack $ formatTime defaultTimeLocale "%Y-%m" date, [mm])]

  let userResponse = User
                      { _userFirstName="Test"
                      , _userLastName="User"
                      , _userBalance=usrB
                      , _userHolidaysLeft=usrHL
                      , _userUtilizationRate=usrUTZ
                      , _userProfilePicture="https://raw.githubusercontent.com/futurice/spiceprogram/gh-pages/assets/img/logo/chilicorn_no_text-128.png"
                      }
  let hoursResponse = HoursUpdateResponse
                         { _hoursUpdateResponseDefaultWorkHours=7.5
                         , _hoursUpdateResponseProjects=p
                         , _hoursUpdateResponseMonths=months }

  pure $ EntryUpdateResponse
          { _eurUser=userResponse
          , _eurHours=hoursResponse }

-- | @POST /entry@
entryEndpoint
  :: Ctx
  -> EntryUpdate
  -> ExceptT ServantErr IO EntryUpdateResponse
entryEndpoint _ctx req = do
  res <- liftIO $ (mkEntryEndPoint req)
  return res

-- | @PUT /entry/#id@
entryIdEndpoint
  :: Ctx
  -> Int
  -> EntryUpdate
  -> ExceptT ServantErr IO EntryUpdateResponse
entryIdEndpoint _ctx _id req = do
  res <- liftIO $ (mkEntryEndPoint req)
  let res' = res & eurHours . hoursUpdateResponseMonths . traverse . traverse
                   . hoursMonthUpdateDays . traverse . traverse
                   . hoursDayUpdateEntry . traverse . entryId .~ PM.Ident 1
  return res'

-- | @DELETE /entry/#id@
entryDeleteEndpoint
  :: Ctx
  -> Int
  -> EntryUpdate
  -> ExceptT ServantErr IO EntryUpdateResponse
entryDeleteEndpoint _ctx _id _ = do
  now <- liftIO getCurrentTime
  let dummyReq = EntryUpdate
          { _euTaskId=PM.Ident 1
          , _euProjectId=PM.Ident 1
          , _euDescription="test"
          , _euDate=utctDay now
          , _euHours=7.5
          , _euClosed=Nothing
          }
  res <- liftIO $ mkEntryEndPoint dummyReq
  let travel = eurHours . hoursUpdateResponseMonths . traverse . traverse
                 . hoursMonthUpdateDays . traverse . traverse
  let res' = res & travel . hoursDayUpdateHours .~ 0
                 & travel . hoursDayUpdateEntry .~ Nothing
  return res'

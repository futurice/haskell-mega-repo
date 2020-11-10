{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Notifications where

import Control.Concurrent.STM      (readTVarIO)
import Control.Lens                (filtered, has, minimumOf)
import Data.Semigroup              (Arg (..))
import Data.Time                   (addDays)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Ctx
import Futurice.App.Checklist.Types

import qualified Slack

data LateTask = LateTask Text Integer (Name Task) deriving Show

checkIsWeekend :: Day -> LogT IO () -> LogT IO ()
checkIsWeekend day m
    -- Weekend
    | wd `elem` [6, 7] = do
        logInfo "Weekend" day
        return ()
    -- otherwise
    | otherwise = m
  where
    (_, _, wd) = toWeekDate day


checkDueDates :: Ctx -> IO ()
checkDueDates ctx = do
    today <- currentDay
    runLogT "checklist-slack-notification" lgr $ checkIsWeekend today $ do
        logInfo_ "Calculating"
        world <- liftIO $ readTVarIO (ctxWorld ctx)
        let employees' = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
        let cutoffDate = addDays (-60) today
        let employees'' = filter (\employee -> cutoffDate < employee ^. employeeStartingDay) employees'

        let lateEmployees employee startingDay =
                let predicate t = has (worldTaskItems . ix eid . ix (t ^. identifier) . _AnnTaskItemTodo) world
                    arg       t = Arg (t ^. taskOffset) t
                    eid = employee ^. identifier
                in case minimumOf (worldTasks . folded . filtered predicate . getter arg) world of
                  Nothing                -> Nothing
                  Just (Arg offset task) ->
                      let dueDate = addDays offset startingDay
                      in if dueDate < today then
                           Just $ LateTask (employee ^. employeeFirstName <> " " <> employee ^. employeeLastName) offset (task ^. taskName)
                         else
                           Nothing

        let res = catMaybes $ flip map employees'' $ \employee ->
              let startingDay = employee ^. employeeStartingDay
              in lateEmployees employee startingDay

        when (length res > 0) $ liftIO $ Slack.evalSlackReqIO token mgr $
            Slack.ReqSendMessage channelId $ "Buongiorno! Today checklist has *"<> (textShow $ length res) <>"* employees whose one or more tasks are overdue. Please check them by going to checklist.app.futurice.com"

    pure ()
  where
    lgr = ctxLogger ctx
    mgr = ctxManager ctx
    token = ctxSlackToken ctx
    channelId = ctxSlackChannel ctx

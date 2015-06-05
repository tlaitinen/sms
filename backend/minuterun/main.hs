{-# LANGUAGE OverloadedStrings #-}
import Settings           
import Data.Maybe (fromMaybe)
import qualified Database.Persist
import Import hiding (Option, (==.), (>=.), isNothing) 
import System.Console.GetOpt
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Control.Concurrent 
import Handler.DB
import Database.Esqueleto
import Data.Time.Clock (getCurrentTime, addUTCTime)
import qualified Control.Exception as E
import System.Exit
import qualified Data.Map as Map
import Data.Time

minuteRun :: SqlPersistT (LoggingT IO) ()
minuteRun = do
    now <- liftIO $ getCurrentTime
    let today = utctDay now
        (year, month, _) = toGregorian today
        firstDay = fromGregorian year month 1
        lastDay = fromGregorian year month (gregorianMonthLength year month)
        prevMonthFirst = addGregorianMonthsRollOver (-1) firstDay
        (prevYear, prevMonth, _) = toGregorian prevMonthFirst
        prevMonthLast = fromGregorian prevYear prevMonth (gregorianMonthLength prevYear prevMonth)

    ugs <- select $ from $ \ug -> do
        where_ $ ug ^. UserGroupCreatePeriods ==. val True
        where_ $ isNothing $ ug ^. UserGroupDeletedVersionId
        where_ $ notExists $ from $ \pp -> do
            where_ $ pp ^. ProcessPeriodLastDay >=. val today
            where_ $ exists $ from $ \ugc -> do
                where_ $ ugc ^. UserGroupContentUserGroupId ==. ug ^. UserGroupId
                where_ $ ugc ^. UserGroupContentProcessPeriodContentId ==. just (pp ^. ProcessPeriodId)
                where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId
        return ug        
    forM_ ugs $ \(Entity ugId _) -> do
        lastDayRows <- select $ from $ \pp -> do
            where_ $ exists $ from $ \ugc -> do
                where_ $ ugc ^. UserGroupContentUserGroupId ==. val ugId
                where_ $ ugc ^. UserGroupContentProcessPeriodContentId ==. just (pp ^. ProcessPeriodId)
                where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId
            return (max_ $ pp ^. ProcessPeriodLastDay)
        let maxDay = case lastDayRows of
                (Database.Esqueleto.Value (Just d)):_ -> d
                _ -> addDays (-1) prevMonthFirst
            periods = if maxDay < prevMonthLast 
                    then [ (prevMonthFirst, prevMonthLast), (firstDay, lastDay) ] 
                    else [ (firstDay, lastDay) ]
        forM_ periods $ \(fDay, lDay) -> void $ do
            ppId <- insert $ newProcessPeriod fDay lDay 
            insert $ (newUserGroupContent ugId) {
                    userGroupContentProcessPeriodContentId = Just ppId
                }
             
main :: IO ()
main = do
    settings <- loadAppSettings [configSettingsYml] [] useEnv
    pool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)
    runStdoutLoggingT (runSqlPool minuteRun pool)


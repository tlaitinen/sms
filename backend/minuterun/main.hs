{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Prelude ()
import System.IO.Temp
import Settings           
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import Text.Printf
import Data.Maybe (fromMaybe)
import qualified Database.Persist
import Import hiding (Option, (==.), (>=.), isNothing, update, (=.), on, joinPath, fileSize) 
import System.Console.GetOpt
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Control.Concurrent 
import Handler.DB
import Database.Esqueleto
import qualified Database.Esqueleto as E
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified Control.Exception as E
import System.Exit
import qualified Data.Map as Map
import Data.Time
import Codec.Archive.Zip
import Network.Mail.SMTP
import Network.Mail.Mime
import System.FilePath

fmtDay :: Day -> Text
fmtDay day = T.pack $ printf "%d.%d.%d" d m (y `mod` 100)
    where (y,m,d) = toGregorian day

mkMessage "App" "messages" "fi"

minuteRun :: AppSettings -> SqlPersistT (LoggingT IO) ()
minuteRun settings = do
    createProcessPeriods
    processLockedPeriods settings

createProcessPeriods :: SqlPersistT (LoggingT IO) () 
createProcessPeriods = do
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
                (E.Value (Just d)):_ -> d
                _ -> addDays (-1) prevMonthFirst
            periods = if maxDay < prevMonthLast 
                    then [ (prevMonthFirst, prevMonthLast), (firstDay, lastDay) ] 
                    else [ (firstDay, lastDay) ]
        forM_ periods $ \(fDay, lDay) -> void $ do
            ppId <- insert $ newProcessPeriod fDay lDay 
            insert $ (newUserGroupContent ugId) {
                    userGroupContentProcessPeriodContentId = Just ppId
                }

packReceipts :: AppSettings -> [(Entity Receipt, Entity File)] -> IO [Archive]
packReceipts settings receipts = pack emptyArchive receipts
    where
        used a = fromIntegral $ sum [ eCompressedSize e | e <- zEntries a ]
        fits a f = used a + fromIntegral (fileSize f) < appMaxEmailSize settings
        path fId = joinPath [ appUploadDir settings, show $ fromSqlKey fId ]
        mtime = floor . utcTimeToPOSIXSeconds . fileInsertionTime
        receiptPath r = T.unpack $ T.concat [ receiptName r, "_", T.pack $ show $ receiptAmount r, ".pdf" ]
        pack a rs'@((Entity _ r, Entity fId f):rs) 
            | fits a f = do
                contents <- LB.readFile $ path fId
                let entry = toEntry (receiptPath r) (mtime f) contents
                pack (addEntryToArchive entry a) rs
            | otherwise = do
                a' <- pack emptyArchive rs'
                return $ a:a'
        pack a [] = return [a]

processLockedPeriods :: AppSettings -> SqlPersistT (LoggingT IO) ()
processLockedPeriods settings = do
    pps <- select $ from $ \(pp `InnerJoin` ugc `InnerJoin` ug)-> do
        on (ugc ^. UserGroupContentUserGroupId ==. ug ^. UserGroupId)
        on (ugc ^. UserGroupContentProcessPeriodContentId ==. just (pp ^. ProcessPeriodId))
        where_ $ pp ^. ProcessPeriodLocked ==. val True
        where_ $ pp ^. ProcessPeriodProcessed ==. val False
        where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId
        return (pp, ugc ^. UserGroupContentUserGroupId, ug)

    forM_ pps $ \(Entity ppId pp, E.Value ugId, Entity _ ug) -> do
        receipts <- select $ from $ \(r `InnerJoin` f)-> do
            on (f ^. FileId ==. r ^. ReceiptFileId)
            where_ $ r ^. ReceiptProcessPeriodId ==. val ppId
            where_ $ isNothing $ r ^. ReceiptDeletedVersionId
            return (r,f)        
        archives <- liftIO $ packReceipts settings receipts    

        let firstDay = processPeriodFirstDay pp
            lastDay  = processPeriodLastDay pp
        forM_ (zip [1..] archives) $ \(part,a) -> liftIO $ withSystemTempDirectory "receipts" $ \tempDir -> do
            let tmpPath = tempDir </> (concat [show firstDay, "_", show lastDay, ".zip"])
                msg = (MsgReceiptEmailTitle (userGroupName ug)  firstDay lastDay
                                      part (length archives))
                message = renderMessage (error "" :: App) ["fi"] msg 
            LB.writeFile tmpPath (fromArchive a)        
            mail <- mySimpleMail 
                (Address (Just $ userGroupName ug) (userGroupEmail ug))
                (Address Nothing $ appSenderEmail settings)
                message (LT.fromChunks [message])
                [("application/zip", tmpPath)] 
            sendMail (appSmtpAddress settings) mail
        update $ \pp' -> do
            where_ $ pp' ^. ProcessPeriodId ==. val ppId
            set pp' [ ProcessPeriodProcessed =. val True ]
    where
        mySimpleMail to from subject plainBody attachments = do
            let m = ((emptyMail from) { mailTo = [to]
                                 , mailHeaders = [("Subject", subject)]
                                                      })
            m' <- addAttachments attachments m
            return $ addPart [plainPart plainBody] m'   
             
main :: IO ()
main = do
    settings <- loadAppSettings [configSettingsYml] [] useEnv
    pool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)
    runStdoutLoggingT (runSqlPool (minuteRun settings) pool)

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
import qualified Data.List as L


fmtDay :: Day -> Text
fmtDay day = T.pack $ printf "%d.%d.%d" d m (y `mod` 100)
    where (y,m,d) = toGregorian day

mkMessage "App" "messages" "fi"

minuteRun :: AppSettings -> SqlPersistT (LoggingT IO) ()
minuteRun settings = do
    {-
    rows <- select $ from $ \tm `CrossJoin` c) -> do
        where_  $ (tm ^. TextMessagePhone) `ilike` ((%) ++. (c ^. ClientPhone) ++. (%))  -- TODO: BETTER MATCHING
        where_ $ not_ $ isNothing $ tm ^. TextMessagePhone
        where_ $ isNothing $ tm ^. TextMessageDeletedVersionId
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        where_ $ isNothing $ tm ^. TextMessageSenderClientId
        return (tm)
    forM_ rows $ \(Entity tmId tm, Entity cId _)  -> do
        update $ \t -> do
            set t [ TextMessageSenderClientId =. val (Just cId) ]
            where_ $ t ^. TextMessageId ==. val tmId
    -}
    textMessages <- select $ from $ \tm  -> do
        where_ $ not_ $ isNothing $ tm ^. TextMessagePhone
        where_ $ isNothing $ tm ^. TextMessageDeletedVersionId
        where_ $ isNothing $ tm ^. TextMessageSenderClientId
        return tm
    clients <- select $ from $ \c -> do
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        return c
    forM_ textMessages  $ \(Entity tmId tm)  -> do
        case L.find (matchesClient $ fromMaybe "" $ textMessagePhone tm)  clients of
            Just (Entity cId _) -> do
                update $ \t -> do
                    set t [ TextMessageSenderClientId =. val (Just cId) ]
                    where_ $ t ^. TextMessageId ==. val tmId
            Nothing -> return ()        
    now <- liftIO getCurrentTime        
    update $ \tm -> do
        set tm [ TextMessageSent =. val (Just now) ]
        where_ $ isNothing $ tm ^. TextMessagePhone
        where_ $ notExists $ from $ \tr -> do
            where_ $ tr ^. TextMessageRecipientTextMessageId ==. tm ^. TextMessageId
            where_ $ isNothing $ tr ^. TextMessageRecipientSent 
        where_ $ isNothing $ tm ^. TextMessageDeletedVersionId        
    return ()
    where
        matchesClient phone (Entity _ c) 
            | T.null (clientPhone c) = False
            | otherwise = T.tail (clientPhone c) `T.isInfixOf` phone

             
main :: IO ()
main = do
    settings <- loadAppSettings [configSettingsYml] [] useEnv
    pool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)
    runStdoutLoggingT (runSqlPool (minuteRun settings) pool)


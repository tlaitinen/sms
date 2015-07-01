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
    return ()

monthName :: [Text] -> Day -> Text
monthName langs d = renderMessage (error "" :: App) langs $ case month of
    1 -> MsgJanuary
    2 -> MsgFebruary
    3 -> MsgMarch
    4 -> MsgApril
    5 -> MsgMay
    6 -> MsgJune
    7 -> MsgJuly
    8 -> MsgAugust
    9 -> MsgSeptember
    10 -> MsgOctober
    11 -> MsgNovember
    12 -> MsgDecember
    where
        (_, month, _) = toGregorian d 
             
main :: IO ()
main = do
    settings <- loadAppSettings [configSettingsYml] [] useEnv
    pool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)
    runStdoutLoggingT (runSqlPool (minuteRun settings) pool)


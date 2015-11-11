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
import Import hiding (Option, (==.), (>=.), isNothing, update, (=.), on, joinPath, fileSize, (<.), (>.), (!=.), (||.)) 
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
import qualified Data.Aeson as A
import qualified Control.Exception as E
import System.Exit
import qualified Data.Map as Map
import Data.Time
import Network.Mail.SMTP
import Network.Mail.Mime
import System.FilePath
import qualified Data.List as L
import qualified Web.Mailchimp as MC
import qualified Web.Mailchimp.Lists as MCL


fmtDay :: Day -> Text
fmtDay day = T.pack $ printf "%d.%d.%d" d m (y `mod` 100)
    where (y,m,d) = toGregorian day

mkMessage "App" "messages" "fi"

syncMailChimp :: AppSettings -> UTCTime -> SqlPersistT (LoggingT IO) ()
syncMailChimp settings now = do
    userGroups <- select $ from $ \ug -> do
        where_ $ not_ $ isNothing $ ug ^. UserGroupMailChimpApiKey
        where_ $ not_ $ isNothing $ ug ^. UserGroupMailChimpListName
        where_ $ isNothing $ ug ^. UserGroupDeletedVersionId
        return ug 
    forM_ userGroups syncList     
    where
        syncList (Entity ugId ug) = do
            subscribes <- updatesQuery ugId True
            unsubscribes <- updatesQuery ugId False
            oldEmails <- selectDistinct $ from $ \(c `InnerJoin` li `InnerJoin` pc) -> do
                on $ pc ^. ClientActiveId ==. just (c ^. ClientId)
                on $ li ^. MailChimpListItemClientId ==. c ^. ClientId
                where_ $ existsUserGroupClientContent ugId c
                where_ $ isNothing $ c ^. ClientDeletedVersionId
                where_ $ c ^. ClientEmail !=. pc ^. ClientEmail
                where_ $ not_ $ isNothing $ pc ^. ClientEmail
                where_ $ pc ^. ClientActiveEndTime >. just (li ^. MailChimpListItemSyncTime)
                return $ pc ^. ClientEmail
            let unsubscribeEmails = L.nub $ [ clientMailChimpEmail c | ((Entity _ c),_) <- unsubscribes ] ++ [ MCL.Email $ fromMaybe "" e | (E.Value e) <- oldEmails ] 
                mkey = (userGroupMailChimpApiKey ug) >>= MC.mailchimpKey
                
            case (mkey, userGroupMailChimpListName ug) of
                (Just key, Just ln) -> do
                    cfg <- MC.defaultMailchimpConfig key
                    let lId = MCL.ListId ln
                    mr <- liftIO $ MC.runMailchimpLogging cfg $ do

                        when (not $ null unsubscribeEmails) $ do
                            _ <- MCL.batchUnsubscribe lId unsubscribeEmails (Just True) (Just False) (Just False)
                            return ()
                        if not $ null subscribes 
                            then fmap Just $ 
                                MCL.batchSubscribe lId (subscriberInfo subscribes) (Just False) (Just True) (Just True)
                            else return Nothing    
                    maybe (return ()) (\r -> forM_ subscribes $ updateListItem r ugId) mr                
                _ -> return ()
        clientMailChimpEmail c = MCL.Email $ fromMaybe "" $ clientEmail c         
        updateListItem r ugId (Entity cId c, mli) 

            | clientMailChimpEmail c `inMailChimpResults` (MCL.bsrAdds r ++ MCL.bsrUpdates r) = case mli of
                    Just (Entity liId _) -> update $ \l -> do 
                        set l [ MailChimpListItemSyncTime =. val now ] 
                        where_ $ l ^. MailChimpListItemId ==. val liId
                    Nothing -> void $ insert $ newMailChimpListItem cId ugId now
            | otherwise = return ()
        inMailChimpResults e rs = e `elem` (map MCL.erEmail rs)    

        subscriberInfo xs = [ 
                (MCL.Email $ fromMaybe "" $ clientEmail c,
                 MCL.EmailTypeHTML,
                [ 
                    ("FIRSTNAME", A.String $ clientFirstName c),
                    ("LASTNAME", A.String $ clientLastName c) 
                ])
                | ((Entity _ c),_) <- xs
            ]
        updatesQuery ugId allowEmail = select $ from $ \(c `LeftOuterJoin` li) -> do
            on $ li ?. MailChimpListItemClientId ==. just (c ^. ClientId)
            where_ $ existsUserGroupClientContent ugId c
            where_ $ isNothing $ c ^. ClientDeletedVersionId
            where_ $ c ^. ClientAllowEmail ==. val allowEmail
            when allowEmail $ where_ $ not_ $ isNothing $ c ^. ClientEmail 
            where_ $ isNothing (li ?. MailChimpListItemSyncTime)
                ||. (just (c ^. ClientActiveStartTime) >. li ?. MailChimpListItemSyncTime)
            return (c, li)
     
        existsUserGroupClientContent ugId c = exists $ from $ \ugc -> do
            where_ $ ugc ^. UserGroupContentUserGroupId ==. val ugId
            where_ $ ugc ^. UserGroupContentClientContentId ==. just (c ^. ClientId)
            where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId
 

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

    now <- liftIO getCurrentTime        
    syncMailChimp settings now
    {-
    update $ \tr -> do
        set tr [ 
                TextMessageRecipientFailCount =. tr ^. TextMessageRecipientFailCount +. val 1,
                TextMessageRecipientFailed =. val Nothing,
                TextMessageRecipientAccepted =. val Nothing
            ]
        where_ $ tr ^. TextMessageRecipientFailed <. val (Just $ addUTCTime (-600) now)
        where_ $ isNothing $ tr ^. TextMessageRecipientSent
        where_ $ isNothing $ tr ^. TextMessageRecipientDelivered
        where_ $ tr ^. TextMessageRecipientFailCount <. val 2
    -}
    update $ \tr -> do
        set tr [
                TextMessageRecipientAccepted =. val Nothing
            ]
        where_ $ tr ^. TextMessageRecipientAccepted <. val (Just $ addUTCTime (-120) now)
        where_ $ isNothing $ tr ^. TextMessageRecipientSent
        where_ $ isNothing $ tr ^. TextMessageRecipientDelivered
        where_ $ isNothing $ tr ^. TextMessageRecipientFailed

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
    update $ \tm -> do
        set tm [ TextMessageSent =. val (Just now) ]
        where_ $ isNothing $ tm ^. TextMessagePhone
        where_ $ notExists $ from $ \tr -> do
            where_ $ tr ^. TextMessageRecipientTextMessageId ==. tm ^. TextMessageId
            where_ $ (isNothing $ tr ^. TextMessageRecipientSent) E.&&. (isNothing $ tr ^. TextMessageRecipientFailed)
        where_ $ isNothing $ tm ^. TextMessageDeletedVersionId        
    return ()
    where
        matchesClient phone (Entity _ c) 
            | T.null cp = False
            | otherwise = T.tail cp `T.isInfixOf` phone
            where 
                cp = fromMaybe "" $ clientPhone c

             
main :: IO ()
main = do
    settings <- loadAppSettings [configSettingsYml] [] useEnv
    pool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf settings)
        (pgPoolSize $ appDatabaseConf settings)
    runStdoutLoggingT (runSqlPool (minuteRun settings) pool)


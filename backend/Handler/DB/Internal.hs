{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.DB.Internal where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import qualified Handler.DB.PathPieces as PP
import Prelude
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Database.Esqueleto
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
import Yesod.Core hiding (fileName, fileContentType)
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)
import Data.Aeson ((.:), (.:?), (.!=), FromJSON, parseJSON, decode)
import Data.Aeson.TH
import Data.Int
import Data.Word
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import qualified Data.Attoparsec as AP
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Read
import qualified Data.Text as T
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.List as DL
import Control.Monad (mzero)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as C
import qualified Network.Wai as W
import Data.Conduit.Lazy (lazyConsume)
import Network.HTTP.Types (status200, status400, status403, status404)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Applicative ((<$>), (<*>))  
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text.Lazy.Builder as TLB
data DB = DB


share [mkPersist sqlSettings, mkMigrate "migrateDB" ] [persistLowerCase|
File json
    contentType Text  
    size Int32  
    previewOfFileId FileId Maybe   default=NULL
    name Text  
    activeId FileId Maybe   default=NULL
    activeStartTime UTCTime  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    insertionTime UTCTime  
    insertedByUserId UserId Maybe   default=NULL
UserGroupContent json
    userGroupId UserGroupId  
    fileContentId FileId Maybe   default=NULL
    userGroupContentId UserGroupId Maybe   default=NULL
    userContentId UserId Maybe   default=NULL
    clientContentId ClientId Maybe   default=NULL
    textMessageContentId TextMessageId Maybe   default=NULL
    deletedVersionId VersionId Maybe   default=NULL
UserGroup json
    email Text  "default=''"
    mailChimpApiKey Text Maybe  
    mailChimpListName Text Maybe  
    current Checkmark  "default=True" nullable
    name Text  
    activeId UserGroupId Maybe   default=NULL
    activeStartTime UTCTime  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUserGroup current name !force
UserGroupItem json
    userGroupId UserGroupId  
    userId UserId  
    mode UserGroupMode  
    deletedVersionId VersionId Maybe   default=NULL
User json
    firstName Text  "default=''"
    lastName Text  "default=''"
    organization Text  "default=''"
    admin Bool  "default=False"
    email Text  "default=''"
    password Text  "default=''"
    salt Text  "default=''"
    defaultUserGroupId UserGroupId  
    timeZone Text  "default='Europe/Helsinki'"
    current Checkmark  "default=True" nullable
    config Text  "default='{}'"
    name Text  
    activeId UserId Maybe   default=NULL
    activeStartTime UTCTime  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUser current name !force
    deriving Typeable
Version json
    time UTCTime  
    userId UserId  
Client json
    firstName Text  "default=''"
    lastName Text  "default=''"
    email Text Maybe  
    phone Text Maybe  
    dateOfBirth Day Maybe  
    card Text Maybe  
    allowSms Bool  "default=True"
    allowEmail Bool  "default=True"
    deletedVersionId VersionId Maybe   default=NULL
    activeId ClientId Maybe   default=NULL
    activeStartTime UTCTime  
    activeEndTime UTCTime Maybe  
    insertionTime UTCTime  
    insertedByUserId UserId Maybe   default=NULL
TextMessage json
    text Text  
    phone Text Maybe  
    senderClientId ClientId Maybe   default=NULL
    replyToTextMessageId TextMessageId Maybe   default=NULL
    queued UTCTime Maybe  
    sent UTCTime Maybe  
    aborted UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    activeId TextMessageId Maybe   default=NULL
    activeStartTime UTCTime  
    activeEndTime UTCTime Maybe  
    insertionTime UTCTime  
    insertedByUserId UserId Maybe   default=NULL
TextMessageRecipient json
    textMessageId TextMessageId  
    clientId ClientId  
    accepted UTCTime Maybe  
    sent UTCTime Maybe  
    delivered UTCTime Maybe  
    failed UTCTime Maybe  
    failCount Int  "default=0"
    failReason Text Maybe  
MailChimpListItem json
    clientId ClientId  
    userGroupId UserGroupId  
    syncTime UTCTime  
UsageLog json
    userId UserId  
    time UTCTime  
    data_ Text   "sql=data"
|]
newFile :: Text -> Int32 -> Text -> UTCTime -> UTCTime -> File
newFile contentType_ size_ name_ activeStartTime_ insertionTime_ = File {
    fileContentType = contentType_,
    fileSize = size_,
    filePreviewOfFileId = Nothing,
    fileName = name_,
    fileActiveId = Nothing,
    fileActiveStartTime = activeStartTime_,
    fileActiveEndTime = Nothing,
    fileDeletedVersionId = Nothing,
    fileInsertionTime = insertionTime_,
    fileInsertedByUserId = Nothing
}    
newUserGroupContent :: UserGroupId -> UserGroupContent
newUserGroupContent userGroupId_ = UserGroupContent {
    userGroupContentUserGroupId = userGroupId_,
    userGroupContentFileContentId = Nothing,
    userGroupContentUserGroupContentId = Nothing,
    userGroupContentUserContentId = Nothing,
    userGroupContentClientContentId = Nothing,
    userGroupContentTextMessageContentId = Nothing,
    userGroupContentDeletedVersionId = Nothing
}    
newUserGroup :: Text -> UTCTime -> UserGroup
newUserGroup name_ activeStartTime_ = UserGroup {
    userGroupEmail = "",
    userGroupMailChimpApiKey = Nothing,
    userGroupMailChimpListName = Nothing,
    userGroupCurrent = Active,
    userGroupName = name_,
    userGroupActiveId = Nothing,
    userGroupActiveStartTime = activeStartTime_,
    userGroupActiveEndTime = Nothing,
    userGroupDeletedVersionId = Nothing
}    
newUserGroupItem :: UserGroupId -> UserId -> UserGroupMode -> UserGroupItem
newUserGroupItem userGroupId_ userId_ mode_ = UserGroupItem {
    userGroupItemUserGroupId = userGroupId_,
    userGroupItemUserId = userId_,
    userGroupItemMode = mode_,
    userGroupItemDeletedVersionId = Nothing
}    
newUser :: UserGroupId -> Text -> UTCTime -> User
newUser defaultUserGroupId_ name_ activeStartTime_ = User {
    userFirstName = "",
    userLastName = "",
    userOrganization = "",
    userAdmin = False,
    userEmail = "",
    userPassword = "",
    userSalt = "",
    userDefaultUserGroupId = defaultUserGroupId_,
    userTimeZone = "Europe/Helsinki",
    userCurrent = Active,
    userConfig = "{}",
    userName = name_,
    userActiveId = Nothing,
    userActiveStartTime = activeStartTime_,
    userActiveEndTime = Nothing,
    userDeletedVersionId = Nothing
}    
newVersion :: UTCTime -> UserId -> Version
newVersion time_ userId_ = Version {
    versionTime = time_,
    versionUserId = userId_
}    
newClient :: UTCTime -> UTCTime -> Client
newClient activeStartTime_ insertionTime_ = Client {
    clientFirstName = "",
    clientLastName = "",
    clientEmail = Nothing,
    clientPhone = Nothing,
    clientDateOfBirth = Nothing,
    clientCard = Nothing,
    clientAllowSms = True,
    clientAllowEmail = True,
    clientDeletedVersionId = Nothing,
    clientActiveId = Nothing,
    clientActiveStartTime = activeStartTime_,
    clientActiveEndTime = Nothing,
    clientInsertionTime = insertionTime_,
    clientInsertedByUserId = Nothing
}    
newTextMessage :: Text -> UTCTime -> UTCTime -> TextMessage
newTextMessage text_ activeStartTime_ insertionTime_ = TextMessage {
    textMessageText = text_,
    textMessagePhone = Nothing,
    textMessageSenderClientId = Nothing,
    textMessageReplyToTextMessageId = Nothing,
    textMessageQueued = Nothing,
    textMessageSent = Nothing,
    textMessageAborted = Nothing,
    textMessageDeletedVersionId = Nothing,
    textMessageActiveId = Nothing,
    textMessageActiveStartTime = activeStartTime_,
    textMessageActiveEndTime = Nothing,
    textMessageInsertionTime = insertionTime_,
    textMessageInsertedByUserId = Nothing
}    
newTextMessageRecipient :: TextMessageId -> ClientId -> TextMessageRecipient
newTextMessageRecipient textMessageId_ clientId_ = TextMessageRecipient {
    textMessageRecipientTextMessageId = textMessageId_,
    textMessageRecipientClientId = clientId_,
    textMessageRecipientAccepted = Nothing,
    textMessageRecipientSent = Nothing,
    textMessageRecipientDelivered = Nothing,
    textMessageRecipientFailed = Nothing,
    textMessageRecipientFailCount = 0,
    textMessageRecipientFailReason = Nothing
}    
newMailChimpListItem :: ClientId -> UserGroupId -> UTCTime -> MailChimpListItem
newMailChimpListItem clientId_ userGroupId_ syncTime_ = MailChimpListItem {
    mailChimpListItemClientId = clientId_,
    mailChimpListItemUserGroupId = userGroupId_,
    mailChimpListItemSyncTime = syncTime_
}    
newUsageLog :: UserId -> UTCTime -> Text -> UsageLog
newUsageLog userId_ time_ data__ = UsageLog {
    usageLogUserId = userId_,
    usageLogTime = time_,
    usageLogData_ = data__
}    
class Named a where
    namedName :: a -> Text
data NamedInstanceFieldName = NamedName 
instance Named File where
    namedName = fileName
instance Named UserGroup where
    namedName = userGroupName
instance Named User where
    namedName = userName
data NamedInstance = NamedInstanceFile (Entity File)
    | NamedInstanceUserGroup (Entity UserGroup)
    | NamedInstanceUser (Entity User)


data NamedInstanceId = NamedInstanceFileId FileId
    | NamedInstanceUserGroupId UserGroupId
    | NamedInstanceUserId UserId
    deriving (Eq, Ord)

reflectNamedInstanceId :: NamedInstanceId -> (Text, Int64)
reflectNamedInstanceId x = case x of
    NamedInstanceFileId key -> ("File", fromSqlKey key)
    NamedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    NamedInstanceUserId key -> ("User", fromSqlKey key)


instance Named NamedInstance where
    namedName x = case x of
        NamedInstanceFile (Entity _ e) -> fileName e
        NamedInstanceUserGroup (Entity _ e) -> userGroupName e
        NamedInstanceUser (Entity _ e) -> userName e
    
data NamedInstanceFilterType = NamedInstanceNameFilter (SqlExpr (Database.Esqueleto.Value (Text)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupNamedInstance :: forall (m :: * -> *). (MonadIO m) =>
    NamedInstanceId -> SqlPersistT m (Maybe NamedInstance)
lookupNamedInstance k = case k of
        NamedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceFile $ Entity key val
        NamedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceUserGroup $ Entity key val
        NamedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceUser $ Entity key val

    
selectNamed :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[NamedInstanceFilterType]] -> SqlPersistT m [NamedInstance]
selectNamed filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. FileName
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserGroupName
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserName
    
            ) exprs
    
        return e

    return $ concat [
        map NamedInstanceFile result_File
        , map NamedInstanceUserGroup result_UserGroup
        , map NamedInstanceUser result_User

        ]
data NamedInstanceUpdateType = NamedInstanceUpdateName (SqlExpr (Database.Esqueleto.Value (Text)))
updateNamed :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[NamedInstanceFilterType]] -> [NamedInstanceUpdateType] -> SqlPersistT m ()
updateNamed filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> FileName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. FileName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> UserGroupName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserGroupName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> UserName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserName
    
            ) exprs
    
     
                

    return ()

class HasInsertInfo a where
    hasInsertInfoInsertionTime :: a -> UTCTime
    hasInsertInfoInsertedByUserId :: a -> Maybe UserId
data HasInsertInfoInstanceFieldName = HasInsertInfoInsertionTime    | HasInsertInfoInsertedByUserId 
instance HasInsertInfo File where
    hasInsertInfoInsertionTime = fileInsertionTime
    hasInsertInfoInsertedByUserId = fileInsertedByUserId
instance HasInsertInfo Client where
    hasInsertInfoInsertionTime = clientInsertionTime
    hasInsertInfoInsertedByUserId = clientInsertedByUserId
instance HasInsertInfo TextMessage where
    hasInsertInfoInsertionTime = textMessageInsertionTime
    hasInsertInfoInsertedByUserId = textMessageInsertedByUserId
data HasInsertInfoInstance = HasInsertInfoInstanceFile (Entity File)
    | HasInsertInfoInstanceClient (Entity Client)
    | HasInsertInfoInstanceTextMessage (Entity TextMessage)


data HasInsertInfoInstanceId = HasInsertInfoInstanceFileId FileId
    | HasInsertInfoInstanceClientId ClientId
    | HasInsertInfoInstanceTextMessageId TextMessageId
    deriving (Eq, Ord)

reflectHasInsertInfoInstanceId :: HasInsertInfoInstanceId -> (Text, Int64)
reflectHasInsertInfoInstanceId x = case x of
    HasInsertInfoInstanceFileId key -> ("File", fromSqlKey key)
    HasInsertInfoInstanceClientId key -> ("Client", fromSqlKey key)
    HasInsertInfoInstanceTextMessageId key -> ("TextMessage", fromSqlKey key)


instance HasInsertInfo HasInsertInfoInstance where
    hasInsertInfoInsertionTime x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertionTime e
        HasInsertInfoInstanceClient (Entity _ e) -> clientInsertionTime e
        HasInsertInfoInstanceTextMessage (Entity _ e) -> textMessageInsertionTime e
    
    hasInsertInfoInsertedByUserId x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertedByUserId e
        HasInsertInfoInstanceClient (Entity _ e) -> clientInsertedByUserId e
        HasInsertInfoInstanceTextMessage (Entity _ e) -> textMessageInsertedByUserId e
    
data HasInsertInfoInstanceFilterType = HasInsertInfoInstanceInsertionTimeFilter (SqlExpr (Database.Esqueleto.Value (UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | HasInsertInfoInstanceInsertedByUserIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe UserId)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupHasInsertInfoInstance :: forall (m :: * -> *). (MonadIO m) =>
    HasInsertInfoInstanceId -> SqlPersistT m (Maybe HasInsertInfoInstance)
lookupHasInsertInfoInstance k = case k of
        HasInsertInfoInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ HasInsertInfoInstanceFile $ Entity key val
        HasInsertInfoInstanceClientId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ HasInsertInfoInstanceClient $ Entity key val
        HasInsertInfoInstanceTextMessageId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ HasInsertInfoInstanceTextMessage $ Entity key val

    
selectHasInsertInfo :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasInsertInfoInstanceFilterType]] -> SqlPersistT m [HasInsertInfoInstance]
selectHasInsertInfo filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. FileInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. FileInsertedByUserId
    
            ) exprs
    
        return e
    result_Client <- select $ from $ \e -> do
        let _ = e ^. ClientId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. ClientInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. ClientInsertedByUserId
    
            ) exprs
    
        return e
    result_TextMessage <- select $ from $ \e -> do
        let _ = e ^. TextMessageId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. TextMessageInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. TextMessageInsertedByUserId
    
            ) exprs
    
        return e

    return $ concat [
        map HasInsertInfoInstanceFile result_File
        , map HasInsertInfoInstanceClient result_Client
        , map HasInsertInfoInstanceTextMessage result_TextMessage

        ]
data HasInsertInfoInstanceUpdateType = HasInsertInfoInstanceUpdateInsertionTime (SqlExpr (Database.Esqueleto.Value (UTCTime)))    | HasInsertInfoInstanceUpdateInsertedByUserId (SqlExpr (Database.Esqueleto.Value (Maybe UserId)))
updateHasInsertInfo :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasInsertInfoInstanceFilterType]] -> [HasInsertInfoInstanceUpdateType] -> SqlPersistT m ()
updateHasInsertInfo filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    HasInsertInfoInstanceUpdateInsertionTime v -> FileInsertionTime =. v
                    HasInsertInfoInstanceUpdateInsertedByUserId v -> FileInsertedByUserId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. FileInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. FileInsertedByUserId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. ClientId
        set e $ map (\u -> case u of
                    HasInsertInfoInstanceUpdateInsertionTime v -> ClientInsertionTime =. v
                    HasInsertInfoInstanceUpdateInsertedByUserId v -> ClientInsertedByUserId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. ClientInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. ClientInsertedByUserId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. TextMessageId
        set e $ map (\u -> case u of
                    HasInsertInfoInstanceUpdateInsertionTime v -> TextMessageInsertionTime =. v
                    HasInsertInfoInstanceUpdateInsertedByUserId v -> TextMessageInsertedByUserId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. TextMessageInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. TextMessageInsertedByUserId
    
            ) exprs
    
     
                

    return ()

class Restricted a where
instance Restricted File where
instance Restricted UserGroup where
instance Restricted User where
instance Restricted Client where
instance Restricted TextMessage where
data RestrictedInstance = RestrictedInstanceFile (Entity File)
    | RestrictedInstanceUserGroup (Entity UserGroup)
    | RestrictedInstanceUser (Entity User)
    | RestrictedInstanceClient (Entity Client)
    | RestrictedInstanceTextMessage (Entity TextMessage)


data RestrictedInstanceId = RestrictedInstanceFileId FileId
    | RestrictedInstanceUserGroupId UserGroupId
    | RestrictedInstanceUserId UserId
    | RestrictedInstanceClientId ClientId
    | RestrictedInstanceTextMessageId TextMessageId
    deriving (Eq, Ord)

reflectRestrictedInstanceId :: RestrictedInstanceId -> (Text, Int64)
reflectRestrictedInstanceId x = case x of
    RestrictedInstanceFileId key -> ("File", fromSqlKey key)
    RestrictedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    RestrictedInstanceUserId key -> ("User", fromSqlKey key)
    RestrictedInstanceClientId key -> ("Client", fromSqlKey key)
    RestrictedInstanceTextMessageId key -> ("TextMessage", fromSqlKey key)


instance Restricted RestrictedInstance where
lookupRestrictedInstance :: forall (m :: * -> *). (MonadIO m) =>
    RestrictedInstanceId -> SqlPersistT m (Maybe RestrictedInstance)
lookupRestrictedInstance k = case k of
        RestrictedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceFile $ Entity key val
        RestrictedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceUserGroup $ Entity key val
        RestrictedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceUser $ Entity key val
        RestrictedInstanceClientId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceClient $ Entity key val
        RestrictedInstanceTextMessageId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceTextMessage $ Entity key val

    
selectRestricted :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
     SqlPersistT m [RestrictedInstance]
selectRestricted  = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
    
        return e
    result_Client <- select $ from $ \e -> do
        let _ = e ^. ClientId
    
        return e
    result_TextMessage <- select $ from $ \e -> do
        let _ = e ^. TextMessageId
    
        return e

    return $ concat [
        map RestrictedInstanceFile result_File
        , map RestrictedInstanceUserGroup result_UserGroup
        , map RestrictedInstanceUser result_User
        , map RestrictedInstanceClient result_Client
        , map RestrictedInstanceTextMessage result_TextMessage

        ]
class Versioned a where
    versionedActiveId :: a -> Maybe VersionedInstanceId
    versionedActiveStartTime :: a -> UTCTime
    versionedActiveEndTime :: a -> Maybe UTCTime
data VersionedInstanceFieldName = VersionedActiveId    | VersionedActiveStartTime    | VersionedActiveEndTime 
instance Versioned File where
    versionedActiveId = (fmap VersionedInstanceFileId) . fileActiveId
    versionedActiveStartTime = fileActiveStartTime
    versionedActiveEndTime = fileActiveEndTime
instance Versioned UserGroup where
    versionedActiveId = (fmap VersionedInstanceUserGroupId) . userGroupActiveId
    versionedActiveStartTime = userGroupActiveStartTime
    versionedActiveEndTime = userGroupActiveEndTime
instance Versioned User where
    versionedActiveId = (fmap VersionedInstanceUserId) . userActiveId
    versionedActiveStartTime = userActiveStartTime
    versionedActiveEndTime = userActiveEndTime
instance Versioned Client where
    versionedActiveId = (fmap VersionedInstanceClientId) . clientActiveId
    versionedActiveStartTime = clientActiveStartTime
    versionedActiveEndTime = clientActiveEndTime
instance Versioned TextMessage where
    versionedActiveId = (fmap VersionedInstanceTextMessageId) . textMessageActiveId
    versionedActiveStartTime = textMessageActiveStartTime
    versionedActiveEndTime = textMessageActiveEndTime
data VersionedInstance = VersionedInstanceFile (Entity File)
    | VersionedInstanceUserGroup (Entity UserGroup)
    | VersionedInstanceUser (Entity User)
    | VersionedInstanceClient (Entity Client)
    | VersionedInstanceTextMessage (Entity TextMessage)


data VersionedInstanceId = VersionedInstanceFileId FileId
    | VersionedInstanceUserGroupId UserGroupId
    | VersionedInstanceUserId UserId
    | VersionedInstanceClientId ClientId
    | VersionedInstanceTextMessageId TextMessageId
    deriving (Eq, Ord)

reflectVersionedInstanceId :: VersionedInstanceId -> (Text, Int64)
reflectVersionedInstanceId x = case x of
    VersionedInstanceFileId key -> ("File", fromSqlKey key)
    VersionedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    VersionedInstanceUserId key -> ("User", fromSqlKey key)
    VersionedInstanceClientId key -> ("Client", fromSqlKey key)
    VersionedInstanceTextMessageId key -> ("TextMessage", fromSqlKey key)


instance Versioned VersionedInstance where
    versionedActiveId x = case x of
        VersionedInstanceFile (Entity _ e) -> (fmap VersionedInstanceFileId) $ fileActiveId e
        VersionedInstanceUserGroup (Entity _ e) -> (fmap VersionedInstanceUserGroupId) $ userGroupActiveId e
        VersionedInstanceUser (Entity _ e) -> (fmap VersionedInstanceUserId) $ userActiveId e
        VersionedInstanceClient (Entity _ e) -> (fmap VersionedInstanceClientId) $ clientActiveId e
        VersionedInstanceTextMessage (Entity _ e) -> (fmap VersionedInstanceTextMessageId) $ textMessageActiveId e
    
    versionedActiveStartTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveStartTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveStartTime e
        VersionedInstanceUser (Entity _ e) -> userActiveStartTime e
        VersionedInstanceClient (Entity _ e) -> clientActiveStartTime e
        VersionedInstanceTextMessage (Entity _ e) -> textMessageActiveStartTime e
    
    versionedActiveEndTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveEndTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveEndTime e
        VersionedInstanceUser (Entity _ e) -> userActiveEndTime e
        VersionedInstanceClient (Entity _ e) -> clientActiveEndTime e
        VersionedInstanceTextMessage (Entity _ e) -> textMessageActiveEndTime e
    
data VersionedInstanceFilterType = VersionedInstanceActiveStartTimeFilter (SqlExpr (Database.Esqueleto.Value (UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | VersionedInstanceActiveEndTimeFilter (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupVersionedInstance :: forall (m :: * -> *). (MonadIO m) =>
    VersionedInstanceId -> SqlPersistT m (Maybe VersionedInstance)
lookupVersionedInstance k = case k of
        VersionedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceFile $ Entity key val
        VersionedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceUserGroup $ Entity key val
        VersionedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceUser $ Entity key val
        VersionedInstanceClientId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceClient $ Entity key val
        VersionedInstanceTextMessageId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceTextMessage $ Entity key val

    
selectVersioned :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[VersionedInstanceFilterType]] -> SqlPersistT m [VersionedInstance]
selectVersioned filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. FileActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. FileActiveEndTime
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserGroupActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserGroupActiveEndTime
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserActiveEndTime
    
            ) exprs
    
        return e
    result_Client <- select $ from $ \e -> do
        let _ = e ^. ClientId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. ClientActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. ClientActiveEndTime
    
            ) exprs
    
        return e
    result_TextMessage <- select $ from $ \e -> do
        let _ = e ^. TextMessageId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. TextMessageActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. TextMessageActiveEndTime
    
            ) exprs
    
        return e

    return $ concat [
        map VersionedInstanceFile result_File
        , map VersionedInstanceUserGroup result_UserGroup
        , map VersionedInstanceUser result_User
        , map VersionedInstanceClient result_Client
        , map VersionedInstanceTextMessage result_TextMessage

        ]
data VersionedInstanceUpdateType = VersionedInstanceUpdateActiveStartTime (SqlExpr (Database.Esqueleto.Value (UTCTime)))    | VersionedInstanceUpdateActiveEndTime (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)))
updateVersioned :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[VersionedInstanceFilterType]] -> [VersionedInstanceUpdateType] -> SqlPersistT m ()
updateVersioned filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> FileActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> FileActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. FileActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. FileActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> UserGroupActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> UserGroupActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserGroupActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserGroupActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> UserActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> UserActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. ClientId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> ClientActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> ClientActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. ClientActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. ClientActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. TextMessageId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> TextMessageActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> TextMessageActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. TextMessageActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. TextMessageActiveEndTime
    
            ) exprs
    
     
                

    return ()

class Deletable a where
    deletableDeletedVersionId :: a -> Maybe VersionId
data DeletableInstanceFieldName = DeletableDeletedVersionId 
instance Deletable File where
    deletableDeletedVersionId = fileDeletedVersionId
instance Deletable UserGroupContent where
    deletableDeletedVersionId = userGroupContentDeletedVersionId
instance Deletable UserGroup where
    deletableDeletedVersionId = userGroupDeletedVersionId
instance Deletable UserGroupItem where
    deletableDeletedVersionId = userGroupItemDeletedVersionId
instance Deletable User where
    deletableDeletedVersionId = userDeletedVersionId
instance Deletable Client where
    deletableDeletedVersionId = clientDeletedVersionId
instance Deletable TextMessage where
    deletableDeletedVersionId = textMessageDeletedVersionId
data DeletableInstance = DeletableInstanceFile (Entity File)
    | DeletableInstanceUserGroupContent (Entity UserGroupContent)
    | DeletableInstanceUserGroup (Entity UserGroup)
    | DeletableInstanceUserGroupItem (Entity UserGroupItem)
    | DeletableInstanceUser (Entity User)
    | DeletableInstanceClient (Entity Client)
    | DeletableInstanceTextMessage (Entity TextMessage)


data DeletableInstanceId = DeletableInstanceFileId FileId
    | DeletableInstanceUserGroupContentId UserGroupContentId
    | DeletableInstanceUserGroupId UserGroupId
    | DeletableInstanceUserGroupItemId UserGroupItemId
    | DeletableInstanceUserId UserId
    | DeletableInstanceClientId ClientId
    | DeletableInstanceTextMessageId TextMessageId
    deriving (Eq, Ord)

reflectDeletableInstanceId :: DeletableInstanceId -> (Text, Int64)
reflectDeletableInstanceId x = case x of
    DeletableInstanceFileId key -> ("File", fromSqlKey key)
    DeletableInstanceUserGroupContentId key -> ("UserGroupContent", fromSqlKey key)
    DeletableInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    DeletableInstanceUserGroupItemId key -> ("UserGroupItem", fromSqlKey key)
    DeletableInstanceUserId key -> ("User", fromSqlKey key)
    DeletableInstanceClientId key -> ("Client", fromSqlKey key)
    DeletableInstanceTextMessageId key -> ("TextMessage", fromSqlKey key)


instance Deletable DeletableInstance where
    deletableDeletedVersionId x = case x of
        DeletableInstanceFile (Entity _ e) -> fileDeletedVersionId e
        DeletableInstanceUserGroupContent (Entity _ e) -> userGroupContentDeletedVersionId e
        DeletableInstanceUserGroup (Entity _ e) -> userGroupDeletedVersionId e
        DeletableInstanceUserGroupItem (Entity _ e) -> userGroupItemDeletedVersionId e
        DeletableInstanceUser (Entity _ e) -> userDeletedVersionId e
        DeletableInstanceClient (Entity _ e) -> clientDeletedVersionId e
        DeletableInstanceTextMessage (Entity _ e) -> textMessageDeletedVersionId e
    
data DeletableInstanceFilterType = DeletableInstanceDeletedVersionIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupDeletableInstance :: forall (m :: * -> *). (MonadIO m) =>
    DeletableInstanceId -> SqlPersistT m (Maybe DeletableInstance)
lookupDeletableInstance k = case k of
        DeletableInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceFile $ Entity key val
        DeletableInstanceUserGroupContentId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroupContent $ Entity key val
        DeletableInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroup $ Entity key val
        DeletableInstanceUserGroupItemId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroupItem $ Entity key val
        DeletableInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUser $ Entity key val
        DeletableInstanceClientId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceClient $ Entity key val
        DeletableInstanceTextMessageId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceTextMessage $ Entity key val

    
selectDeletable :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[DeletableInstanceFilterType]] -> SqlPersistT m [DeletableInstance]
selectDeletable filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. FileDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroupContent <- select $ from $ \e -> do
        let _ = e ^. UserGroupContentId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupContentDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroupItem <- select $ from $ \e -> do
        let _ = e ^. UserGroupItemId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupItemDeletedVersionId
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserDeletedVersionId
    
            ) exprs
    
        return e
    result_Client <- select $ from $ \e -> do
        let _ = e ^. ClientId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. ClientDeletedVersionId
    
            ) exprs
    
        return e
    result_TextMessage <- select $ from $ \e -> do
        let _ = e ^. TextMessageId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. TextMessageDeletedVersionId
    
            ) exprs
    
        return e

    return $ concat [
        map DeletableInstanceFile result_File
        , map DeletableInstanceUserGroupContent result_UserGroupContent
        , map DeletableInstanceUserGroup result_UserGroup
        , map DeletableInstanceUserGroupItem result_UserGroupItem
        , map DeletableInstanceUser result_User
        , map DeletableInstanceClient result_Client
        , map DeletableInstanceTextMessage result_TextMessage

        ]
data DeletableInstanceUpdateType = DeletableInstanceUpdateDeletedVersionId (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)))
updateDeletable :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[DeletableInstanceFilterType]] -> [DeletableInstanceUpdateType] -> SqlPersistT m ()
updateDeletable filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> FileDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. FileDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupContentId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupContentDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupContentDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupItemId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupItemDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupItemDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. ClientId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> ClientDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. ClientDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. TextMessageId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> TextMessageDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. TextMessageDeletedVersionId
    
            ) exprs
    
     
                

    return ()

userGroupContentContentId :: UserGroupContent -> Maybe (RestrictedInstanceId)
userGroupContentContentId e = listToMaybe $ catMaybes [
        userGroupContentFileContentId e >>= (return . RestrictedInstanceFileId)
        , userGroupContentUserGroupContentId e >>= (return . RestrictedInstanceUserGroupId)
        , userGroupContentUserContentId e >>= (return . RestrictedInstanceUserId)
        , userGroupContentClientContentId e >>= (return . RestrictedInstanceClientId)
        , userGroupContentTextMessageContentId e >>= (return . RestrictedInstanceTextMessageId)

    ]

class UserGroupContentContentIdField e where
    userGroupContentContentIdField :: SqlExpr (Database.Esqueleto.Value (Maybe (Key e))) -> EntityField UserGroupContent (Maybe (Key e)) 

instance UserGroupContentContentIdField TextMessage where
    userGroupContentContentIdField _ = UserGroupContentTextMessageContentId
instance UserGroupContentContentIdField Client where
    userGroupContentContentIdField _ = UserGroupContentClientContentId
instance UserGroupContentContentIdField User where
    userGroupContentContentIdField _ = UserGroupContentUserContentId
instance UserGroupContentContentIdField UserGroup where
    userGroupContentContentIdField _ = UserGroupContentUserGroupContentId
instance UserGroupContentContentIdField File where
    userGroupContentContentIdField _ = UserGroupContentFileContentId
    

userGroupContentContentIdExprFromString :: Text -> SqlExpr (Entity UserGroupContent) -> Text -> Maybe Text -> Maybe (SqlExpr (E.Value Bool))
userGroupContentContentIdExprFromString "TextMessage" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentTextMessageContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentTextMessageContentId) nothing
   
userGroupContentContentIdExprFromString "Client" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentClientContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentClientContentId) nothing
   
userGroupContentContentIdExprFromString "User" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) nothing
   
userGroupContentContentIdExprFromString "UserGroup" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) nothing
   
userGroupContentContentIdExprFromString "File" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) nothing
   

userGroupContentContentIdExprFromString _ _ _ _ = Nothing

userGroupContentContentIdExpr2FromString :: Text -> SqlExpr (Entity UserGroupContent) -> Text -> SqlExpr (Entity UserGroupContent) -> Maybe (SqlExpr (E.Value Bool))
userGroupContentContentIdExpr2FromString "TextMessage" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentTextMessageContentId) (e2 ^. UserGroupContentTextMessageContentId)
userGroupContentContentIdExpr2FromString "Client" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentClientContentId) (e2 ^. UserGroupContentClientContentId)
userGroupContentContentIdExpr2FromString "User" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) (e2 ^. UserGroupContentUserContentId)
userGroupContentContentIdExpr2FromString "UserGroup" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) (e2 ^. UserGroupContentUserGroupContentId)
userGroupContentContentIdExpr2FromString "File" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) (e2 ^. UserGroupContentFileContentId)

userGroupContentContentIdExpr2FromString _ _ _ _ = Nothing


instance ToJSON Day where
    toJSON = toJSON . show

instance FromJSON Day where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero 

instance ToJSON TimeOfDay where
    toJSON = toJSON . show

instance FromJSON TimeOfDay where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero

instance ToJSON Checkmark where
    toJSON Active   = A.String "Active"
    toJSON Inactive = A.String "Inactive"            

instance FromJSON Checkmark where
    parseJSON (A.String "Active") = return Active
    parseJSON (A.String "Inactive") = return Inactive    
    parseJSON _ = mzero   

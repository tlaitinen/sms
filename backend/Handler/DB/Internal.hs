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
module Handler.DB.Internal where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import qualified Handler.DB.PathPieces as PP
import Prelude
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow)
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp, unsafeSqlExtractSubField, UnsafeSqlFunctionArgument)
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

data FilterJsonMsg = FilterJsonMsg {
    filterJsonMsg_type :: Text,
    filterJsonMsg_value :: Text,
    filterJsonMsg_field :: Text,
    filterJsonMsg_property :: Text,
    filterJsonMsg_comparison :: Text
} 
filterJsonMsg_field_or_property :: FilterJsonMsg -> Text
filterJsonMsg_field_or_property fjm
    | not $ T.null $ filterJsonMsg_field fjm = filterJsonMsg_field fjm
    | otherwise = filterJsonMsg_property fjm

instance FromJSON FilterJsonMsg where
    parseJSON (A.Object v) = FilterJsonMsg <$>
        v .:? "type" .!= "string" <*> 
        (parseStringOrInt v) <*>
        v .:? "field" .!= "" <*>
        v .:? "property" .!= "" <*>
        v .:? "comparison" .!= "eq"
    parseJSON _ = mzero

instance IsString (Maybe Text) where
    fromString "" = Nothing
    fromString a  = Just $ T.pack a

parseStringOrInt :: HMS.HashMap Text A.Value -> AT.Parser Text
parseStringOrInt hm = case HMS.lookup "value" hm of
    Just (A.Number n) -> return $ T.pack $ show n
    Just (A.String s) -> return s
    _ -> mzero

data SortJsonMsg = SortJsonMsg {
    sortJsonMsg_property :: Text,
    sortJsonMsg_direction :: Text
}

$(deriveJSON defaultOptions{fieldLabelModifier = drop 12} ''SortJsonMsg)

-- defaultFilterOp :: forall v typ. PersistField typ => Text -> EntityField v typ -> typ -> Filter v
defaultFilterOp "eq" = (==.)
defaultFilterOp "neq" = (!=.)
defaultFilterOp "lt" = (<.)
defaultFilterOp "gt" = (>.)
defaultFilterOp "le" = (<=.)
defaultFilterOp "ge" = (>=.)
defaultFilterOp _ = (==.)

ilike = unsafeSqlBinOp " ILIKE "
is = unsafeSqlBinOp " IS "

extractSubField :: UnsafeSqlFunctionArgument a => TLB.Builder -> a -> SqlExpr (Database.Esqueleto.Value Double)
extractSubField = unsafeSqlExtractSubField

getDefaultFilter maybeGetParam defaultFilterJson p = do
    f <- maybe maybeGetParam Just getFilter
    PP.fromPathPiece f
    where 
        getFilter = do            
            j <- defaultFilterJson
            v <- DL.find (\fjm -> filterJsonMsg_property fjm == p) j
            return (filterJsonMsg_value v)
hasDefaultFilter maybeGetParam defaultFilterJson p = isJust $
    maybe maybeGetParam Just getFilter
    where
        getFilter = do
            j <- defaultFilterJson
            v <- DL.find (\fjm -> filterJsonMsg_property fjm == p) j
            return (filterJsonMsg_value v)
share [mkPersist sqlOnlySettings, mkMigrate "migrateDB" ] [persistLowerCase|
File json
    contentType Text  
    size Int32  
    name Text  
    activeId FileId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    insertionTime UTCTime  
    insertedByUserId UserId Maybe   default=NULL
UserGroupContent json
    userGroupId UserGroupId  
    fileContentId FileId Maybe   default=NULL
    userGroupContentId UserGroupId Maybe   default=NULL
    userContentId UserId Maybe   default=NULL
    deletedVersionId VersionId Maybe   default=NULL
UserGroup json
    current Checkmark  "default=True" nullable
    name Text  
    activeId UserGroupId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
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
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUser current name !force
    deriving Typeable
Version json
    time UTCTime  
    userId UserId  
|]
newFile :: Text -> Int32 -> Text -> UTCTime -> File
newFile contentType_ size_ name_ insertionTime_ = File {
    fileContentType = contentType_,
    fileSize = size_,
    fileName = name_,
    fileActiveId = Nothing,
    fileActiveStartTime = Nothing,
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
    userGroupContentDeletedVersionId = Nothing
}    
newUserGroup :: Text -> UserGroup
newUserGroup name_ = UserGroup {
    userGroupCurrent = Active,
    userGroupName = name_,
    userGroupActiveId = Nothing,
    userGroupActiveStartTime = Nothing,
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
newUser :: UserGroupId -> Text -> User
newUser defaultUserGroupId_ name_ = User {
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
    userActiveStartTime = Nothing,
    userActiveEndTime = Nothing,
    userDeletedVersionId = Nothing
}    
newVersion :: UTCTime -> UserId -> Version
newVersion time_ userId_ = Version {
    versionTime = time_,
    versionUserId = userId_
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


instance Named NamedInstance where
    namedName x = case x of
        NamedInstanceFile (Entity _ e) -> fileName e
        NamedInstanceUserGroup (Entity _ e) -> userGroupName e
        NamedInstanceUser (Entity _ e) -> userName e
    
data NamedInstanceFilterType = NamedInstanceNameFilter (SqlExpr (Database.Esqueleto.Value (Text)) -> SqlExpr (Database.Esqueleto.Value Bool))
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
        set e $ map (\update -> case update of
                    NamedInstanceUpdateName v -> FileName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. FileName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\update -> case update of
                    NamedInstanceUpdateName v -> UserGroupName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserGroupName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\update -> case update of
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
data HasInsertInfoInstance = HasInsertInfoInstanceFile (Entity File)


data HasInsertInfoInstanceId = HasInsertInfoInstanceFileId FileId


instance HasInsertInfo HasInsertInfoInstance where
    hasInsertInfoInsertionTime x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertionTime e
    
    hasInsertInfoInsertedByUserId x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertedByUserId e
    
data HasInsertInfoInstanceFilterType = HasInsertInfoInstanceInsertionTimeFilter (SqlExpr (Database.Esqueleto.Value (UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | HasInsertInfoInstanceInsertedByUserIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe UserId)) -> SqlExpr (Database.Esqueleto.Value Bool))
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

    return $ concat [
        map HasInsertInfoInstanceFile result_File

        ]
data HasInsertInfoInstanceUpdateType = HasInsertInfoInstanceUpdateInsertionTime (SqlExpr (Database.Esqueleto.Value (UTCTime)))    | HasInsertInfoInstanceUpdateInsertedByUserId (SqlExpr (Database.Esqueleto.Value (Maybe UserId)))
updateHasInsertInfo :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasInsertInfoInstanceFilterType]] -> [HasInsertInfoInstanceUpdateType] -> SqlPersistT m ()
updateHasInsertInfo filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\update -> case update of
                    HasInsertInfoInstanceUpdateInsertionTime v -> FileInsertionTime =. v
                    HasInsertInfoInstanceUpdateInsertedByUserId v -> FileInsertedByUserId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. FileInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. FileInsertedByUserId
    
            ) exprs
    
     
                

    return ()

class Restricted a where
instance Restricted File where
instance Restricted UserGroup where
instance Restricted User where
data RestrictedInstance = RestrictedInstanceFile (Entity File)
    | RestrictedInstanceUserGroup (Entity UserGroup)
    | RestrictedInstanceUser (Entity User)


data RestrictedInstanceId = RestrictedInstanceFileId FileId
    | RestrictedInstanceUserGroupId UserGroupId
    | RestrictedInstanceUserId UserId


instance Restricted RestrictedInstance where
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

    return $ concat [
        map RestrictedInstanceFile result_File
        , map RestrictedInstanceUserGroup result_UserGroup
        , map RestrictedInstanceUser result_User

        ]
class Versioned a where
    versionedActiveId :: a -> Maybe VersionedInstanceId
    versionedActiveStartTime :: a -> Maybe UTCTime
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
data VersionedInstance = VersionedInstanceFile (Entity File)
    | VersionedInstanceUserGroup (Entity UserGroup)
    | VersionedInstanceUser (Entity User)


data VersionedInstanceId = VersionedInstanceFileId FileId
    | VersionedInstanceUserGroupId UserGroupId
    | VersionedInstanceUserId UserId


instance Versioned VersionedInstance where
    versionedActiveId x = case x of
        VersionedInstanceFile (Entity _ e) -> (fmap VersionedInstanceFileId) $ fileActiveId e
        VersionedInstanceUserGroup (Entity _ e) -> (fmap VersionedInstanceUserGroupId) $ userGroupActiveId e
        VersionedInstanceUser (Entity _ e) -> (fmap VersionedInstanceUserId) $ userActiveId e
    
    versionedActiveStartTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveStartTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveStartTime e
        VersionedInstanceUser (Entity _ e) -> userActiveStartTime e
    
    versionedActiveEndTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveEndTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveEndTime e
        VersionedInstanceUser (Entity _ e) -> userActiveEndTime e
    
data VersionedInstanceFilterType = VersionedInstanceActiveStartTimeFilter (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | VersionedInstanceActiveEndTimeFilter (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))
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

    return $ concat [
        map VersionedInstanceFile result_File
        , map VersionedInstanceUserGroup result_UserGroup
        , map VersionedInstanceUser result_User

        ]
data VersionedInstanceUpdateType = VersionedInstanceUpdateActiveStartTime (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)))    | VersionedInstanceUpdateActiveEndTime (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)))
updateVersioned :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[VersionedInstanceFilterType]] -> [VersionedInstanceUpdateType] -> SqlPersistT m ()
updateVersioned filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\update -> case update of
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
        set e $ map (\update -> case update of
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
        set e $ map (\update -> case update of
                    VersionedInstanceUpdateActiveStartTime v -> UserActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> UserActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserActiveEndTime
    
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
data DeletableInstance = DeletableInstanceFile (Entity File)
    | DeletableInstanceUserGroupContent (Entity UserGroupContent)
    | DeletableInstanceUserGroup (Entity UserGroup)
    | DeletableInstanceUserGroupItem (Entity UserGroupItem)
    | DeletableInstanceUser (Entity User)


data DeletableInstanceId = DeletableInstanceFileId FileId
    | DeletableInstanceUserGroupContentId UserGroupContentId
    | DeletableInstanceUserGroupId UserGroupId
    | DeletableInstanceUserGroupItemId UserGroupItemId
    | DeletableInstanceUserId UserId


instance Deletable DeletableInstance where
    deletableDeletedVersionId x = case x of
        DeletableInstanceFile (Entity _ e) -> fileDeletedVersionId e
        DeletableInstanceUserGroupContent (Entity _ e) -> userGroupContentDeletedVersionId e
        DeletableInstanceUserGroup (Entity _ e) -> userGroupDeletedVersionId e
        DeletableInstanceUserGroupItem (Entity _ e) -> userGroupItemDeletedVersionId e
        DeletableInstanceUser (Entity _ e) -> userDeletedVersionId e
    
data DeletableInstanceFilterType = DeletableInstanceDeletedVersionIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)) -> SqlExpr (Database.Esqueleto.Value Bool))
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

    return $ concat [
        map DeletableInstanceFile result_File
        , map DeletableInstanceUserGroupContent result_UserGroupContent
        , map DeletableInstanceUserGroup result_UserGroup
        , map DeletableInstanceUserGroupItem result_UserGroupItem
        , map DeletableInstanceUser result_User

        ]
data DeletableInstanceUpdateType = DeletableInstanceUpdateDeletedVersionId (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)))
updateDeletable :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[DeletableInstanceFilterType]] -> [DeletableInstanceUpdateType] -> SqlPersistT m ()
updateDeletable filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\update -> case update of
                    DeletableInstanceUpdateDeletedVersionId v -> FileDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. FileDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupContentId
        set e $ map (\update -> case update of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupContentDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupContentDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\update -> case update of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupItemId
        set e $ map (\update -> case update of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupItemDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupItemDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\update -> case update of
                    DeletableInstanceUpdateDeletedVersionId v -> UserDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserDeletedVersionId
    
            ) exprs
    
     
                

    return ()

userGroupContentContentId :: UserGroupContent -> Maybe (RestrictedInstanceId)
userGroupContentContentId e = listToMaybe $ catMaybes [
        userGroupContentFileContentId e >>= (return . RestrictedInstanceFileId)
        , userGroupContentUserGroupContentId e >>= (return . RestrictedInstanceUserGroupId)
        , userGroupContentUserContentId e >>= (return . RestrictedInstanceUserId)

    ]

class UserGroupContentContentIdField e where
    userGroupContentContentIdField :: SqlExpr (Database.Esqueleto.Value (Maybe (Key e))) -> EntityField UserGroupContent (Maybe (Key e)) 

instance UserGroupContentContentIdField User where
    userGroupContentContentIdField _ = UserGroupContentUserContentId
instance UserGroupContentContentIdField UserGroup where
    userGroupContentContentIdField _ = UserGroupContentUserGroupContentId
instance UserGroupContentContentIdField File where
    userGroupContentContentIdField _ = UserGroupContentFileContentId
    
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

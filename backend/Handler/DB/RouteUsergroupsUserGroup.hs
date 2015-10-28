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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Handler.DB.RouteUsergroupsUserGroup where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import Handler.DB.Internal
import Handler.DB.Validation
import qualified Handler.DB.FilterSort as FS
import qualified Handler.DB.PathPieces as PP
import Prelude
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist, AuthEntity)
import Yesod.Core hiding (fileName, fileContentType)
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)
import Control.Monad (when)
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
import Data.Text (Text)
import qualified Data.List as DL
import Control.Monad (mzero, forM_)
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
import Handler.Utils (nonEmpty)
import Handler.Utils (prepareNewUser,hasWritePerm,hasReadPermMaybe,hasReadPerm)

putUsergroupsUserGroupIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => UserGroupId -> HandlerT DB (HandlerT master IO) A.Value
putUsergroupsUserGroupIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_mailChimpListName <- case HML.lookup "mailChimpListName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute mailChimpListName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute mailChimpListName in the JSON object in request body" :: Text)
            ]
    attr_mailChimpApiKey <- case HML.lookup "mailChimpApiKey" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute mailChimpApiKey in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute mailChimpApiKey in the JSON object in request body" :: Text)
            ]
    attr_email <- case HML.lookup "email" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute email in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute email in the JSON object in request body" :: Text)
            ]
    attr_name <- case HML.lookup "name" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute name in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute name in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    _ <- do
        result <- select $ from $ \(ug ) -> do
            let ugId' = ug ^. UserGroupId
            where_ (((ug ^. UserGroupId) ==. (val p1)) &&. (hasWritePerm (val authId) (ug ^. UserGroupId)))

            limit 1
            return ug
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        e2 <- do
    
            return $ Version {
                            versionTime = __currentTime
                    ,
                            versionUserId = authId
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_versionId <- P.insert (e2 :: Version)
        result_ug <- do
            r <- get $ ((p1) :: UserGroupId)
            case r of
                Just e -> return e
                _ -> sendResponseStatus status400 $ A.object [  
                    "message" .= ("Could not get entity UserGroup" :: Text) 
                   ] 
        e4 <- do
            let e = result_ug
    
            return $ e {
                            userGroupCurrent = Inactive
                    ,
                            userGroupActiveId = (Just p1)
                    ,
                            userGroupActiveEndTime = (Just __currentTime)
                    ,
                            userGroupDeletedVersionId = (Just result_versionId)
    
                }
        vErrors <- lift $ validate e4
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e4 :: UserGroup)
        e5 <- do
            es <- select $ from $ \o -> do
                where_ (o ^. UserGroupId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing UserGroup" :: Text)
                    ]
    
            return $ e {
                            userGroupEmail = attr_email
                    ,
                            userGroupMailChimpApiKey = attr_mailChimpApiKey
                    ,
                            userGroupMailChimpListName = attr_mailChimpListName
                    ,
                            userGroupName = attr_name
                    ,
                            userGroupActiveStartTime = (Just __currentTime)
    
                }
        vErrors <- lift $ validate e5
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e5 :: UserGroup)
        return AT.emptyObject
    return $ runDB_result
deleteUsergroupsUserGroupIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => UserGroupId -> HandlerT DB (HandlerT master IO) A.Value
deleteUsergroupsUserGroupIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    __currentTime <- liftIO $ getCurrentTime
    _ <- do
        result <- select $ from $ \(ug ) -> do
            let ugId' = ug ^. UserGroupId
            where_ (((ug ^. UserGroupId) ==. (val p1)) &&. (hasWritePerm (val authId) (ug ^. UserGroupId)))

            limit 1
            return ug
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        e2 <- do
    
            return $ Version {
                            versionTime = __currentTime
                    ,
                            versionUserId = authId
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_versionId <- P.insert (e2 :: Version)
        e3 <- do
            es <- select $ from $ \o -> do
                where_ (o ^. UserGroupId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing UserGroup" :: Text)
                    ]
    
            return $ e {
                            userGroupCurrent = Inactive
                    ,
                            userGroupActiveEndTime = (Just __currentTime)
                    ,
                            userGroupDeletedVersionId = (Just result_versionId)
    
                }
        vErrors <- lift $ validate e3
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e3 :: UserGroup)
        return AT.emptyObject
    return $ runDB_result

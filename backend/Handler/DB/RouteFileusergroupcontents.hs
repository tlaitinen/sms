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
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.DB.RouteFileusergroupcontents where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import Handler.DB.Internal
import Handler.DB.Validation
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

deleteFileusergroupcontentsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
deleteFileusergroupcontentsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_userGroupContentIdList <- case HML.lookup "userGroupContentIdList" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute userGroupContentIdList in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute userGroupContentIdList in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    runDB_result <- do
        e1 <- do
    
            return $ Version {
                            versionTime = __currentTime
                    ,
                            versionUserId = authId
    
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_versionId <- P.insert (e1 :: Version)
        forM_ (attr_userGroupContentIdList) $ \result_ugcId -> do
                    e1 <- do
                        es <- select $ from $ \o -> do
                            where_ (o ^. UserGroupContentId ==. (val result_ugcId))
                            limit 1
                            return o
                        e <- case es of
                            [(Entity _ e')] -> return e'    
                            _ -> sendResponseStatus status404 $ A.object [ 
                                    "message" .= ("Could not update a non-existing UserGroupContent" :: Text)
                                ]
                
                        return $ e {
                                        userGroupContentDeletedVersionId = (Just result_versionId)
                
                            }
                    vErrors <- lift $ validate e1
                    case vErrors of
                         xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                                     "message" .= ("Entity validation failed" :: Text),
                                     "errors" .= toJSON xs 
                                 ])
                         _ -> P.repsert result_ugcId (e1 :: UserGroupContent)

        return AT.emptyObject
    return $ runDB_result
postFileusergroupcontentsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postFileusergroupcontentsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_userGroupIdList <- case HML.lookup "userGroupIdList" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute userGroupIdList in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute userGroupIdList in the JSON object in request body" :: Text)
            ]
    attr_fileId <- case HML.lookup "fileId" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute fileId in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute fileId in the JSON object in request body" :: Text)
            ]
    runDB_result <- do
        forM_ (attr_userGroupIdList) $ \result_userGroupId -> do
                    e1 <- do
                
                        return $ UserGroupContent {
                                        userGroupContentUserGroupId = result_userGroupId
                                ,
                                        userGroupContentFileContentId = attr_fileId
                                ,
                                        userGroupContentUserGroupContentId = Nothing
                                ,
                                        userGroupContentUserContentId = Nothing
                                ,
                                        userGroupContentReceiptContentId = Nothing
                                ,
                                        userGroupContentDeletedVersionId = Nothing
                
                            }
                    vErrors <- lift $ validate e1
                    case vErrors of
                        xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                                    "message" .= ("Entity validation failed" :: Text),
                                    "errors" .= toJSON xs 
                                ])
                        _ -> return ()
                    P.insert (e1 :: UserGroupContent)

        return AT.emptyObject
    return $ runDB_result

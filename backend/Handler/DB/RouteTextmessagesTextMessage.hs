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
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Handler.DB.RouteTextmessagesTextMessage where
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

getTextmessagesTextMessageIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => TextMessageId -> HandlerT DB (HandlerT master IO) A.Value
getTextmessagesTextMessageIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    let baseQuery limitOffsetOrder = from $ \(t `LeftOuterJoin` c`LeftOuterJoin` rt) -> do
        on ((rt ?. TextMessageId) ==. (t ^. TextMessageReplyToTextMessageId))
        on ((c ?. ClientId) ==. (t ^. TextMessageSenderClientId))
        let tId' = t ^. TextMessageId
        where_ ((hasReadPerm (val authId) (t ^. TextMessageId)) &&. ((t ^. TextMessageId) ==. (val p1)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                orderBy [  ]

                 
            else return ()
        return (t ^. TextMessageId, t ^. TextMessageText, t ^. TextMessagePhone, t ^. TextMessageSenderClientId, t ^. TextMessageReplyToTextMessageId, t ^. TextMessageQueued, t ^. TextMessageSent, t ^. TextMessageAborted, t ^. TextMessageDeletedVersionId, t ^. TextMessageActiveId, t ^. TextMessageActiveStartTime, t ^. TextMessageActiveEndTime, t ^. TextMessageInsertionTime, t ^. TextMessageInsertedByUserId, c ?. ClientFirstName, c ?. ClientLastName, rt ?. TextMessageText)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7), (Database.Esqueleto.Value f8), (Database.Esqueleto.Value f9), (Database.Esqueleto.Value f10), (Database.Esqueleto.Value f11), (Database.Esqueleto.Value f12), (Database.Esqueleto.Value f13), (Database.Esqueleto.Value f14), (Database.Esqueleto.Value f15), (Database.Esqueleto.Value f16), (Database.Esqueleto.Value f17)) -> A.object [
                    "id" .= toJSON f1,
                    "text" .= toJSON f2,
                    "phone" .= toJSON f3,
                    "senderClientId" .= toJSON f4,
                    "replyToTextMessageId" .= toJSON f5,
                    "queued" .= toJSON f6,
                    "sent" .= toJSON f7,
                    "aborted" .= toJSON f8,
                    "deletedVersionId" .= toJSON f9,
                    "activeId" .= toJSON f10,
                    "activeStartTime" .= toJSON f11,
                    "activeEndTime" .= toJSON f12,
                    "insertionTime" .= toJSON f13,
                    "insertedByUserId" .= toJSON f14,
                    "firstName" .= toJSON f15,
                    "lastName" .= toJSON f16,
                    "replyToText" .= toJSON f17                                    
                    ]
                _ -> A.object []
            ) results)
       ]
putTextmessagesTextMessageIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => TextMessageId -> HandlerT DB (HandlerT master IO) A.Value
putTextmessagesTextMessageIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_text <- case HML.lookup "text" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute text in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute text in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    _ <- do
        result <- select $ from $ \(t ) -> do
            let tId' = t ^. TextMessageId
            where_ (((t ^. TextMessageId) ==. (val p1)) &&. ((hasWritePerm (val authId) (t ^. TextMessageId)) &&. (((t ^. TextMessageQueued) `is` (nothing)) &&. ((t ^. TextMessageSenderClientId) `is` (nothing)))))

            limit 1
            return t
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
        result_t <- do
            r <- get $ ((p1) :: TextMessageId)
            case r of
                Just e -> return e
                _ -> sendResponseStatus status400 $ A.object [  
                    "message" .= ("Could not get entity TextMessage" :: Text) 
                   ] 
        e4 <- do
            let e = result_t
    
            return $ e {
                            textMessageDeletedVersionId = (Just result_versionId)
                    ,
                            textMessageActiveId = (Just p1)
                    ,
                            textMessageActiveEndTime = (Just __currentTime)
    
                }
        vErrors <- lift $ validate e4
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e4 :: TextMessage)
        e5 <- do
            es <- select $ from $ \o -> do
                where_ (o ^. TextMessageId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing TextMessage" :: Text)
                    ]
    
            return $ e {
                            textMessageText = attr_text
    
                }
        vErrors <- lift $ validate e5
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e5 :: TextMessage)
        return AT.emptyObject
    return $ runDB_result
deleteTextmessagesTextMessageIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => TextMessageId -> HandlerT DB (HandlerT master IO) A.Value
deleteTextmessagesTextMessageIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    __currentTime <- liftIO $ getCurrentTime
    _ <- do
        result <- select $ from $ \(t ) -> do
            let tId' = t ^. TextMessageId
            where_ (((t ^. TextMessageId) ==. (val p1)) &&. ((hasWritePerm (val authId) (t ^. TextMessageId)) &&. ((((t ^. TextMessageQueued) `is` (nothing)) ||. (not_ ((t ^. TextMessageAborted) `is` (nothing)))) &&. ((t ^. TextMessageDeletedVersionId) `is` (nothing)))))

            limit 1
            return t
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
                where_ (o ^. TextMessageId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing TextMessage" :: Text)
                    ]
    
            return $ e {
                            textMessageDeletedVersionId = (Just result_versionId)
    
                }
        vErrors <- lift $ validate e3
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e3 :: TextMessage)
        return AT.emptyObject
    return $ runDB_result

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
module Handler.DB.RouteTextmessages where
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
import Handler.TextMessage (addTextMessageRecipients)

getTextmessagesR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getTextmessagesR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    (filterParam_query) <- lookupGetParam "query"
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FS.Filter]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [FS.Sort]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing PP.fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing PP.fromPathPiece defaultLimitParam) :: Maybe Int64
    let baseQuery limitOffsetOrder = from $ \(t ) -> do
        let tId' = t ^. TextMessageId
        where_ (hasReadPerm (val authId) (t ^. TextMessageId))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                            "text" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageText) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageText) ] 
                                _      -> return ()
                            "sendertextMessageId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageSendertextMessageId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageSendertextMessageId) ] 
                                _      -> return ()
                            "queued" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageQueued) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageQueued) ] 
                                _      -> return ()
                            "sent" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageSent) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageSent) ] 
                                _      -> return ()
                            "insertionTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageInsertionTime) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageInsertionTime) ] 
                                _      -> return ()
                            "insertedByUserId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageInsertedByUserId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageInsertedByUserId) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ desc (t ^. TextMessageInsertionTime) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case FS.f_field fjm of
                "id" -> case (FS.f_value fjm >>= PP.fromPathPiece)  of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageId) (val v')
                    _        -> return ()
                "text" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageText) ((val v'))
                    _        -> return ()
                "sendertextMessageId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSendertextMessageId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSendertextMessageId) nothing
                           
                "queued" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageQueued) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageQueued) nothing
                           
                "sent" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSent) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSent) nothing
                           
                "insertionTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageInsertionTime) ((val v'))
                    _        -> return ()
                "insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageInsertedByUserId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageInsertedByUserId) nothing
                           

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (t ^. TextMessageText) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))
            Nothing -> return ()
        return (t ^. TextMessageId, t ^. TextMessageText, t ^. TextMessageSendertextMessageId, t ^. TextMessageQueued, t ^. TextMessageSent, t ^. TextMessageInsertionTime, t ^. TextMessageInsertedByUserId)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7)) -> A.object [
                    "id" .= toJSON f1,
                    "text" .= toJSON f2,
                    "sendertextMessageId" .= toJSON f3,
                    "queued" .= toJSON f4,
                    "sent" .= toJSON f5,
                    "insertionTime" .= toJSON f6,
                    "insertedByUserId" .= toJSON f7                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postTextmessagesR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postTextmessagesR  = lift $ runDB $ do
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
    (Entity _ __auth) <- lift $ requireAuth
    runDB_result <- do
        e1 <- do
    
            return $ TextMessage {
                            textMessageText = attr_text
                    ,
                            textMessageSendertextMessageId = Nothing
                    ,
                            textMessageQueued = Nothing
                    ,
                            textMessageSent = Nothing
                    ,
                            textMessageDeletedVersionId = Nothing
                    ,
                            textMessageActiveId = Nothing
                    ,
                            textMessageActiveStartTime = Nothing
                    ,
                            textMessageActiveEndTime = Nothing
                    ,
                            textMessageInsertionTime = __currentTime
                    ,
                            textMessageInsertedByUserId = (Just authId)
    
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_tId <- P.insert (e1 :: TextMessage)
        addTextMessageRecipients (authId) (result_tId)
        e3 <- do
    
            return $ UserGroupContent {
                            userGroupContentUserGroupId = userDefaultUserGroupId __auth
                    ,
                            userGroupContentFileContentId = Nothing
                    ,
                            userGroupContentUserGroupContentId = Nothing
                    ,
                            userGroupContentUserContentId = Nothing
                    ,
                            userGroupContentClientContentId = Nothing
                    ,
                            userGroupContentTextMessageContentId = (Just result_tId)
                    ,
                            userGroupContentDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e3
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e3 :: UserGroupContent)
        return $ A.object [ "id" .= (toJSON result_tId) ]
    return $ runDB_result

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
module Handler.DB.RouteReceipts where
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
import Handler.Utils (nonEmpty)
import Handler.Utils (hasWritePerm,hasReadPermMaybe,hasReadPerm)

getReceiptsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getReceiptsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FilterJsonMsg]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [SortJsonMsg]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing PP.fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing PP.fromPathPiece defaultLimitParam) :: Maybe Int64
    (filterParam_query) <- lookupGetParam "query"
    (filterParam_hideDeleted :: Maybe Text) <- lookupGetParam "hideDeleted"
    let baseQuery limitOffsetOrder = from $ \(r `InnerJoin` f`LeftOuterJoin` pf) -> do
        on ((pf ?. FilePreviewOfFileId) ==. (just (just (f ^. FileId))))
        on ((f ^. FileId) ==. (r ^. ReceiptFileId))
        let rId' = r ^. ReceiptId
        where_ (hasReadPerm (val authId) (r ^. ReceiptId))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "fileId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptFileId) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptFileId) ] 
                                _      -> return ()
                            "amount" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptAmount) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptAmount) ] 
                                _      -> return ()
                            "name" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptName) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptName) ] 
                                _      -> return ()
                            "insertionTime" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptInsertionTime) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptInsertionTime) ] 
                                _      -> return ()
                            "insertedByUserId" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptInsertedByUserId) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptInsertedByUserId) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (r ^. ReceiptName) ]

                case defaultOffset of
                    Just o -> offset o
                    Nothing -> return ()
                case defaultLimit of
                    Just l -> limit (min 10000 l)
                    Nothing -> return ()
                 
            else return ()
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case filterJsonMsg_field_or_property fjm of
                "id" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptId) (val v')
                    _        -> return ()
                "f.contentType" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FileContentType) ((val v'))
                    _        -> return ()
                "f.size" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FileSize) ((val v'))
                    _        -> return ()
                "f.previewOfFileId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FilePreviewOfFileId) (just ((val v')))
                    _        -> return ()
                "f.name" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FileName) ((val v'))
                    _        -> return ()
                "f.insertionTime" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FileInsertionTime) ((val v'))
                    _        -> return ()
                "f.insertedByUserId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (f  ^.  FileInsertedByUserId) (just ((val v')))
                    _        -> return ()
                "pf.contentType" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FileContentType) (just ((val v')))
                    _        -> return ()
                "pf.size" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FileSize) (just ((val v')))
                    _        -> return ()
                "pf.previewOfFileId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FilePreviewOfFileId) (just (just ((val v'))))
                    _        -> return ()
                "pf.name" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FileName) (just ((val v')))
                    _        -> return ()
                "pf.insertionTime" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FileInsertionTime) (just ((val v')))
                    _        -> return ()
                "pf.insertedByUserId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pf  ?.  FileInsertedByUserId) (just (just ((val v'))))
                    _        -> return ()
                "fileId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptFileId) ((val v'))
                    _        -> return ()
                "amount" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptAmount) ((val v'))
                    _        -> return ()
                "name" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptName) ((val v'))
                    _        -> return ()
                "insertionTime" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptInsertionTime) ((val v'))
                    _        -> return ()
                "insertedByUserId" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (r  ^.  ReceiptInsertedByUserId) (just ((val v')))
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (r ^. ReceiptName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))
            Nothing -> return ()
        if hasDefaultFilter filterParam_hideDeleted defaultFilterJson "hideDeleted" 
            then do 
                 
                where_ $ (r ^. ReceiptDeletedVersionId) `is` (nothing)
            else return ()
        return (r ^. ReceiptId, r ^. ReceiptFileId, r ^. ReceiptAmount, r ^. ReceiptName, r ^. ReceiptInsertionTime, r ^. ReceiptInsertedByUserId, pf ?. FileId)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    return $ A.object [
        "success" .= ("true" :: Text),
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7)) -> A.object [
                    "id" .= toJSON f1,
                    "fileId" .= toJSON f2,
                    "amount" .= toJSON f3,
                    "name" .= toJSON f4,
                    "insertionTime" .= toJSON f5,
                    "insertedByUserId" .= toJSON f6,
                    "previewFileId" .= toJSON f7                                    
                    ]
                _ -> A.object []
            ) results)
       ]
postReceiptsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postReceiptsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_amount <- case HML.lookup "amount" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute amount in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute amount in the JSON object in request body" :: Text)
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
    (Entity _ __auth) <- lift $ requireAuth
    runDB_result <- do
        e1 <- do
    
            return $ Receipt {
                            receiptFileId = attr_fileId
                    ,
                            receiptAmount = attr_amount
                    ,
                            receiptName = attr_name
                    ,
                            receiptActiveId = Nothing
                    ,
                            receiptActiveStartTime = (Just __currentTime)
                    ,
                            receiptActiveEndTime = Nothing
                    ,
                            receiptDeletedVersionId = Nothing
                    ,
                            receiptInsertionTime = __currentTime
                    ,
                            receiptInsertedByUserId = Nothing
    
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_rId <- P.insert (e1 :: Receipt)
        e2 <- do
    
            return $ UserGroupContent {
                            userGroupContentUserGroupId = userDefaultUserGroupId __auth
                    ,
                            userGroupContentFileContentId = Nothing
                    ,
                            userGroupContentUserGroupContentId = Nothing
                    ,
                            userGroupContentUserContentId = Nothing
                    ,
                            userGroupContentReceiptContentId = (Just result_rId)
                    ,
                            userGroupContentDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e2 :: UserGroupContent)
        return $ A.object [ "id" .= (toJSON result_rId) ]
    return $ runDB_result

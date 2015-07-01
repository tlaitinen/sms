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
module Handler.DB.RouteReceipts where
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

getReceiptsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getReceiptsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FS.Filter]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [FS.Sort]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing PP.fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing PP.fromPathPiece defaultLimitParam) :: Maybe Int64
    (filterParam_query) <- lookupGetParam "query"
    (filterParam_hideDeleted :: Maybe Text) <- lookupGetParam "hideDeleted"
    (filterParam_processPeriodId) <- lookupGetParam "processPeriodId"
    let baseQuery limitOffsetOrder = from $ \(r `InnerJoin` f`LeftOuterJoin` pf) -> do
        on ((pf ?. FilePreviewOfFileId) ==. (just (just (f ^. FileId))))
        on ((f ^. FileId) ==. (r ^. ReceiptFileId))
        let rId' = r ^. ReceiptId
        where_ ((hasReadPerm (val authId) (r ^. ReceiptId)) &&. ((r ^. ReceiptDeletedVersionId) `is` (nothing)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                            "fileId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptFileId) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptFileId) ] 
                                _      -> return ()
                            "processPeriodId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptProcessPeriodId) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptProcessPeriodId) ] 
                                _      -> return ()
                            "amount" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptAmount) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptAmount) ] 
                                _      -> return ()
                            "processed" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptProcessed) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptProcessed) ] 
                                _      -> return ()
                            "name" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptName) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptName) ] 
                                _      -> return ()
                            "insertionTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (r  ^.  ReceiptInsertionTime) ] 
                                "DESC" -> orderBy [ desc (r  ^.  ReceiptInsertionTime) ] 
                                _      -> return ()
                            "insertedByUserId" -> case (FS.s_direction sjm) of 
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
            Just xs -> mapM_ (\fjm -> case FS.f_field fjm of
                "id" -> case (FS.f_value fjm >>= PP.fromPathPiece)  of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptId) (val v')
                    _        -> return ()
                "f.contentType" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileContentType) ((val v'))
                    _        -> return ()
                "f.size" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileSize) ((val v'))
                    _        -> return ()
                "f.previewOfFileId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FilePreviewOfFileId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FilePreviewOfFileId) nothing
                           
                "f.name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileName) ((val v'))
                    _        -> return ()
                "f.insertionTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertionTime) ((val v'))
                    _        -> return ()
                "f.insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertedByUserId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertedByUserId) nothing
                           
                "pf.contentType" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileContentType) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileContentType) nothing
                           
                "pf.size" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileSize) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileSize) nothing
                           
                "pf.previewOfFileId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FilePreviewOfFileId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FilePreviewOfFileId) nothing
                           
                "pf.name" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileName) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileName) nothing
                           
                "pf.insertionTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileInsertionTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileInsertionTime) nothing
                           
                "pf.insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileInsertedByUserId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (pf  ?.  FileInsertedByUserId) nothing
                           
                "fileId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptFileId) ((val v'))
                    _        -> return ()
                "processPeriodId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptProcessPeriodId) ((val v'))
                    _        -> return ()
                "amount" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptAmount) ((val v'))
                    _        -> return ()
                "processed" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptProcessed) ((val v'))
                    _        -> return ()
                "name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptName) ((val v'))
                    _        -> return ()
                "insertionTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptInsertionTime) ((val v'))
                    _        -> return ()
                "insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptInsertedByUserId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (r  ^.  ReceiptInsertedByUserId) nothing
                           

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (r ^. ReceiptName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))
            Nothing -> return ()
        if FS.hasDefaultFilter filterParam_hideDeleted defaultFilterJson "hideDeleted" 
            then do 
                 
                where_ $ (r ^. ReceiptDeletedVersionId) `is` (nothing)
            else return ()
        case FS.getDefaultFilter filterParam_processPeriodId defaultFilterJson "processPeriodId" of
            Just localParam -> do 
                
                where_ $ (r ^. ReceiptProcessPeriodId) ==. (val localParam)
            Nothing -> return ()
        return (r ^. ReceiptId, r ^. ReceiptFileId, r ^. ReceiptProcessPeriodId, r ^. ReceiptAmount, r ^. ReceiptProcessed, r ^. ReceiptName, r ^. ReceiptInsertionTime, r ^. ReceiptInsertedByUserId, pf ?. FileId)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7), (Database.Esqueleto.Value f8), (Database.Esqueleto.Value f9)) -> A.object [
                    "id" .= toJSON f1,
                    "fileId" .= toJSON f2,
                    "processPeriodId" .= toJSON f3,
                    "amount" .= toJSON f4,
                    "processed" .= toJSON f5,
                    "name" .= toJSON f6,
                    "insertionTime" .= toJSON f7,
                    "insertedByUserId" .= toJSON f8,
                    "previewFileId" .= toJSON f9                                    
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
    attr_processPeriodId <- case HML.lookup "processPeriodId" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute processPeriodId in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute processPeriodId in the JSON object in request body" :: Text)
            ]
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
                            receiptProcessPeriodId = attr_processPeriodId
                    ,
                            receiptAmount = attr_amount
                    ,
                            receiptProcessed = False
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
                            userGroupContentProcessPeriodContentId = Nothing
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

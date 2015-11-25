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
module Handler.DB.RouteClients where
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

getClientsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getClientsR  = lift $ runDB $ do
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
    (filterParam_dateOfBirthMonth) <- lookupGetParam "dateOfBirthMonth"
    let baseQuery limitOffsetOrder = from $ \(c ) -> do
        let cId' = c ^. ClientId
        where_ (hasReadPerm (val authId) (c ^. ClientId))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                            "firstName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientFirstName) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientFirstName) ] 
                                _      -> return ()
                            "lastName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientLastName) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientLastName) ] 
                                _      -> return ()
                            "email" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientEmail) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientEmail) ] 
                                _      -> return ()
                            "phone" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientPhone) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientPhone) ] 
                                _      -> return ()
                            "dateOfBirth" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientDateOfBirth) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientDateOfBirth) ] 
                                _      -> return ()
                            "card" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientCard) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientCard) ] 
                                _      -> return ()
                            "allowSms" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientAllowSms) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientAllowSms) ] 
                                _      -> return ()
                            "allowEmail" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientAllowEmail) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientAllowEmail) ] 
                                _      -> return ()
                            "deletedVersionId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientDeletedVersionId) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientDeletedVersionId) ] 
                                _      -> return ()
                            "activeId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientActiveId) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientActiveId) ] 
                                _      -> return ()
                            "activeStartTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientActiveStartTime) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientActiveStartTime) ] 
                                _      -> return ()
                            "activeEndTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientActiveEndTime) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientActiveEndTime) ] 
                                _      -> return ()
                            "insertionTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientInsertionTime) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientInsertionTime) ] 
                                _      -> return ()
                            "insertedByUserId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientInsertedByUserId) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientInsertedByUserId) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (c ^. ClientLastName), asc (c ^. ClientFirstName) ]

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
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientId) (val v')
                    _        -> return ()
                "firstName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientFirstName) ((val v'))
                    _        -> return ()
                "lastName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientLastName) ((val v'))
                    _        -> return ()
                "email" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientEmail) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientEmail) nothing
                           
                "phone" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientPhone) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientPhone) nothing
                           
                "dateOfBirth" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientDateOfBirth) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientDateOfBirth) nothing
                           
                "card" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientCard) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientCard) nothing
                           
                "allowSms" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientAllowSms) ((val v'))
                    _        -> return ()
                "allowEmail" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientAllowEmail) ((val v'))
                    _        -> return ()
                "deletedVersionId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientDeletedVersionId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientDeletedVersionId) nothing
                           
                "activeId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveId) nothing
                           
                "activeStartTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveStartTime) ((val v'))
                    _        -> return ()
                "activeEndTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveEndTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveEndTime) nothing
                           
                "insertionTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientInsertionTime) ((val v'))
                    _        -> return ()
                "insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientInsertedByUserId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientInsertedByUserId) nothing
                           

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ ((c ^. ClientFirstName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((c ^. ClientLastName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((c ^. ClientEmail) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))) ||. (((c ^. ClientPhone) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))) ||. ((c ^. ClientCard) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))))))
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_dateOfBirthMonth defaultFilterJson "dateOfBirthMonth" of
            Just localParam -> do 
                
                where_ $ ((extractSubField "MONTH" $ c ^. ClientDateOfBirth)) ==. ((val (localParam :: Double)))
            Nothing -> return ()
        return (c ^. ClientId, c ^. ClientFirstName, c ^. ClientLastName, c ^. ClientEmail, c ^. ClientPhone, c ^. ClientDateOfBirth, c ^. ClientCard, c ^. ClientAllowSms, c ^. ClientAllowEmail, c ^. ClientDeletedVersionId, c ^. ClientActiveId, c ^. ClientActiveStartTime, c ^. ClientActiveEndTime, c ^. ClientInsertionTime, c ^. ClientInsertedByUserId)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    (return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7), (Database.Esqueleto.Value f8), (Database.Esqueleto.Value f9), (Database.Esqueleto.Value f10), (Database.Esqueleto.Value f11), (Database.Esqueleto.Value f12), (Database.Esqueleto.Value f13), (Database.Esqueleto.Value f14), (Database.Esqueleto.Value f15)) -> A.object [
                    "id" .= toJSON f1,
                    "firstName" .= toJSON f2,
                    "lastName" .= toJSON f3,
                    "email" .= toJSON f4,
                    "phone" .= toJSON f5,
                    "dateOfBirth" .= toJSON f6,
                    "card" .= toJSON f7,
                    "allowSms" .= toJSON f8,
                    "allowEmail" .= toJSON f9,
                    "deletedVersionId" .= toJSON f10,
                    "activeId" .= toJSON f11,
                    "activeStartTime" .= toJSON f12,
                    "activeEndTime" .= toJSON f13,
                    "insertionTime" .= toJSON f14,
                    "insertedByUserId" .= toJSON f15                                    
                    ]
                _ -> A.object []
            ) results)
       ])
postClientsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postClientsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_allowEmail <- case HML.lookup "allowEmail" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute allowEmail in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute allowEmail in the JSON object in request body" :: Text)
            ]
    attr_allowSms <- case HML.lookup "allowSms" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute allowSms in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute allowSms in the JSON object in request body" :: Text)
            ]
    attr_card <- case HML.lookup "card" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute card in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute card in the JSON object in request body" :: Text)
            ]
    attr_dateOfBirth <- case HML.lookup "dateOfBirth" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute dateOfBirth in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute dateOfBirth in the JSON object in request body" :: Text)
            ]
    attr_phone <- case HML.lookup "phone" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute phone in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute phone in the JSON object in request body" :: Text)
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
    attr_lastName <- case HML.lookup "lastName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute lastName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute lastName in the JSON object in request body" :: Text)
            ]
    attr_firstName <- case HML.lookup "firstName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute firstName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute firstName in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    (Entity _ __auth) <- lift $ requireAuth
    runDB_result <- do
        e1 <- do
    
            return $ Client {
                            clientFirstName = attr_firstName
                    ,
                            clientLastName = attr_lastName
                    ,
                            clientEmail = attr_email
                    ,
                            clientPhone = attr_phone
                    ,
                            clientDateOfBirth = attr_dateOfBirth
                    ,
                            clientCard = attr_card
                    ,
                            clientAllowSms = attr_allowSms
                    ,
                            clientAllowEmail = attr_allowEmail
                    ,
                            clientDeletedVersionId = Nothing
                    ,
                            clientActiveId = Nothing
                    ,
                            clientActiveStartTime = __currentTime
                    ,
                            clientActiveEndTime = Nothing
                    ,
                            clientInsertionTime = __currentTime
                    ,
                            clientInsertedByUserId = (Just authId)
    
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_cId <- P.insert (e1 :: Client)
        e2 <- do
    
            return $ UserGroupContent {
                            userGroupContentUserGroupId = (userDefaultUserGroupId __auth)
                    ,
                            userGroupContentFileContentId = Nothing
                    ,
                            userGroupContentUserGroupContentId = Nothing
                    ,
                            userGroupContentUserContentId = Nothing
                    ,
                            userGroupContentClientContentId = (Just result_cId)
                    ,
                            userGroupContentTextMessageContentId = Nothing
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
        return $ A.object [ "id" .= (toJSON result_cId) ]
    return $ runDB_result

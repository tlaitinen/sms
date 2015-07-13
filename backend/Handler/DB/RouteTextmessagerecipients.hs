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
module Handler.DB.RouteTextmessagerecipients where
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

getTextmessagerecipientsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getTextmessagerecipientsR  = lift $ runDB $ do
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
    let baseQuery limitOffsetOrder = from $ \(tr `InnerJoin` c) -> do
        on ((c ^. ClientId) ==. (tr ^. TextMessageRecipientClientId))
        let trId' = tr ^. TextMessageRecipientId
        where_ ((hasReadPerm (val authId) (tr ^. TextMessageRecipientTextMessageId)) &&. (hasReadPerm (val authId) (c ^. ClientId)))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                            "textMessageId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (tr  ^.  TextMessageRecipientTextMessageId) ] 
                                "DESC" -> orderBy [ desc (tr  ^.  TextMessageRecipientTextMessageId) ] 
                                _      -> return ()
                            "clientId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (tr  ^.  TextMessageRecipientClientId) ] 
                                "DESC" -> orderBy [ desc (tr  ^.  TextMessageRecipientClientId) ] 
                                _      -> return ()
                            "accepted" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (tr  ^.  TextMessageRecipientAccepted) ] 
                                "DESC" -> orderBy [ desc (tr  ^.  TextMessageRecipientAccepted) ] 
                                _      -> return ()
                            "sent" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (tr  ^.  TextMessageRecipientSent) ] 
                                "DESC" -> orderBy [ desc (tr  ^.  TextMessageRecipientSent) ] 
                                _      -> return ()
                            "delivered" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (tr  ^.  TextMessageRecipientDelivered) ] 
                                "DESC" -> orderBy [ desc (tr  ^.  TextMessageRecipientDelivered) ] 
                                _      -> return ()
                            "firstName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientFirstName) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientFirstName) ] 
                                _      -> return ()
                            "lastName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientLastName) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientLastName) ] 
                                _      -> return ()
                            "phone" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ^.  ClientPhone) ] 
                                "DESC" -> orderBy [ desc (c  ^.  ClientPhone) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [  ]

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
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientId) (val v')
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
                           
                "activeStartTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveStartTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ^.  ClientActiveStartTime) nothing
                           
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
                           
                "textMessageId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientTextMessageId) ((val v'))
                    _        -> return ()
                "clientId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientClientId) ((val v'))
                    _        -> return ()
                "accepted" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientAccepted) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientAccepted) nothing
                           
                "sent" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientSent) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientSent) nothing
                           
                "delivered" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientDelivered) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (tr  ^.  TextMessageRecipientDelivered) nothing
                           

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ ((c ^. ClientFirstName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((c ^. ClientLastName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. ((c ^. ClientPhone) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))))
            Nothing -> return ()
        return (tr ^. TextMessageRecipientId, tr ^. TextMessageRecipientTextMessageId, tr ^. TextMessageRecipientClientId, tr ^. TextMessageRecipientAccepted, tr ^. TextMessageRecipientSent, tr ^. TextMessageRecipientDelivered, c ^. ClientFirstName, c ^. ClientLastName, c ^. ClientPhone)
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
                    "textMessageId" .= toJSON f2,
                    "clientId" .= toJSON f3,
                    "accepted" .= toJSON f4,
                    "sent" .= toJSON f5,
                    "delivered" .= toJSON f6,
                    "firstName" .= toJSON f7,
                    "lastName" .= toJSON f8,
                    "phone" .= toJSON f9                                    
                    ]
                _ -> A.object []
            ) results)
       ]

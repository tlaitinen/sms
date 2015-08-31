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
import Handler.TextMessage (addReplyTextMessageRecipient,addTextMessageRecipients)

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
    let baseQuery limitOffsetOrder = from $ \(t `LeftOuterJoin` c`LeftOuterJoin` rt) -> do
        on ((rt ?. TextMessageId) ==. (t ^. TextMessageReplyToTextMessageId))
        on ((c ?. ClientId) ==. (t ^. TextMessageSenderClientId))
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
                            "phone" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessagePhone) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessagePhone) ] 
                                _      -> return ()
                            "senderClientId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageSenderClientId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageSenderClientId) ] 
                                _      -> return ()
                            "replyToTextMessageId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageReplyToTextMessageId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageReplyToTextMessageId) ] 
                                _      -> return ()
                            "queued" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageQueued) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageQueued) ] 
                                _      -> return ()
                            "sent" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageSent) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageSent) ] 
                                _      -> return ()
                            "aborted" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageAborted) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageAborted) ] 
                                _      -> return ()
                            "deletedVersionId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageDeletedVersionId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageDeletedVersionId) ] 
                                _      -> return ()
                            "activeId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageActiveId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageActiveId) ] 
                                _      -> return ()
                            "activeStartTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageActiveStartTime) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageActiveStartTime) ] 
                                _      -> return ()
                            "activeEndTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageActiveEndTime) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageActiveEndTime) ] 
                                _      -> return ()
                            "insertionTime" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageInsertionTime) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageInsertionTime) ] 
                                _      -> return ()
                            "insertedByUserId" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (t  ^.  TextMessageInsertedByUserId) ] 
                                "DESC" -> orderBy [ desc (t  ^.  TextMessageInsertedByUserId) ] 
                                _      -> return ()
                            "firstName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ?.  ClientFirstName) ] 
                                "DESC" -> orderBy [ desc (c  ?.  ClientFirstName) ] 
                                _      -> return ()
                            "lastName" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (c  ?.  ClientLastName) ] 
                                "DESC" -> orderBy [ desc (c  ?.  ClientLastName) ] 
                                _      -> return ()
                            "replyToText" -> case (FS.s_direction sjm) of 
                                "ASC"  -> orderBy [ asc (rt  ?.  TextMessageText) ] 
                                "DESC" -> orderBy [ desc (rt  ?.  TextMessageText) ] 
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
                "firstName" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientFirstName) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientFirstName) nothing
                           
                "lastName" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientLastName) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientLastName) nothing
                           
                "email" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientEmail) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientEmail) nothing
                           
                "c.phone" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientPhone) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientPhone) nothing
                           
                "dateOfBirth" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientDateOfBirth) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientDateOfBirth) nothing
                           
                "card" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientCard) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientCard) nothing
                           
                "allowSms" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientAllowSms) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientAllowSms) nothing
                           
                "allowEmail" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientAllowEmail) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientAllowEmail) nothing
                           
                "c.deletedVersionId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientDeletedVersionId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientDeletedVersionId) nothing
                           
                "c.activeId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveId) nothing
                           
                "c.activeStartTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveStartTime) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveStartTime) nothing
                           
                "c.activeEndTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveEndTime) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientActiveEndTime) nothing
                           
                "c.insertionTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientInsertionTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientInsertionTime) nothing
                           
                "c.insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientInsertedByUserId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (c  ?.  ClientInsertedByUserId) nothing
                           
                "rt.text" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageText) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageText) nothing
                           
                "rt.phone" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessagePhone) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessagePhone) nothing
                           
                "rt.senderClientId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageSenderClientId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageSenderClientId) nothing
                           
                "rt.replyToTextMessageId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageReplyToTextMessageId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageReplyToTextMessageId) nothing
                           
                "rt.queued" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageQueued) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageQueued) nothing
                           
                "rt.sent" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageSent) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageSent) nothing
                           
                "rt.aborted" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageAborted) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageAborted) nothing
                           
                "rt.deletedVersionId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageDeletedVersionId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageDeletedVersionId) nothing
                           
                "rt.activeId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveId) nothing
                           
                "rt.activeStartTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveStartTime) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveStartTime) nothing
                           
                "rt.activeEndTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveEndTime) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageActiveEndTime) nothing
                           
                "rt.insertionTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageInsertionTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageInsertionTime) nothing
                           
                "rt.insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageInsertedByUserId) (just (just ((val v'))))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (rt  ?.  TextMessageInsertedByUserId) nothing
                           
                "text" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageText) ((val v'))
                    _        -> return ()
                "phone" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessagePhone) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessagePhone) nothing
                           
                "senderClientId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSenderClientId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageSenderClientId) nothing
                           
                "replyToTextMessageId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageReplyToTextMessageId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageReplyToTextMessageId) nothing
                           
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
                           
                "aborted" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageAborted) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageAborted) nothing
                           
                "deletedVersionId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageDeletedVersionId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageDeletedVersionId) nothing
                           
                "activeId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveId) nothing
                           
                "activeStartTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveStartTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveStartTime) nothing
                           
                "activeEndTime" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveEndTime) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (t  ^.  TextMessageActiveEndTime) nothing
                           
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
                            textMessagePhone = Nothing
                    ,
                            textMessageSenderClientId = Nothing
                    ,
                            textMessageReplyToTextMessageId = Nothing
                    ,
                            textMessageQueued = Nothing
                    ,
                            textMessageSent = Nothing
                    ,
                            textMessageAborted = Nothing
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

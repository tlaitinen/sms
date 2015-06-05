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
module Handler.DB.RouteProcessperiods where
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

getProcessperiodsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getProcessperiodsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FilterJsonMsg]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [SortJsonMsg]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing PP.fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing PP.fromPathPiece defaultLimitParam) :: Maybe Int64
    let baseQuery limitOffsetOrder = from $ \(pp ) -> do
        let ppId' = pp ^. ProcessPeriodId
        where_ ((hasReadPerm (val authId) (pp ^. ProcessPeriodId)) &&. ((pp ^. ProcessPeriodLocked) ==. ((val False))))

        _ <- if limitOffsetOrder
            then do 
                offset 0
                limit 10000
                case defaultSortJson of 
                    Just xs -> mapM_ (\sjm -> case sortJsonMsg_property sjm of
                            "firstDay" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (pp  ^.  ProcessPeriodFirstDay) ] 
                                "DESC" -> orderBy [ desc (pp  ^.  ProcessPeriodFirstDay) ] 
                                _      -> return ()
                            "lastDay" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (pp  ^.  ProcessPeriodLastDay) ] 
                                "DESC" -> orderBy [ desc (pp  ^.  ProcessPeriodLastDay) ] 
                                _      -> return ()
                            "locked" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (pp  ^.  ProcessPeriodLocked) ] 
                                "DESC" -> orderBy [ desc (pp  ^.  ProcessPeriodLocked) ] 
                                _      -> return ()
                            "processed" -> case (sortJsonMsg_direction sjm) of 
                                "ASC"  -> orderBy [ asc (pp  ^.  ProcessPeriodProcessed) ] 
                                "DESC" -> orderBy [ desc (pp  ^.  ProcessPeriodProcessed) ] 
                                _      -> return ()
                
                            _ -> return ()
                        ) xs
                    Nothing -> orderBy [ asc (pp ^. ProcessPeriodFirstDay) ]

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
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pp  ^.  ProcessPeriodId) (val v')
                    _        -> return ()
                "firstDay" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pp  ^.  ProcessPeriodFirstDay) ((val v'))
                    _        -> return ()
                "lastDay" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pp  ^.  ProcessPeriodLastDay) ((val v'))
                    _        -> return ()
                "locked" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pp  ^.  ProcessPeriodLocked) ((val v'))
                    _        -> return ()
                "processed" -> case (PP.fromPathPiece $ filterJsonMsg_value fjm) of 
                    (Just v') -> where_ $ defaultFilterOp (filterJsonMsg_comparison fjm) (pp  ^.  ProcessPeriodProcessed) ((val v'))
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        return (pp ^. ProcessPeriodId, pp ^. ProcessPeriodFirstDay, pp ^. ProcessPeriodLastDay, pp ^. ProcessPeriodLocked, pp ^. ProcessPeriodProcessed)
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
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5)) -> A.object [
                    "id" .= toJSON f1,
                    "firstDay" .= toJSON f2,
                    "lastDay" .= toJSON f3,
                    "locked" .= toJSON f4,
                    "processed" .= toJSON f5                                    
                    ]
                _ -> A.object []
            ) results)
       ]

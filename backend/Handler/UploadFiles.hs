{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.UploadFiles where

import qualified Import as I
import Import hiding (fileContentType, fileName, joinPath)
import Yesod.Auth
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Settings
import System.FilePath
import System.Directory (renameFile)
import Data.Text
import System.IO
import Handler.DB
import Data.Aeson


postUploadFilesR :: Handler Value
postUploadFilesR = do
    (Entity userId user) <- requireAuth
    (_, files) <- runRequestBody
    fi <- maybe notFound return $ lookup "file" files
    now <- liftIO $ getCurrentTime
    settings<- fmap appSettings getYesod
    let name = joinPath [appUploadDir settings,  
                         show now ++ "-" ++ T.unpack (I.fileName fi)]
    liftIO $ fileMove fi name
    size <- liftIO $ withFile name ReadMode hFileSize 
        
        
    let fileObj = File {
            fileName = I.fileName fi,
            fileContentType = I.fileContentType fi,
            fileDeletedVersionId = Nothing,
            fileInsertionTime = now,
            fileInsertedByUserId = Just userId,
            fileSize = fromIntegral size,
            fileActiveId = Nothing,
            fileActiveStartTime = Just now,
            fileActiveEndTime = Nothing
        }
    fileId <- runDB $ do
        fileId' <- insert fileObj
        insert $ (newUserGroupContent $ userDefaultUserGroupId user) {
                userGroupContentFileContentId = Just $ fileId'
            }
        let fileId = fromSqlKey fileId'
        let name' = joinPath [ appUploadDir settings, show fileId]
        liftIO $ renameFile name name' 
        return fileId

    return $ object [
            "result" .= ("ok" :: Text),
            "fileId" .= (toJSON fileId) 
        ]


{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.ClientsXlsx where
import Yesod.Auth
import Import hiding (isNothing)
import Database.Persist.Sql
import Database.Esqueleto
import System.FilePath.Posix
import qualified Data.List as L
import Data.Double.Conversion.Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status400)
import Data.Aeson.TH
import Control.Applicative
import Data.Maybe hiding (isNothing)
import Control.Monad
import Control.Monad.Reader
import Control.Exception (catch)
import qualified Control.Monad.State.Lazy as St
import Handler.DB
import Handler.Utils
import Data.Time
import System.Time
import Codec.Xlsx
import qualified Data.Map as Map
import qualified Data.Text as T

getClientsXlsxR :: Handler TypedContent
getClientsXlsxR = do
    uId <- requireAuthId
    clients <- runDB $ select $ from $ \c -> do
        where_ $ hasReadPerm (val uId) (c ^. ClientId)
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        orderBy [ asc (c ^. ClientLastName),
                  asc (c ^. ClientFirstName) ]
        return c
    ct <- liftIO getClockTime    
    let content = fromXlsx ct $ Xlsx (clientsXlsx clients) emptyStyles
    let contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    today <- liftIO $ fmap utctDay getCurrentTime
    addHeader "Content-Disposition" $ T.concat
            [ "attachment; filename=\"clients-", T.pack $ show today, ".xlsx" ]
    sendResponse (TE.encodeUtf8 contentType, toContent content)
    where
        clientsXlsx clients = Map.fromList $ [ ("---", sheet clients) ] 
                                        ++ [ (T.pack $ (if m < 10 then "0" else "") ++ show m, 
                                              sheet $ filter ((==Just m) . monthOfBirth) clients)
                                             | m <- [1..12] ]
        monthOfBirth (Entity _ c) = do
            (_,m,_) <- fmap toGregorian $ clientDateOfBirth c
            Just m
        sheet cs = Worksheet [] Map.empty (cellMap cs) []
        cd = cd' . Just
        cd' v = Cell { _cellValue = v, _cellStyle = Nothing }

        cellMap cs = Map.fromList $ concat [
                [
                    ((row, 1), cd $ CellText $ clientFirstName c),
                    ((row, 2), cd $ CellText $ clientLastName c),
                    ((row, 3), cd' $ (if clientAllowEmail c then clientEmail c else Nothing) >>= Just . CellText),
                    ((row, 4), cd' $ clientPhone c >>= Just . CellText),
                    ((row, 5), cd' $ clientDateOfBirth c >>= Just . CellText . T.pack . show),
                    ((row, 6), cd' $ clientCard c >>= Just . CellText)
                ]
                | (row,(Entity _ c)) <- zip [1..] cs
            ]

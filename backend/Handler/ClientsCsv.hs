{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.ClientsCsv where
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
import Data.Csv
import Data.Maybe hiding (isNothing)
import Control.Monad
import Control.Monad.Reader
import Control.Exception (catch)
import qualified Control.Monad.State.Lazy as St
import Handler.DB
import Handler.Utils
import Data.Time
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

getClientsCsvR :: Handler TypedContent
getClientsCsvR = do
    uId <- requireAuthId
    clients <- runDB $ select $ from $ \c -> do
        where_ $ hasReadPerm (val uId) (c ^. ClientId)
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        orderBy [ asc (c ^. ClientLastName),
                  asc (c ^. ClientFirstName) ]
        return c
    let content = clientsToCsv clients
    let contentType = "application/csv"
    today <- liftIO $ fmap utctDay getCurrentTime
    addHeader "Content-Disposition" $ T.concat
            [ "attachment; filename=\"clients-", T.pack $ show today, ".csv" ]
    sendResponse (TE.encodeUtf8 contentType, toContent content)
    where
        clientsToCsv clients = Data.Csv.encode $ [
                [
                    clientFirstName c, 
                    clientLastName c, 
                    fromMaybe "" $ if clientAllowEmail c then clientEmail c else Nothing,
                    fromMaybe "" $ clientPhone c,
                    maybe "" (T.pack . show) $ clientDateOfBirth c,
                    fromMaybe "" $ clientCard c
                ]
                | (Entity _ c) <- clients
            ]

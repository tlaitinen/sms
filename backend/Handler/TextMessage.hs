{-# LANGUAGE RankNTypes #-}
module Handler.TextMessage where
import Prelude
import Database.Esqueleto
import Data.Text
import Handler.DB.Internal
import Handler.DB.Enums
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import qualified  Database.Persist as P
import Handler.Utils
import Handler.DB.Esqueleto
addReplyTextMessageRecipient :: forall m. (MonadIO m) => TextMessageId -> TextMessageId -> ReaderT SqlBackend m ()
addReplyTextMessageRecipient dstId srcId = do
    msrc <- P.get srcId
    maybe (return ()) (insert_ . (newTextMessageRecipient dstId)) $  
         msrc >>= textMessageSenderClientId

addTextMessageRecipients :: forall m. (MonadIO m) => UserId -> TextMessageId -> Maybe Text -> Maybe Int -> ReaderT SqlBackend m ()
addTextMessageRecipients authId tId query dateOfBirthMonth = do
    clientIds <- select $ from $ \c -> do
        where_ $ hasReadPerm (val authId) (c ^. ClientId)
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        where_ $ not_ $ c ^. ClientPhone ==. val (Just "")
        where_ $ not_ $ isNothing $ c ^. ClientPhone
        where_ $ c ^. ClientAllowSms ==. val True
        case query of
            Just localParam -> where_ $ ((c ^. ClientFirstName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((c ^. ClientLastName) `ilike` (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%"))))) ||. (((c ^. ClientEmail) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))) ||. (((c ^. ClientPhone) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))) ||. ((c ^. ClientCard) `ilike` (just (((val "%")) ++. (((val (localParam :: Text))) ++. ((val "%")))))))))
            Nothing -> return()
        case dateOfBirthMonth of 
            Just localParam -> where_ $ ((extractSubField "MONTH" $ c ^. ClientDateOfBirth)) ==. ((val (fromIntegral localParam)))    
            Nothing -> return ()
        return $ c ^. ClientId
    forM_ clientIds $ \(Value cId) -> insert $ newTextMessageRecipient tId cId

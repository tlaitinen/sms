{-# LANGUAGE RankNTypes #-}
module Handler.TextMessage where
import Prelude
import Database.Esqueleto
import Handler.DB.Internal
import Handler.DB.Enums
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import qualified  Database.Persist as P
import Handler.Utils

addTextMessageRecipients :: forall m. (MonadIO m) => UserId -> TextMessageId -> ReaderT SqlBackend m ()
addTextMessageRecipients authId tId = do
    clientIds <- select $ from $ \c -> do
        where_ $ hasReadPerm (val authId) (c ^. ClientId)
        where_ $ isNothing $ c ^. ClientDeletedVersionId
        where_ $ c ^. ClientAllowSms ==. val True
        return $ c ^. ClientId
    forM_ clientIds $ \(Value cId) -> insert $ newTextMessageRecipient tId cId

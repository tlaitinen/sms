{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.MailChimp where
import Yesod.Auth
import Import hiding ((==.), (=.), isNothing, update, on)
import Handler.DB
import Control.Monad.Trans.Maybe
import Control.Monad
import Database.Esqueleto
import Data.Time

getMailChimpR :: Handler ()
getMailChimpR = return ()

{-
    "type": "unsubscribe", 
    "fired_at": "2009-03-26 21:40:57",  
    "data[action]": "unsub",
    "data[reason]": "manual", 
    "data[id]": "8a25ff1d98", 
    "data[list_id]": "a6b5da1054",
    "data[email]": "api+unsub@mailchimp.com", 
    "data[email_type]": "html", 
    "data[merges][EMAIL]": "api+unsub@mailchimp.com", 
    "data[merges][FNAME]": "MailChimp", 
    "data[merges][LNAME]": "API", 
    "data[merges][INTERESTS]": "Group1,Group2", 
    "data[ip_opt]": "10.20.10.30",
    "data[campaign_id]": "cb398d21d2",
    "data[reason]": "hard"
-}
postMailChimpR :: Handler ()
postMailChimpR = do
    runMaybeT $ do
        type_ <- MaybeT $ lookupPostParam ("type" :: Text)
        listId <- MaybeT $ lookupPostParam ("data[list_id]" :: Text)
        email <- MaybeT $ lookupPostParam ("data[email]" :: Text)
        lift $ runDB $ do
            cs <- select $ from $ \(c `InnerJoin` ug `InnerJoin` li) -> do
                on $ li ^. MailChimpListItemClientId ==. c ^. ClientId &&. li ^. MailChimpListItemUserGroupId ==. ug ^. UserGroupId
                on $ exists $ from $ \ugc -> do
                    where_ $ ugc ^. UserGroupContentUserGroupId ==. ug ^. UserGroupId
                    where_ $ ugc ^. UserGroupContentClientContentId ==. just (c ^. ClientId)
                where_ $ isNothing $ ug ^. UserGroupDeletedVersionId
                where_ $ ug ^. UserGroupMailChimpListName ==. val (Just listId)
                where_ $ c ^. ClientEmail ==. val (Just email)
                where_ $ c ^. ClientAllowEmail ==. val True
                where_ $ isNothing $ c ^. ClientDeletedVersionId
                return (c, li)
            case cs of
                ((Entity cId c,Entity liId _):_) -> when (type_ == "unsubscribe") $ do
                    now <- liftIO getCurrentTime
                    versionId <- insert $ newVersion now
                    _ <- insert $ c {
                            clientDeletedVersionId = Just versionId,
                            clientActiveEndTime = Just now,
                            clientActiveId = Just cId
                        }
                    update $ \c' -> do
                        set c' [ 
                                ClientActiveStartTime =. val now,
                                ClientAllowEmail =. val False
                            ]
                        where_ $ c' ^. ClientId ==. val cId
                    update $ \li' -> do
                        set li' [
                                MailChimpListItemSyncTime =. val now
                            ]
                        where_ $ li' ^. MailChimpListItemId ==. val liId

                _ -> return ()   
    return ()

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where
import Yesod.Auth
import Import
import Handler.DB
getHomeR :: Handler Value
getHomeR = do
    (Entity _ u) <- requireAuth
    mug <- runDB $ get $ userDefaultUserGroupId u
    return $ object [
            "user" .= object [
                "name" .= userName u,
                "firstName" .= userFirstName u,
                "lastName" .= userLastName u,
                "email" .= userEmail u,
                "config" .= userConfig u,
                "defaultUserGroupEmail" .= (mug >>= (Just . userGroupEmail))
            ]
        ]

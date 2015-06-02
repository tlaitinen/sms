{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.SetUserPassword (postSetUserPasswordR) where

import Import 
import Handler.DB
import Network.HTTP.Types (status400)
import Yesod.Auth
import Yesod.Auth.HashDB (setPassword)
import qualified Data.Aeson as A

postSetUserPasswordR :: UserId -> Handler ()
postSetUserPasswordR userId = do
    (Entity authId auth) <- requireAuth
    if authId == userId || userAdmin auth 
        then do
            password <- lookupPostParam ("password"::Text)
            case password of
                Just pw -> do
                    maybeUser <- runDB $ get userId
                    case maybeUser of
                        Just user -> do
                            user' <- liftIO $ setPassword pw user
                            runDB $ update userId [ 
                                    UserPassword =. userPassword user',
                                    UserSalt     =. userSalt user'
                                ]
                        Nothing -> reply "User not found"
                Nothing -> reply "Missing required parameter 'password'"
        else reply "Unauthorized password change attempt"
    where reply msg = 
            sendResponseStatus status400 $ A.object [ "error" .= (msg :: Text)]
                    
 
                            

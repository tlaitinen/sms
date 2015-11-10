{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.MailChimp where
import Yesod.Auth
import Import
import Handler.DB
getMailChimpR :: Handler Value
getMailChimpR = do
    return $ object [ ]

postMailChimpR :: Handler Value 
postMailChimpR = do
    return $ object []

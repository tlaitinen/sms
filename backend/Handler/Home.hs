{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where
import Yesod.Auth
import Import
getHomeR :: Handler Text
getHomeR = do
    _ <- requireAuthId
    return "OK"

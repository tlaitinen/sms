{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.SpellCheck where
import Yesod.Auth
import Import hiding (joinPath)
import System.FilePath
import qualified Data.Text as T
import qualified Data.Aeson  as A
import Handler.DB
import System.Process

getSpellCheckR :: Handler Value
getSpellCheckR = do
    (Entity _ u) <- requireAuth
    mtext <- lookupGetParam "text"

    case mtext of 
        Just text -> do
            settings<- fmap appSettings getYesod
            (code, stdout, stderr) <- liftIO $ readProcessWithExitCode (joinPath [ "scripts", "spellcheck" ]) [ T.unpack $ appSpellCheckLanguage settings ] (T.unpack text)
            return $ A.object [
                    "unknownWords" .= words stdout
                ]

        Nothing -> sendResponseStatus status400 $ A.object [
                        "message" .= ("Expected attribute text in the query string" :: Text)
                            ]

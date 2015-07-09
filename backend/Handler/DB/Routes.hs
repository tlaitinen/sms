{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Handler.DB.Routes where
import Prelude (const)
import Handler.DB.Enums
import Handler.DB.Esqueleto
import Handler.DB.Internal
import Handler.DB.RouteFiles
import Handler.DB.RouteFilesFile
import Handler.DB.RouteFileusergroupcontents
import Handler.DB.RouteUsergroupcontents
import Handler.DB.RouteUsergroups
import Handler.DB.RouteUsergroupsUserGroup
import Handler.DB.RouteUsergroupitems
import Handler.DB.RouteUsergroupitemsUserGroupItem
import Handler.DB.RouteUsers
import Handler.DB.RouteUsersUser
import Handler.DB.RouteVersions
import Handler.DB.RouteClients
import Handler.DB.RouteClientsClient
import Handler.DB.RouteIncomingtextmessages
import Handler.DB.RouteTextmessages
import Handler.DB.RouteTextmessagesTextMessage
import Handler.DB.RouteTextmessagesTextMessageQueue
import Handler.DB.RouteTextmessagesTextMessageAbort
import Handler.DB.RouteTextmessagerecipients
import Handler.DB.RouteTextmessagerecipientsqueue
import Handler.DB.RouteTextmessagerecipientsTextMessageRecipientAccept
import Handler.DB.RouteTextmessagerecipientsTextMessageRecipientSent
import Handler.DB.RouteTextmessagerecipientsTextMessageRecipientFail

import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
import Yesod.Core
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)


getDB :: a -> DB
getDB = const DB

mkYesodSubData "DB" [parseRoutes|
/files        FilesR      GET
/files/#FileId        FilesFileIdR      GET PUT DELETE
/fileusergroupcontents        FileusergroupcontentsR      DELETE POST
/usergroupcontents        UsergroupcontentsR      GET
/usergroups        UsergroupsR      GET POST
/usergroups/#UserGroupId        UsergroupsUserGroupIdR      PUT DELETE
/usergroupitems        UsergroupitemsR      GET POST
/usergroupitems/#UserGroupItemId        UsergroupitemsUserGroupItemIdR      DELETE
/users        UsersR      GET POST
/users/#UserId        UsersUserIdR      GET DELETE PUT
/versions        VersionsR      GET
/clients        ClientsR      GET POST
/clients/#ClientId        ClientsClientIdR      DELETE PUT
/incomingtextmessages        IncomingtextmessagesR      POST
/textmessages        TextmessagesR      GET POST
/textmessages/#TextMessageId        TextmessagesTextMessageIdR      PUT DELETE
/textmessages/#TextMessageId/queue        TextmessagesTextMessageIdQueueR      POST
/textmessages/#TextMessageId/abort        TextmessagesTextMessageIdAbortR      POST
/textmessagerecipients        TextmessagerecipientsR      GET
/textmessagerecipientsqueue        TextmessagerecipientsqueueR      GET
/textmessagerecipients/#TextMessageRecipientId/accept        TextmessagerecipientsTextMessageRecipientIdAcceptR      POST
/textmessagerecipients/#TextMessageRecipientId/sent        TextmessagerecipientsTextMessageRecipientIdSentR      POST
/textmessagerecipients/#TextMessageRecipientId/fail        TextmessagerecipientsTextMessageRecipientIdFailR      POST
|]


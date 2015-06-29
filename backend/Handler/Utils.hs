module Handler.Utils where
import Prelude
import Database.Esqueleto
import Handler.DB.Internal
import Handler.DB.Enums

nonEmpty "" = return False
nonEmpty _ = return True


hasReadPermMaybe :: (UserGroupContentContentIdField e, PersistField (Key e)) => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Maybe (Key e))) -> SqlExpr (Database.Esqueleto.Value Bool)
hasReadPermMaybe aId fld = exists $ from $ \ugc -> do
    where_ $ fld ==. ugc ^. (userGroupContentContentIdField fld)
    where_ $ exists $ from $ \(ug`InnerJoin` ugi) -> do 
        on $ ug ^. UserGroupId ==. ugi ^. UserGroupItemUserGroupId
        where_ $ ugi ^. UserGroupItemUserId ==. aId
        where_ $ not_ $ isNothing $ ugc ^. (userGroupContentContentIdField fld)
        where_ $ isNothing $ ugi ^. UserGroupItemDeletedVersionId
        where_ $ ug ^. UserGroupId ==. ugc ^. UserGroupContentUserGroupId
        where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId

hasReadPerm :: (UserGroupContentContentIdField e, PersistField (Key e))
        => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Key e)) -> SqlExpr (Database.Esqueleto.Value Bool)
hasReadPerm aId fld = hasReadPermMaybe aId (just fld)

hasWritePerm :: (UserGroupContentContentIdField e, PersistField (Key e))
    => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Key e)) -> SqlExpr (Database.Esqueleto.Value Bool)
hasWritePerm aId fld' = let fld = just fld' in exists $ from $ \ugc -> do
    where_ $ fld ==. ugc ^. (userGroupContentContentIdField fld)
    where_ $ exists $ from $ \(ug`InnerJoin` ugi) -> do 
        on $ ug ^. UserGroupId ==. ugi ^. UserGroupItemUserGroupId
        where_ $ ugi ^. UserGroupItemUserId ==. aId
        where_ $ ugi ^. UserGroupItemMode ==. val UserGroupModeReadWrite
        where_ $ not_ $ isNothing $ ugc ^. (userGroupContentContentIdField fld)
        where_ $ isNothing $ ugi ^. UserGroupItemDeletedVersionId
        where_ $ ug ^. UserGroupId ==. ugc ^. UserGroupContentUserGroupId
        where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId

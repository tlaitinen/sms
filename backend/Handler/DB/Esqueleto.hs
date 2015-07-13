{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , MultiParamTypeClasses
           , OverloadedStrings
           , UndecidableInstances
 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}           
module Handler.DB.Esqueleto where
import Prelude
import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((***), first)
import Control.Exception (throw, throwIO)
import Control.Monad ((>=>), ap, void, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.Int 
import Data.Word
import Data.Time
import Data.List (intersperse)
import Data.Monoid (Monoid(..), (<>))
import Data.Proxy (Proxy(..))
import qualified Database.Persist as P
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Writer as W
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLBI
import Data.Text (Text)
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
uncommas :: [TLB.Builder] -> TLB.Builder
uncommas = mconcat . intersperse ", " . filter (/= mempty)

uncommas' :: Monoid a => [(TLB.Builder, a)] -> (TLB.Builder, a)
uncommas' = (uncommas *** mconcat) . unzip


type NegateFlag = Bool
baseDefaultFilterOp :: P.PersistField a => NegateFlag -> Text -> SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
baseDefaultFilterOp neg op a b = if neg then not_ $ f op a b  else f op a b
    where
        f  "eq" = (==.)
        f "neq" = (!=.)
        f "lt" = (<.)
        f "gt" = (>.)
        f "le" = (<=.)
        f "ge" = (>=.)
        f "is" = is
        f "is not" = isNot 
        f _ = (==.)

textDefaultFilterOp :: (P.PersistField a) => NegateFlag -> Text -> SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
textDefaultFilterOp neg op a b = case op of
    "like" -> if neg then not_ $ unsafe_like a b else unsafe_like a b
    "ilike" -> if neg then not_ $ unsafe_ilike a b else unsafe_ilike a b
    _ -> baseDefaultFilterOp neg op a b
class P.PersistField a => FieldFilter a where
    defaultFilterOp :: NegateFlag -> Text -> SqlExpr (Value a) -> SqlExpr (Value a) -> SqlExpr (Value Bool)
    defaultFilterOp = baseDefaultFilterOp
instance FieldFilter Text where
    defaultFilterOp = textDefaultFilterOp
instance FieldFilter (Maybe Text) where
    defaultFilterOp = textDefaultFilterOp


instance P.PersistEntity a => FieldFilter (P.Key a) where
instance FieldFilter P.Checkmark where
instance FieldFilter Double where
instance FieldFilter Word32 where
instance FieldFilter Word64 where
instance FieldFilter Int32 where
instance FieldFilter Int64 where
instance FieldFilter Int where
instance FieldFilter Day where
instance FieldFilter TimeOfDay where
instance FieldFilter UTCTime where
instance FieldFilter Bool where
instance P.PersistEntity a => FieldFilter (Maybe (P.Key a)) where
instance FieldFilter (Maybe P.Checkmark) where
instance FieldFilter (Maybe Double) where
instance FieldFilter (Maybe Word32) where
instance FieldFilter (Maybe Word64) where
instance FieldFilter (Maybe Int32) where
instance FieldFilter (Maybe Int64) where
instance FieldFilter (Maybe Int) where
instance FieldFilter (Maybe Day) where
instance FieldFilter (Maybe TimeOfDay) where
instance FieldFilter (Maybe UTCTime) where
instance FieldFilter (Maybe Bool) where
instance FieldFilter (Maybe a) => FieldFilter (Maybe (Maybe a)) where
    defaultFilterOp neg op a b = defaultFilterOp neg op (joinV a) (joinV b)

is = unsafeSqlBinOp " IS "
isNot = unsafeSqlBinOp " IS NOT "
unsafe_like = unsafeSqlBinOp " LIKE "
unsafe_ilike = unsafeSqlBinOp " ILIKE "

extractSubField :: UnsafeSqlFunctionArgument a => TLB.Builder -> a -> SqlExpr (Value Double)
extractSubField = unsafeSqlExtractSubField
instance ( SqlSelect i1 o1, SqlSelect i2 o2, SqlSelect i3 o3, SqlSelect i4 o4, SqlSelect i5 o5, SqlSelect i6 o6, SqlSelect i7 o7, SqlSelect i8 o8, SqlSelect i9 o9, SqlSelect i10 o10, SqlSelect i11 o11, SqlSelect i12 o12, SqlSelect i13 o13, SqlSelect i14 o14, SqlSelect i15 o15, SqlSelect i16 o16, SqlSelect i17 o17 ) => SqlSelect (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) (o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16, o17) where
  sqlSelectCols esc (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) =
    uncommas' [ sqlSelectCols esc i1, sqlSelectCols esc i2, sqlSelectCols esc i3, sqlSelectCols esc i4, sqlSelectCols esc i5, sqlSelectCols esc i6, sqlSelectCols esc i7, sqlSelectCols esc i8, sqlSelectCols esc i9, sqlSelectCols esc i10, sqlSelectCols esc i11, sqlSelectCols esc i12, sqlSelectCols esc i13, sqlSelectCols esc i14, sqlSelectCols esc i15, sqlSelectCols esc i16, sqlSelectCols esc i17 ]
  sqlSelectColCount   = sqlSelectColCount . from17P
  sqlSelectProcessRow = fmap to17 . sqlSelectProcessRow

from17P :: Proxy (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17) -> Proxy ((i1,i2), (i3,i4), (i5,i6), (i7,i8), (i9,i10), (i11,i12), (i13,i14), (i15,i16), i17)
from17P = const Proxy

to17 :: ((i1,i2), (i3,i4), (i5,i6), (i7,i8), (i9,i10), (i11,i12), (i13,i14), (i15,i16), i17) -> (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17)
to17 ((i1,i2), (i3,i4), (i5,i6), (i7,i8), (i9,i10), (i11,i12), (i13,i14), (i15,i16), i17) = (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17)

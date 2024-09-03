{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module PkgsUpdate where

import GHC.Generics
import Data.Aeson
import Text.PrettyPrint.Tabulate
import qualified Data.Text as T

data PkgUpdate = PkgUpdate {
          name :: T.Text
        , before :: T.Text
        , after :: T.Text
        , path :: T.Text
        , warnings :: [T.Text]
        } deriving (Eq, Generic, Show)

instance ToJSON PkgUpdate where
        toEncoding = genericToEncoding defaultOptions

instance FromJSON PkgUpdate

data PkgUpdateRow = PkgUpdateRow {
          rname :: T.Text
        , rbefore :: T.Text
        , rafter :: T.Text
        , rpath :: T.Text
        , rwarning :: T.Text
        } deriving (Eq, Generic, Show)

class ToPkgUpdateRow a where
        toPkgUpdateRow :: a -> PkgUpdateRow

instance ToPkgUpdateRow PkgUpdate where
        toPkgUpdateRow (PkgUpdate n b a p ws) =
                PkgUpdateRow n b a
                        (head $ T.splitOn (T.pack "/") p)
                        (if (length ws) < 2 then (T.pack "N/A") else (last ws))

instance CellValueFormatter T.Text
instance Tabulate PkgUpdateRow DoNotExpandWhenNested

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.PrettyPrint.Tabulate
import System.Directory
import PkgsUpdate
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Simple

subscriptionFile :: FilePath -> IO [T.Text]
subscriptionFile p = do
        packages <- fmap T.lines (T.readFile p)
        return packages

onlineData :: IO [PkgUpdate]
onlineData = do
        res <- httpJSON "https://raw.githubusercontent.com/AOSC-Dev/anicca/main/pkgsupdate.json"
        return (getResponseBody res :: [PkgUpdate])

assocPkgsUpdate :: T.Text -> [PkgUpdate] -> Maybe PkgUpdate
assocPkgsUpdate _ [] = Nothing
assocPkgsUpdate p (x:xs) = if (p == (name x)) then Just x else assocPkgsUpdate p xs

search :: [T.Text] -> [PkgUpdate] -> [PkgUpdate]
search ps pkgsupdate = foldl (\acc p ->
        if (assocPkgsUpdate p pkgsupdate) == Nothing then
                acc
        else (acc ++ [fromJust (assocPkgsUpdate p pkgsupdate)])) [] ps

main :: IO ()
main = do
        homedir <- getHomeDirectory
        packages <- subscriptionFile (homedir ++ "/.config/anicca-subscribe/subscribe.txt")
        pkgsupdate <- onlineData
        let updates = map toPkgUpdateRow $ sortBy (\a b -> compare (name a) (name b)) $ search packages pkgsupdate
        printTable updates

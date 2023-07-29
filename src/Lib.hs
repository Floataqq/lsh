{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib (listdir) where

import Parser (Args(..))
import Data.List (zipWith4)
import Data.Map qualified as Map
import System.FilePath
import System.Directory
import Config
import Colors hiding (color)

color :: String -> String -> IO String
color path f = do
    dir <- doesDirectoryExist $ path </> f
    if dir then
        return $ directory ++ " " ++ byellow f
    else
        return $ Map.findWithDefault file (takeExtension $ path </> f) icons ++ " " ++ bgreen f

rightPad :: Int -> String -> String
rightPad x xs = xs ++ replicate (x - length xs) ' ' 

pad :: [String] -> [String]
pad xs = map (rightPad m) xs where m = maximum $ map length xs

filesize :: FilePath -> IO Integer
filesize path = do
    isFile <- doesFileExist path
    if isFile
        then getFileSize path
        else return 0          -- if a directory

parseFilesize :: Integer -> String
parseFilesize s =
    if | s == 0      -> bred "---"
       | s <= 1024   -> show  s               ++ " " ++ red      "B"
       | s <= 1024^2 -> show (s `div` 1024)   ++ " " ++ byellow  "KB"
       | s <= 1024^3 -> show (s `div` 1024^2) ++ " " ++ cyan     "MB"
       | otherwise   -> show (s `div` 1024^3) ++ " " ++ magenta  "GB"

parsePermissions :: Permissions -> String
parsePermissions p = 
    concat [bgreen rr, cyan ww, byellow xx, magenta ss, " | "] where
    rr = if readable p   then "r" else "-"
    ww = if writable p   then "w" else "-"
    xx = if executable p then "x" else "-"
    ss = if searchable p then "s" else "-"

lslist :: Args -> IO ()
lslist args = do
    dc <- getDirectoryContents path
    let files = reverse dc
    --line numbers
    let numbers = pad $ if nums then [show i ++ "." | i <- [1..length files]]
                                else ["" | _ <- files]
    --filenames and icons
    colored <- mapM (color path) files
    let filenames = map ( ++ " |") x where 
        x = if dots then pad colored 
                    else pad $ filter (\xs -> xs!!5 /= '.') colored
    --file permissions
    perms <- if perm 
        then do
            p <- mapM (\f -> getPermissions $ path </> f) files
            d <- mapM (\f -> doesDirectoryExist $ path </> f) files
            let dflags = map (\b -> red $ if b then "d" else ".") d
            return $ zipWith (++) dflags $ map parsePermissions p
        else do
            return ["" | _ <- files]
    --file sizes
    filesizes <- if size 
        then do
            s <- mapM (\f -> filesize $ path </> f) files
            let sizes = map parseFilesize s
            return $ map (++ " | ") $ pad sizes
        else do
            return ["" | _ <- files]


    putStrLn $ unlines $ zipWith4 (\n p s f -> n ++ " | " ++ p ++ s ++ f) 
                            numbers perms filesizes filenames
    where Args {list=_, size, dots, perm, nums, path, afl=_} = args 

listdir :: Args -> IO ()
listdir args = do
    if afl then
        lslist Args {list=True, size=True, dots=True, perm=True, 
                     nums=True, path, afl=True}
    else if or [list, size, perm, nums] then --stuff that implies -l
        lslist args
    else do 
        files <- getDirectoryContents path
        colored <- mapM (color path) files
        if dots then
            putStrLn $ unwords $ reverse colored
        else
            putStrLn $ unwords $ reverse $ filter (\x -> x!!5 /= '.') colored   
    where Args {list, size, dots, perm, nums, path, afl} = args 
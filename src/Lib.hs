{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}

module Lib (-- | This is a binary package, and @Lib@ is the heart of it, so 
            --   there are no library functions in this module.
            listf,
            lsList,
            listDir) where

import Parser (Args(..))
import Data.List (zipWith5)
import Data.Map qualified as Map
import System.FilePath ( (</>), takeExtension)
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      getDirectoryContents,
      getModificationTime,
      getFileSize,
      getPermissions,
      Permissions(..) )
import Data.Time.Clock (UTCTime)
import Config
import Colors

splitAtChar :: Char -> String -> [String]
splitAtChar _ [] = []
splitAtChar c str =
  let (before, rest) = break (== c) str
  in before : case rest of
       []    -> []
       (_:t) -> splitAtChar c t

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

formatFilename :: String -> String -> IO String
formatFilename path f = do
    dir <- doesDirectoryExist $ path </> f
    if dir then
        return $ directoryIcon ++ " " ++ byellow f
    else
        return $ Map.findWithDefault fileIcon (takeExtension $ path </> f) iconConfig ++ " " ++ bgreen f

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

parseTime :: UTCTime -> String
parseTime t = bcyan x ++ " " ++ byellow y ++ bred " UTC" where
    (x:y:_) = splitAtChar ' ' (head (splitAtChar '.' $ show t))

lsList :: Args -> IO String
lsList args = do
    dc <- getDirectoryContents path
    let files = reverse dc
    --line numbers
    let numbers = pad $ if nums then [show i ++ "." | i <- [1..length files]]
                                else ["" | _ <- files]
    --filenames and icons
    colored <- mapM (formatFilename path) files
    let filenames = map ( ++ " | ") x where
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
    -- modidfication times
    modtimes <- if time
        then do
            t <- mapM (\f -> getModificationTime $ path </> f) files
            let times = map parseTime t
            return $ map (++ " | ") $ pad times
        else do
            return ["" | _ <- files]

    return $ unlines $ zipWith5 (\n p s f t -> n ++ " | " ++ p ++ s ++ f ++ t)
                            numbers perms filesizes filenames modtimes
    where Args {list=_, size, dots, perm, nums, time, path, afl=_} = args

listDir :: Args -> IO String
listDir args = do
    files <- getDirectoryContents path
    colored <- mapM (formatFilename path) files
    if dots then
        return $ unwords $ reverse colored
    else
        return $ unwords $ reverse $ filter (\x -> x!!5 /= '.') colored
    where Args {list, size, dots, perm, nums, time, path, afl} = args

-- | Entry point, @lslist@ is called when @-l@ is enabled or implied
listf :: Args -> IO ()
listf args = do
    if afl then do                                    -- -a flag
        l <- lsList Args {list=True, size=True, dots=True, perm=True,
                     nums=True, time=True, path, afl=True}
        putStrLn l
    else if or [list, size, perm, nums, time] then do --stuff that implies -l
        l <- lsList args
        putStrLn l
    else do
        l <- listDir args
        putStrLn l
    where Args {list, size, dots, perm, nums, time, path, afl} = args
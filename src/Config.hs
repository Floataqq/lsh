{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Map
import Colors hiding (color)

-- you must color them somehow, otherwise the formatting
-- won't work (use white in case)
-- default when extension is not found
file :: String
file = bgreen "\983578"

-- when the object is a directory
directory :: String
directory = byellow "\60035"


-- there must be a colors function for every icon in the tuples, 
-- otherwise formatting will not work correctly
-- use white and bwhite in case
-- if you want an empty icon, use exactly one space
icons :: Map String String
icons = fromList [
    (".bz",    bgreen  "\61894"),
    (".c",     blue    "\58910"),
    (".cs",    magenta "\983835"),
    (".css",   blue    "\59209"),
    (".cpp",   blue    "\58909"),
    (".dart",  blue    "\58956"),
    (".deb",   bred    "\62214"),
    --("dockerfile", "\62216"),       -- SPOILERS ??
    (".gz",    bgreen  "\61894"),
    (".h",     magenta "\58910"),
    (".hi",    magenta "\986258"),
    (".hs",    magenta "\986258"),
    (".html",  red     "\61755"),
    (".jpeg",  bgreen  "\983631"),
    (".jpg",   bgreen  "\983631"),
    (".js",    byellow "\983838"),
    (".kt",    blue    "\987673"),
    (".lock",  blue    "\983870"),
    (".lua",   blue    "\58912"),
    (".mp3",   bgreen  "\983942"),
    (".mp4",   bgreen  "\61764"),
    (".out",   white   "\58898"),
    (".png",   bgreen  "\983631"),
    (".py",    blue    "\58886"),
    (".rar",   bgreen  "\61894"),
    --("robots", "\984745"),
    (".toml",  blue    "\61459"),
    (".ts",    blue    "\984806"),
    (".ttf",   red     "\61489"),
    (".rb",    bred    "\59193"),
    (".rpm",   bred    "\62230"),
    (".vue",   bgreen  "\985156"),
    (".woff",  red     "\61489"),
    (".voff2", red     "\61489"),
    (".xz",    bgreen  "\61894"),
    (".zip",   bgreen  "\61894")
    ]
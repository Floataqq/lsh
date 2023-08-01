{-# LANGUAGE OverloadedStrings #-}

module Config (fileIcon, directoryIcon, iconConfig) where

import Data.Map
import Colors

-- you must color them somehow, otherwise the formatting
-- won't work (use white in case)
-- default when extension is not found
fileIcon :: String
fileIcon = bgreen "\983578"

-- when the object is a directory
directoryIcon :: String
directoryIcon = byellow "\60035"


-- there must be a colors function for every icon in the tuples, 
-- otherwise formatting will not work correctly
-- use white and bwhite in case
-- if you want an empty icon, use exactly one space
iconConfig :: Map String String
iconConfig = fromList [
    (".bz",            cyan    "\61894"),
    (".c",             blue    "\58910"),
    (".cl",            blue    "\984615"),
    (".class",         red     "\57942"),
    (".cs",            magenta "\983835"),
    (".css",           blue    "\59209"),
    (".cpp",           blue    "\58909"),
    (".dart",          blue    "\58956"),
    (".deb",           bred    "\62214"),
    --("dockerfile", "\62216"),       -- SPOILERS ??
    (".fasl",          blue    "\984615"),
    (".gitignore",     bcyan "\58973"),
    (".gitattributes", bcyan "\58973"),
    (".gz",            cyan    "\61894"),
    (".h",             magenta "\58910"),
    (".hi",            magenta "\986258"),
    (".hs",            magenta "\986258"),
    (".html",          red     "\61755"),
    (".java",          red     "\57942"),
    (".jar",           red     "\57942"),
    (".jpeg",          bgreen  "\983631"),
    (".jpg",           bgreen  "\983631"),
    (".js",            byellow "\983838"),
    (".json",          byellow "\58891"),
    (".kt",            yellow  "\987673"),
    (".l",             blue    "\984615"),
    (".lisp",          blue    "\984615"),
    (".lsp",           blue    "\984615"),
    (".lock",          blue    "\983870"),
    (".lua",           blue    "\58912"),
    (".md",            cyan    "\58889"),
    (".mp3",           magenta "\983942"),
    (".mp4",           bred    "\61764"),
    (".out",           white   "\58898"),
    (".pl",            cyan    "\59006"),
    (".png",           bgreen  "\983631"),
    (".py",            blue    "\58886"),
    (".rar",           cyan    "\61894"),
    --("robots", "\984745"),
    (".toml",          blue    "\61459"),
    (".ts",            blue    "\984806"),
    (".ttf",           red     "\61489"),
    (".rb",            bred    "\59193"),
    (".rpm",           bred    "\62230"),
    (".vue",           bgreen  "\985156"),
    (".woff",          red     "\61489"),
    (".woff2",         red     "\61489"),
    (".xml",           cyan    "\984512"),
    (".xz",            cyan    "\61894"),
    (".yaml",          magenta "\59048"),
    (".yml",           magenta "\59048"),
    (".zip",           cyan    "\61894")
    ]
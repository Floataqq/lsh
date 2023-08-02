module Parser(-- * Argument parsers
              Args(..), args, opts) where

import Options.Applicative

data Args = Args {    -- ^ Structure for accepting arguments
    list :: Bool,     -- ^ @\-l@ flag (show files as a table)
    size :: Bool,     -- ^ @\-s@ flag (show file sizes), implies @-l@
    dots :: Bool,     -- ^ @\-x@ flag (show dotfiles and directories)
    perm :: Bool,     -- ^ @\-p@ flag (display file permissions), implies @-l@
    nums :: Bool,     -- ^ @\-n@ flag (display line numbers), implies @-l@
    time :: Bool,     -- ^ @\-t@ flag (display last modification time), implies @-l@
    afl  :: Bool,     -- ^ @\-a@ flag (equivalent to -lapsnt)
    path :: String    -- ^ path to the directory to list
} deriving (Show, Eq)

-- | Argument parser using @optparse-applicative@
args :: Parser Args
args = Args 
    <$> switch (
        long "list" <> short 'l' <>
        help "Wheter to display each file on a new line"
    )
    <*> switch (
        long "filesize" <> short 's' <>
        help "Whether to display file sizes (implies -l)"
    )
    <*> switch (
        long "extra"   <> short 'x' <>
        help "Whether to display dotfiles/hidden files"
    )
    <*> switch (
        long "perms"  <> short 'p' <>
        help "Whether to display file permissions (implies -l)"
    )
    <*> switch (
        long "line-numbers" <> short 'n' <>
        help "Whether to display line numbers in listing (implies -l)"
    )
    <*> switch (
        long "mod-times" <> short 't' <>
        help "Whether to display last modification times in listing (implies -l)"
    )   
    <*> switch (
        long "all" <> short 'a' <>
        help "Equivalent to -lapsnt"
    )
    <*> argument str (
        metavar "DIR" <>
        help "Directory to list" <>
        value "."
    )

-- | expanded version to be actually executed by @lsh@ 
opts :: ParserInfo Args
opts = info (args <**> helper) (
            fullDesc <>
            progDesc "List files in a directory" <>
            header "lsh - list your files with style"
            )
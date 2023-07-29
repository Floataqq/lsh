module Parser(Args(..), args, opts) where

import Options.Applicative

data Args = Args {
    list :: Bool,
    size :: Bool,
    dots :: Bool,     --list dotfiles/hidden files or not
    perm :: Bool,     --drwxs
    nums :: Bool,     --line numbers
    afl  :: Bool,
    path :: String
} deriving (Show, Eq)

args :: Parser Args
args = Args 
    <$> switch (
        long "list" <> short 'l' <>
        help "Wheter to display each file on a new line"
    )
    <*> switch (
        long "fsize" <> short 's' <>
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
        long "all" <> short 'a' <>
        help "Equivalent to -lapsn"
    )
    <*> argument str (
        metavar "DIR" <>
        help "Directory to list" <>
        value "."
    )

opts :: ParserInfo Args
opts = info (args <**> helper) (
            fullDesc <>
            progDesc "List files in a directory" <>
            header "lsh - a little sample haskell project :)" <>
            footer "ᓚᘏᗢ"
            )
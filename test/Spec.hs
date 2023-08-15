{-# LANGUAGE NamedFieldPuns #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Parser as P
import Lib
import System.FilePath
import System.Directory
import Data.List (isInfixOf)

instance Arbitrary P.Args where
    arbitrary = do
        list <- arbitrary
        size <- arbitrary
        dots <- arbitrary
        perm <- arbitrary
        nums <- arbitrary
        time <- arbitrary
        afl  <- arbitrary
        return $ P.Args {
                     P.list = list
                    ,P.size = size
                    ,P.dots = dots
                    ,P.perm = perm
                    ,P.nums = nums
                    ,P.time = time
                    ,P.afl  = afl
                    ,P.path = ""
                }

allFilesListedPropM :: FilePath -> P.Args -> Property
allFilesListedPropM p args = monadicIO $ do
    files <- run (lsList args{P.path=p})
    filenames <- run (getDirectoryContents p)
    let filesToTest = if dots || afl
        then filenames
        else filter (\x -> head x /= '.') filenames
    assert (
        all (`isInfixOf` files) filesToTest
        )
    where P.Args {P.list, P.size, P.dots, P.perm, P.nums, P.time, P.path, P.afl} = args
main :: IO ()
main = do
    putStrLn "Testing that all required files are present in the listing"
    putStrLn "Dir - / - 200 tests"
    quickCheck $ withMaxSuccess 200 (allFilesListedPropM ".")
    putStrLn "Dir - test/icons - 200 tests"
    quickCheck $ withMaxSuccess 200 (allFilesListedPropM $ "test" </> "icons")
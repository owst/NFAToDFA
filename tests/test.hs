import Test.Tasty
import Test.Tasty.Golden as G

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.List ( isSuffixOf, isPrefixOf )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getDirectoryContents, doesFileExist, removeFile )
import System.Process ( readProcessWithExitCode )

main = do
    let isTest = isSuffixOf ".test"
    testFiles <- filter isTest <$> getDirectoryContents testDir
    tests <- mapM toTest testFiles
    defaultMain $ testGroup "golden-tests" tests

prog :: String -> String -> IO ()
prog inFile outFile = do
    let runner = "dist/build/NFAToDFA/NFAToDFA"
    input <- readFile inFile
    let shouldDoSubsetConstruction =
            isPrefixOf "-subset-" . dropWhile (/= '-') $ inFile
        args = ["-s" | shouldDoSubsetConstruction]
    (_, stdout, stderr) <- readProcessWithExitCode runner args input
    writeFile outFile $ stdout ++ stderr

toTest f = do
    let testFile = testDir </> f
        goldenFile = testFile <.> "golden"
        outputFile = testFile <.> "output"
    outputExists <- doesFileExist outputFile
    when outputExists $
        removeFile outputFile
    return $ goldenVsFile f goldenFile outputFile (prog testFile outputFile)

testDir = "tests"

import Test.Tasty
import Test.Tasty.Golden as G

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.List ( isSuffixOf )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getDirectoryContents, doesFileExist, removeFile )
import System.Process ( readProcessWithExitCode )

main = do
    let isTest f = isSuffixOf ".test" f
    testFiles <- filter isTest <$> getDirectoryContents testDir
    tests <- mapM toTest testFiles
    defaultMain $ testGroup "golden-tests" tests

prog :: String -> String -> IO ()
prog inFile outFile = do
    let runner = "dist/build/NFAToDFA/NFAToDFA"
    input <- readFile inFile
    (_, stdout, stderr) <- readProcessWithExitCode runner [] input
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

import Tldr.App (appMain)
import Tldr (renderPage)
import Tldr.Types (ColorSetting(..))
import Test.Tasty
-- import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, assertFailure, assertEqual)
import Test.Tasty.Golden (goldenVsFile)
import System.IO (withBinaryFile, IOMode(..))
-- import Control.Exception (throw, SomeException)
import Data.Monoid ((<>))
import System.Exit (ExitCode(ExitSuccess))

tests :: TestTree
tests = testGroup "tldr Tests" [goldenUnitTests, integrationTests]

---------------------- Golden Unit Tests ------------------------
-- test that tldr CLI is correctly rendering source markdown files to plain-text in terminal
-- currently testing this for tldr of commands: ls, grep, ps
-- For each command, these files are there in test/data/ folder:
--   * COMMAND.md - this is input markdown file (from tldr-pages github), 
--                  which has to be rendered to plain-text
--   * COMMAND.golden - this is expected output (plain-text files)
--   * COMMAND.output - this is actual output (when tests were run)
-- NOTE: COMMAND.output files are generated when `stack test` is run

goldenUnitTests :: TestTree
goldenUnitTests = testGroup "Golden tests" [gtests]

renderPageToFile :: FilePath -> FilePath -> IO ()
renderPageToFile mdfile opfile = do
  withBinaryFile opfile WriteMode (\handle -> renderPage mdfile handle UseColor)

-- For adding new command, you need to add:
-- A new ".md" file for that command
-- A new ".golden" file for the expected output

commandTest :: String -> TestTree
commandTest str = goldenVsFile (str <> " test") (golden str) (output str) (renderPageToFile (md str) (output str))
    where
      prefix = "test/data/"
      golden cmd = prefix <> cmd <> ".golden"
      output cmd = prefix <> cmd <> ".output"
      md cmd = prefix <> cmd <> ".md"

gtests :: TestTree
gtests = testGroup "(render test)"
         [
          commandTest "ls"
         , commandTest "ps"
         , commandTest "grep"
         ]

----------------------- Integration Tests --------------------------
-- test that various CLI flags are working (i.e., exit code is 0 for success)
-- commands to test: 'tldr', 'tldr --help', 'tldr --version'

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests" [itests]

-- -- Function to run a command and check if exit code is success
-- cliCommandSuccessful :: String -> IO ()
-- cliCommandSuccessful cmd = do
--     result <- try (readCreateProcess (shell cmd) "") :: IO (Either SomeException String)
--     case result of
--         Left _ -> assertFailure $ "Command failed: " ++ cmd
--         Right _ -> return ()

-- -- BUG: tldr on PATH is not necessarily the right tldr (built from current source)
-- itests :: TestTree
-- itests = testGroup "(cli flags working test)"
--           [
--             -- testCase "Always Fails" $ assertFailure "Failing",
--             testCase "tldr with no arguments" $ cliCommandSuccessful "tldr"
--             , testCase "tldr --help" $ cliCommandSuccessful "tldr --help"
--             , testCase "tldr --version" $ cliCommandSuccessful "tldr --version"
--           ]

-- BUG: all tests failing
-- TODO: suppress stdout of appMain (IO action)
itests :: TestTree
itests = testGroup "(cli flags working test)" 
         [
          testCase "tldr with no arguments" $ appMain [] >>= assertEqual expectedSuccessMsg ExitSuccess
          , testCase "tldr --help" $ appMain ["--help"] >>= assertEqual expectedSuccessMsg ExitSuccess
          , testCase "tldr --version" $ appMain ["--version"] >>= assertEqual expectedSuccessMsg ExitSuccess
          , testCase "tldr --list" $ appMain ["--list"] >>= assertEqual expectedSuccessMsg ExitSuccess
         ]
    where expectedSuccessMsg = "Exit code should be 0 (success)"

--------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

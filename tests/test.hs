import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.Posix.Files
import Data.Bits

import System.IO.Temp

main = do
  setFileCreationMask 0
  sys_tmp_dir <- getTemporaryDirectory

  defaultMain $ testGroup "Tests"
    [ testCase "openNewBinaryFile creates file with 666 permissions" $ do
        (fp, fh) <- openNewBinaryFile sys_tmp_dir "test.txt"
        status <- getFileStatus fp
        fileMode status .&. 0o777  @?= 0o666 
    ]

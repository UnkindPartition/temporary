{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.IO
import System.FilePath
import Data.Bits
import Data.List
#ifndef mingw32_HOST_OS
import System.Posix.Files
#endif

import System.IO.Temp

main = do
#ifndef mingw32_HOST_OS
  setFileCreationMask 0
#endif
  sys_tmp_dir <- getTemporaryDirectory

  defaultMain $ testGroup "Tests"
    [ testCase "openNewBinaryFile creates file with 666 permissions" $ do
        (fp, fh) <- openNewBinaryFile sys_tmp_dir "test.txt"
        let fn = takeFileName fp
        assertBool ("Does not match template: " ++ fn) $
          ("test" `isPrefixOf` fn) && (".txt" `isSuffixOf` fn)
        assertBool ("Is not in the right directory: " ++ fp) $
          takeDirectory fp == sys_tmp_dir
        hClose fh
        assertBool "File does not exist" =<< doesFileExist fp
#ifndef mingw32_HOST_OS
        status <- getFileStatus fp
        fileMode status .&. 0o777  @?= 0o666 
#endif
        removeFile fp
    ]

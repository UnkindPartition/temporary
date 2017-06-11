{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import System.IO
import System.FilePath
import Data.Bits
import Data.List
import GHC.IO.Handle
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
    [ testCase "openNewBinaryFile" $ do
        (fp, fh) <- openNewBinaryFile sys_tmp_dir "test.txt"
        let fn = takeFileName fp
        assertBool ("Does not match template: " ++ fn) $
          ("test" `isPrefixOf` fn) && (".txt" `isSuffixOf` fn)
        assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
          takeDirectory fp `equalFilePath` sys_tmp_dir
        hClose fh
        assertBool "File does not exist" =<< doesFileExist fp
#ifndef mingw32_HOST_OS
        status <- getFileStatus fp
        fileMode status .&. 0o777  @?= 0o666 
#endif
        removeFile fp
    , testCase "withSystemTempFile" $ do
        (fp, fh) <- withSystemTempFile "test.txt" $ \fp fh -> do
          let fn = takeFileName fp
          assertBool ("Does not match template: " ++ fn) $
            ("test" `isPrefixOf` fn) && (".txt" `isSuffixOf` fn)
          assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
            takeDirectory fp `equalFilePath` sys_tmp_dir
          assertBool "File not open" =<< hIsOpen fh
          hPutStrLn  fh "hi"
          assertBool "File does not exist" =<< doesFileExist fp
#ifndef mingw32_HOST_OS
          status <- getFileStatus fp
          fileMode status .&. 0o777  @?= 0o600
#endif
          return (fp, fh)
        assertBool "File still exists" . not =<< doesFileExist fp
        assertBool "File not closed" =<< hIsClosed fh
    , testCase "withSystemTempDirectory" $ do
        fp <- withSystemTempDirectory "test.dir" $ \fp -> do
          let fn = takeFileName fp
          assertBool ("Does not match template: " ++ fn) $
            ("test.dir" `isPrefixOf` fn)
          assertBool (fp ++ " is not in the right directory " ++ sys_tmp_dir) $
            takeDirectory fp `equalFilePath` sys_tmp_dir
          assertBool "Directory does not exist" =<< doesDirectoryExist fp
#ifndef mingw32_HOST_OS
          status <- getFileStatus fp
          fileMode status .&. 0o777  @?= 0o700
#endif
          return fp
        assertBool "Directory still exists" . not =<< doesDirectoryExist fp
    , testCase "writeSystemTempFile" $ do
        fp <- writeSystemTempFile "blah.txt" "hello"
        str <- readFile fp
        "hello" @?= str
        removeFile fp
    , testCase "emptySystemTempFile" $ do
        fp <- emptySystemTempFile "empty.txt"
        assertBool "File doesn't exist" =<< doesFileExist fp
        removeFile fp
    ]

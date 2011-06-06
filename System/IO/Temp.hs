module System.IO.Temp (
    withSystemTempFile, withSystemTempDirectory,
    withTempFile, withTempDirectory,
    module Distribution.Compat.TempFile
  ) where

-- NB: this module was extracted directly from "Distribution/Simple/Utils.hs"
-- in a Cabal tree whose most recent commit was on Sun Oct 10 22:00:26
--
-- The files in the Distribution/Compat tree are exact copies of the corresponding
-- file in the Cabal checkout.


import qualified Control.Exception as Exception

import System.Directory
import System.IO

import Distribution.Compat.TempFile


-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempFile :: String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> Handle -> IO a) -- ^ Callback that can use the file
                   -> IO a
withSystemTempFile template action = getTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: String   -- ^ Directory name template. See 'openTempFile'.
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withSystemTempDirectory template action = getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Use a temporary filename that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of the
-- template. The temp file is deleted after use. For example:
--
-- > withTempFile "src" "sdist." $ \tmpFile hFile -> do ...
--
-- The @tmpFlie@ will be file in the given directory, e.g.
-- @src/sdist.342@.
withTempFile :: FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> IO a) -- ^ Callback that can use the file
             -> IO a
withTempFile tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> hClose handle >> ignoringIOErrors (removeFile name))
    (uncurry action)

-- | Create and use a temporary directory.
--
-- Creates a new temporary directory inside the given directory, making use
-- of the template. The temp directory is deleted after use. For example:
--
-- > withTempDirectory "src" "sdist." $ \tmpDir -> do ...
--
-- The @tmpDir@ will be a new subdirectory of the given directory, e.g.
-- @src/sdist.342@.
withTempDirectory :: FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> IO a) -- ^ Callback that can use the directory
                  -> IO a
withTempDirectory targetDir template =
  Exception.bracket
    (createTempDirectory targetDir template)
    (ignoringIOErrors . removeDirectoryRecursive)

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `Exception.catch` (\e -> const (return ()) (e :: IOError))

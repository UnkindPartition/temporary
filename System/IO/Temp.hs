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


import Control.Monad.Catch as Exception

import Control.Monad.IO.Class
import System.Directory
import System.IO

import Distribution.Compat.TempFile


-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempFile :: (MonadIO m, MonadMask m) =>
                      String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: (MonadIO m, MonadMask m) =>
                           String   -- ^ Directory name template. See 'openTempFile'.
                        -> (FilePath -> m a) -- ^ Callback that can use the directory
                        -> m a
withSystemTempDirectory template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempDirectory tmpDir template action


-- | Use a temporary filename that doesn't already exist.
--
-- Creates a new temporary file inside the given directory, making use of the
-- template. The temp file is deleted after use. For example:
--
-- > withTempFile "src" "sdist." $ \tmpFile hFile -> do ...
--
-- The @tmpFlie@ will be file in the given directory, e.g.
-- @src/sdist.342@.
withTempFile :: (MonadIO m, MonadMask m) =>
                FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  Exception.bracket
    (liftIO (openTempFile tmpDir template))
    (\(name, handle) -> liftIO (hClose handle >> ignoringIOErrors (removeFile name)))
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
withTempDirectory :: (MonadMask m, MonadIO m) =>
                     FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
  Exception.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)

ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `Exception.catch` (\e -> const (return ()) (e :: IOError))

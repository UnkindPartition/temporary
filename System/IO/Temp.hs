{-# LANGUAGE CPP #-}
module System.IO.Temp (
    withSystemTempFile, withSystemTempDirectory,
    withTempFile, withTempDirectory,
    openNewBinaryFile,
    createTempDirectory,
    writeTempFile, writeSystemTempFile,
    emptyTempFile, emptySystemTempFile,
    -- * Re-exports from System.IO
    openTempFile,
    openBinaryTempFile
  ) where

import qualified Control.Monad.Catch as MC

import Control.Monad.IO.Class
import System.Directory
import System.IO (Handle, hClose, openTempFile, openBinaryTempFile,
       openBinaryTempFileWithDefaultPermissions, hPutStr)
import System.IO.Error        (isAlreadyExistsError)
import System.Posix.Internals (c_getpid)
import System.FilePath        ((</>))
#ifdef mingw32_HOST_OS
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
#endif

-- | Create and use a temporary file in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempFile', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempFile :: (MonadIO m, MC.MonadMask m) =>
                      String   -- ^ File name template. See 'openTempFile'.
                   -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
                   -> m a
withSystemTempFile template action = liftIO getTemporaryDirectory >>= \tmpDir -> withTempFile tmpDir template action

-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getTemporaryDirectory'.
withSystemTempDirectory :: (MonadIO m, MC.MonadMask m) =>
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
-- The @tmpFile@ will be file in the given directory, e.g.
-- @src/sdist.342@.
withTempFile :: (MonadIO m, MC.MonadMask m) =>
                FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> m a) -- ^ Callback that can use the file
             -> m a
withTempFile tmpDir template action =
  MC.bracket
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
withTempDirectory :: (MC.MonadMask m, MonadIO m) =>
                     FilePath -- ^ Temp directory to create the directory in
                  -> String   -- ^ Directory name template. See 'openTempFile'.
                  -> (FilePath -> m a) -- ^ Callback that can use the directory
                  -> m a
withTempDirectory targetDir template =
  MC.bracket
    (liftIO (createTempDirectory targetDir template))
    (liftIO . ignoringIOErrors . removeDirectoryRecursive)


-- | Create a unique new file, write (text mode) a given data string to it,
--   and close the handle again. The file will not be deleted automatically,
--   and only the current user will have permission to access the file
--   (see `openTempFile` for details).
--
-- @since 1.2.1
writeTempFile :: FilePath    -- ^ Directory in which to create the file
              -> String      -- ^ File name template.
              -> String      -- ^ Data to store in the file.
              -> IO FilePath -- ^ Path to the (written and closed) file.
writeTempFile targetDir template content = MC.bracket
    (openTempFile targetDir template)
    (\(_, handle) -> hClose handle)
    (\(filePath, handle) -> hPutStr handle content >> return filePath)

-- | Like 'writeTempFile', but use the system directory for temporary files.
--
-- @since 1.2.1
writeSystemTempFile :: String      -- ^ File name template.
                    -> String      -- ^ Data to store in the file.
                    -> IO FilePath -- ^ Path to the (written and closed) file.
writeSystemTempFile template content
    = getTemporaryDirectory >>= \tmpDir -> writeTempFile tmpDir template content

-- | Create a unique new empty file. (Equivalent to 'writeTempFile' with empty data string.)
--   This is useful if the actual content is provided by an external process.
--
-- @since 1.2.1
emptyTempFile :: FilePath    -- ^ Directory in which to create the file
              -> String      -- ^ File name template.
              -> IO FilePath -- ^ Path to the (written and closed) file.
emptyTempFile targetDir template = MC.bracket
    (openTempFile targetDir template)
    (\(_, handle) -> hClose handle)
    (\(filePath, _) -> return filePath)

-- | Like 'emptyTempFile', but use the system directory for temporary files.
--
-- @since 1.2.1
emptySystemTempFile :: String      -- ^ File name template.
                    -> IO FilePath -- ^ Path to the (written and closed) file.
emptySystemTempFile template
    = getTemporaryDirectory >>= \tmpDir -> emptyTempFile tmpDir template


ignoringIOErrors :: MC.MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `MC.catch` (\e -> const (return ()) (e :: IOError))

-- | Like 'openBinaryTempFile', but uses 666 rather than 600 for the
-- permissions.
--
-- Equivalent to 'openBinaryTempFileWithDefaultPermissions'.
openNewBinaryFile :: FilePath -> String -> IO (FilePath, Handle)
openNewBinaryFile = openBinaryTempFileWithDefaultPermissions

-- | Create a temporary directory. See 'withTempDirectory'.
createTempDirectory
  :: FilePath -- ^ Temp directory to create the directory in
  -> String -- ^ Directory name template
  -> IO FilePath
createTempDirectory dir template = do
  pid <- c_getpid
  findTempName pid
  where
    findTempName x = do
      let dirpath = dir </> template ++ show x
      r <- MC.try $ mkPrivateDir dirpath
      case r of
        Right _ -> return dirpath
        Left  e | isAlreadyExistsError e -> findTempName (x+1)
                | otherwise              -> ioError e

mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif

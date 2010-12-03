module System.IO.Temp (
    module System.IO.Temp,
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


-- | Use a temporary filename that doesn't already exist.
--
withTempFile :: FilePath -- ^ Temp dir to create the file in
             -> String   -- ^ File name template. See 'openTempFile'.
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, handle) -> hClose handle >> removeFile name)
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
--
withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory targetDir template =
  Exception.bracket
    (createTempDirectory targetDir template)
    (removeDirectoryRecursive)
module Paths_Gemini (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/batman/Library/Haskell/ghc-7.6.3/lib/Gemini-0.2/bin"
libdir     = "/Users/batman/Library/Haskell/ghc-7.6.3/lib/Gemini-0.2/lib"
datadir    = "/Users/batman/Library/Haskell/ghc-7.6.3/lib/Gemini-0.2/share"
libexecdir = "/Users/batman/Library/Haskell/ghc-7.6.3/lib/Gemini-0.2/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Gemini_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gemini_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gemini_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gemini_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

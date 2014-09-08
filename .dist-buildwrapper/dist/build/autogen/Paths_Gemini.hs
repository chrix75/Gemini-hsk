module Paths_Gemini (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/batman/Library/Haskell/bin"
libdir     = "/Users/batman/Library/Haskell/ghc-7.8.3-x86_64/lib/Gemini-0.3"
datadir    = "/Users/batman/Library/Haskell/share/ghc-7.8.3-x86_64/Gemini-0.3"
libexecdir = "/Users/batman/Library/Haskell/libexec"
sysconfdir = "/Users/batman/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gemini_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gemini_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Gemini_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gemini_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gemini_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

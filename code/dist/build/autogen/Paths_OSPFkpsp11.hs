module Paths_OSPFkpsp11 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/retolehnherr/.cabal/bin"
libdir     = "/Users/retolehnherr/.cabal/lib/OSPFkpsp11-0.1.0.0/ghc-7.4.2"
datadir    = "/Users/retolehnherr/.cabal/share/OSPFkpsp11-0.1.0.0"
libexecdir = "/Users/retolehnherr/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "OSPFkpsp11_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OSPFkpsp11_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "OSPFkpsp11_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OSPFkpsp11_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

module Paths_mailchimp (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/bjterry/.cabal/bin"
libdir     = "/Users/bjterry/.cabal/lib/mailchimp-0.1.0.0/ghc-7.6.3"
datadir    = "/Users/bjterry/.cabal/share/mailchimp-0.1.0.0"
libexecdir = "/Users/bjterry/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "mailchimp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mailchimp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "mailchimp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mailchimp_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

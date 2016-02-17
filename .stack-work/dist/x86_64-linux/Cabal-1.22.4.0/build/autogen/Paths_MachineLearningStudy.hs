module Paths_MachineLearningStudy (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ugnom/work/study/MachineLearningStudy/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/bin"
libdir     = "/home/ugnom/work/study/MachineLearningStudy/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/lib/x86_64-linux-ghc-7.10.2/MachineLearningStudy-0.1.0.0-CNwdqNQ21MrLKFQxwEedkV"
datadir    = "/home/ugnom/work/study/MachineLearningStudy/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/share/x86_64-linux-ghc-7.10.2/MachineLearningStudy-0.1.0.0"
libexecdir = "/home/ugnom/work/study/MachineLearningStudy/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/libexec"
sysconfdir = "/home/ugnom/work/study/MachineLearningStudy/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MachineLearningStudy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MachineLearningStudy_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MachineLearningStudy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MachineLearningStudy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MachineLearningStudy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

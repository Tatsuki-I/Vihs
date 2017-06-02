{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Vihs (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/bin"
libdir     = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/lib/x86_64-osx-ghc-8.0.2/Vihs-0.1.0.0-DMVo40OgO5qKdq1xmuMUG2"
dynlibdir  = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/share/x86_64-osx-ghc-8.0.2/Vihs-0.1.0.0"
libexecdir = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/libexec"
sysconfdir = "/Users/tatsuki/GitHub/Vihs/.stack-work/install/x86_64-osx/lts-8.16/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Vihs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Vihs_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Vihs_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Vihs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Vihs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Vihs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

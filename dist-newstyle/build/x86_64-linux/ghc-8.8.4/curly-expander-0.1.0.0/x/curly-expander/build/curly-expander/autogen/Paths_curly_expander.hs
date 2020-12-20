{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_curly_expander (
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

bindir     = "/home/p/p-dev/.cabal/bin"
libdir     = "/home/p/p-dev/.cabal/lib/x86_64-linux-ghc-8.8.4/curly-expander-0.1.0.0-inplace-curly-expander"
dynlibdir  = "/home/p/p-dev/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/p/p-dev/.cabal/share/x86_64-linux-ghc-8.8.4/curly-expander-0.1.0.0"
libexecdir = "/home/p/p-dev/.cabal/libexec/x86_64-linux-ghc-8.8.4/curly-expander-0.1.0.0"
sysconfdir = "/home/p/p-dev/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "curly_expander_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curly_expander_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "curly_expander_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "curly_expander_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curly_expander_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "curly_expander_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

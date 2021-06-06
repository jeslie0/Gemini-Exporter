{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Gemini (
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

bindir     = "/home/james/.cabal/bin"
libdir     = "/home/james/.cabal/lib/x86_64-linux-ghc-8.10.4/Gemini-0.1.0.0-inplace-BlogExport"
dynlibdir  = "/home/james/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/james/.cabal/share/x86_64-linux-ghc-8.10.4/Gemini-0.1.0.0"
libexecdir = "/home/james/.cabal/libexec/x86_64-linux-ghc-8.10.4/Gemini-0.1.0.0"
sysconfdir = "/home/james/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Gemini_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Gemini_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Gemini_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Gemini_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Gemini_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Gemini_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

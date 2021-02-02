{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskellPeli (
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

bindir     = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\bin"
libdir     = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\lib\\x86_64-windows-ghc-8.8.4\\haskellPeli-0.1.0.0-2nrSvKa62aL6yUTPaCOpTh-haskellPeli"
dynlibdir  = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\lib\\x86_64-windows-ghc-8.8.4"
datadir    = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\share\\x86_64-windows-ghc-8.8.4\\haskellPeli-0.1.0.0"
libexecdir = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\libexec\\x86_64-windows-ghc-8.8.4\\haskellPeli-0.1.0.0"
sysconfdir = "C:\\Users\\35850\\OneDrive\\kurssit\\TIEA341\\haskellPeli\\.stack-work\\install\\c1d7e9ee\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskellPeli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskellPeli_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskellPeli_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskellPeli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskellPeli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskellPeli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
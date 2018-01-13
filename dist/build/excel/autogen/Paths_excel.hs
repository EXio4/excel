{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_excel (
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

bindir     = "/home/exio4/excel/.cabal-sandbox/bin"
libdir     = "/home/exio4/excel/.cabal-sandbox/lib/x86_64-linux-ghc-8.2.0.20170522/excel-0.1.0.0-7ZsbMQaaOTd5RePEoTAZ9V-excel"
dynlibdir  = "/home/exio4/excel/.cabal-sandbox/lib/x86_64-linux-ghc-8.2.0.20170522"
datadir    = "/home/exio4/excel/.cabal-sandbox/share/x86_64-linux-ghc-8.2.0.20170522/excel-0.1.0.0"
libexecdir = "/home/exio4/excel/.cabal-sandbox/libexec/x86_64-linux-ghc-8.2.0.20170522/excel-0.1.0.0"
sysconfdir = "/home/exio4/excel/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "excel_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "excel_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "excel_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "excel_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "excel_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "excel_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

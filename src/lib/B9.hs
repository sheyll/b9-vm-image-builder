module B9
       ( module B9.Builder
       , module System.Exit
       , module System.FilePath
       , module Control.Applicative
       , module Control.Monad
       , module Control.Monad.IO.Class
       , module Data.Monoid
       , module Data.List
       , module Data.Maybe
       , module Text.Printf
       , load
       , configure
       , build
       ) where

import B9.Builder
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.List
import Data.Maybe
import Data.Word
import System.Directory ( createDirectoryIfMissing
                        , createDirectory
                        , setCurrentDirectory
                        , getCurrentDirectory
                        , canonicalizePath
                        , renameFile
                        , removeFile
                        , copyFile
                        , removeDirectoryRecursive
                        )
import System.Exit ( exitWith
                   , ExitCode (..) )
import System.FilePath ( takeDirectory
                       , takeFileName
                       , replaceExtension
                       , (</>)
                       , (<.>) )
import System.Process ( callCommand )
import System.Random ( randomIO )
import Text.Printf ( printf )

import qualified B9.LibVirtLXC as LibVirtLXC

configure :: MonadIO m => Maybe SystemPath -> B9Config -> m ConfigParser
configure b9ConfigPath cliConfig = do
  writeInitialB9Config b9ConfigPath cliConfig LibVirtLXC.setDefaultConfig
  readB9Config b9ConfigPath

load :: MonadIO m => FilePath -> m Project
load projectFile = maybeConsult (Just projectFile) emptyProject

build :: Project -> ConfigParser -> B9Config -> [String] -> IO Bool
build = buildProject

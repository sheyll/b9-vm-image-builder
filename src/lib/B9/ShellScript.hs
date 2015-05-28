{-# LANGUAGE DeriveDataTypeable #-}
{-| Definition of 'Script' and functions to convert 'Script's to bash
    scripts. -}
module B9.ShellScript ( writeSh
                      , emptyScript
                      , CmdVerbosity (..)
                      , Cwd (..)
                      , User (..)
                      , Script (..)
                      ) where

import Data.Data
import Data.Monoid
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Data.List ( intercalate )
import System.Directory ( getPermissions
                        , setPermissions
                        , setOwnerExecutable )

data Script = In FilePath [Script]
            | As String [Script]
            | IgnoreErrors Bool [Script]
            | Verbosity CmdVerbosity [Script]
            | Begin [Script]
            | Run FilePath [String]
            | NoOP
            deriving (Show, Read, Typeable, Data,Eq)

instance Monoid Script where
  mempty = NoOP
  NoOP `mappend` s = s
  s `mappend` NoOP = s
  (Begin ss) `mappend` (Begin ss') = Begin (ss ++ ss')
  (Begin ss) `mappend` s' = Begin (ss ++ [s'])
  s `mappend` (Begin ss') = Begin (s : ss')
  s `mappend` s' = Begin [s, s']

data Cmd = Cmd String
               [String]
               User
               Cwd
               Bool
               CmdVerbosity
               deriving (Show, Read)

data CmdVerbosity = Debug | Verbose | OnlyStdErr | Quiet
                  deriving (Show, Read, Typeable, Data,Eq)

data Cwd = Cwd FilePath | NoCwd deriving (Show, Read)

data User = User String | NoUser deriving (Show, Read)

data Ctx = Ctx { ctxCwd :: Cwd
               , ctxUser :: User
               , ctxIgnoreErrors :: Bool
               , ctxVerbosity :: CmdVerbosity }

-- | Convert 'script' to bash-shell-script written to 'file' and make 'file'
-- executable.
writeSh :: FilePath -> Script -> IO ()
writeSh file script = do
  writeFile file (toBash $ toCmds script)
  getPermissions file >>= setPermissions file . setOwnerExecutable True

-- | Check if a script has the same effect as 'NoOP'
emptyScript :: Script -> Bool
emptyScript = null . toCmds

toCmds :: Script -> [Cmd]
toCmds s = runReader (toLLC s) (Ctx NoCwd NoUser False Debug)
  where
    toLLC :: Script -> Reader Ctx [Cmd]
    toLLC NoOP = return []
    toLLC (In d cs) = local (\ ctx -> ctx{ctxCwd = Cwd d})
                      (toLLC (Begin cs))
    toLLC (As u cs) = local (\ ctx -> ctx{ctxUser = User u})
                      (toLLC (Begin cs))
    toLLC (IgnoreErrors b cs) = local (\ ctx -> ctx { ctxIgnoreErrors = b })
                                (toLLC (Begin cs))
    toLLC (Verbosity v cs) = local (\ ctx -> ctx { ctxVerbosity = v})
                             (toLLC (Begin cs))
    toLLC (Begin cs) = concat <$> mapM toLLC cs
    toLLC (Run cmd args) = do
      c <- reader ctxCwd
      u <- reader ctxUser
      i <- reader ctxIgnoreErrors
      v <- reader ctxVerbosity
      return [Cmd cmd args u c i v]

toBash :: [Cmd] -> String
toBash cmds =
  intercalate "\n\n" $
  bashHeader ++ (cmdToBash <$> cmds)

bashHeader :: [String]
bashHeader = [ "#!/bin/bash"
             , "set -e" ]

cmdToBash :: Cmd -> String
cmdToBash (Cmd cmd args user cwd ignoreErrors verbosity) =
  intercalate "\n" $ disableErrorChecking
                     ++ pushd cwdQ
                     ++ execCmd
                     ++ popd cwdQ
                     ++ reenableErrorChecking
  where
    execCmd = [ unwords (runuser ++ [cmd] ++ args ++ redirectOutput) ]
      where runuser = case user of
              NoUser -> []
              User "root" -> []
              User u -> ["runuser", "-p", "-u", u, "--"]
    pushd NoCwd = [ ]
    pushd (Cwd cwdPath) = [ unwords (["pushd", cwdPath] ++ redirectOutput) ]
    popd NoCwd = [ ]
    popd (Cwd cwdPath) = [ unwords (["popd"] ++ redirectOutput ++ ["#", cwdPath]) ]
    disableErrorChecking = ["set +e" | ignoreErrors]
    reenableErrorChecking = ["set -e" | ignoreErrors]
    cwdQ = case cwd of
      NoCwd -> NoCwd
      Cwd d -> Cwd ("'" ++ d ++ "'")
    redirectOutput = case verbosity of
      Debug -> []
      Verbose -> []
      OnlyStdErr -> [">", "/dev/null"]
      Quiet -> ["&>", "/dev/null"]

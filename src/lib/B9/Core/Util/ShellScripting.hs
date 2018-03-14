module B9.Core.Util.ShellScripting where

import           Control.Monad.Writer
import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.ShellEscape as S
import           Data.List
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import           Data.Word
import           GHC.TypeLits
import           System.FilePath
import           Text.Printf

-- * High-Level Unix commands

installExecutables :: SourceDir -> [FilePath] -> ScriptBuilder ()
installExecutables (SourceDir srcDir) exes = do
    let binDir = "/usr/bin"
    forM_ exes $
        \exe -> do
            mv ((srcDir </> exe) --> (binDir </> exe))
            chmod (FileModeN 0 7 5 5) (Target (binDir </> exe))

-- * Low-level unix commands

data ShellCmd = ShellCmd

sh :: (CmdBuilder ShellCmd a) => String -> a
sh = toCmdLine ShellCmd buildCmd

instance Cmd ShellCmd where
    type MandatoryArgs ShellCmd a = String -> a
    execFilePath _ = ""
    initialCmdLine ShellCmd = CmdLine "" []
    -- toCmdLine :: c -> (CmdLine c -> a) -> MandatoryArgs c a
    toCmdLine ShellCmd k firstArg = k (CmdLine firstArg [])

instance CmdArg ShellCmd String where
    setArg = appendArg

shArgs :: String -> [String] -> ScriptBuilder ()
shArgs c as = tell (Run (escape c) (map escape as))

pushd :: FilePath -> ScriptBuilder a -> ScriptBuilder a
pushd d nested = do
    let (a, s) = toScript nested
    tell (In d [ s ])
    return a

ignoreErrors :: Bool -> ScriptBuilder a -> ScriptBuilder a
ignoreErrors ignore nested = do
    let (a, s) = toScript nested
    tell (IgnoreErrors ignore [ s ])
    return a

-- * Generate unix commands
newtype ScriptBuilder a = ScriptBuilder { runScriptBuilder :: Writer Script a }
    deriving (Monad, Applicative, Functor, MonadWriter Script)

toScript :: ScriptBuilder a -> (a, Script)
toScript = runWriter . runScriptBuilder

-- * Parameters for most unix commands
data Verbose = Verbose | V | VV | VVV

data ShowProgress = ShowProgress

data SourceDest = SourceDest FilePath FilePath

data DryRun = DryRun

newtype UserName = UserName {fromUserName :: String}
    deriving (Eq, Ord)

newtype GroupName = GroupName String

newtype Password = Password String

infixl 1 -->

(-->) :: FilePath -> FilePath -> SourceDest
(-->) = SourceDest

newtype SourceDir = SourceDir FilePath

newtype Target = Target FilePath

newtype TargetDir = TargetDir FilePath

data Recursive = Recursive

data Force = Force

-- * Unix commands
-- * Generic Unix commands
unixCmd :: (CmdBuilder (UnixCmd k) a, KnownSymbol k) => UnixCmd k -> a
unixCmd cmd = toCmdLine cmd buildCmd

instance CmdArg (UnixCmd a) b =>
         CmdArg (UnixCmd a) [b] where
    setArg bs = appEndo combined
      where
        combined = mconcat setArgs
        setArgs = Endo . setArg <$> bs

data UnixCmd (k :: Symbol) = UnixCmd

instance KnownSymbol k =>
         Show (UnixCmd k) where
    show = symbolVal

instance (KnownSymbol k) =>
         Cmd (UnixCmd k)

instance CmdArg (UnixCmd a) Verbose where
    setArg Verbose = appendArg "--verbose"
    setArg V = appendArg "-v"
    setArg VV = appendArg "-vv"
    setArg VVV = appendArg "-vvv"

-- * @mv@
mv :: CmdBuilder (UnixCmd "mv") a => SourceDest -> a
mv = unixCmd (UnixCmd :: UnixCmd "mv")

instance CmdArg (UnixCmd "mv") SourceDest where
    setArg (SourceDest s d) =
        appendArgs [ quoted s, quoted d ]

instance CmdArg (UnixCmd "mv") Recursive where
    setArg Recursive = prependArg "--recursive"

instance CmdArg (UnixCmd "mv") Force where
    setArg Force = prependArg "--force"

-- * @cp@
cp :: CmdBuilder (UnixCmd "cp") a => SourceDest -> a
cp = unixCmd (UnixCmd :: UnixCmd "cp")

instance CmdArg (UnixCmd "cp") SourceDest where
    setArg (SourceDest s d) =
        appendArgs [ quoted s, quoted d ]

instance CmdArg (UnixCmd "cp") Recursive where
    setArg Recursive = prependArg "--recursive"

instance CmdArg (UnixCmd "cp") Force where
    setArg Force = prependArg "--force"

-- * @rm@
rm :: CmdBuilder (UnixCmd "rm") a => a
rm = unixCmd (UnixCmd :: UnixCmd "rm")

rm'rf :: CmdBuilder (UnixCmd "rm") a => a
rm'rf = rm Recursive Force

instance CmdArg (UnixCmd "rm") Target where
    setArg (Target d) = appendArg $ quoted d

instance CmdArg (UnixCmd "rm") TargetDir where
    setArg (TargetDir d) = setArg (Target d) . setArg Recursive

instance CmdArg (UnixCmd "rm") Recursive where
    setArg Recursive = prependArg "--recursive"

instance CmdArg (UnixCmd "rm") Force where
    setArg Force = prependArg "--force"

-- * @ln@
ln :: CmdBuilder (UnixCmd "ln") a => a
ln = unixCmd (UnixCmd :: UnixCmd "ln")

ln's :: SourceDest -> ScriptBuilder ()
ln's = ln Symbolic

ln'nsf :: SourceDest -> ScriptBuilder ()
ln'nsf = ln Symbolic Force NoDereference

data FileLinkArg = Symbolic | Relative | Logical | NoDereference | NoTargetDir

instance CmdArg (UnixCmd "ln") TargetDir where
    setArg (TargetDir d) = prependArg $ printf "--target-directory='%s'" d

instance CmdArg (UnixCmd "ln") SourceDest where
    setArg (SourceDest s d) =
        appendArgs [ quoted s, quoted d ]

instance CmdArg (UnixCmd "ln") FileLinkArg where
    setArg Symbolic = prependArg "--symbolic"
    setArg Relative = prependArg "--relative"
    setArg Logical = prependArg "--logical"
    setArg NoDereference = prependArg "--no-dereference"
    setArg NoTargetDir = prependArg "--no-target-directory"

instance CmdArg (UnixCmd "ln") Force where
    setArg Force = prependArg "--force"

-- * @mkdir@
mkdir :: CmdBuilder (UnixCmd "mkdir") a => a
mkdir = unixCmd (UnixCmd :: UnixCmd "mkdir")

data CreateParents = CreateParents

instance CmdArg (UnixCmd "mkdir") CreateParents where
    setArg CreateParents = prependArg "--parents"

instance CmdArg (UnixCmd "mkdir") TargetDir where
    setArg (TargetDir d) = appendArg (quoted d)

instance CmdArg (UnixCmd "mkdir") Target where
    setArg (Target d) = appendArg d

-- * Group management
data GroupAdd = GroupAdd

data GroupAddArg = GID Word16
                 | SystemGroup
    deriving ((Show))

instance Cmd GroupAdd where
    execFilePath _ = "groupadd"

instance CmdArg GroupAdd GroupName where
    setArg (GroupName t) = appendArg $ quoted t

instance CmdArg GroupAdd GroupAddArg where
    setArg (GID t) = prependArgs [ "--gid", show t ]
    setArg SystemGroup = prependArg "--system"

groupAdd :: CmdBuilder GroupAdd a => a
groupAdd = toCmdLine GroupAdd buildCmd

gpasswd :: CmdBuilder (UnixCmd "gpasswd") a => GroupName -> a
gpasswd = unixCmd (UnixCmd :: UnixCmd "gpasswd")

data AddUserToGroup = AddUserToGroup String

data DeleteUserFromGroup = DeleteUserFromGroup String

instance CmdArg (UnixCmd "gpasswd") GroupName where
    setArg (GroupName n) = appendArg n

instance CmdArg (UnixCmd "gpasswd") AddUserToGroup where
    setArg (AddUserToGroup n) =
        prependArgs [ "--add", n ]

instance CmdArg (UnixCmd "gpasswd") DeleteUserFromGroup where
    setArg (DeleteUserFromGroup n) =
        prependArgs [ "--del", n ]

-- * User management
data UserAdd = UserAdd

data UserAddArg = HomeDir FilePath
                | CreateHomeDir Bool
                | CreateUserGroup Bool
                | UserID Word16
                | GroupID Word16
                | LoginShell FilePath
                | Groups [String]
                | SkelDir FilePath
                | UserComment String
                | DefaultUserSettings
                | SystemUser
                | AllowNonUnique
                | Inactive
                | SELinuxUser String
    deriving ((Show))

instance Cmd UserAdd where
    execFilePath _ = "useradd"

instance CmdArg UserAdd UserName where
    setArg (UserName t) = appendArg $ quoted t

instance CmdArg UserAdd UserAddArg where
    setArg (HomeDir t) = prependArgs [ "--home-dir", quoted t ]
    setArg (CreateHomeDir True) =
        prependArg "--create-home"
    setArg (CreateHomeDir False) =
        prependArg "-M"
    setArg (CreateUserGroup True) =
        prependArg "--user-group"
    setArg (CreateUserGroup False) =
        prependArg "--no-user-group"
    setArg (UserID t) = prependArgs [ "--uid", show t ]
    setArg (GroupID t) = prependArgs [ "--gid", show t ]
    setArg (LoginShell t) = prependArgs [ "--shell", quoted t ]
    setArg (Groups []) = id
    setArg (Groups gs) = prependArgs [ "--groups", intercalate "," gs ]
    setArg (SkelDir t) = prependArgs [ "--skell", quoted t ]
    setArg (UserComment t) =
        prependArgs [ "--comment", quoted t ]
    setArg (SELinuxUser t) =
        prependArgs [ "--selinux-user", quoted t ]
    setArg DefaultUserSettings =
        prependArg "--defaults"
    setArg SystemUser = prependArg "--system"
    setArg Inactive = prependArg "--inactive"
    setArg AllowNonUnique = prependArg "--non-unique"

userAdd :: CmdBuilder UserAdd a => a
userAdd = toCmdLine UserAdd buildCmd

serviceUserAdd :: CmdBuilder UserAdd a => a
serviceUserAdd = userAdd (CreateUserGroup True)
                         (CreateHomeDir True)
                         (Groups [ "wheel" ])

chpasswd :: UserName -> Password -> ScriptBuilder ()
chpasswd (UserName username) (Password password) =
    sh $ printf "echo '%s:%s' | chpasswd" username password

-- * wget
data WGet = WGet

instance Cmd WGet where
    execFilePath _ = "wget"

data WGetArg = OutputDocument FilePath

data Tries = Tries Word8

data TimeOut = TimeOut Word8

data Url = Url String

instance CmdArg WGet Url where
    setArg (Url url) = appendArg (quoted url)

instance CmdArg WGet WGetArg where
    setArg (OutputDocument f) = prependArg ("--output-document=" ++ quoted f)

instance CmdArg WGet ShowProgress where
    setArg ShowProgress = prependArg "--show-progress"

instance CmdArg WGet Tries where
    setArg (Tries n) = prependArg (printf "--tries=%d" n)

instance CmdArg WGet TimeOut where
    setArg (TimeOut n) = prependArg (printf "--timeout=%d" n)

wget :: CmdBuilder WGet a => a
wget = toCmdLine WGet buildCmd

-- * @unzip@
unzip :: CmdBuilder Unzip a => ZipArchive -> a
unzip = toCmdLine Unzip buildCmd

data Unzip = Unzip

instance Cmd Unzip where
    execFilePath _ = "unzip"
    type MandatoryArgs Unzip r = ZipArchive -> r
    toCmdLine Unzip k f = k $ setArg f $ initialCmdLine Unzip

data ZipArchive = ZipArchive FilePath

instance CmdArg Unzip TargetDir where
    setArg (TargetDir d) = appendArgs [ "-d", quoted d ]

instance CmdArg Unzip ZipArchive where
    setArg (ZipArchive f) = appendArg $ quoted f

-- * @tar@
downloadAndUnpack :: Url -> Word8 -> ScriptBuilder FilePath
downloadAndUnpack (Url url) componentsToStrip = do
    let downloadDir = "/tmp/Downloads" </> filename
        unpackDir = "/tmp/Unpack" </> filename
        downloadFilename = downloadDir </> filename
        filename = takeFileName url
    mkdir CreateParents (TargetDir downloadDir)
    mkdir CreateParents (TargetDir unpackDir)
    wget (Url url) (OutputDocument downloadFilename)
    tar (XVFA downloadFilename)
        (TargetDir unpackDir)
        (StripComponents componentsToStrip)
    rm (Target downloadFilename) Force
    return unpackDir

unpack :: FilePath -> FilePath -> Word8 -> ScriptBuilder ()
unpack archive destDir componentsToStrip = do
    mkdir CreateParents (Q destDir)
    tar (Archive archive)
        (TargetDir destDir)
        (StripComponents componentsToStrip)
        AutoCompress
        Extract

tar :: CmdBuilder Tar a => TarArchive -> a
tar = toCmdLine Tar buildCmd

data Tar = Tar

instance Cmd Tar where
    execFilePath _ = "tar"
    type MandatoryArgs Tar r = TarArchive -> r
    toCmdLine Tar k f = k $ setArg f $ initialCmdLine Tar

data TarArg = Extract
            | Create
            | ListContents
            | KeepOldFiles
            | KeepNewerFiles
            | Overwrite
            | OverwriteDir
            | Verify
            | UnlinkFirst
            | RemoveFiles
            | RecursiveUnlink
            | AutoCompress
            | GZip
            | XZ
            | BZip2
            | StripComponents Word8

data TarArchive = Archive FilePath
                | XVFA FilePath

instance CmdArg Tar TargetDir where
    setArg (TargetDir d) = appendArgs [ "--directory", quoted d ]

instance CmdArg Tar TarArchive where
    setArg (Archive f) = appendArgs [ "--file", quoted f ]
    setArg (XVFA f) = appendArgs [ "xvfa", quoted f ]

instance CmdArg Tar TarArg where
    setArg Extract = appendArg "--extract"
    setArg Create = appendArg "--create"
    setArg ListContents = appendArg "--list"
    setArg KeepOldFiles = appendArg "--keep-old-files"
    setArg KeepNewerFiles = appendArg "--keep-newer-files"
    setArg Overwrite = appendArg "--overwrite"
    setArg OverwriteDir = appendArg "--overwrite-dir"
    setArg Verify = appendArg "--verify"
    setArg UnlinkFirst = appendArg "--unlink-first"
    setArg RemoveFiles = appendArg "--remove-files"
    setArg RecursiveUnlink =
        appendArg "--recursive-unlink"
    setArg AutoCompress = appendArg "--auto-compress"
    setArg GZip = appendArg "--gzip"
    setArg XZ = appendArg "--xz"
    setArg BZip2 = appendArg "--bzip2"
    setArg (StripComponents n) =
        appendArg $ printf "--strip-components=%d" n

-- * chmod
data FileMode = FileMode String
              | FileModeN Word8 Word8 Word8 Word8

data ChMod = ChMod

instance Cmd ChMod where
    execFilePath _ = "chmod"
    type MandatoryArgs ChMod res = FileMode -> Target -> res
    toCmdLine ChMod k mode dest =
        k $ setArg dest $ setArg mode $ initialCmdLine ChMod

instance CmdArg ChMod FileMode where
    setArg (FileMode m) = prependArg m
    setArg (FileModeN s u g o) =
        prependArg (printf "%d%d%d%d" s u g o)

instance CmdArg ChMod Target where
    setArg (Target f) = appendArg (quoted f)

instance CmdArg ChMod Recursive where
    setArg Recursive = prependArg "--recursive"

chmod :: CmdBuilder ChMod a => FileMode -> Target -> a
chmod = toCmdLine ChMod buildCmd

-- * chown
chown :: CmdBuilder ChOwn a => Owner -> Target -> a
chown = toCmdLine ChOwn buildCmd

data Owner = Owner { oUser  :: String
                   , oGroup :: String
                   }
    deriving (Eq, Show)

data ChOwn = ChOwn

instance Cmd ChOwn where
    type MandatoryArgs ChOwn res = Owner -> Target -> res
    execFilePath _ = "chown"
    initialCmdLine ChOwn = CmdLine "chown" []
    toCmdLine ChOwn k o dest =
        k $ setArg dest $ setArg o $ initialCmdLine ChOwn

instance CmdArg ChOwn Owner where
    setArg (Owner u g) = appendArg (u ++ ":" ++ g)

instance CmdArg ChOwn Target where
    setArg (Target f) = appendArg (quoted f)

instance CmdArg ChOwn Recursive where
    setArg Recursive = prependArg "--recursive"

-- * @yum@
yum :: CmdBuilder (UnixCmd "yum") a => a
yum = unixCmd (UnixCmd :: UnixCmd "yum")

data YumCmd = CleanMetaData
            | MakeCache
            | EnableRepo String
            | DisableRepo String

instance CmdArg (UnixCmd "yum") YumCmd where
    setArg CleanMetaData   = prependArgs [ "clean", "metadata", "-y" ]
    setArg MakeCache       = prependArgs ["makecache", "-y"  ]
    setArg (EnableRepo r)  = appendArg ("--enablerepo=" ++ quoted r)
    setArg (DisableRepo r) = appendArg ("--disablerepo=" ++ quoted r)

instance CmdArg (UnixCmd "yum") RpmCmd where
    setArg (Upgrade p) = appendArgs (quoted <$> p) .
        prependArgs [ "-y", "update" ]
    setArg (Install p) = appendArgs (quoted <$> p) .
        prependArgs [ "-y", "install" ]
    setArg (Erase p) = appendArgs (quoted <$> p) .
        prependArgs [ "-y", "remove" ]

-- * @rpm@
rpm :: CmdBuilder (UnixCmd "rpm") a => RpmCmd -> a
rpm = unixCmd (UnixCmd :: UnixCmd "rpm")

data RpmCmd = Upgrade [String]
            | Install [String]
            | Erase [String]

data RpmOpt = AllFiles
            | IgnoreSize
            | IgnoreArch
            | IgnoreOs
            | NoDigest
            | NoSignature
            | NoDeps
            | NoOrder
            | NoScripts
            | NoPre
            | NoPost
            | NoPreUn
            | NoPostUn
            | NoPreTrans
            | NoPostTrans
            | NoTriggers
            | NoInstallationTriggers
            | NoUninstallTrigger
            | NoPreInstTrigger
            | NoPostUninstTrigger
            | OldPackage
            | Percent
            | Prefix FilePath
            | Relocate FilePath FilePath
            | ReplacePackages

instance CmdArg (UnixCmd "rpm") RpmCmd where
    setArg (Upgrade p) = appendArgs (quoted <$> p) . prependArg "-U"
    setArg (Install p) = appendArgs (quoted <$> p) . prependArg "-i"
    setArg (Erase p) = appendArgs (quoted <$> p) . prependArg "-e"

instance CmdArg (UnixCmd "rpm") Force where
    setArg Force = prependArg "--force"

instance CmdArg (UnixCmd "rpm") ShowProgress where
    setArg _ = prependArg "-h"

instance CmdArg (UnixCmd "rpm") DryRun where
    setArg _ = prependArg "--test"

instance CmdArg (UnixCmd "rpm") RpmOpt where
    setArg AllFiles = prependArg "--allfiles"
    setArg IgnoreSize = prependArg "--ignoresize"
    setArg IgnoreArch = prependArg "--ignorearch"
    setArg IgnoreOs = prependArg "--ignoreos"
    setArg NoSignature = prependArg "--nosignature"
    setArg NoDeps = prependArg "--nodeps"
    setArg NoDigest = prependArg "--nodigest"
    setArg NoOrder = prependArg "--noorder"
    setArg NoScripts = prependArg "--noscripts"
    setArg NoPre = prependArg "--nopre"
    setArg NoPost = prependArg "--nopost"
    setArg NoPreUn = prependArg "--nopreun"
    setArg NoPostUn = prependArg "--nopostun"
    setArg NoPreTrans = prependArg "--nopretrans"
    setArg NoPostTrans = prependArg "--noposttrans"
    setArg NoTriggers = prependArg "--notriggers"
    setArg NoInstallationTriggers =
        prependArg "--notriggerin"
    setArg NoUninstallTrigger =
        prependArg "--notriggerun"
    setArg NoPreInstTrigger =
        prependArg "--notriggerprein"
    setArg NoPostUninstTrigger =
        prependArg "--notriggerpostun"
    setArg OldPackage = prependArg "--oldpackage"
    setArg Percent = prependArg "--percent"
    setArg (Prefix x) = prependArgs [ "--prefix", quoted x ]
    setArg (Relocate x y) = prependArgs [ "--relocate", printf "%s=%s" x y ]
    setArg ReplacePackages =
        prependArg "--replacepkgs"

-- * @rsync@
rsync :: CmdBuilder (UnixCmd "rsync") a => SourceDest -> a
rsync = unixCmd (UnixCmd :: UnixCmd "rsync")

data Rule = Merge FilePath
    deriving (Show)

data RsyncOpt = ArchiveMode
              | Filter Rule

instance CmdArg (UnixCmd "rsync") SourceDest where
    setArg (SourceDest s d) =
        appendArgs [ quoted s, quoted d ]

instance CmdArg (UnixCmd "rsync") RsyncOpt where
    setArg ArchiveMode = prependArg "--archive"
    setArg (Filter (Merge f)) =
        prependArgs [ "--filter", dquoted "merge " ++ f ]

-- * Internal machinery
class Cmd c where
    type MandatoryArgs c r
    execFilePath :: c -> FilePath
    initialCmdLine :: c -> CmdLine c
    toCmdLine :: c -> (CmdLine c -> a) -> MandatoryArgs c a
    -- default impl
    type MandatoryArgs c r = r
    default execFilePath :: Show c => c -> FilePath
    execFilePath = show
    default initialCmdLine :: c -> CmdLine c
    initialCmdLine c = CmdLine (execFilePath c) []
    default toCmdLine :: (MandatoryArgs c a ~ a) => c -> (CmdLine c -> a) -> MandatoryArgs c a
    toCmdLine c k = k (initialCmdLine c)

prependArg :: String -> CmdLine c -> CmdLine c
prependArg f c = c { cmdLineArgs = f : cmdLineArgs c }

prependArgs :: [String] -> CmdLine c -> CmdLine c
prependArgs fs c = c { cmdLineArgs = fs ++ cmdLineArgs c }

appendArg :: String -> CmdLine c -> CmdLine c
appendArg f c = c { cmdLineArgs = cmdLineArgs c ++ [ f ] }

appendArgs :: [String] -> CmdLine c -> CmdLine c
appendArgs fs c = c { cmdLineArgs = cmdLineArgs c ++ fs }

data CmdLine cmd = CmdLine { cmdLineCmd  :: String
                           , cmdLineArgs :: [String]
                           }
    deriving (Eq, Show)

class CmdArg cmd arg where
    setArg :: arg -> CmdLine cmd -> CmdLine cmd

data QuotedArg = Q String
               | QQ String

data RawArg = Append String
            | Prepend String

instance CmdArg cmd QuotedArg where
    setArg (Q s) = appendArg (quoted s)
    setArg (QQ s) = appendArg (dquoted s)

instance CmdArg cmd RawArg where
    setArg (Append s) = appendArg s
    setArg (Prepend s) = prependArg s

class CmdBuilder cmd a where
    buildCmd :: CmdLine cmd -> a

instance CmdBuilder cmd (CmdLine cmd) where
    buildCmd = id

instance (a ~ ()) =>
         CmdBuilder cmd (ScriptBuilder a) where
    buildCmd (CmdLine c args) =
        shArgs c args

instance (CmdArg cmd arg, CmdBuilder cmd args) =>
         CmdBuilder cmd (arg -> args) where
    buildCmd cmdArgs arg = buildCmd $ setArg arg cmdArgs

-- | Escape a string so bash will grok it.
escape :: String -> String
escape = T.unpack . E.decodeUtf8 . S.unescape . S.bash . S.pack

-- | Quote a string so bash will not expand it.
quoted :: String -> String
quoted = printf "'%s'"

-- | Double-Quote a string so bash will expand it.
dquoted :: String -> String
dquoted = printf "\"%s\""

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module B9.Content.SystemD where

import B9.Content.PropList
import B9.Common

-- * @Unit@ section

testSdUnit = renderProperties
     (  sdUnit
     <++ (Value "blah" :: Property "Description")
     <++ wants "sdf.service"
     <++ wants "foo.service"
     <++ (reset :: Property "Wants")
     <++ (reset :: Property "Requires")
     <++ wants "sdf.service"
     )

testSdUnit2 = renderProperties
     (  sdUnit
     <++ (Value "blah" :: Property "Description")
     <++ wants "sdf.service"
     <++ wants "foo.service"
     <++ (reset :: Property "Wants")
     <++ (reset :: Property "Requires")
     <++ wants "sdf.service"
     )

mkSection :: SufficientProperties "Unit" _
mkSection = toProperties sdUnit (Value "blah" :: Property "Description") (wants "sdf")

class PropertiesBuilder (section :: k) (required :: [e]) (keys :: [e]) args where
  toProperties :: Properties section required keys -> args

instance PropertiesBuilder section required keys (Properties section required keys) where
  toProperties = id

instance (IsProperty key
         ,CanAddProperty (Cardinality key section) key keys ~ 'True
         ,(Remove key required) ~ required')
          =>
          PropertiesBuilder
              section
              required
              keys
              (Property key -> Properties section required' (key ': keys))
          where
  toProperties propsIn = \prop -> AddProperty prop propsIn

sdUnit :: EmptyProperties "Unit"
sdUnit = Begin

wants :: String -> Property "Wants"
wants = Value . SdSet . SystemDUnit

reset :: (ValueType k ~ SdResetable a, IsProperty k)
      => Property k
reset = Value SdResetPrior

must :: (ValueType k ~ SdMightFail a, IsProperty k)
      => a -> Property k
must = Value . SdMustSucceed

forgive :: (ValueType k ~ SdMightFail a, IsProperty k)
        => a -> Property k
forgive = Value . SdMayFail

instance IsProperty "Requires2" where
  type Cardinality "Requires2" "Unit" = 'ZeroOrMore
  type ValueType   "Requires2"        = SdResetable (SdList SystemDUnit)

type instance RequiredKeys "Unit" = '["Description"]

instance IsProperty "Description" where
  type Cardinality "Description" "Unit" = 'AtMostOnce

instance IsProperty "Documentation" where
  type Cardinality "Documentation" "Unit" = 'AtMostOnce

instance IsProperty "Requires" where
  type Cardinality "Requires" "Unit" = 'ZeroOrMore
  type ValueType   "Requires"        = SdResetable SystemDUnit

instance IsProperty "Requisite" where
  type Cardinality "Requisite" "Unit" = 'ZeroOrMore
  type ValueType   "Requisite"        = SdResetable SystemDUnit

instance IsProperty "Wants" where
  type Cardinality "Wants" "Unit" = 'ZeroOrMore
  type ValueType   "Wants"        = SdResetable SystemDUnit

instance IsProperty "BindsTo" where
  type Cardinality "BindsTo" "Unit" = 'ZeroOrMore
  type ValueType   "BindsTo"        = SdResetable SystemDUnit

instance IsProperty "PartOf" where
  type Cardinality "PartOf" "Unit" = 'ZeroOrMore
  type ValueType   "PartOf"        = SdResetable SystemDUnit

instance IsProperty "Conflicts" where
  type Cardinality "Conflicts" "Unit" = 'ZeroOrMore
  type ValueType   "Conflicts"        = SdResetable SystemDUnit

instance IsProperty "Before" where
  type Cardinality "Before" "Unit" = 'ZeroOrMore
  type ValueType   "Before"        = SdResetable SystemDUnit

instance IsProperty "After" where
  type Cardinality "After" "Unit" = 'ZeroOrMore
  type ValueType   "After"        = SdResetable SystemDUnit

instance IsProperty "OnFailure" where
  type Cardinality "OnFailure" "Unit" = 'ZeroOrMore
  type ValueType   "OnFailure"        = SdResetable SystemDUnit

instance IsProperty "PropagatesReloadTo" where
  type Cardinality "PropagatesReloadTo" "Unit" = 'ZeroOrMore
  type ValueType   "PropagatesReloadTo"        = SdResetable SystemDUnit

instance IsProperty "PropagatesReloadFrom" where
  type Cardinality "PropagatesReloadFrom" "Unit" = 'ZeroOrMore
  type ValueType   "PropagatesReloadFrom"        = SdResetable SystemDUnit

instance IsProperty "JoinsNamespaceOf" where
  type Cardinality "JoinsNamespaceOf" "Unit" = 'ZeroOrMore
  type ValueType   "JoinsNamespaceOf"        = SdResetable SystemDUnit

instance IsProperty "RequiresMountFor" where
  type Cardinality "RequiresMountFor" "Unit" = 'ZeroOrMore
  type ValueType   "RequiresMountFor"        = [FilePath]


-- data SdService where
--         SdSimple :: (SdServiceOptions Sd1 Sd0) -> SdService
--         SdForking :: (SdServiceOptions Sd1 Sd0) -> SdService
--         SdOneShot :: (SdServiceOptions Sd0_n Sd0) -> SdService
--         SdDBus :: (SdServiceOptions Sd1 Sd1) -> SdService
--         SdNotify :: (SdServiceOptions Sd1 Sd0) -> SdService
--         SdIdle :: (SdServiceOptions Sd1 Sd0) -> SdService

-- | Create a systemd unit that executes a given 'Script'.
data SdServiceOptions execStartF busNameF =
  SdServiceOptions {sdServiceExecStartPre :: [SdCommand]
                   ,sdServiceExecStart :: execStartF SdCommand
                   ,sdServiceExecStartPost :: [SdCommand]
                   ,sdServiceExecStop :: [SdCommand]
                   ,sdServiceExecStopPost :: [SdCommand]
                   ,sdServiceRemainAfterExit :: Maybe Bool
                   ,sdServiceGuessMainPID :: Maybe Bool
                   ,sdServicePIDFile :: Maybe FilePath
                   ,sdServiceBusName :: busNameF SdBusName
                   ,sdServiceRestartSec :: Maybe Word32
                   ,sdServiceTimeoutStartSec :: Maybe Word32
                   ,sdServiceTimeoutStopSec :: Maybe Word32
                   ,sdServiceRuntimeMaxSec :: Maybe Word32
                   ,sdServiceWatchdogSec :: Maybe Word32
                   ,dsServiceRestart :: Maybe SystemDServiceRestart
                   ,dsServiceSuccessExitStatus :: [SystemDServiceExitStatus]
                   ,dsServiceRestartPreventExitStatus :: [SystemDServiceExitStatus]
                   ,dsServiceRestartForceExitStatus :: [SystemDServiceExitStatus]
                   ,dsServicePermissionsStartOnly :: Maybe Bool
                   ,dsServiceRootDirectoryStartOnly :: Maybe Bool
                   ,dsServiceNonBlocking :: Maybe Bool
                   ,dsServiceNotifyAccess :: Maybe SystemDServiceNotifyAccess
                   ,dsServiceSockets :: [SystemDUnit]
                   ,dsServiceFailureAction :: Maybe SystemDFailureAction
                   ,dsServiceFileDescriptorStoreMax :: Maybe Word32
                   ,dsServiceUSBFunctionDescriptors :: Maybe FilePath
                   ,dsServiceUSBFunctionStrings :: Maybe FilePath
                   ,dsServiceExecConfiguration :: Maybe SystemDExec}

sdServiceOptions
  :: f SdCommand -> g SdBusName -> SdServiceOptions f g
sdServiceOptions execStart busName =
  SdServiceOptions {sdServiceExecStartPre = def
                   ,sdServiceExecStart = execStart
                   ,sdServiceExecStartPost = def
                   ,sdServiceExecStop = def
                   ,sdServiceExecStopPost = def
                   ,sdServiceRemainAfterExit = Nothing
                   ,sdServiceGuessMainPID = Nothing
                   ,sdServicePIDFile = Nothing
                   ,sdServiceBusName = busName
                   ,sdServiceRestartSec = Nothing
                   ,sdServiceTimeoutStartSec = Nothing
                   ,sdServiceTimeoutStopSec = Nothing
                   ,sdServiceRuntimeMaxSec = Nothing
                   ,sdServiceWatchdogSec = Nothing
                   ,dsServiceRestart = Nothing
                   ,dsServiceSuccessExitStatus = []
                   ,dsServiceRestartPreventExitStatus = []
                   ,dsServiceRestartForceExitStatus = []
                   ,dsServicePermissionsStartOnly = Nothing
                   ,dsServiceRootDirectoryStartOnly = Nothing
                   ,dsServiceNonBlocking = Nothing
                   ,dsServiceNotifyAccess = Nothing
                   ,dsServiceSockets = []
                   ,dsServiceFailureAction = Nothing
                   ,dsServiceFileDescriptorStoreMax = Nothing
                   ,dsServiceUSBFunctionDescriptors = Nothing
                   ,dsServiceUSBFunctionStrings = Nothing
                   ,dsServiceExecConfiguration = Nothing}

data SystemDServiceRestart
  = SdAlways
  | SdOnSuccess
  | SdOnFailure
  | SdOnAbnormal
  | SdOnAbort
  | SdOnWatchdog

data SystemDServiceExitStatus
  = SdExitCode Int
  | SdExitSignal SdSignal

data SdSignal
  = SdSIGTERM
  | SdSIGINT
  | SdSIGQUIT
  | SdSIGKILL
  | SdSIGHUP
  | SdSIGFPE
  | SdSIGILL
  | SdSIGSEGV
  | SdSIGABRT
  | SdSIGIOT
  | SdSIGTRAP
  | SdSIGEMT
  | SdSIGSYS
  | SdSIGPIPE
  | SdSIGLOST
  | SdSIGXCPU
  | SdSIGXFSZ

newtype SdCommand =
  SdCommand (SdResetable (SdMightFail (FilePath,[String])))

newtype SdBusName =
  SdBusName String

data SystemDServiceNotifyAccess
  = SdNotifyAccessNone
  | SdNotifyAccessMain
  | SdNotifyAccessAll

data SystemDFailureAction
  = SdFailureActionReboot
  | SdFailureActionRebootForce
  | SdFailureActionRebootImmediate
  | SdFailureActionPoweroff
  | SdFailureActionPoweroffForce
  | SdFailureActionPoweroffImmediate

data SystemDExec =
  SystemDExec
              {sdExecWorkingDirectory :: Maybe FilePath
              ,sdExecRootDirectory :: Maybe FilePath
              ,sdExecUser :: Maybe String
              ,sdExecGroup :: Maybe String
              ,sdExecSupplementaryGroups :: [String]
              ,sdExecNice :: Maybe Int
              ,sdExecOOMScoreAdjust :: Maybe Int
              ,sdExecIOSchedulingClass :: Maybe SdIOSchedulingClass
              ,sdExecIOSchedulingPriority :: Maybe SdIOSchedulingPriority
              ,sdExecCPUSchedulingPolicy :: Maybe SdCPUSchedulingPolicy
              ,sdExecCPUSchedulingPriority :: Maybe Word8
              ,sdExecCPUSchedulingResetOnFork :: Maybe Bool
              ,sdExecCPUAffinity :: [Word32]
              ,sdExecUMask :: Maybe (Word8,Word8,Word8)
              ,sdExecEnvironment :: [SdEnvironmentVar]
              ,sdExecEnvironmentFile :: [SdEnvironmentFile]
              ,sdExecPassEnvironment :: [SdPassEnvironment]
              ,sdExecStandardInput :: Maybe SdExecStandardInput
              ,sdExecStandardOutput :: Maybe SdExecStandardOutput
              ,sdExecStandardError :: Maybe SdExecStandardOutput
              ,sdExecTTYPath :: Maybe FilePath
              ,sdExecTTYReset :: Maybe Bool
              ,sdExecTTYDisallocate :: Maybe Bool
              ,sdExecSyslogIdentifier :: Maybe String
              ,sdExecSyslogFacility :: Maybe SdSyslogFacility
              ,sdExecSyslogLevel :: Maybe SdSyslogLevel
              ,sdExecSyslogLevelPrefix :: Maybe Bool
              ,sdExecTimerSlackNSec :: Maybe Word64
              ,sdExecLimitCPU :: Maybe Word32
              ,sdExecLimitFSIZE :: Maybe Word32
              ,sdExecLimitDATA :: Maybe Word32
              ,sdExecLimitSTACK :: Maybe Word32
              ,sdExecLimitCORE :: Maybe Word32
              ,sdExecLimitRSS :: Maybe Word32
              ,sdExecLimitNOFILE :: Maybe Word32
              ,sdExecLimitAS :: Maybe Word32
              ,sdExecLimitNPROC :: Maybe Word32
              ,sdExecLimitMEMLOCK :: Maybe Word32
              ,sdExecLimitLOCKS :: Maybe Word32
              ,sdExecLimitSIGPENDING :: Maybe Word32
              ,sdExecLimitMSGQUEUE :: Maybe Word32
              ,sdExecLimitNICE :: Maybe Int
              ,sdExecLimitRTPRIO :: Maybe Word8
              ,sdExecLimitRTTIME :: Maybe Word32
              ,sdExecPAMName :: Maybe String
              ,sdExecCapabilityBoundingSet :: [SdCapabilitySet]
              ,sdExecAmbientCapabilities :: [SdCapabilitySet]
              ,sdExecSecureBits :: [SdSecureBits]
              ,sdExecReadWriteDirectories :: [FilePath]
              ,sdExecReadOnlyDirectories :: [FilePath]
              ,sdExecReadInaccessibleDirectories :: [FilePath]
              ,sdExecPrivateTmp :: Maybe Bool
              ,sdExecPrivateDevices :: Maybe Bool
              ,sdExecPrivateNetwork :: Maybe Bool
              ,sdExecProtectSystem :: Maybe SdProtectSystem
              ,sdExecProtectHome :: Maybe SdProtectHome
              ,sdExecMountFlags :: Maybe SdMountPropagation
              ,sdExecUtmpIdentifier :: Maybe (Char,Char,Char,Char)
              ,sdExecUtmpMode :: Maybe SdUtmpMode
              ,sdExecSELinuxContext :: Maybe (SdMightFail String)
              ,sdExecAppArmorProfile :: Maybe (SdMightFail String)
              ,sdExecSmackProcessLabel :: Maybe (SdMightFail String)
              ,sdExecIgnoreSIGPIPE :: Maybe Bool
              ,sdExecNoNewPrivileges :: Maybe Bool
              ,sdExecSystemCallFilter :: [SdSystemCallFilter]
              ,sdExecSystemCallErrorNumber :: Maybe String
              ,sdExecSystemCallArchitectures :: [SdSystemCallArchitecture]
              ,sdExecRestrictAddressFamilies :: Maybe (SdBWList SdAddressFamily)
              ,sdExecPersonality :: Maybe SdPersonality
              ,sdRuntimeDirectory :: [FilePath]
              ,sdRuntimeMode :: Maybe (Word8,Word8,Word8,Word8)}

data SdIOSchedulingClass
  = SdIOSchedulingClassNone
  | SdIOSchedulingClassRealtime
  | SdIOSchedulingClassBestEffort
  | SdIOSchedulingClassIdle

data SdIOSchedulingPriority
  = SdIOSchedulingPriority0
  | SdIOSchedulingPriority1
  | SdIOSchedulingPriority2
  | SdIOSchedulingPriority3
  | SdIOSchedulingPriority4
  | SdIOSchedulingPriority5
  | SdIOSchedulingPriority6
  | SdIOSchedulingPriority7

data SdCPUSchedulingPolicy
  = CPUSchedulingPolicyOther
  | CPUSchedulingPolicyBatch
  | CPUSchedulingPolicyFifo
  | CPUSchedulingPolicyRR

newtype SdEnvironmentVar =
  SdEnvironmentVar (SdResetable [(String,Maybe String)])

newtype SdEnvironmentFile =
  SdEnvironmentFile (SdResetable (SdMightFail FilePath))

newtype SdPassEnvironment =
  SdPassEnvironment (SdResetable String)

data SdExecStandardInput
  = SdExecStandardInputNull
  | SdExecStandardInputTty
  | SdExecStandardInputTtyForce
  | SdExecStandardInputTtyFail
  | SdExecStandardInputSocket

data SdExecStandardOutput
  = SdExecStandardOutputInherit
  | SdExecStandardOutputNull
  | SdExecStandardOutputTty
  | SdExecStandardOutputJournal
  | SdExecStandardOutputSyslog
  | SdExecStandardOutputKmsg
  | SdExecStandardOutputJournalAndConsole
  | SdExecStandardOutputSyslogAndConsole
  | SdExecStandardOutputKmsgAndConsole
  | SdExecStandardOutputSocket

data SdSyslogFacility
  = SdSyslogFacilityKern
  | SdSyslogFacilityUser
  | SdSyslogFacilityMail
  | SdSyslogFacilityDaemon
  | SdSyslogFacilityAuth
  | SdSyslogFacilitySyslog
  | SdSyslogFacilityLpr
  | SdSyslogFacilityNews
  | SdSyslogFacilityUucp
  | SdSyslogFacilityCron
  | SdSyslogFacilityAuthpriv
  | SdSyslogFacilityFtp
  | SdSyslogFacilityLocal0
  | SdSyslogFacilityLocal1
  | SdSyslogFacilityLocal2
  | SdSyslogFacilityLocal3
  | SdSyslogFacilityLocal4
  | SdSyslogFacilityLocal5
  | SdSyslogFacilityLocal6
  | SdSyslogFacilityLocal7

data SdSyslogLevel
  = SdSyslogLevelEmerg
  | SdSyslogLevelAlert
  | SdSyslogLevelCrit
  | SdSyslogLevelErr
  | SdSyslogLevelWarning
  | SdSyslogLevelNotice
  | SdSyslogLevelInfo
  | SdSyslogLevelDebug

newtype SdCapabilitySet =
  SdCapabilitySet (SdBWList String)

data SdSecureBits
  = SdSecureBitsReset
  | SdSecureBitKeepCaps
  | SdSecureBitKeepCapsLocked
  | SdSecureBitNoSetuidFixup
  | SdSecureBitNoSetuidFixupLocked
  | SdSecureBitNoroot
  | SdSecureBitNorootLocked

data SdProtectSystem
  = SdProtectSystemFull
  | SdProtectSystemYes
  | SdProtectSystemNo

data SdProtectHome
  = SdProtectHomeReadOnly
  | SdProtectHomeYes
  | SdProtectHomeNo

data SdMountPropagation
  = SdMountPropagationShared
  | SdMountPropagationSlave
  | SdMountPropagationPrivate

data SdUtmpMode
  = SdUtmpModeInit
  | SdUtmpModeLogin
  | SdUtmpModeUser

newtype SdSystemCallFilter =
  SdSystemCallFilter (SdBWList String)

data SdSystemCallArchitecture
  = SdSystemCallArchitecture_X86
  | SdSystemCallArchitecture_X86_64
  | SdSystemCallArchitecture_X32
  | SdSystemCallArchitecture_Arm
  | SdSystemCallArchitecture_Native

data SdPersonality
  = SdPersonality_X86
  | SdPersonality_X86_64
  | SdPersonality_Ppc
  | SdPersonality_PpcLe
  | SdPersonality_Ppc64
  | SdPersonality_S390
  | SdPersonality_S390X

data SdAddressFamily
  = SdAF_UNIX
  | SdAF_INET
  | SdAF_INET6

data SystemDUnit = SystemDUnit String

instance Show SystemDUnit where
  show (SystemDUnit u) = u

-- * Parameterized data types

data SdNegatable a
  = SdNot a
  | SdId a

instance Show a => Show (SdNegatable a) where
  show (SdNot a) = "!" ++ show a
  show (SdId a)  =        show a

data SdMightFail a
  = SdMayFail a
  | SdMustSucceed a

instance Show a => Show (SdMightFail a) where
  show (SdMayFail a)     = "-" ++ show a
  show (SdMustSucceed a) =        show a

data SdResetable a
  = SdResetPrior
  | SdSet a

instance Show a => Show (SdResetable a) where
  show (SdSet a)    = show a
  show SdResetPrior = ""

newtype SdList a = SdList [a] deriving (Functor, Applicative)

instance Show a => Show (SdList a) where
  show (SdList as) = unwords (show <$> as)

data SdBWList a
  = SdBlackListAll
  | SdWhiteListAll
  | SdBlackList (SdList a)
  | SdWhiteList (SdList a)

instance Show a => Show (SdBWList a) where
  show (SdBlackList a) = "~" ++ show a
  show (SdWhiteList a) =        show a
  show SdBlackListAll  = "~"
  show SdWhiteListAll  = ""


data SdUnit =
  SdUnit { sdUnitRequires :: [SystemDUnit]
         , sdUnitRequisite :: [SystemDUnit]
         , sdUnitWants :: [SystemDUnit]
         , sdUnitBindsTo :: [SystemDUnit]
         , sdUnitPartOf :: [SystemDUnit]
         , sdUnitConflicts :: [SystemDUnit]
         , sdUnitBefore :: [SystemDUnit]
         , sdUnitAfter :: [SystemDUnit]
         , sdUnitOnFailure :: [SystemDUnit]
         , sdUnitPropagatesReloadTo :: [SystemDUnit]
         , sdUnitPropagatesReloadFrom :: [SystemDUnit]
         , sdUnitJoinsNamespaceOf :: [SystemDUnit]
         }

data SystemDUnit = SystemDUnit String

-- | Create a systemd unit that executes a given 'Script'.
data SdService =
  SdService {sdServiceType :: SystemDServiceType
            ,sdServiceExecStartPre :: [SdCommand]
            ,sdServiceExecStartPost :: [SdCommand]
            ,sdServiceExecStop :: [SdCommand]
            ,sdServiceExecStopPost :: [SdCommand]
            ,sdServiceRemainAfterExit :: Maybe Bool
            ,sdServiceGuessMainPID :: Maybe Bool
            ,sdServicePIDFile :: Maybe FilePath
            ,sdServiceBusName :: Maybe String
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

instance Default SdService where
  def =
    SdService {sdServiceType = SdOneShot []
              ,sdServiceExecStartPre = []
              ,sdServiceExecStartPost = []
              ,sdServiceExecStop = []
              ,sdServiceExecStopPost = []
              ,sdServiceRemainAfterExit = Nothing
              ,sdServiceGuessMainPID = Nothing
              ,sdServicePIDFile = Nothing
              ,sdServiceBusName = Nothing
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

data SystemDServiceType
  = SdSimple SystemDServiceExec
  | SdForking SystemDServiceExec
  | SdOneShot [SystemDServiceExec]
  | SdDBus SystemDServiceExec
  | SdNotify SystemDServiceExec
  | SdIdle SystemDServiceExec

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
  SdCommand (SdResetable (SdOptional (FilePath,[String])))

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
  SystemDExec {sdExecWorkingDirectory :: Maybe FilePath
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
              ,sdExecLimitNICE :: Maybe Int8
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
              ,sdExecSELinuxContext :: Maybe (SdOptional String)
              ,sdExecAppArmorProfile :: Maybe (SdOptional String)
              ,sdExecSmackProcessLabel :: Maybe (SdOptional String)
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
  SdEnvironmentFile (SdResetable (SdOptional FilePath))

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

-- * Parameterized data types
data SdOptional a
  = SdOptional a
  | SdRequired a

data SdResetable a
  = SdResetPrior
  | SdSet a

data SdBWList a
  = SdBlackListAll
  | SdWhiteListAll
  | SdBlackList [a]
  | SdWhiteList [a]

{-# LANGUAGE GADTs #-}
module B9.Builder ( module B9.B9Monad
                  , module B9.ConfigUtils
                  , module B9.B9Config
                  , module B9.ExecEnv
                  , module B9.DiskImages
                  , module B9.DiskImageBuilder
                  , module B9.ShellScript
                  , module B9.Repository
                  , module B9.RepositoryIO
                  , module B9.ArtifactGenerator
                  , module B9.ArtifactGeneratorImpl
                  , module B9.Vm
                  , module B9.VmBuilder
                  , module B9.ErlTerms
                  , module B9.PropLists
                  , module B9.ConcatableSyntax
                  , buildArtifacts
                  ) where

import Data.Monoid
import Text.Printf ( printf )
import Text.Show.Pretty (ppShow)

import B9.B9Monad
import B9.ConfigUtils
import B9.B9Config
import B9.ExecEnv
import B9.DiskImages
import B9.DiskImageBuilder
import B9.ShellScript
import B9.Repository
import B9.RepositoryIO
import B9.ArtifactGenerator
import B9.ArtifactGeneratorImpl
import B9.Vm
import B9.VmBuilder
import B9.ErlTerms
import B9.PropLists
import B9.ConcatableSyntax

buildArtifacts :: ArtifactGenerator -> ConfigParser -> B9Config -> IO Bool
buildArtifacts artifactGenerator cfgParser cliCfg =
  withB9Config cfgParser cliCfg $ \cfg ->
    run cfgParser cfg $ do
      infoL "BUILDING ARTIFACTS"
      getConfig >>= traceL . printf "USING BUILD CONFIGURATION: %v" . ppShow
      assemble artifactGenerator
      return True

withB9Config :: ConfigParser
             -> B9Config
             -> (B9Config -> IO Bool)
             -> IO Bool
withB9Config cfgParser cliCfg f = do
  let parsedCfg' = parseB9Config cfgParser
  case parsedCfg' of
    Left e -> do
      putStrLn (printf "B9 Failed to start: %s" e)
      return False
    Right parsedCfg ->
      let cfg = defaultB9Config <> parsedCfg <> cliCfg
          in f cfg


xxx :: ArtifactGenerator
xxx =
  Each
   [("domain", ["tec.lbaum.eu"
               ,"test.meetyoo.de"])
   ,("altdomain", ["tec.lbaum.eu"
                  ,"testyoo.de"])
   ,("ip_prefix", ["10.1.40"
                  ,"192.168.111"])
   ,("ip_prefix_erl", ["10,1,40"
                      ,"192,168,111"])
   ,("chassis_config", ["COMMON/chassis/switchraumOG4-motorola"
                       ,"COMMON/chassis/meetyoo-test"])
   ,("sipstack_config", ["COMMON/sipstack/lbm-QSC"
                        ,"COMMON/sipstack/meetyoo-test"])
   ,("outbound_proxies_config", ["COMMON/outbound_proxies/lbm-QSC"
                                ,"COMMON/outbound_proxies/meetyoo-test"])
   ]
  [Let
   [("dns","ns1.${domain}")
   ,("dns_ip", "${ip_prefix}.10")
   ,("gw", "lb-gw1.${domain}")
   ,("gw_ip","${ip_prefix}.254")
   ,("syslog","lb-log1.${domain}")
   ,("syslog_ip", "${ip_prefix}.110")
   ,("mail", "mail.${altdomain}")
   ,("mail_ip", "${ip_prefix}.11")
   ,("file_server", "lb-ds1.${domain}")
   ,("file_server_ip", "${ip_prefix}.111")
   ,("tts", "lb-tts1.${domain}")
   ,("tts_ip", "${ip_prefix}.102")
   ,("mrfc", "lb-mrfc1.${domain}")
   ,("mrfc_ip", "${ip_prefix}.103")
   ,("sipproxy", "lb-sipproxy1.${domain}")
   ,("db", "lb-db1.${domain}")
   ,("db_ip", "${ip_prefix}.101")
   ,("app", "lb-app1.${domain}")
   ,("app_ip", "${ip_prefix}.100")
   ,("freeswitch", "lb-fs1.${domain}")
   ,("freeswitch_ip", "${ip_prefix}.113")
   ,("mp1", "lb-mp1.${domain}")
   ,("mp1_ip", "${ip_prefix}.112")
   ,("mrfp1", "lb-mrfp1.${domain}")
   ,("mrfp1_ip", "${ip_prefix}.106")
   ,("display1", "lb-dp1.${domain}")
   ,("display1_ip", "${ip_prefix}.105")
   ,("rtpproxy1", "lb-rp1.${domain}")
   ,("rtpproxy1_ip", "${ip_prefix}.120")]
   [Sources [YamlLiteral
               "user-data"
               (YamlDict
                 [("write_files"
                  ,YamlEmbedString
                     (ErlangTerms
                        "runtime.config"
                        [Template "${chassis_config}", Template "${sipstack_config}"]))])
            ]
            [Artifact (IID "$hostname")
                      (CloudInit [CI_DIR,CI_ISO] "${out_dir}/mrfp/cloud-init/${instance_id}")
            ]
   ]]

{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "b9"; version = "3.2.3"; };
      license = "MIT";
      copyright = "2015, 2016, 2017, 2018, 2019, 2020 Sven Heyll <svh@posteo.de>";
      maintainer = "svh@posteo.de";
      author = "Sven Heyll <svh@posteo.de>";
      homepage = "https://github.com/sheyll/b9-vm-image-builder";
      url = "";
      synopsis = "A tool and library for building virtual machine images.";
      description = "Build virtual machine images for vm-deployments; resize,\nun-partition, create from scratch or convert disk image\nfiles in a variety of formats; assemble and generate all\nassociated files from templates and regular files.\nVM images can further be modifed through scripts, which are\nexecuted in LXC containers into which the vm-images as well\nas arbitrary directories from the host are mounted.\nAll assembled files can also be accessed by vm build\nscripts through a special directory mounted in the build\ncontainer, and/or can be written to directories, ISO- or\nVFAT-images.\nThe ISO/VFAT images that B9 creates are compatible to\n'cloud-init's 'NoCloud' data source;\nB9 is also very well suited for compiling in a\ncontainerized environment. For these applications, the\nimages can be marked as 'Transient' to indicate no further\ninterest in the VM-image itself, and B9 will discard them\nafter the build.\nB9 will never over-write source files, not even large\nvm-image files - there is no intended way to modify a\nsource vm-image file 'in-place'.\nB9 operates in random build directories, which are\ndiscarded when the build exists.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        ".gitignore"
        "CONTRIBUTORS"
        "CONTRIBUTING.md"
        "CODE_OF_CONDUCT.md"
        "CHANGELOG.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."ConfigFile" or (errorHandler.buildDepError "ConfigFile"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."posix-pty" or (errorHandler.buildDepError "posix-pty"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template" or (errorHandler.buildDepError "template"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          ];
        buildable = true;
        modules = [
          "Paths_b9"
          "B9"
          "B9/Artifact"
          "B9/Artifact/Content"
          "B9/Artifact/Content/AST"
          "B9/Artifact/Content/CloudConfigYaml"
          "B9/Artifact/Content/ErlTerms"
          "B9/Artifact/Content/ErlangPropList"
          "B9/Artifact/Content/Readable"
          "B9/Artifact/Content/StringTemplate"
          "B9/Artifact/Content/YamlObject"
          "B9/Artifact/Readable"
          "B9/Artifact/Readable/Interpreter"
          "B9/Artifact/Readable/Source"
          "B9/B9Config"
          "B9/B9Config/Container"
          "B9/B9Config/Docker"
          "B9/B9Config/LibVirtLXC"
          "B9/B9Config/Podman"
          "B9/B9Config/Repository"
          "B9/B9Config/SystemdNspawn"
          "B9/B9Error"
          "B9/B9Exec"
          "B9/B9Logging"
          "B9/B9Monad"
          "B9/BuildInfo"
          "B9/Container"
          "B9/DiskImageBuilder"
          "B9/DiskImages"
          "B9/Docker"
          "B9/Environment"
          "B9/ExecEnv"
          "B9/LibVirtLXC"
          "B9/MBR"
          "B9/PartitionTable"
          "B9/Podman"
          "B9/QCUtil"
          "B9/Repository"
          "B9/RepositoryIO"
          "B9/Shake"
          "B9/Shake/Actions"
          "B9/Shake/SharedImageRules"
          "B9/ShellScript"
          "B9/SystemdNspawn"
          "B9/Text"
          "B9/Vm"
          "B9/VmBuilder"
          "Data/ConfigFile/B9Extras"
          "System/IO/B9Extras"
          ];
        hsSourceDirs = [ "src/lib" ];
        };
      exes = {
        "b9c" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."b9" or (errorHandler.buildDepError "b9"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
            ];
          buildable = true;
          modules = [ "Paths_b9" ];
          hsSourceDirs = [ "src/cli" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extensible-effects" or (errorHandler.buildDepError "extensible-effects"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."neat-interpolation" or (errorHandler.buildDepError "neat-interpolation"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."b9" or (errorHandler.buildDepError "b9"))
            ];
          buildable = true;
          modules = [
            "B9/ArtifactGeneratorImplSpec"
            "B9/B9ConfigSpec"
            "B9/B9ExecSpec"
            "B9/Content/ErlTermsSpec"
            "B9/Content/ErlangPropListSpec"
            "B9/Content/YamlObjectSpec"
            "B9/DiskImagesSpec"
            "B9/DiskImageBuilderSpec"
            "B9/EnvironmentSpec"
            "B9/RepositoryIOSpec"
            "B9/RepositorySpec"
            "B9/Shake/SharedImageRulesSpec"
            "Paths_b9"
            ];
          hsSourceDirs = [ "src/tests" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }
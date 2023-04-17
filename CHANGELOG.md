# Changelog for B9

## 4.0.0

* Rewrite the ArtifactAssembly `VmImagesPostFix` to have a `Script` argument as
  _postfix_ argument and change the behavior such that the script is executed
  on the host machine instead of in a container.
  This allows the script to be used to fix for example a boot loader installed
  in a vm-image (in the VBR) after a resize operation.
  The script is called with the paths to all raw vm image files in the isolated
  build directory after the main build script was executed and the images were
  resized to their output size.


## 3.3.0

* Add new ArtifactAssembly `VmImagesPostFix` that executes a given script **after**
  the actual build has finished and the build images were resized to the size of the
  destination images.
  The motivation is to allow reinstalling a VBR boot loader, i.e. EXTLINUX, after
  the root image was resized and therefore the _geometry_ of the root image was
  altered, leading to spurious `boot error`s.

## 3.2.4.1

Fix the previous release: Actually expose:

* `b9cOsRuntimeDeps`
* `b9cRuntimeDeps`

in `lib.${system}`

## 3.2.4

* Improve the `b9c` wrapper to prefer the OS provided `$PATH` entries for:
  * `libvirt`
  * `systemd`
  * `rsync`
  * `docker`
  * `podman`
* Expose the deps used by the wrapper in the overlay:
  * `b9cOsRuntimeDeps`
  * `b9cRuntimeDeps`
* `b9cRuntimeDeps` is no longer a function, but merely a list
* Remove obsolet nix files
* Remove stack support
* Use `flake-compat` for `shell.nix`
* Expose the `haskell.nix` derivation in `overlay.nix`
* Do not depend on `niv` anymore
* Add a `materializationUpdater` to the flake

## 3.2.3

* Provide nix flake

## 3.2.2

* Relax version dependencies

## 3.2.1

* Use with-utf8 package to get around `invalid argument (invalid character)` kind of
  problems.

## 3.2.0

* Workarounds for libvirtd path length restrictions;
  Libvirt doesn't like paths longer than 63
* Add config file option `image_file_names_shortener_base_path`

## 3.1.0

* Introduced `getVirtualSizeForRawImage`, `cmdStdout`, `hostCmdStdoutEither`, `HostCommandStdout`.
* More stable parsing of qemu-img info output in `getVirtualSizeForRawImage`.
* Fixes a bug were size files contain garbage if newer versions of qemu-img are used.

## 3.0.0

* Run the libvirt_lxc builder in an artificial pseudo-terminal if b9c is run
  outside a terminal.
* Relax version constraints for `hashable` dependency

### Breaking Changes

* Change B9 configuration API: Change the field to a list

## 2.1.0

* Add a new config parameter for the default Ext4 file system attributes.
* `Ext4_64` is deprecated from now on, please use `Ext4` and configure the
  `ext4_attributes: ["64bit"]`.

## 2.0.0

### Breaking Changes

* Specify upper version bounds for all project dependencies
* Move the `interactive` flag from the `B9Config` to the `BuildInfo`
  * `isInteractive` now requires the `BuildInfoReader` effect
  * Add the `runB9Interactive` function, that can by used to
    make the `bsIsInteractive` field in `BuildInfo` `True`
  * Add the `cmdInteractive` function, that executes a command
    with inherited stdin/stdout if `runB9Interactive` was called.
  * Remove the interactivity functionality from the `cmd` function

### Minor Changes

* Fix bug in the parsing of `console` key in the `systemd-nspawn`
  configuration section: When a user specifies `console: pipe`
  this effects will be the same as if `console: read-only` was specified.
* Fix bug where the timeout factor configuration option given in a
  configuration file is ignored.

## 1.1.1

* When `unique_build_dirs` __is enabled:__
    use a truely random build-id, such that no matter how much
    or how little time passes between two consequitive builds, the `BUILD_ID`s
    will always differ with the same probability.

## 1.1.0

* Fix unintended deletion of images that are wrongly classified as obsolete.
* Add a configuration parameter for guarding operations, for which
  no timeout values was specified: `default_timeout_seconds`.
* Introduce a configuration parameter to specify an optional **timeout factor**.
  Since the introduction of __SystemdNspawn__ support, most invokation of
  external system commands, e.g. `mount`, are guarded by short timeouts.
  During testing I discovered that some systems are just slower and might
  run into a timeout.
  To mitigate that, a user can now configure a **timeout factor** that is
  applied to the internal, hard-coded time values.
* Change NIX expressions to use `haskellPackages`
  instead of `myHaskellPackages`.

## 1.0.1

* Remove dhall
* Cleanup `b9.cabal`
* Replace `fail` with `error`

## 1.0.0

* Add `SystemdNspawn` support
* Improve command execution
* Improve nix build expressions

## 0.5.69.0

* Add new `ImageResize` option `ShrinkToMinimumAndIncrease`
* Remove the byte unit from `SizeUnit`
* Add new utility functions to `B9.DiskImages` for image size calculations

## 0.5.68.3

* Fix issue #10: The CLI parameter `--network host` is ignored
* Flush the log output after every log message to stdout

## 0.5.68.2

* Fix a bug in the shared image Shake rule causing unnecessary rebuilds

* Rename the `buildDirRoot` string template environment variable to `buildDir`

## 0.5.68.1

* Fix positional argument enumeration in the `Enviromnent`; CLI arguments
  are passed via the keys `arg_1 .. arg_N` in the `Environment`.
  They are currently passed in reverse order and start from `arg_0`
  instead of `arg_1`. See github issues #6 and #9.

* Fix #7 shared image meta info download and
  generalize and simplify `B9ConfigOverrid` while at it.

* Remove `Trying to load config file` messages

* Fix #8 - resurrect the environment variable `buildDirRoot`.

## 0.5.68

* Allow version specific config file resolution

  B9 changes the config file format from time to time. To enable
  simultaneous use of different B9 versions with conflicting config file formats
  the user may provide version specific default config files, and B9 will
  prefer the file matching its own version most closely.

  When loading its config file, B9 will now look for, and prefer
  config files suffixed with the longest matching version.

  For example: If B9 has version 0.5.68 these config files will be tried:

  * ~/.b9/b9.conf.0.5.68
  * ~/.b9/b9.conf.0.5
  * ~/.b9/b9.conf.0
  * ~/.b9/b9.conf

  If custom config file path are passed to B9 they will be tried first, and
  also with the version appended.
  For example, when runing `b9c -c other-config-path list` these paths would be tried:

  * other-config-path.0.5.68
  * other-config-path.0.5
  * other-config-path.0
  * other-config-path
  * /home/sven/.b9/b9.conf.0.5.68
  * /home/sven/.b9/b9.conf.0.5
  * /home/sven/.b9/b9.conf.0
  * /home/sven/.b9/b9.conf

## 0.5.67

* Iron out UTF-8 decoding issues

  * Use `Data.Text` in more places

  * Remove `ByteStringGenerator`

  * Change `ToContentGenerator` to produce only (strict) `Data.Text.Text`s

* Add `B9.Text` a module with conversion functions for different string types.

* Add helper functions and type for Erlang parsing:

  * `textToErlangAst`,

  * `stringToErlangAst` and

  * `ErlangAST`

## 0.5.66

* Fix the Nix package:

  * Get rid of the virsh path configuration item

  * Allow setting the path to @/.../libexec/libvirt_lxc@ via environment variable

## 0.5.65

* Refactor the B9 Monad to use `extensible-effects`

## 0.5.64

* Refactor and prepare for more drastic changes

* Fix runtime errors due to `undefined` values in the environment

## 0.5.63

* Depend on `shake-0.17.6` to fix build errors

## 0.5.62

* Rewrite `B9.Shake.SharedImageRules` in line with
  Shake's example for custom rules

* Replace `ConcatableSyntax` by using `Binary` instances, and also

  * Remove/Inline `encodeSyntax` by using `Binary.encode`

  * Rename `decodeSyntax` to `decodeOrFail'` and delegate to `Binary.decodeOrFail`.

* Add a newtype wrapper around `YamlObject` for **cloud-init** yaml documents
  `CloudConfigYaml`

  This new type serves the purpose of add the `#cloud-config`
  line to the top of the generated yaml document,
  as [required by cloud-init](https://cloudinit.readthedocs.io/en/latest/topics/format.html#cloud-config-data).

  The `Binary` instance adds this **header line** and
  delegates the rendering of the yaml document to
  `YamlObject`.

* Remove the rendering of this header line in the `YamlObject`
  `Binary` instance.

* Rename `RenderYaml` to `RenderYamlObject` In order to prevent unexpected
  runtime behaviour in code that uses this library.

* Introduce the type `Environment` that replaces the ubiquotus `[(String, String)]`
  by a lazy `Text` based `HashMap`.

* Add `addLocalPositionalArguments`

* Rename the previous `B9.Artifact.Content` to `B9.Artifact.Content`

* Introduce `ContentGenerator` as an open, extensible alternative
  to `Content`, `AST` and `CanRender` in the module
  `B9.Artifact.Content`
* Rename-and-Split refactor `B9.ArtifactSource{Impl}` to `B9.Artifact.Generator.{..}`

* Move `CanRender` from `B9.Artifact.Content.AST` to `B9.Artifact.Content`

* Switch to lazy `Text`s and `ByteString`s where possible, since B9 might
  read/generate large files.

* Rename `CanRender` to `ToContentGenerator`
  and change the method signatur to return the new `ContentGenerator` type

* Fix spelling: Rename B9Invokation to B9Invocation

* Rename `FromAST` to `FromAST`

* Rearrange modules for content generation:

  * Introduce `Content.FromByteString`

  * Remove deprecated `Concatenation`

# Changelog for B9

## 1.1.0

* Use the `SharedImageName` for hash calculations, to prevent multiple
  shared images sharing a single `SharedImageBuildId` as in this example:

```
prod-el7.centos-19.1.4 2020-03-30-09:30:14 3A870EB586731FA8                 
prod-el7.centos-19.1.5 2020-04-09-18:08:04 3A870EB586731FA8                 
```

* Fix unintended deletion of images that are wrongly classified as obsolete.
* Make the default timeout of 10s for several shell commands, like 
  loopback mounting of disk images, configurable.
* Change NIX expressions to use `haskellPackages`
  instead of `myHaskellPackages`.
* Add configuration options for __SystemdNspawn__:
  * `setenv`: A comma seperated list of `key=value` pairs 
    with environment variable assignments passed to the container
  * `chdir`: An optional working directory to change to before
    running the script in the container.
  * `user`: An optional user to change to after entering the container.
    The user must exist in the container image, e.g. in `/etc/passwd`.
  * `hostname`: An optional hostname to use as the kernel hostname 
    inside the container.

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

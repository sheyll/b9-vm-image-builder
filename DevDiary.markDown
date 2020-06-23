# Developer Diary

## 04.06.2016

## BuildStep simplification

I decided to remove the additional type parameter 'm' from 'BuildStep'.

That type parameter was introduced to allow different implementations of the
same artifact builders.

Also the build step now contains built-in data constructors for basic low-level
stuff like:

* Logging
* Build UUID
* Build timestamp
* Extra command line parameters
* Project root directory
* Build output directory
* System environment variables

## New module layout

I decided to re-factor split the project into `B9Core` and `B9Linux`.

### B9.Core

This is the structural core of B9. It is platform independent in the context of
VM-builds, since it contains no functionality to access disk image files,
hypervisors or execution containers.

    B9.Core.
    - Dsl.
        - Basic.     (some builders for artifacts/resources provided by B9Monad)
          - Document       (types/type classes for transforming or combining raw
                              or structured content, e.g. raw binaries, strings,
                                                       Erlang terms, JSON, YAML)
             - Raw                  (raw bytestrings, texts, strings, everything
                                                       that can be concatenated)
             - Dictionary       (A document of Key value pairs with unique keys)
             - Dictionaries.                        (Dictionary implementations)
                - ErlangTerm
                - HaskellTerm                              (Read/Show instances)
                - Json
                - Yaml
          - System    (Reading and writing to local or remote storage, launching
                                                       of commands using the OS)
            - Command                             (execution of system commands)
            - File                                    (using and creating files)
            - Http                        (up- downloading via HTTP PUT and GET)
          - Mono                   (A builder for Monoids based on a Writer)
        - Interpreter                      (combine BuildSteps to a B9IO action)
        - Types.
          - BuildStep            (Monad and type classes for declarative builds.
                                         Renamed, used to be called B9.Dsl.Core)
          - Handle
    - System.                                                        (Impure IO)
        - B9IO                 (thin wrapper monad around IO, command execution,
                                              logging, build directory creation)
        - Command                     8.22            (execution of system commands)
        - Configuration  (parse/generate B9s configuration file and command line
                                                                           args)
        - Directory                          (finding files, paths, directories)
    - Prelude (Re-export of very commonly used modules)
    - Util.
        - DynMap                                  (a type safe heterogenous Map)
        - Logging                                (Types for printf-like logging)
        - PropList                                (heterogenous key-value lists)
        - Size                 (ADT for power of integral two disk/memory sizes)

### B9.Linux - Basic VM build/tooling

BuildStep implementations for disk image conversion, execution of scripts inside
Linux containers, creation of cloud-init ISO images, and uplads/downloads from
repositories.

    B9.Linux.
    - BuildContainer.
      - NoContainer
      - Chroot
      - QemuKvm
      - LibvirtLxc
    - Document.Raw.
      - CloudInit
      - SystemD
      - Shellscript
    - FileSystem.
      - Iso9660
      - Vfat
      - Ext4
      - Xfs
      - Btrfs
      - Squash
    - DiskImage.
      - QemuImg
    - PartitionTable.
      - Mbr
      - Gpt
    - Repository.
      - RSync
    - Hypervisor.
      - QemuKvm
      - SystemDNSpawn
      - LibvirtEsx
      - LibvirtKvm

## 03.06.2016

Q: What's B9? Should I keep it around??
A: Yes.

It should help large teams and organizations to create CI for their software
running distributed on several virtual machines.

Haskell is used because it has a simple syntax and allows for vm-build
configuration programs that look only a little more complex than typical
configuration files, but it provides all the necessary, under the hood magic,
for automatic consistency checks.

Q: What's at the core of B9?

A: It consists of a platform independent core, that allows to write  the build
system using monads (i.e. the do notation), in a more-or-less declarative style.
It allows users to write vm-build recipes by stating all the  dependencies
between the resources without worrying about when, what build step has finished.
Think GNU Make.

This machinery itself does not create or deploy any VMs, its merely the
foundation for creating eDSLs. _Programs_ written in that DSL are pure and
symbolic with regard to _real_ I/O.

Each resource or artifact that can be built, or used as part of the build
process by other build steps(e.g. intermediate results), is  referenced to by an
opaque *handle* and is associated to impure, imperative,
potentially-missle-launching I/O code using (possibly uninhabited) Haskell types.

The current B9-package bundles some instances to build VM images on Linux using
qemu to create/convert sytem images and libvirt to run containers.

Q: What's at the top of B9?

A: No clear idea yet. Could be _servant_ style types, could be something else.

## 06.01.2016 05:04

Thinking about cleaning up `B9.ConfigUtils`.

Since B9 isn't focused around `read`ing and `showing` `ArtifactGenerators`
anymore, the functions `tell`, `consult`, `maybeConsult`,
`maybeConsultSystemPath` and the `ReaderException` might be obsolete.

The UUID handling should be refactored away. The type `UUID` should move to
`B9.CommonTypes` and the function `randomUUID` should go into `B9Monad` maybe.

## 06.01.2016 04:47

I will refactor the module structure of B9 such that the top-level package B9
will be a lot cleaner and suffer from less redundance.

This is achied by moving `B9Config` and `B9Monad` to `B9IO.Impl.Config/Monad`.

The Content package will be seperated such that no direct IO is done in a
`Content`.

The modules:

* `DiskImages`
* `ExecEnv`
* `FileSystems`
* `PartitionTable` (partly)
* `Repository` (partly)
* `ShellScript` (partly)
* `CommonTypes`
* `Logging`

Will be merged into the corresponding modules in `Dsl.*`

The modules:

* `FileSystemsImpl`
* `GPT`
* `LibVirtLXC`
* `Logging`
* `ExecEnv`
* `MBR`
* `QemuImg`
* `RepositoryIO`
* `Repository` (partly)
* `ShellScript` (partly)
* `PartitionTable` (partly)

The profiling stuff in `B9Monad` has never been used, I should scrub it.

The module `ConfigUtils` should follow `B9Config`.

## 06.01.2016 04:46

DSL.hs hash been refactored to use type-classes and open type families.

## 12.12.2015 23:20

Thinking about wether DSL.hs should be based upon open type families or stay
simply closed, the way it is. Then I could add a servant like type API ontop of
that, analogous how servant relates to wai.

Or should I rather convert it to type classes with associated type families?
Which could be:

* FileSystem functions
* VmImage conversiion and partitioning functions
* deployment functions
* script execution
* configuration management

First steps could be:

* Use _relaxed_ kinds
* convert everything to a single typeclass with associated type
* split Artifact
* rewrite the interpretation module

Or should I split it up just so the code is more seperate and clean?

I think I try the later...

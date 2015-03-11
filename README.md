# B9 - A Benign VM-Build Tool

[![Build Status](https://travis-ci.org/sheyll/b9-vm-image-builder.svg?branch=master)](https://travis-ci.org/sheyll/b9-vm-image-builder)

[![Hackage](https://img.shields.io/badge/hackage-B9-green.svg?style=flat)](http://hackage.haskell.org/package/b9)

Use B9 to compile your software into a deployable set of Linux-VM- or
configuration images, from a set of scripts and input files and templates

The main goal of this tool is to provide a build tool to increase automation and
reduce redundancy when creating configured, ready-to-run VM-images.

It is designed to help implementing what's buzz-worded as _immutable_
infrastructure, by making whole-VM-deployments as safe and a fast as possible.

B9 does not bring infrastructure to run and connect any VM-image in production,
it is merely a build tool to assemble deployable artifacts.

One big thing is that it can produce many machines and cloud-configs from a
single build file, because build files can describe concrete as well as
parameterized generators. It can create parameterized VM-Images by uploading
(e.g. system-)files assembled by syntax aware template application and
combination, all statically checked by during the build.

This sets B9 apart from e.g. cloud-init or other configuration management
systems that provide configuration via user provided dynamic script-programs,
which rely on the user to contain correct error handling.

The general idea is the same as in statically type programming languages: catch
errors as early as possible without relying on the user to create a covering set
of tests/error checks.

Certain sacrifies were made; there might be a steep laerning curve, but you will
eventually get there. The tool at hand works stable and reliable. The build
files are check rigorously, all builds happen in a random build directory and
failure leaves no stale LXC-containers running or multiple GiB of temporary disk
image files around. Also, there is no way modify an existing image in
place. Work on VM-Images is always done on a copy of an image, and to speed
things up, it is possible to explicitly use copy-on-write images.

B9 creates bootable virtual machine images, without necessarily using
virtualization itself.

In essence B9 is a tool for *creation*, *configuration* and *sharing* of VM images and
all peripheral artifacts, it creates:

* VMDK/QCOW2/Raw VM images
* Converted, extracted and resized copies of existing vm images
* Empty VM images with extended 4 file system
* Cloud-Config Images
* Text files from template files with variable interpolation
* Erlang OTP sys.config files
* beqemu-life-installer compatible VM images

B9 creates/converts/assembles virtual disk images as well as any number of
config-input files and executes scripts in LXC containers into which these
images are mounted.

The input is in both cases a single, text-based configuration file wich can be
put along side with other build files (e.g. Makefiles, maven poms, ...).

## Some Random Features:

* Tailored for both software compilation environments and VM image creation
* Creation of cloud-init (NoCloud) ISO images, VFAT images and directories
* Assembly and creation of arbitrary files with safe variable interpolation
* Creation of multiple images/machines/cloud-configs based on creation rules
* Syntax-checked merging of several cloud-config yaml user-data files with
  variable expansion
* Syntax-checked parsing and recombination of Erlang/OTP sys.config files with
  variable expansion
* Reusing and Sharing of vm-images, e.g. via The Internet using 'push' and 'pull'
* Arbitrary command execution inside a guest container
* Execution of interactive commands inside guest containers
* Create empty VM Images with file system
* Builtin config file formatter
* Create CopyOnWrite-Images backed by existing QCow2 or Raw images
* Create disk images from other disk image
* Derive disk images from a partition inside of an existing disk image
* Transparent support for QCOW2, VMDK and Raw (intermediate images will have the appropriate formats)
* Resize images and optionally also file systems inside disk images
* Support for 64-bit and 32-bit guests
* Share directories with the host
* Haskell library for parsing the config files and running builds
* Speed: Smart disk image conversion, raw image preference, flexible configuration, simple profiling
* Configurable Logging
* Automatic build clean-up
* Configurable LibVirtLXC parameters
* Configurable remote (ssh with pubkey auth + rsync) image shareing
* Local caching of shared images

## Compilation from Source

To build B9 first install:

*  `ghc` version 7.6 or higher
*  `cabal-install` version 1.16 or higher

B9 uses stackage and cabal sandboxes. The build result can be found in
`.cabal-sandbox/bin/`. To run a complete fresh build, execute:

    ./installDeps.sh
    cabal install

To launch b9c run:

    ./build_and_run.sh

To execute a ghci-repl run:

    cabal repl

To execute unit tests run:

    ./build_and_test.sh


## Installation

To be able to use B9 install

* lxc
* libvirt with lxc support (libvirt-driver-lxc, libvirt-daemon-lxc,...)
* virsh
* qemu
* ext4 tools
* genisoimage
* mtools
* vfat tools
* ssh
* rsync
* bash
* wget
* sudo

B9 has been tested with libvirt version 1.2.12.

Make sure that all neccessary daemons, e.g. `libvirtd.service`, `lxc.service`,..
are active, that SELinux is configured correctly and that the `nbd` kernel
module is loaded.

If neccessary create a libvirt network configuration, e.g. by using
the GUI front-end`virt-manager`.

Depending upon the libvirt and lxc configuration of the system it might be
nessary to allow the user, that will execute `b9c`, password-less `sudo` for
these commands:

* `virsh`
* `rm`
* `cat`
* `cp`
* `mv`

After installing B9 (either from a binary package or by building from source)
all its glory is availbe through a single executable called `b9c`.

When `b9c` is started for the first time, it creates a configuration file in
the users home directory called `~/.b9/b9.conf`. The path to that file can be
changed using command line arguments. Execute:

    b9c -h

for a list of command line parameters and commands.

`b9c` command line arguments always follow this pattern:

    b9c <global-options> <command> <command-options> -- <build-script-extra-args>


To enable B9 to work correctly on your machine edit the config file and make
necessary adaptions.

## B9 configuration file

This is an example of a B9 configuration file, by default found in
`~/.b9/b9.conf`:

    [global]
    # optional alternative directory for temporary build files. If 'Nothing'
    # the current directory is used.
    build_dir_root: Just "/home/sven/tmp"
    environment_vars: []
    exec_env: LibVirtLXC
    keep_temp_dirs: False
    # if set to 'Just "filename"
    log_file: Nothing
    profile_file: Nothing
    unique_build_dirs: True
    verbosity: Just LogInfo

    [libvirt-lxc]
    connection: lxc:///
    emulator_path: /usr/lib/libvirt/libvirt_lxc
    # contains `Just "libvirt-network-name"` or `Nothing` for your libvirt
    # default network settings
    network: Nothing
    use_sudo: True
    virsh_path: /usr/bin/virsh

Some of the options can also be specified on the command line.

# Writing B9 build files

If you really need to write these file, you are basically f'ed.

For now, look at existing config files and read the sources, if anything,
make sure to read at least the chapter _Anger-Management_ before throwing stuff
around.

More documentation is comming soon!

## General Structure

A B9 configuration describes a single `ArtifactGenerator`. It generates files
belonging to a VM, such as qcow2/raw/vmdk-image file(s) and e.g. cloud-init ISO
image files.

Just to recap: a `something.b9` build file _is_ always ever only a mere
`ArtifactGenerator` literal, no matter how many `Let`, `Each`, `Artifacts`,
etc... you see flying around there.

## Creating artifacts

To get any _real_ artifact out of an artifact generator use the `Artifact`
constructor. It takes *2* parameters an arbitrary id and a describtion of what
the artifact consists of:

     Artifact (IID "some_instance_id")
              (VmImages ... | CloudInit ...)

An artifact can either be a (set of) VM-disk-image(s) likely in combination
with some shell script to install software, etc *or* a static collection of
files put on a cloud-init image(VFAT or ISO or directory).

### Defining artifact generators that produce vm image files

To produce vm image files, e.g. with some software installed use the `VmImages`
artifact generator. It has only *2* parameters:


     VmImages
        [ ... disk image targets ... ]
        ( ... installation script ...)

Of course it must be wrapped in an `Artifact` definition, so we get this structure:

     Artifact (IID "my_first_image")
       (VmImages [...] (...))

#### ImageTargets

The first argument to `VmImages` is a list of `ImageTarget`. Each describes
a single VM-disk-image. The syntax is:

    ImageTarget
      ImageDestination
      ImageSource
      MountPoint

* An `ImageDestination` specifies if/where to put the output image.
* An `ImageSource` specifies how the image is created or from where it is taken.
* A `MountPoint` specifies where to mount the image during the execution of an
  optional `VmBuild`-script.

### Parameterized artifact generators

B9 supports `$varnam` variable interpolation in all strings anywhere in an
`ArtifactGenerator`:
* All filenames and paths
* All id strings and names
* Template files included via e.g. `Template`
* In every string in `VmScript`s (e.g. in `Run "${cmd}" ["${world}"]`)
* Also in all included template files (e.g. included via `Template`)

Parameters can be defined using `Let`, `Each` and special command line
arguments.

To pass parameters via the command line, append them after the argument delimiter
option `--` which ends the list of regular b9c arguments:

    b9c -v build -f file1.b9 .. -- arg_1 arg_2 ...

The parameters are bound to `${arg_1}`, `${arg_2}`, that is variables indicating
the corresponding *position* on the command line.

To define variables using `Let`, write:

    Let [key-value-pairs, ...]
        [artifactgenerators, ...]

All key-value bindings defined in the first list are available in every artifact
generator contained in the second list (_body_).

A key-value binding, e.g. `("hostname", "www.acme.org")`, consist of two strings
on parens seperated by a `,` (comma). The left string is the key, and the right
string is the value.

This `("webserver", "www.${domainname}")` is an example to show that the *value*
may also contain a variable reference. (Of course, only to variabled defined
*before*)

## Anger-Management

B9 build files contain a single literal `ArtifactGenerator` value
in Haskell syntax. B9 currently 'parses' the config file without any
error checking, so writing config files is VERY frustrating without
some tricks:

### Trick 1

Start with a working file and run

    b9c reformat -f <filename>

after each modification. The `reformat` command only parses and - hence the
name - (re-) formats/pretty-prints the files passed with `-f` options.

You will immediately know if a modification broke the file.

NOTE: If your build file refers to any `${arg_...}` positional arguments pass
them to `reformat` using `--` followed by the argument list.

### Trick 2

Obtain and build the sources of B9, start an interactive haskell shell with the
B9 code loaded and try to paste the contents of the config file to see if ghci
accepts it. Use the ghci macros `:{` and `:}` to begin and end a multi-line input
and paste the raw contents of the config file in question in between.


    $ cabal install
    $ cabal repl

    ... (many lines omitted) ...

    *B9> :{
    *B9| Artifact (IID "filer")
    *B9|   (VmImages [ ImageTarget
    *B9|                 (LocalFile (Image "EXPORT/machines/filer/disks/0.vmdk" Vmdk Ext4)
    *B9|                             KeepSize)
    *B9|                 (From "fedora-20-prod" KeepSize)
    *B9|                 (MountPoint "/")
    *B9|             , ImageTarget
    *B9|                 (LocalFile (Image "EXPORT/machines/filer/disks/1.vmdk" Vmdk Ext4)
    *B9|                             KeepSize)
    *B9|                 (EmptyImage "audio_files" Ext4 Raw (ImageSize 64 GB))
    *B9|                 (MountPoint "/export/lb/audio")
    *B9|             ]
    *B9|             (VmScript X86_64
    *B9|               [ SharedDirectoryRO "./filer" (MountPoint "/mnt/build_root")
    *B9|               , SharedDirectoryRO "../_common/upload" (MountPoint "/mnt/common")]
    *B9|               (Begin
    *B9|                  [ Run "dhclient" []
    *B9|                  , In "/mnt/build_root" [ Run "./machine-" [] ]
    *B9|                  , In "/mnt/common" [ Run "./post_export.sh" [] ]
    *B9|                  ])))
    *B9| :}

    Artifact (IID "filer") (VmImages
    [ImageTarget (LocalFile (Image "EXPORT/machines/filer/disks/0.vmdk" Vmdk Ext4) KeepSize)
    (From "fedora-20-prod" KeepSize) (MountPoint "/"),ImageTarget
    (LocalFile (Image "EXPORT/machines/filer/disks/1.vmdk" Vmdk Ext4) KeepSize)
    (EmptyImage "audio_files" Ext4 Raw (ImageSize 64 GB)) (MountPoint "/export/lb/audio")]
    (VmScript X86_64
    [SharedDirectoryRO "./filer" (MountPoint "/mnt/build_root"),
    SharedDirectoryRO "../_common/upload" (MountPoint "/mnt/common")]
    (Begin [Run "dhclient" [],In "/mnt/build_root" [Run "./machine-" []],In
    "/mnt/common" [Run "./post_export.sh" []]])))

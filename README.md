# B9 - A Benign VM-Build Tool [![changelog](https://img.shields.io/badge/changelog-green.svg?style=flat)](CHANGELOG.md)

[![Build Status](https://travis-ci.org/sheyll/b9-vm-image-builder.svg?branch=0.5)](https://travis-ci.org/sheyll/b9-vm-image-builder) [![Hackage](https://img.shields.io/badge/hackage-B9-green.svg?style=flat)](http://hackage.haskell.org/package/b9)

## What it can do

### Overview

**B9** is an executable and a Haskell library, that consumes a Haskell term describing 
the generation of VM-Images. 

A few core `data` types form an EDSL, and  `B9` contains functions to `read` and `show` then. 

Such a term can then be stored into a **text file** and is interpreted by a **command line invokation**. 

### Make VM Disk Images 

* Extract partitions from MRB Partitioned images
* Create, resuse, resize EXT-4 on Qcow2, vmdk or raw Images 
* Run commands on images to create new image, similar to what docker build does, 
  * Using libvirt-lxc 
  * Using docker 
  * Using systemd-nspawn

### Manage Disk Images 

* Cache images locally
* Distribute cached images via SCP

### Assemble Cloud-init Configuration

* Merge YAML Expressions
* Merge Erlang Terms 
* Load local files 
* Load files from HTTP servers
* Support `${identifier}` variable interpolation 
* Create cloud-init 
  * ISO images 
  * Floppy images 
  * yaml files

### Input files 

The input files can be in:

* DHALL format
* Haskell values interpreted by the `Read` instances.

### Usage as Library 

* Use as a Haskell library

### Configuration 

B9 uses a *`.ini` - style* configuration file.

### Incremental Builds 

B9 uses `shake` so some degree of incremental build is available.

## Installation 

### Installation on NixOS

* As **command line utility** in current directory:

      $ nix-build -E 'import ((fetchTarball https://github.com/sheyll/b9-vm-image-builder/archive/0.5.tar.gz) + "/release.nix") {}'
    
  Now the executable `b9c` is in `./result/bin/`:

      $ result/bin/b9c      

### Runtime dependencies

To be able to use B9 install:

* Linux
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

### B9 configuration file

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


## Goal of the project

Use B9 to compile your software into a deployable set of Linux-VM- or
configuration images, from a set of scripts and input files and templates.

The main goal of this tool is to provide a build tool to increase automation and
reduce redundancy when creating configured, ready-to-run VM-images.

One big thing is that it can produce many machines and cloud-configs from a
single build file, because build files can describe concrete as well as
parameterized generators. It can create parameterized VM-Images by uploading
(e.g. system-)files assembled by syntax aware template application and
combination, all statically checked by during the build.

This sets B9 apart from e.g. cloud-init or other configuration management
systems that provide configuration through user provided dynamic script-programs,
which rely on the user to contain correct error handling.

The general idea is the same as in statically type programming languages: catch
errors as early as possible without relying on the user to create a covering set
of tests/error checks.

Certain sacrifies were made; there might be a steep laerning curve, but you will
eventually get there. The tool at hand works stable and reliable. 

All builds happen in isolation, i.e. by default in random build directories, and
are cleaned on failure. 

Also, B9 does not modify cached images. 
Work on VM-Images is always done on a copy of an image, and to speed
things up, it is possible to explicitly use copy-on-write images.

B9 creates bootable virtual machine images, without necessarily using
virtualization itself.

In essence B9 is a tool for _creation_, _configuration_ and _sharing_ of VM images and
all peripheral artifacts, it creates:

* VMDK/QCOW2/Raw VM images
* Converted, extracted and resized copies of existing vm images
* Empty VM images with extended 4 file system
* Cloud-Config Images
* Text files from template files with variable interpolation
* Erlang OTP sys.config files


## Writing B9 build files

If you really need to write these file, you are basically f'ed.

For now, look at existing config files and read the sources, if anything,
make sure to read at least the chapter _Anger-Management_ before throwing stuff
around.

More documentation is comming soon!

### General Structure

A B9 configuration describes a single `ArtifactGenerator`. It generates files
belonging to a VM, such as qcow2/raw/vmdk-image file(s) and e.g. cloud-init ISO
image files.

Just to recap: a `something.b9` build file _is_ always ever only a mere
`ArtifactGenerator` literal, no matter how many `Let`, `Each`, `Artifacts`,
etc... you see flying around there.

### Creating artifacts

To get any _real_ artifact out of an artifact generator use the `Artifact`
constructor. It takes _2_ parameters an arbitrary id and a describtion of what
the artifact consists of:

     Artifact (IID "some_instance_id")
              (VmImages ... | CloudInit ...)

An artifact can either be a (set of) VM-disk-image(s) likely in combination
with some shell script to install software, etc _or_ a static collection of
files put on a cloud-init image(VFAT or ISO or directory).

#### Defining artifact generators that produce vm image files

To produce vm image files, e.g. with some software installed use the `VmImages`
artifact generator. It has only _2_ parameters:

     VmImages
        [ ... disk image targets ... ]
        ( ... installation script ...)

Of course it must be wrapped in an `Artifact` definition, so we get this structure:

     Artifact (IID "my_first_image")
       (VmImages [...] (...))

##### ImageTargets

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

#### Parameterized artifact generators

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
the corresponding _position_ on the command line.

To define variables using `Let`, write:

    Let [key-value-pairs, ...]
        [artifactgenerators, ...]

All key-value bindings defined in the first list are available in every artifact
generator contained in the second list (_body_).

A key-value binding, e.g. `("hostname", "www.acme.org")`, consist of two strings
on parens seperated by a `,` (comma). The left string is the key, and the right
string is the value.

This `("webserver", "www.${domainname}")` is an example to show that the _value_
may also contain a variable reference. (Of course, only to variabled defined
_before_)

### Anger-Management

B9 build files contain a single literal `ArtifactGenerator` value
in Haskell syntax. B9 currently 'parses' the config file without any
error checking, so writing config files is VERY frustrating without
some tricks:

#### Trick 1

Start with a working file and run

    b9c reformat -f <filename>

after each modification. The `reformat` command only parses and - hence the
name - (re-) formats/pretty-prints the files passed with `-f` options.

You will immediately know if a modification broke the file.

NOTE: If your build file refers to any `${arg_...}` positional arguments pass
them to `reformat` using `--` followed by the argument list.

#### Trick 2

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

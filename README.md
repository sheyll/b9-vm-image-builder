# B9 - A Benign VM-Build Tool

[![Build Status](https://travis-ci.org/sheyll/b9-vm-image-builder.svg?branch=0.6)](https://travis-ci.org/sheyll/b9-vm-image-builder) [![Hackage](https://img.shields.io/badge/hackage-B9-green.svg?style=flat)](http://hackage.haskell.org/package/b9) [![b9 LTS](http://stackage.org/package/b9/badge/lts)](http://stackage.org/lts/package/b9)

_User Story:_

    As a developer and operator of a complex service,
    comprised of several Linux machines hosting several applications
    talking to each other, I would like to fearlessly manage
    multi server system installations and configurations.

This tool tackles this requirement by allowing the user to
declare reusable installation scripts and configurations
using a non-intimidating configuration DSL.

Use B9 to compile your software into a deployable set of Linux-VM- or
configuration images, from a set of scripts and input files and templates

The main goal of this tool is to provide a build tool to increase automation and
reduce redundancy when creating configured, ready-to-run VM-images.

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

This tool at hand works stable and reliable. The build
files are check rigorously, all builds happen in a distinct build directory and
failure leaves no stale build directories around

In essence B9 is a tool for _creation_, _configuration_ and _sharing_ of VM images and
all peripheral artifacts.

B9 creates/converts/assembles virtual disk images as well as any number of
config-input files and executes scripts in LXC containers into which these
images are mounted.

The input is in both cases a single, text-based configuration file wich can be
put along side with other build files (e.g. Makefiles, maven poms, ...).

This file is actually code (Haskell).

## Features

- VMDK/QCOW2/Raw VM images
- Converted, extracted and resized copies of existing vm images
- Empty VM images with extended 4 file system
- Cloud-Config Images
- Text files from template files with variable interpolation
- Erlang OTP sys.config files
- YAML and JSON configuration files
- Tailored for both software compilation environments and VM image creation
- Creation of cloud-init (NoCloud) ISO images, VFAT images and directories
- Assembly and creation of arbitrary files with safe variable interpolation
- Creation of multiple images/machines/cloud-configs based on creation rules
- Syntax-checked merging of several cloud-config yaml user-data files with
  variable expansion
- Syntax-checked parsing and recombination of Erlang/OTP sys.config files with
  variable expansion
- Reusing and Sharing of vm-images, e.g. via The Internet using 'push' and 'pull'
- Arbitrary command execution inside a guest container
- Execution of interactive commands inside guest containers
- Create empty VM Images with file system
- Builtin config file formatter
- Create CopyOnWrite-Images backed by existing QCow2 or Raw images
- Create disk images from other disk image
- Derive disk images from a partition inside of an existing disk image
- Transparent support for QCOW2, VMDK and Raw (intermediate images will have the appropriate formats)
- Resize images and optionally also file systems inside disk images
- Support for 64-bit and 32-bit guests
- Share directories with the host
- Haskell library for parsing the config files and running builds
- Speed: Smart disk image conversion, raw image preference, flexible configuration, simple profiling
- Configurable Logging
- Automatic build clean-up
- Configurable LibVirtLXC parameters
- Configurable remote (ssh with pubkey auth + rsync) image shareing
- Local caching of shared images

## Installation

### b9 executable and library

Install via `stack`:

    $ stack install b9

### Runtime dependencies

To be able to use B9 install:

- Linux
- lxc
- libvirt with lxc support (libvirt-driver-lxc, libvirt-daemon-lxc,...)
- virsh
- qemu
- ext4 tools
- genisoimage
- mtools
- vfat tools
- ssh
- rsync
- bash
- wget
- sudo

B9 has been tested with libvirt version 1.2.12.

Make sure that all neccessary daemons, e.g. `libvirtd.service`, `lxc.service`,..
are active, that SELinux is configured correctly and that the `nbd` kernel
module is loaded.

If neccessary create a libvirt network configuration, e.g. by using
the GUI front-end`virt-manager`.

Depending upon the libvirt and lxc configuration of the system it might be
nessary to allow the user, that will execute `b9c`, password-less `sudo` for
these commands:

- `virsh`
- `rm`
- `cat`
- `cp`
- `mv`

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

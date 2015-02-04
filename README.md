B9 - A Benign VM-Build Tool
===========================

A set of Linux tools for using and creating VM Images from a build script using
Libvirt-LXC.

B9 creates/converts/assembles virtual disk images and executes scripts in LXC
containers into which these images are mounted.

Use B9 to compile your software on an isolated 32bit Archlinux, or to build a
custom, bootable virtual machine on a qcow2 image containing for DevOps-style
deployment.

The input is in both cases a single, text-based configuration file wich can be
put along side with other build files (e.g. Makefiles, maven poms, ...).

Some Basic Features:
--------------------
* Tailored for bot software compilation environments and VM image creation
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

Underlying Software:
--------------------

B9 uses Linux, LibVirt, LXC and Qemu/KVM and is written in Haskell.


Examples
========

Make a Custom Fedora Cloud Image
--------------------------------
TODO

Usage Example: Build an Archlinux Package
-----------------------------------------
TODO

Installation and System-Setup
=============================
TODO

Reference
=========
TODO

Invokation
-----------
TODO

B9 Projects
------------
TODO

Configuration
-------------
TODO

B9 Library
----------
TODO

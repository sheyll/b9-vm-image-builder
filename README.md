B9 - A Benign VM-Build Tool
===========================

A set of Linux tools for using and creating VM Images from a declaretiv build
script using Libvirt-LXC.

B9 creates/converts/assembles virtual disk images and executes scripts in LXC
containers into which these images are mounted.

Use B9 to compile your software on an isolated 32bit Archlinux, or to build a
custom, bootable virtual machine on a qcow2 image containing for DevOps-style
deployment.

The input is in both cases a single, text-based configuration file wich can be
put along side with other build files (e.g. Makefiles, maven poms, ...).

Some Basic Features:
--------------------
* Tailored for software compilation environments and VM image creation
* Run commands inside a guest container
* Create empty VM Images with file system
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
* Declaretive Configuration
* Automatic build clean-up

Underlying Software:
--------------------

B9 uses Linux, LibVirt, LXC and Qemu/KVM.


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

# B9 - A Benign VM-Build Tool

[![Build Status](https://travis-ci.org/sheyll/b9-vm-image-builder.svg?branch=master)](https://travis-ci.org/sheyll/b9-vm-image-builder)
[![Hackage](https://img.shields.io/badge/hackage-B9-green.svg?style=flat)](http://hackage.haskell.org/package/b9)
[![b9 on Stackage LTS 2](http://stackage.org/package/b9/badge/lts-2)](http://stackage.org/lts-2/package/b9)
[![b9 on Stackage LTS 3](http://stackage.org/package/b9/badge/lts-3)](http://stackage.org/lts-3/package/b9)
[![b9 on Stackage Nightly](http://stackage.org/package/b9/badge/nightly)](http://stackage.org/nightly/package/b9)

## What does it do?

It is foremost __code__ and not config. Hence it is testable and reusable and can
be structured using abstraction.

It contains an API for hosts, services, inter-service dependencies and
deployment targets, ranging from the individual host to a distributed system.

Use it to:

* Build VM-images and Linux containers
* Deploy VMs and containers
* Distribute and Share VM-Images in a network repository

## Usage Examples:

__B9 IS IN THE PROCESS OF BEING REWRITTEN__

    Examples soon!

## Why care?

It offers a _programmable_ VM-build and deployment tool.
To repeat, it's an API that allows you to:

* write installation,
* configuration and
* deployment code for
   * VM-images,
   * Linux containers and
   * cloud-init configurations.

## A bit deeper down the rabbit hole

Use B9 to compile your software into a deployable set of Linux-VM- or
configuration images, from a set of scripts and input files and templates.

B9 is a library for *creation* and *deployment* of virtual machines or Linux
containers, and for storing, retrieving, identifying, versioning and sharing
QCow2, Vmdk, etc disk images across the network using SCP/SSH.

You can:

* Compile/Assemble Cloud-Init artifacts
* Create and convert Ext4 file systems
* Create and convert QCow2, Vmdk, ISO9660, VFAT and Raw disk images
* Add files to disk-images
* Get files from disk-images
* Modify disk images using shell scripts inside a libvirt-lxc container

Planned features:

* Create docker images
* Deploy/Undeploy/Update to remote Libvirt-LXC, Libvirt-KVM and Libvirt-ESX
* Deploy/Undeploy/Update to Amazon, Openstack
* Deploy/Undeploy/Update to MyLittleCloud

* Create/Deploy Windows VMs (Omfg)

Anyway, I hope for the quick widespread adoption of the uni-kernel approach,
rendering this code - as well as docker - useless.

### Difference to docker?

Actually B9 is not the same as docker, it might *use* docker as execution
environment or deployment target.

The main difference is that you can write a *single* *type-safe* code base for
software installation, configuration and deployment in a classical programming
language, where a compiler offers some help to catch errors.  Sure, there are
linters for 'Dockerfile' and docker has a REST API.

But this is orthogonal to the aspect of creating actual infrastructure code,
that is structured to capture the system architecture and offers testability and
reuse.

Also, the B9 library is implemented such that one can write unit tests for the
builds with mocked IO!

Also while docker is nice and trendy, it is simply not the only option to deploy
in Linux containers.

In a real-world use-case I needed to install a custom kernel module and I had to
run an installation using KVM or ESX, instead of a Linux-Container. In a future
version of B9 there will be execution environments implemented by KVM.

### Difference to JuJu?
B9 is more similar to canonical JuJu. But everything is defined using statically
typed code.

### Difference to Kubernetes?
Kubernetes interaction can be the result of your B9-program.

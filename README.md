[![Build Status](https://travis-ci.org/sheyll/b9-vm-image-builder.svg?branch=master)](https://travis-ci.org/sheyll/b9-vm-image-builder)
[![Hackage](https://img.shields.io/badge/hackage-B9-green.svg?style=flat)](http://hackage.haskell.org/package/b9)
[![b9 on Stackage LTS 2](http://stackage.org/package/b9/badge/lts-2)](http://stackage.org/lts-2/package/b9)
[![b9 on Stackage LTS 3](http://stackage.org/package/b9/badge/lts-3)](http://stackage.org/lts-3/package/b9)
[![b9 on Stackage Nightly](http://stackage.org/package/b9/badge/nightly)](http://stackage.org/nightly/package/b9)

# B9 - A Benign VM-Build Tool

It *builds* and *deploys* virtual machines.

It contains an API for hosts, services, inter-service dependencies and
deployment targets, ranging from the individual host to a distributed system.

__B9 IS IN THE PROCESS OF BEING REWRITTEN__

## Usage Examples:

    Examples soon!

## Why care?

Describing VM-build and VM-deployment using __code__ leads to testable and
reusable results, simply because of the super-power of abstraction.

B9 offers a simple toy language that can be both interpreted and generated
easily with pure code, and is to some extend type safe.

You get to party really fancy without a hangover!

This offering comes in the disguise of a layered-library.

The first layer of the library abstracts away all real I/O and is extremely
intuitive and simple.

The second layer is already front facing API, although actually a little more
complex than the first, it is still appropriate to call it a toy-language.

It has exactly *four* primitives similar to CRUD. It describes how to `Create`,
`Add`, `Convert` and `Export` things that - for no good reason - are called
`Artifacts`.

Heck, you could even invent your own artifact types, just implement an instance
of the data-family `Artifact` and specify inhabitants of the type families
accompanying the methods of the toy language, i.e.: `CreateSpec`,
`AddSpec`, `ConvertSpec`, `ExportSpec` and `ExportResult`. (Oh, don't forget to
stack your own `Interpreter` type-class implementation on top of the existing
interpreter, otherwise your shiny new toys will merely yield some _Not Yet
Implemented_ utterances.

This offers all the hooks needed, for fancy tricks like, algorithmic
distribution of services across available domains, such that costs are
minimized, availability and/or consistency is at a specified level and
constraints regarding all aspects of distributed computing, ranging from network
infrastructure availability to software version and license requirements are
satisfied.

Some of these tricks might be suitable for all of human kind, some others could
be well guarded secrets to make you and your organization stand out.

## Some cherry-picked features

Right now you are empowered to:

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

## What does it cost?

It is free as in free beer, it costs *nothing* and will stay as expensive as
that *for ever*.


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

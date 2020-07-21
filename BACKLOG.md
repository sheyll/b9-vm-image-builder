# B9 Backlog

## +0.1.0

* Allow lookup of every external executable via environment variable:
  `B9_PATH_xxxxx` where `xxxxx` the name of the tool reduced to alpha-numeric 
  characters, e.g. for `systemd-nspawn` this will be `B9_PATH_systemdnspawn`

* Add configuration options for __SystemdNspawn__:
  * `setenv`: A comma seperated list of `key=value` pairs 
    with environment variable assignments passed to the container
  * `chdir`: An optional working directory to change to before
    running the script in the container.
  * `user`: An optional user to change to after entering the container.
    The user must exist in the container image, e.g. in `/etc/passwd`.
  * `hostname`: An optional hostname to use as the kernel hostname 
    inside the container.

## +0.0.1

* When `unique_build_dirs` __is disabled:__ 
  Form the build-id by hashing a product of:
   * the command line parameters
   * the project directory
   * the `Environment`
   * the `B9Configuration`

## +0.1.0

* Add support for rendering **nix-expressions** 

## +0.1.0

* Add support for `.dhall` for `.b9` content

## +0.1.0

* Add TAR build-env
* Add Tar VmScript alternative

## +0.1.0

* Add docker/oci image import/export

## +0.1.0

* Add CHROOT build-env

# Release 1.0.0 Back-Log 

**DEPRECATED most of this was not in 1.0.0**

* TODO What should go into 1.0.0?

## Feature Back-Log

### Introduce _eventually_ reproducable builds

Make everything as pure as possible in the configuration phase
and explicitely allow variations in items such as build date. By every command MUST be able to name its
inputs. All input must be instances of Hashable.

### Increase User Experience

* Switch to HOCON(initially thought of yaml, so examples below need rewriting) configuration; get rid of the ArtifactGenerator

* Add dummy modes where possible, dummy mode should go down into every module and component

* Answer "What are you doing?", "Why are you doing this?"
  using a query interface, that allows for **interactive introspection**, e.g. by using live monitoring and
  console.

* Require every component to have it's own logging as part of a logging tree. For example:

   1. **source

### Improve Code Maintainability

* More modules;

* Name: Never use abbrev.

* Simplify Types, remove types that have almost identical names and semantics using polymorphism
  change B9Config to be polymorphic and a functor, e.g.:

    data B9ConfigF f = B9C { _b9cHostBuildDirectory :: f FilePath
                           , _b9cLogLevel           :: f LogLevel
                           , ...
                           }
    type B9Config       = B9ConfigF Identity
    type B9ConfigMonoid = B9ConfigF Last

* Introduce a clearly seperated set of core principles, namely:
   **executor**, **configuration**, **source**, **host**, **builder** and **target**

  * **executor** the code that holds together and connects the other parts

    * Has a `cli-tool` (i.e. a _Main_ module)

    * Provides a high-level API

    * Reads the **local** OS and platform

    * Calls **configuration**

    * **configuration** Framework

      * Either `type classes` or `records` for combining command line and
           file based configuration of all sub-components.

      * every component could provide a config data type that is  parameterized   over a functor, e.g.:

               data SystemDNSpawnConfig1 f = SNSConfig { _snsAddToSplice :: f    (Maybe String)
                                                       , ...
                                                       }
                                                       deriving (Generic1)

               type SystemDNSpawnConfig = SystemDNSpawnConfig1 Identity

               type SystemDNSpawnConfigMonoid = SystemDNSpawnConfig1 Maybe

           This way ... TODO

      * Parses command line configuration

      * Parses configuration files

        * every component must provide a list of configuration files to inspect

        * every component must provide a command line switch to overwrite the    configuration file

    * **source**-components provide inputs used by **host** **build** and
   **target**

     Each input must specify a platform triple for compatibility with the other three components.

     E.g.: The `raw ext4 creator/transformer` **source** component needs to run _Linux commands_ on **host**
     e.g. `mkfs.ext4`, it also needs _Linux image mounting_ system during **build** and will most likely
     contain a Linux based **target** system.

     Source unpacking:

     How can we unify .tar.gz archives, MBR disk images with ext4 filesystems in them, and local directories
     in the light of building in them and generating output targets?

     Now suppose there is `Linux Container` **host** component, and a `Linux Shell` **build** component,
     then a B9 execution would be the orchestration of the sources, hosts and builds:

        inputs:
            - compressed-mrf-build-image:
                from-git-repo:
                    repo-url: git://binary-repo.local/os-images
                    file: mrf-build-image.qcow2.xz
                    rev: 0.91.0

            - mrf-build-image:
                resized-ext4-filesystem:
                    fs-label: root
                    size: 12GiB
                    from:
                        qemu-converted-disk-image:
                            format-out: raw
                            format-in: qcow2
                            from: compressed-mrf-build-image

            - rpm-build-script:
                local-files:
                    required-files:
                        - main.sh
                    included-files:
                        - src/rpm-build/*.sh

        outputs:
            - rpms-output-directory:
                local-directory: /tmp/mrf-rpms

        build:
            - mrf-rpm-build:
                linux-shell-script:
                    host: default-host                         (optional, default value: 'host')
                    main: main.sh
                    source-directory: rpm-build-script
                    output-directory: rpms-out

        hosts:
            - systemd-nspawn:
                label: host                                    (optional with default value: 'host')
                root: mrf-build-image                          (optional with default value: 'root-image')
                output-bindings:
                 - rpm-output-directory
                input-bindings: []


   * **host**

   * **builder**

   * **target**

### **source** Examples

1. docker images

2. nix

3. specially crafted git repos

4. nexus binary repository access

5. multi-source

6. bind-mounts

7. ext4 image creator/transformer

8. fetch urls

9. local files

10. unpack local archive files

### **host** Examples:

1. systemd-nspawn

2. SSH remote

3. docker

4. nix

5. distributed kubernetes build

### **builder** Examples:

0. Static File Writing (ala Cloud-Init)

1. Yaml files

2. Shell/Python/Ruby scripts with template extrapolation

3. Classical B9 Haskell

4. Interpret Haskell Module using `hint`

5. Docker

6. nix expressions

### **target** Examples:

0. VMDK images generated by `qemu-img`

1. `NullTarget`

2. git repo for generated images

3. HTML rendering of the output

4. host directories

5. rsync target

6. zip archive

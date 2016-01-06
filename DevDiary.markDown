06.01.2016 05:04
----------------

Thinking about cleaning up `B9.ConfigUtils`.

Since B9 isn't focused around `read`ing and `showing` `ArtifactGenerators`
anymore, the functions `tell`, `consult`, `maybeConsult`,
`maybeConsultSystemPath` and the `ReaderException` might be obsolete.

The UUID handling should be refactored away. The type `UUID` should move to
`B9.CommonTypes` and the function `randomUUID` should go into `B9Monad` maybe.


06.01.2016 04:47
----------------

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

06.01.2016 04:46
----------------

DSL.hs hash been refactored to use type-classes and open type families.


12.12.2015 23:20
----------------

Thinking about wether DSL.hs should be based upon open type families or stay
simply closed, the way it is. Then I could add a servant like type API ontop of
that, analogous how servant relates to wai.

Or should I rather convert it to type classes with associated type families?
Which could be:
- FileSystem functions
- VmImage conversiion and partitioning functions
- deployment functions
- script execution
- configuration management

First steps could be:
- Use _relaxed_ kinds
- convert everything to a single typeclass with associated type
- split Artifact
- rewrite the interpretation module

Or should I split it up just so the code is more seperate and clean?

I think I try the later...

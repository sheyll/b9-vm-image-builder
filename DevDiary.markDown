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

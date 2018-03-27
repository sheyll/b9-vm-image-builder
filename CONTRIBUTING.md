# Contribute

Let me make it clear: this project has no real community; I beleive in it's long term future, but that's just my opinion.

So every contribution is welcome. 

If you contribute, you may show of your best skills and I will praise you.

If you want to take things into your own hands, I will step back and let you make it yours. 
If I don't agree, I will make a pull request or fork.

If you want a small issue fixed, and you make a crappy pull request, I will be grateful and try to bring in the changes,
and I will praise you a little less.

# Strong Suggestions

* Use the english language for identifiers and documentation
* Use spaces and not tabs
* Either reformat all modules with your favorite formatter, or adapt to the existing formatting 
* Make many small commits
* Tag each release with the version number

# General Suggestions

## Favor immediate failure over program endurance

B9 is a **batch processing** tool, it will be run **once** to produce a single artifact.
Let it crash, or let it fail during type checking rather than making the code defensively handle unexpected result.

## Favor easy to understand and to extend code over "correct" abstractions

This project is part of the **waste** accumulated in software development. There is often little appreciation for code quality
investments, also there is always the threat of immenant project death if other tools take of, therefore make your code friendly
to the uninclined contributor. 

## Favor mathematical and logical rigor over intuitive, spontanous models

Prefer using functors, monads, profunctors and free algebras over concrete concepts found in the problem domain. 

## Favor standard library type constructors like `Maybe` or `(a->)` over custom types

## Derive `Typeable`, `Eq`, `Hashable`, `Binary`, `NFData`, `Show` for user facing data types 

## Favor easy and fast to understand names over short and fast to write names

## Good beats Perfect: Favor a mediocre contribution over _no contribution_, that would have been brilliant and elegant.

If you don't have time to follow all the rules, contribute anyway.

If it motivates you to make brilliant but hard to learn abstractions, then, please go ahead and do it. 

## When in doubt, favor semantic versioning over small, continously increasing version values


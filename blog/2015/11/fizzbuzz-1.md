# Haskell As Engineering Language

Haskell offers the following for software engineers

*    Reproducability - The build system stack is gaining steam. But there are others, such as [nix](http://www.cse.chalmers.se/~bernardy/nix.html)

*    Solid Testing Tools - [QuickCheck](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) and [HUnit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide)

*    Maintainable - GHC is my pair programming partner. Change a type's meaning
 and you will hear about it everywhere the code is affected when you try and compile. The type system makes refactoring a breeze.

*    Parallel and Concurrent Programming - [Superior Constructs](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/lang-parallel.html) for parallel and concurrent programming

*    Performance Analysis - [criterion](http://www.serpentine.com/criterion/)

*    Debugging Tools - [Stack tracing](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/ghci-debugger.html), old-timey [printf](https://hackage.haskell.org/package/base-4.8.1.0/docs/Debug-Trace.html) style debugging,[thread analysis](https://wiki.haskell.org/ThreadScope)


In this series we will use fizzbuzz to examine some of these facilities,
starting with Stack. Stack solves the problem of dependency hell and
therefore makes a codebase more easily reproducable.
[install stack](https://github.com/commercialhaskell/stack/tree/master/doc), then [download fizzbuzzfib](https://github.com/mlitchard/fizzbuzzfib).
with the top-level directory of the git repo
, open up the ghc interpreter like so:
`stack ghci`
Now off you go to [Part 2](/blog/2015/11/fizzbuzz-2)

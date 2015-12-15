# Haskell As Engineering Language

Haskell offers the following for software engineers

*    Reproducability - The build system stack is gaining steam. But there are others, such as [nix](http://www.cse.chalmers.se/~bernardy/nix.html), and [halcyon](http://halcyon.sh)

*    Solid Testing Tools - [QuickCheck](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) and [HUnit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide)

*    Maintainablility - Via [concise semantics](https://www.fpcomplete.com/business/about/haskell-improves-productivity/) and a powerful [type system](http://programmers.stackexchange.com/questions/279316/what-exactly-makes-the-haskell-type-system-so-revered-vs-say-java) 

*    Parallel and Concurrent Programming - [Superior Constructs](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/lang-parallel.html) for parallel and concurrent programming

*    Performance Analysis - [criterion](http://www.serpentine.com/criterion/)

*    Debugging Tools - [Stack tracing](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/ghci-debugger.html), old-timey [printf](https://hackage.haskell.org/package/base-4.8.1.0/docs/Debug-Trace.html) style debugging,[thread analysis](https://wiki.haskell.org/ThreadScope)


In this series we will use fizzbuzz to examine some of these facilities,
starting with Stack. Stack solves the problem of [dependency hell](http://stackoverflow.com/questions/25869041/whats-the-reason-behind-cabal-dependency-hell) and therefore makes a codebase more easily reproducable.
[install stack](https://github.com/commercialhaskell/stack/tree/master/doc), then [download fizzbuzzfib](https://github.com/mlitchard/fizzbuzzfib).
`Stack` uses .cabal files much like it's predecessor [cabal-install](https://www.fpcomplete.com/user/simonmichael/how-to-cabal-install). With it, you describe compiler flags, dependencies, testing environments,exposed and hidden modules (among other things). In [Part 3](/blog/2015/11/fizzbuzz-3) we'll take a look at it. For now, make sure your present working directory is the repo's toplevel and type the following:

`stack ghci`

Now off you go to [Part 2](/blog/2015/11/fizzbuzz-2).

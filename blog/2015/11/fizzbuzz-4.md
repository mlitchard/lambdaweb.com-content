## Unit Testing

I've been really bad about making sure I have good test coverage. When first
putting this project together I only decided to include tests because this
series is focused on engineering. As it turns out the unit tests I made
actually caught one of my functions as only being partially defined.
That combined with problems with my game
[Emporos](/blog/2015/11/momentio-testing.md), I have repented of my evil ways.

This section is about [HSpec](http://hspec.github.io/writing-specs.html) and how we can use it to test behavior. Here's what the fizzbuzzfib test set looks like.

    spec :: Spec
    spec = do
      describe "Clean Input, Correct Control Structure" $ do
        it "returns (NoInput :: FibError)" $
          fizzBuzzFib [] `shouldBe` (Left NoInput)
        it "returns (OnlyOne :: FibError)" $
          fizzBuzzFib ["10","20"] `shouldBe` (Left OnlyOne)
        it "returns (NotAnInteger :: FibError)" $
          fizzBuzzFib buffalo `shouldBe` (Left NotAnInteger)
      describe "Fibonacci check" $
        it "returns a list of integers each wrapped in a Right constructor" $
          map fib [1 .. 10] `shouldBe` fibs
      describe "FizzBuzz check" $
        it "returns proper result according to spec, see test/Spec.hs" $
          fizzBuzzFib ["10"] `shouldBe` fizzBuzzFibs

The first three tests make sure the error conditions are caught. This is where I discovered that I had initially written 'mustHaveOne` as a partial function. 
These tests are a bit boring so I decided by defining [buffalo](https://simple.wikipedia.org/wiki/Buffalo_buffalo_Buffalo_buffalo_buffalo_buffalo_Buffalo_buffalo) as:

`buffalo = ["Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo"]`

`fibs` and `fizzBuzzFibs` are values defined in test/Spec.hs.

In order to use these tests, we need to configure the `.cabal` file like so:

    test-suite fizzbuzzfib-test
      type:                exitcode-stdio-1.0
      hs-source-dirs:      test
      main-is:             Spec.hs
      build-depends:       base
                         , fizzbuzzfib
                         , hspec == 2.1.7
      ghc-options:         -threaded -rtsopts -with-rtsopts=-N
      default-language:    Haskell2010

and now we can run our tests

`mlitchard@mlitchard-personal:~/projects/git/fizzbuzzfib$ stack test`

<snip>

    Clean Input, Correct Control Structure
      returns (NoInput :: FibError)
      returns (OnlyOne :: FibError)
      returns (NotAnInteger :: FibError)
    Fibonacci check
      returns a list of integers each wrapped in a Right constructor
    FizzBuzz check
      returns proper result according to spec, see test/Spec.hs

    Finished in 0.0005 seconds
    5 examples, 0 failures
    Completed all 2 actions.

To see something more complicated, click the link above to my blog entry about writing a test for my game Emporos. To get an idea of what a real world test-suit might look like, have a look at [warp's](https://github.com/yesodweb/wai/blob/master/warp/test/RunSpec.hs) test suite.

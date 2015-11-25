Fizzbuzz is a chance to examine Haskell as an engineering language.
I started writing a fizzbuzz because a local startup wanted it done as part of a coding test. I decided against the company
but got interested in some things about fizzbuzz. The
least of which is the spec itself. That we can capture the patterns
expressed in fizzbuzz, and use it to talk about Haskell and it's ecosystem, is what is interesting about fizzbuzz, not a particular
concrete implementation.
 
Examining this problem also provides opportunity to examine Haskell itself:
*    It's [semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics)
*    It's [leading build system](https://github.com/commercialhaskell/stack/blob/master/doc/README.md)
*    [Error Handling](https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling) - Because if there is input, there's error handling

*    [Unit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide) and [property](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing)
## [Part I](/blog/2015/11/fizzbuzz-1)
 Wherein I speak on Haskell As Engineering Language.
 The first thing to do is install stack, because we want easy builds. 
 Then download my github repo [fizzbuzzfib](https://github.com/mlitchard/fizzbuzzfib)

## [Part II](/blog/2015/11/fizzbuzz-2)

 I'm going to look at the spec and work through how we get to monad comprehensions as part of a extensible,maintainable solution,while at the same time
reinforcing things [you've already seen](http://learnyouahaskell.com/chapters).

## [Part III](/blog/2015/11/fizzbuzz-3)

 Here is where things get dirty. Error handling. Here's I'll show you the dirt and how we can use a monad to clean it up. 

Fizzbuzz is a chance to examine Haskell as an engineering language. In this series it serves as an example of what development in Haskell can look like, a way to examine what it offers as a language for engineers, and a story for a typical internal project.

Fizzbuzz struck me as interesting, because fizzbuzz is not about fizzbuzz. The banal observation is that it serves to filter out people who can't actually program. What makes it interesting is that it forms a common pattern found in many problem domains. If your language has the proper constructs, the pattern reveals itself easily. This became apparent to me after reading this [blog post](http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html).

In the above article, the author identifies the general pattern of fizzbuzz as something you find in many real-world systems. That we can capture the pattern expressed in FizzBuzz and use it to talk about Haskell is what compelles interest.
Not a particular concrete implementation.

The intended audience for this series are engineers who have at least glanced through material found in resources like [Learn You A Haskell](http://learnyouahaskell.com/).

Examining this problem also provides opportunity to examine Haskell itself:
*    It's [semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics)
*    It's [leading build system](https://github.com/commercialhaskell/stack/blob/master/doc/README.md)
*    [Error Handling](https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling)

*    [Unit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide) and [property](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) testing.

## [Part I](/blog/2016/05/fizzbuzz-1)
   Wherein the story begins, and we talk a little about github and stack.

## [Part II](/blog/2016/05/fizzbuzz-2)
   I'm going to look at the spec and work through how we get to monad comprehensions as part of an extensible, maintainable solution; while at the same time reinforcing things [you've already seen](http://learnyouahaskell.com/chapters).

## [Part III](/blog/2016/05/fizzbuzz-3)
  Here is where things get dirty: error handling. I'll show you the dirt and how we can use `Either` monad to clean it up.

## [Part IV](/blog/2016/05/fizzbuzz-4)
  Well we have a pretty good idea of what this should look like by now. Let's start testing with Hspec and QuickCheck.

## [Part V](blog/2016/05/fizzbuzz-5)
  The tests ran rather slow? Why? Profiling might find out. (Spoiler: it does.)

## [Part V](/blog/2016/05/fizzbuzz-6)
  The Spec Changes! The boss has said the Eye of Sauron has looked upon the project and requested the integers for the fizzbuzz machine be generated with a fibonacci generator. We'll do it of course, but to what nefarious ends?

## [Part VI](/blog/2016/05/fizzbuzz-7)
  With the new changes, we'll need to change the tests. Why do we write the code first and the tests second? Find out with this post.

## [Part VII](/blog/2016/05/fizzbuzz-8)
  Bringing Travis CI into the mix

## [Part VII](/blog/2016/05/fizzbuzz-9)
  Benchmarking with Criterion!

## [Part VII](/blog/2016/05/fizzbuzz-10)
  Bringing Travis CI into the mix
  
## [Part IV](/blog/2016/05/fizzbuzz-11)
  The Spec Changes! The new fizzbuzz machine is so popular it spreads to other teams. These teams don't like the unix shell as an interface. What do we do?

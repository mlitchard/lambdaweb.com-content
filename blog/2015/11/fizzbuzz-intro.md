Fizzbuzz is a chance to examine Haskell as an engineering language.
I started writing a fizzbuzz as part of a coding test. As I was googling around,
I came across this [blog post](http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html). In it he identifies the general pattern of fizzbuzz as something you find in many real-world systems. That we can capture the pattern expressed
in FizzBuzz and use it to talk about Haskell is what compelles interest.
Not a particular concrete implementation.

The intended audience for this series are people who have at least glanced through [Learn You A Haskell](http://learnyouahaskell.com/).
 
Examining this problem also provides opportunity to examine Haskell itself:
*    It's [semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics)
*    It's [leading build system](https://github.com/commercialhaskell/stack/blob/master/doc/README.md)
*    [Error Handling](https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling) 

*    [Unit](https://wiki.haskell.org/HUnit_1.0_User%27s_Guide) and [property](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) testing.
## [Part I](/blog/2015/11/fizzbuzz-1)
 There are a few ways to manage development builds. We talk about the corporate-backed one, stack. 

## [Part II](/blog/2015/11/fizzbuzz-2)

 I'm going to look at the spec and work through how we get to monad comprehensions as part of a extensible, maintainable solution; while at the same time
reinforcing things [you've already seen](http://learnyouahaskell.com/chapters).

## [Part III](/blog/2015/11/fizzbuzz-3)

 Here is where things get dirty: error handling. I'll show you the dirt and how we can use `Either` monad to clean it up.

## [Part IV](/blog/2015/11/fizzbuzz-4)
 A discussion about unit and property testing.

## [Part V](/blog/2015/11/fizzbuzz-5)
 Deployment with Docker. 

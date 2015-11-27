## Problem Specification

### Given an Integer n, the program should generate up to the nth Fibonnacci
### number. This list will then be used as input for the fizzbuzz function.
### This function will emit a `"fizz":: String` when it evaluates a number to
### be divisible by 3, a `"buzz" :: String` when it evaluates a number to be
### divisible by 5, and `"fizzbuzz" :: String` when a number is a prime number.
### If the number doesn't evaluate as either divisible by 3 or 5, then the function should just evaluate to the number being evaluated. The design will be such that other features may be added easily.
### I/O needs to be managed. Take bad input into consideration. 

Example:



Now we can get to it. The first thing I wanted to play with, was examining the
reason as to why we need monad comprehensions. Why not plain list
comprehensions? I'll build up to answering this question slowly. First
let's get a reminder of what list comprehensions are for, because I hardly ever
use them. Here's what [wikipedia](https://en.wikipedia.org/wiki/List_comprehension) has to say:

> A list comprehension is a syntactic construct available in some programming
> languages for creating a list based on existing lists. It follows the form of
> the mathematical set-builder notation (set comprehension) as distinct from the
> use of map and filter functions.

So in the case of what we'd be looking for in a fizzbuzz program:

```
*FizzBuzz> ["fizz " | 15 `mod` 3 == 0]
["fizz "]

```
But this only checks the number 15. Using a lambda (anaonymous) function,
we can generalize to check for any arbitrary number.
```
*FizzBuzz> (\i -> ["fizz " | i `mod` 3 == 0]) 15
["fizz "]
```
This is as no suprise to experienced programmers, but there
are other patterns that can be abstracted that may suprise you.
We'll cover that later. For now, another anonymous function
for our fizzbuzz program. This time checking for numbers divisable by 5.

```
*FizzBuzz> (\i -> ["buzz " | i `mod` 5 == 0]) 11
[]
```
That empty list will prove to be a problem, we'll have to come up with
a better way to represent failure. For now, let's bind these functions
to names.
```
*FizzBuzz> let fizz3 = (\i -> ["fizz " | i `mod` 3 == 0])
*FizzBuzz> let buzz5 = (\i -> ["buzz " | i `mod` 5 == 0])
```
Next, we'll have to add the [associative](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)operator `(<>)`.

```
*FizzBuzz> :m + Data.Semigroup
```

We do this because we'd like to have each number checked by both functions, like so:

```
*FizzBuzz Data.Semigroup> (buzz5 <> fizz3) 10
```
```
["buzz "]
```
So, what just happened? To begin with, let's look at what happens when each function evaluates `10`.

```
*FizzBuzz Data.Semigroup> fizz3 10 
[]
```
```
*FizzBuzz Data.Semigroup> buzz5 10
["buzz "]
```
and what happens when we apply the `(<>)` associative operator to the evaluation of `fizz3 10` and `buzz5 10`
```
*FizzBuzz Data.Semigroup> [] <> ["buzz "]
["buzz "]
```
ghci will confirm the following are logically equivilent:
```
*FizzBuzz Data.Semigroup> ([] <> ["buzz "]) == ((buzz5 <> fizz3) 10)
True
```
from [`Data.Semigroup`](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)
```
class Semigroup a where Source

Minimal complete definition

Nothing

Methods

(<>) :: a -> a -> a infixr 6 Source

An associative operation.

(a <> b) <> c = a <> (b <> c)

If a is also a Monoid we further require

(<>) = mappend
```
And the [] instance of Semigroup
```
instance Semigroup [a] where
  (<>) = (++)
```
defines `concat` as the associative operator

This would be fine, except the requirement that the number being evaluated
should be the output if it does not evaluate to being either divisible by 3 or 5. Without monadic comprehensions, we would have to do something like this
```
badbuzz :: Int -> String
badbuzz a =
  case (fizzbuzz') of
    []  -> show a
    res -> unwords res
  where
    fizzbuzz' = ["fizz " | a `mod` 3 == 0] <>
                ["buzz " | a `mod` 5 == 0] <>
                ["fizzbuzz " | isPrime i]

```
The case control structure is the wrong tool for the job. But we have to use
that, or guards or if-then-else, unless we have other options. We do.
Haskell has monads, and this is where Monad Comprehensions save us
from ugly error prone, hard-to-maintain case constructs. Here's the
definition from [GHC's MonadComprehensions page](https://ghc.haskell.org/trac/ghc/wiki/MonadComprehensions)
> With {-# LANGUAGE MonadComprehensions #-} the comprehension
 [ f x | x <- xs, x>4 ]
> is interpreted in an arbitrary monad, rather than being restricted to lists.

Now, what is this Monad we want to use? Right now, `fizzbuzz'` evaluates to `["fizz"]`, `["buzz"]`, `["fizzbuzz"]` or `[]`. It's the empty list that requires
that extra control construct. Additionally, it doesn't actually represent
what we want. `[]` is a stand-in for `Nothing` which represents a failed
evaluation. That's what we actually want. The `Maybe` type, and it's `Nothing` value.

```
data Maybe a = Just a
             | Nothing
                deriving Show
```
Now, this works out for us, because `Maybe` is a `Monad`.

This means we can use Maybe as the arbitrary Monad for our monad comprehension.

Compare 
```
*FizzBuzz Data.Semigroup> let buzz5 = (\i -> ["buzz " | i `mod` 5 == 0]) :: (Integral a) => a -> [String]
*FizzBuzz Data.Semigroup> let fizz3 = (\i -> ["fizz " | i `mod` 3 == 0]) :: (Integral a) => a -> [String]
*FizzBuzz Data.Semigroup> let fizzbuzz = fizz3 <> buzz5 :: (Integral a) => a -> [String]
*FizzBuzz Data.Semigroup> :t fizzbuzz
fizzbuzz :: Integral a => a -> [String]
```
with,
```
*FizzBuzz Data.Semigroup> :set -XMonadComprehensions
*FizzBuzz Data.Semigroup> let may_buzz5 = (\i -> ["buzz " | i `mod` 5 == 0]) :: (Integral a) => a -> Maybe String
*FizzBuzz Data.Semigroup> let may_fizz3 = (\i -> ["fizz " | i `mod` 3 == 0]) :: (Integral a) => a -> Maybe String
*FizzBuzz Data.Semigroup> :t may_fizzbuzz
may_fizzbuzz :: Integral a => a -> Maybe String
```

We have two monads, a List and a Maybe. The list proves to not quite express failure as precisely as we would like. With monad comprehensions, we have the option of a more precise monad with a built-in value that expresses failure in a more general way, not just for a list that is empty.

```
*FizzBuzz Data.Semigroup> map may_fizzbuzz [1,2 .. 10]
[Nothing,Nothing,Just "fizz ",Nothing,Just "buzz ",Just "fizz ",Nothing,Nothing,Just "fizz ",Just "buzz "]
```
Now we're much closer to the spec. We need to replace the `Nothing` value with the number that failed the test and remove the `Just` constructor.

Fortunately, there is a function for that, [fromMaybe](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html)

>    fromMaybe :: a -> Maybe a -> a Source

>    The fromMaybe function takes a default value and and Maybe value. If the Maybe is Nothing, it returns the default values; otherwise, it returns the value contained in the Maybe.

So we can do this:

```
*FizzBuzz Data.Semigroup> let mf = (\i -> fromMaybe (show i) $ may_fizzbuzz i)
*FizzBuzz Data.Semigroup> map mf [1 .. 10]
["1","2","fizz ","4","buzz ","fizz ","7","8","fizz ","buzz "]

```

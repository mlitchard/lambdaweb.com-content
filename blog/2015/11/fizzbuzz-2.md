## Problem Specification

This program will return a `"fizz" :: String` when it evaluates a number to
be divisible by 3, a `"buzz" :: String` when it evaluates a number to be
divisible by 5, and `"fizzbuzz" :: String` when a number is divisible by both 3 and 5. When a number fails to evaluate
as either being divisible by 3 or 5, the function returns " < some number > " :: String.

In the post,["FizzBuzz, A Deep Navel Gaze Into"](http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html), the author solves the problem with monad comprehensions. I'd like to examine why we would want monad comprehensions, in the first place. Why not plain list comprehensions?
Let's start with a reminder of what list comprehensions are. Here's what [wikipedia](https://en.wikipedia.org/wiki/List_comprehension) has to say:

> A list comprehension is a syntactic construct available in some programming
> languages for creating a list based on existing lists. It follows the form of
> the mathematical set-builder notation (set comprehension) as distinct from the
> use of map and filter functions.

So in the case of what we'd be looking for in a fizzbuzz program:

```
FizzBuzz> (\i -> ["fizz" | i `mod` 3 == 0]) 15
["fizz"]

FizzBuzz> (\i -> ["buzz" | i `mod` 5 == 0]) 11
[]
```
That empty list will prove to be a problem, we'll have to come up with
a better way to represent failure. For now, let's bind these functions
to names.
```
FizzBuzz> let fizz3 = (\i -> ["fizz" | i `mod` 3 == 0])
FizzBuzz> let buzz5 = (\i -> ["buzz" | i `mod` 5 == 0])
```
Next, we'll have to add the [associative](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html) operator `(<>)`.
```
FizzBuzz> :m + Data.Semigroup
```
We do this because we'd like to have each number checked by both functions, like so:
```
FizzBuzz Data.Semigroup> (buzz5 <> fizz3) 10
["buzz"]
```
So, what just happened? To begin with, let's look at what happens when each function evaluates `10`.
```
FizzBuzz Data.Semigroup> fizz3 10 
[]
```
```
FizzBuzz Data.Semigroup> buzz5 10
["buzz"]
```
And what happens when we apply the `(<>)` associative operator to the evaluation of `fizz3 10` and `buzz5 10`.
```
FizzBuzz Data.Semigroup> [] <> ["buzz"]
["buzz"]
```
ghci will confirm the following are logically equivilent:
```

FizzBuzz Data.Semigroup> ([] <> ["buzz"]) == ((buzz5 <> fizz3) 10)
True
```
from [`Data.Semigroup`](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)
```
class Semigroup a where 

Minimal complete definition

Nothing

Methods

(<>) :: a -> a -> a infixr 6 

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
defines plusplus as the associative operator. 

This would be fine, except the requirement that the number being evaluated
should be the output if it does not evaluate to being either divisible by 3 or 5. Without monadic comprehensions, we would have to do something like this
```
badbuzz :: Int -> String
badbuzz a =
  case (fizzbuzz') of
    []  -> show a
    res -> unwords res
  where
    fizzbuzz' = ["fizz" | a `mod` 3 == 0] <>
                ["buzz" | a `mod` 5 == 0] <>

```
The case control structure is the wrong tool for the job. But we have to use
that, or guards, or if-then-else; unless we have other options. We do.
Haskell has monads, and this is where Monad Comprehensions save us
from ugly, error prone, hard-to-maintain case constructs. Here's the
definition from [GHC's MonadComprehensions page](https://ghc.haskell.org/trac/ghc/wiki/MonadComprehensions)

> With {-# LANGUAGE MonadComprehensions #-} the comprehension
> [ f x | x <- xs, x>4 ]
> is interpreted in an arbitrary monad, rather than being restricted to lists.

Now, what is this monad we want to use? Right now, `fizzbuzz'` evaluates to `["fizz"]`, `["buzz"]`, `["fizzbuzz"]` or `[]`. It's the empty list that requires
that extra control construct. Additionally, it doesn't actually represent
what we want. `[]` is a stand-in for `Nothing` which represents a failed
evaluation. That's what we actually want. The `Maybe` type, and it's `Nothing` value.

```
data Maybe a = Just a
             | Nothing
                deriving Show
```
Now, this works out for us, because `Maybe` is a monad.

This means we can use Maybe as the arbitrary monad for our monad comprehension.

Compare 
```
FizzBuzz Data.Semigroup> let buzz5 = (\i -> ["buzz" | i `mod` 5 == 0]) :: (Integral a) => a -> [String]

FizzBuzz Data.Semigroup> let fizz3 = (\i -> ["fizz" | i `mod` 3 == 0]) :: (Integral a) => a -> [String]

FizzBuzz Data.Semigroup> let fizzbuzz = fizz3 <> buzz5 

FizzBuzz Data.Semigroup> :t fizzbuzz
fizzbuzz :: Integral a => a -> [String]
```
with,
```
FizzBuzz Data.Semigroup> :set -XMonadComprehensions

FizzBuzz Data.Semigroup> let may_buzz5 = (\i -> ["buzz" | i `mod` 5 == 0]) :: (Integral a) => a -> Maybe String

FizzBuzz Data.Semigroup> let may_fizz3 = (\i -> ["fizz" | i `mod` 3 == 0]) :: (Integral a) => a -> Maybe String

FizzBuzz> let may_fizzbuzz = may_fizz3 <> may_buzz5
FizzBuzz Data.Semigroup> :t may_fizzbuzz
may_fizzbuzz :: Integral a => a -> Maybe String
```

We have two monads, a List and a Maybe. The list proves to not quite express failure as precisely as we would like. With monad comprehensions, we have the option of a more precise monad with a built-in value that expresses failure in a more precise way.

```
FizzBuzz Data.Semigroup> map may_fizzbuzz [1,2 .. 10]
[Nothing,Nothing,Just "fizz",Nothing,Just "buzz",Just "fizz",Nothing,Nothing,Just "fizz",Just "buzz"]
```
Now we're much closer to the spec. We need to replace the `Nothing` value with the number that failed the test and remove the `Just` constructor.

Fortunately, there is a function for that: [fromMaybe](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html)

>    fromMaybe :: a -> Maybe a -> a 

>    The fromMaybe function takes a default value and and Maybe value. If the Maybe is Nothing, it returns the default values; otherwise, it returns the value contained in the Maybe.

So we can do this:

```

FizzBuzz Data.Semigroup> let mf = (\i -> fromMaybe (show i) $ may_fizzbuzz i)
FizzBuzz Data.Semigroup> map mf [1 .. 15]
["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz"]
```

So, that's the gist. But we're not quite finished.

## The Trouble With Maybe

Here's the standard Haskell Definition for the Maybe monoid
```
Monoid a => Monoid (Maybe a)
```

This is gross, it's only a historical accident that it was allowed.
The correct thing to do is create a `Monoid` from a `Semigroup`.
The next best thing is the [`Option`](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html#t:Option) type.

### A better monoid for Maybe
```
newtype Option a 

Option is effectively Maybe with a better instance of Monoid, built off of an underlying Semigroup instead of an underlying Monoid.

Ideally, this type would not exist at all and we would just fix the Monoid instance of Maybe

Constructors
Option	 

getOption :: Maybe a
```
So, a slight adjustment to our definitions is required.
```
FizzBuzz Data.Semigroup> let opt_fizz3 = (\i -> ["fizz" | i `mod` 3 == 0]) :: (Integral a) => a -> Option String

FizzBuzz Data.Semigroup> let opt_buzz5 = (\i -> ["buzz" | i `mod` 5 == 0]) :: (Integral a) => a -> Option String

FizzBuzz Data.Semigroup> let opt_fizzbuzz = opt_fizz3 <> opt_buzz5

FizzBuzz Data.Semigroup> :t opt_fizzbuzz
opt_fizzbuzz :: Integral a => a -> Option String

FizzBuzz Data.Semigroup> let opt_fb = (\i -> fromMaybe (show i) $ getOption $ opt_fizzbuzz i)

FizzBuzz Data.Semigroup> :t opt_fb
opt_fb :: (Show a, Integral a) => a -> String
```
Now we can do the exact same thing before, with code that is more sound.
```
FizzBuzz Data.Semigroup> map opt_fb [1 .. 15]
["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz"]
```
We worked out a solution to this problem entirely in the interpreter,
here's what the function would look like:

```
fizzbuzz :: Integer -> String
fizzbuzz i = fromMaybe (show i) $ getOption fizzbuzz'
  where
    fizzbuzz' =
      ["fizz" | i `rem` 3 == 0] <>
      ["buzz" | i `rem` 5 == 0]
```

Ah, so we're done. No! In our hypothetical scenario, the client now
wants `"bang!" :: String` to be returned when a number is a prime.
Not a problem. Here's what we would do in the interpreter:


```
FizzBuzz Data.Semigroup> :m + Data.Numbers.Primes
FizzBuzz Data.Semigroup Data.Numbers.Primes>
FizzBuzz Data.Semigroup Data.Numbers.Primes> let opt_isPrime = (\i -> ["bang!" | isPrime i]) :: (Integral a) => a -> Option String
FizzBuzz Data.Semigroup Data.Numbers.Primes> let opt_fizzbuzz = opt_fizz3 <> opt_buzz5 <> opt_isPrime
FizzBuzz Data.Semigroup Data.Numbers.Primes> let opt_fb = (\i -> fromMaybe (show i) $ getOption $ opt_fizzbuzz i)
FizzBuzz Data.Semigroup Data.Numbers.Primes> map opt_fb [1 .. 15]
["1","bang!","fizzbang!","4","buzzbang!","fizz","bang!","8","fizz","buzz","bang!","fizz","bang!","14","fizzbuzz"]
```
Adding the feature to `fizzbuzz` would look like this:

```
fizzbuzz :: Integer -> String
fizzbuzz i = fromMaybe (show i) $ getOption fizzbuzz'
  where
    fizzbuzz' =
      ["fizz " | i `rem` 3 == 0] <>
      ["buzz " | i `rem` 5 == 0] <>
      ["bang!" | isPrime i]
```
Whew. Okay. Finally done defining fizzbuzz. We can compose more "if number has property x return string y" features as needed, and easily.

But we're not done.
The client has changed requirements yet again and needs the input type to be a fibonnacci number. Not a problem.
Let's work out how to generate the fibonnacci sequence.
Step 1 is admitting we don't know how to do that in a way that is not naive. So Google Away!

Google brings us to a [wiki page](https://wiki.haskell.org/The_Fibonacci_sequence#Constant-time_implementations) that tells us what we need to know.

### Binet's formula
```
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2
```
So let's make some definitions in the interpreter and see what we can do.

```
FizzBuzz Data.Semigroup Data.Numbers.Primes> let sq5 = sqrt 5 :: Double
FizzBuzz Data.Semigroup Data.Numbers.Primes> let phi = (1 + sq5) / 2
FizzBuzz Data.Semigroup Data.Numbers.Primes> let fib = (\n -> round $ phi ** fromIntegral n / sq5)
FizzBuzz Data.Semigroup Data.Numbers.Primes> map fib [1 .. 10]
[1,1,2,3,5,8,13,21,34,55]
FizzBuzz Data.Semigroup Data.Numbers.Primes> map (opt_fb . fib) [1 .. 15]
["1","1","bang!","fizzbang!","buzzbang!","8","bang","fizz","34","buzz","bang!","fizz","bang!","377","buzz"]
```
The key word here is _composition_ . See how easy it was to bring it all together? Monad comprehensions makes this possible.

But since this is an engineering exercise, we're not done.
In [Part 3](/blog/2015/11/fizzbuzz-3) we'll look at I/O, which means error
handling. Which means a big bowl of yuck, and monads to the rescue.

<footer> This is part II of the Haskell As An Engineering Language Series. Part I is <a href="/blog/2015/11/fizzbuzz-1">Here</a></footer>

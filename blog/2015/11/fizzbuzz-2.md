## Problem Specification

### Given an Integer n, the program should generate up to the nth Fibonnacci
### number. This list will then be used as input for the fizzbuzz function.
### This function will emit a `"fizz":: String` when it evaluates a number to
### be divisible by 3, a `"buzz" :: String` when it evaluates a number to be divisible by 5, and `"fizz buzz" :: String` when a number is divisible by 
both 3 and 5. The design will be such that other features may be added easily.
I/O needs to be managed. Take bad input into consideration. 


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

*FizzBuzz Data.Semigroup> buzz5 9
[]
```

```
(fizz3 <> fizz5) 9

["fizz "]
```

```
*FizzBuzz Data.Semigroup> :t (fizz3 <> fizz5) 9
(fizz3 <> fizz5) 9 :: [[Char]]
```
```
*FizzBuzz Data.Semigroup> :t (fizz3 <> fizz5)
(fizz3 <> fizz5) :: Integral a => a -> [[Char]]
```


from [`Data.Semigroup`](https://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)

From [GHC's MonadComprehensions page](https://ghc.haskell.org/trac/ghc/wiki/MonadComprehensions)
> With {-# LANGUAGE MonadComprehensions #-} the comprehension
 [ f x | x <- xs, x>4 ]
> is interpreted in an arbitrary monad, rather than being restricted to lists.


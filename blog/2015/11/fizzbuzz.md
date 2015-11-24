Fizzbuzz is a chance to examine contemporary Haskell

This blog post is for people who have seen a bit of Haskell; maybe have read
some [Learn You A Haskell](http://learnyouahaskell.com/), and appreciate 
seeing the building process in action. I spend a considerable amount of time
in ghci, because I find it useful as a development tool. The first thing
to do is install stack, then we'll get right into ghci and start unpacking
fizzbuzz.

To follow along, you'll want to (install stack)[https://github.com/commercialhaskell/stack/tree/master/doc], then (download fizzbuzzfib)[https://github.com/mlitchard/fizzbuzzfib].
Next, open up the ghc interpreter like so:
`stack ghci`
Now we can get to it. The first thing I wanted to play with, was examining the
reason as to why we need monad comprehensions. Why not plain list
comprehensions. I'll build up to answering this question slowly. First
let's get a reminder of what list comprehensions are for. I hardly ever
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
Prelude> (\i -> ["fizz " | i `mod` 3 == 0]) 15
["fizz "]
```
This is as no suprise to experienced programmers, but there
are other patterns that can be abstracted that may suprise you.
We'll cover that later. For now, another anonymous function 
for our fizzbuzz program. This time checking for numbers divisable by 5.

```
Prelude> (\i -> ["buzz " | i `mod` 5 == 0]) 11
[]
```

```
Prelude> let fizz3 = (\i -> ["fizz " | i `mod` 3 == 0])
Prelude> let buzz5 = (\i -> ["buzz " | i `mod` 5 == 0])
```

```
Prelude Data.Semigroup> fizz3 9
["fizz "]
Prelude Data.Semigroup> buzz5 9
[]
```

```
(fizz3 <> fizz5) 9

["fizz "]
```

```
Prelude Data.Semigroup> :t (fizz3 <> fizz5) 9
(fizz3 <> fizz5) 9 :: [[Char]]
```
```
Prelude Data.Semigroup> :t (fizz3 <> fizz5)
(fizz3 <> fizz5) :: Integral a => a -> [[Char]]
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



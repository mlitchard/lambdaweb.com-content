## Dirty Dirty I/O And The Errors It Makes Us Handle

What do we have thus far.

1. A fizzbuzz function
2. A feeder function for fizzbuzz
3. No way for anyone to use these.

Okay then, lets go ahead and make a command line executable for our fuzzbuzz

```
module Main where
import FizzBuzz
import System.Environment

main :: IO ()
main = do
  input <- getArgs
```  

Okay now what. getArgs has type String :: IO (), and we need an Int.
Well, can we get a String -> Integer? Let's ask Hoogle

Hoogle says nope. But look at what we did find
`read :: Read a => String -> a`

In our case we would need an `a :: Integer`. And lucky for us,
`Integer` has a `Read` instance:
[Here Be Dragons](http://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Read.html#line-464)

Let's unpack `read`, there's something cool going on there, and it's called
`id`.

```
-- | The 'read' function reads input from a string, which must be
-- completely consumed by the input process.
read :: Read a => String -> a
read s = either error id (readEither s)
```
We'll go left to right, starting with the [`either`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Either.html) function.
```
either :: (a -> c) -> (b -> c) -> Either a b -> c 
```
>    Case analysis for the Either type. If the value is Left a, apply the first function to a; if it is Right b, apply the second function to b.

Both [`error`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:error) and [`id`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#v:id) use a returned value given by [`readEither`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Text-Read.html)

```
readEither :: Read a => String -> Either String a Source

Parse a string using the Read instance. Succeeds if there is exactly one valid result. A Left value indicates a parse error.
```
So if someone wanted to be a jerkface and give our fizzbuzz program a string that no on would recognize as an `Integer`, but we expected one, `readEither` would do this:
```
Prelude> :m + Text.Read
Prelude Text.Read> readEither "foogle" :: Either String Integer
Left "Prelude.read: no parse"
```
`either` takes the `Left String` and transforms it into a `String`, which is then passed to `error`.

Here's where it gets hairy, because `read` uses `error`, it's a [partial function](https://wiki.haskell.org/Partial_functions). And we want total functions,
every time.
# note show the consequences of having to use a partial function in gross code #

Well now, we can't use `read`, because it's gross. What else is there? While investigating `readEither`, you may have come across [`readMaybe`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Text-Read.html). And we'll use that instead.



# nanoid-hs

Nanoid package is an implementation of [ai/nanoid](https://github.com/ai/nanoid)
in Haskell.

> A tiny, secure, URL-friendly, unique string ID generator for JavaScript.

```js
var nanoid = require('nanoid')
model.id = nanoid() //=> "Uakgb_J5m9g~0JDMbcJqLJ"
```

The core nanoid
[generator](https://github.com/ai/nanoid/blob/f2dc36fc83785f0d132f364769cb6e0f6ba7f083/format.js)
function is the point of our interest. The goal of this project is not to
implement the UID generator in Haskell, but to show that purely functional
language is as practical as an imperative one, and the translation of an
imperative algorithm to a purely functional language is a straightforward and
almost mechanical process.

This package is written as an article, in a documentation to the
[Nanoid.hs](Nanoid.hs) module, and serves purely educational purposes. I
choosed `ai/nanoid` because it is a useful real-world program, it is small, it
has mutability and side effects, and it already has been ported to [other
programming lanugages](https://github.com/ai/nanoid#other-programming-languages).

The technique which I used is not new and was described in
[Lazy functional state threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237)
paper.

You can also check [Imperative Hakskell](http://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/)
blog post on this topic. It uses the same approach to convert classic CS algorightms to Haskell.

## Build

All the examples are runnable from GHCi REPL provided by the awesome
Cabal new build infrastructure.

```
$ cabal new-repl
In order, the following will be built (use -v for more details):
 - nanoid-hs-0.1.0.0 (lib) (file Nanoid.hs changed)
Preprocessing library for nanoid-hs-0.1.0.0..
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Nanoid           ( Nanoid.hs, interpreted )
Ok, one module loaded.
*Nanoid>
```

Generate HTML version of the package documentation with:

```
$ cabal new-haddock
```

## References

- nanoid https://github.com/ai/nanoid
- Lazy functional state threads http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237
- Imperative Haskell http://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/

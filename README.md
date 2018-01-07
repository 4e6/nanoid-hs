# nanoid-hs

Nanoid package is a copy of [ai/nanoid](https://github.com/ai/nanoid) JS package.

The article is written as a documentation to [Nanoid.hs](Nanoid.hs) module,
you can read the source and run examples in the REPL.

It was inspired by the recent ai's twitter post and serves purely
educational purposes. I noticed that the idea that imperative program can be
replicated in a purely functional language might be unclear. And 'ai/nanoid'
package might be a good example of a useful real-world program translated to a
purely functional language.

The technique which I used to translate the imperative program into
a functional one was described in
[Lazy functional state threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237)
paper.

You can also check [Imperative Hakskell](http://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/)
blog post on this topic illustrated with classic CS algorightms.

## Build

All the examples below are runnable from GHCi REPL provided by the awesome
Cabal new build commands.

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

You can also generate HTML version as a package documentation with:

```
$ cabal new-haddock
```

## References

- nanoid https://github.com/ai/nanoid
- Lazy functional state threads http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237
- Imperative Haskell http://vaibhavsagar.com/blog/2017/05/29/imperative-haskell/

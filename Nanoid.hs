-- | "Nanoid" package is a copy of <https://github.com/ai/nanoid> JS package
--
-- It was inspired by the post
-- <https://twitter.com/andrey_sitnik/status/949611808335323136>. This package
-- serves purely educational purposes. I noticed that the idea that imperative
-- program can be replicated in a purely functional language might be
-- unclear. And 'ai/nanoid' package should be a good example of a useful
-- real-world program translated to a purely functional language.
--
-- The technique which I used to translate the imperative program into
-- a functional one was described in `Lazy functional state threads' paper
-- <http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237>
--
-- All the examples below are runnable from GHCi REPL provided by the awesome
-- Cabal new build commands.
--
-- @
--     $ cabal new-repl
--     In order, the following will be built (use -v for more details):
--      - nanoid-hs-0.1.0.0 (lib) (file Nanoid.hs changed)
--     Preprocessing library for nanoid-hs-0.1.0.0..
--     GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
--     [1 of 1] Compiling Nanoid           ( Nanoid.hs, interpreted )
--     Ok, one module loaded.
--     *Nanoid>
-- @
--
-- You can also generate HTML version with
--
-- @
--     $ cabal new-haddock
-- @
module Nanoid where

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Trans.Loop
import Data.Bits
import Data.IORef
import Data.STRef
import System.Random (randomIO)

-- |
-- = Stateful generator function
--
-- Function 'generatorIO' is a replica of the ai/nanoid JS function
-- <https://github.com/ai/nanoid/blob/f2dc36fc83785f0d132f364769cb6e0f6ba7f083/format.js>
-- The idea was to show how imperative algorithm can be reproduced in a
-- functional language, and my goal was to achieve line-to-line correspondence.
-- There's also one change compared to JS function when we are breaking loop
-- twice instead of an early return,
--
-- @
--     if (id.length === size) break
-- @
--
-- but I'm fine with it as far as it doesn't change the computational
-- complexity.
--
-- When implemented in Haskell we immediately see the problem with
-- 'generatorIO'. It is a God-function with infinite powers, it runs in IO and
-- is able to do arbitrary effects (even to launch the missiles, as they
-- say). Moreover, it is hard to tell what is happening inside because there is
-- a lot of manipulations with state and indexes which is hard to follow.
--
-- Looking at the interface we can tell that @IO@ is required because of
-- the stateful @random@ function it takes as an input.
--
-- @
--     random :: Int -> IO [Int]
-- @
--
-- To eliminate @IO@ we can leverage the laziness feature of the Haskell
-- language and pass an infinite stream of integers instead. This takes us to
-- the next 'generatorST' implementation.
--
-- == Example
--
-- Module "Nanoid" provides 'randomN' and 'url' helpers for convenience
--
-- >>> generatorIO randomN url 21
-- "bd3xzujGEwjzP_91sXIf2"
generatorIO :: (Int -> IO [Int]) -> String -> Int -> IO String
generatorIO random alphabet size = do                                       --  function (random, alphabet, size)
  let mask = (2 `rotate`
        truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1  --  var mask = (2 << Math.log(alphabet.length - 1) / Math.LN2) - 1
  let step = ceiling (1.6 * fromIntegral mask * fromIntegral size
                       / fromIntegral (length alphabet))                    --  var step = Math.ceil(1.6 * mask * size / alphabet.length)

  varId <- newIORef ""                                                      --  var id = ''
  while (pure True) $ do                                                    --  while (true) {
    bytes <- liftIO $ random step                                           --    var bytes = random(step)
    foreach [0..step] $ \i -> do                                            --    for (var i = 0; i < step; i++) {
      let byte = (bytes !! i) .&. mask                                      --      var byte = bytes[i] & mask
      when (length alphabet > byte) $ do                                    --      if (alphabet[byte]) {
        liftIO $ modifyIORef varId (\id -> id ++ [alphabet !! byte])        --        id += alphabet[byte]
                                                                            --      }
        id <- liftIO $ readIORef varId                                      --
        when (length id == size) exit                                       --      if (id.length === size) break
                                                                            --    }
    id <- liftIO $ readIORef varId                                          --
    when (length id == size) exit                                           --    if (id.length === size) break
                                                                            --  }
  readIORef varId                                                           --  return id

-- | Return a list of @n@ random integers
randomN :: Int -> IO [Int]
randomN n = replicateM n randomIO

-- | Example alphabet
url :: String
url = "_~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Simple interface for 'generator' function
--
-- >>> simple 21
-- "U~EkpQORQ7W_TYUasNv3c"
simple :: Int -> IO String
simple = generatorIO randomN url

-- |
-- = Referentially trasparent generator function.
--
-- Instead of effectful @random@ function it takes an infinite stream of
-- integers @randoms :: [Int]@. Function still mutates the state inside, but now
-- it has pure interface. State is hidden inside the 'ST' monad, and looking at
-- the signature we can tell that function is at least referentially
-- transparent.
--
-- The only change required in the algorithm is to handle the @randoms@
-- state. Note that JS functions here are just a pseudocode translated from
-- Haskell.
--
-- @
--     ...
--     var randoms = randoms
--     while (true) {
--       const bytes = randoms.take(step)
--       randoms = randoms.drop(step)
--     ...
-- @
--
-- 'generatorST' function is much better than 'generatorIO' because we are sure
-- it cannot do anything fishy, cannot affect the outside world. The algorithm
-- itself is still a mess, but due to the referential transparency we can reason
-- about it. We can forget the implementation details and compose it with other
-- functions being guided just by its type.
--
-- So far this function is a mechanical translation from the imperative code. As
-- a conclusion I wanted to come up with more idiomatic Haskell program that
-- impelments the same algorithm. Soon I realized that this program just doing
-- the filtering and transformation the input stream of integers. Here we
-- transition to the 'generator' function.
--
-- == Example
--
-- Now we delegated the problem of creating the stream of randoms to the caller.
-- Example below is using 'randoms' from the "System.Random" module
-- to create the infinite stream of integers.
--
-- >>> import System.Random
--
-- >>> :t getStdGen
-- getStdGen :: IO StdGen
--
-- >>> :t randoms <$> getStdGen
-- randoms <$> getStdGen :: Random a => IO [a]
--
-- >>> (\rs -> generatorST rs url 21) <$> randoms <$> getStdGen
-- "u~0IlMNwisEzQGF2jJ_7T"
generatorST :: [Int] -> String -> Int -> String
generatorST randoms alphabet size = runST $ do                              --  function (randoms, alphabet, size)
  let mask = (2 `rotate`
        truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1  --  const mask = (2 << Math.log(alphabet.length - 1) / Math.LN2) - 1
  let step = ceiling (1.6 * fromIntegral mask * fromIntegral size
                       / fromIntegral (length alphabet))                    --  const step = Math.ceil(1.6 * mask * size / alphabet.length)

  varId <- newSTRef ""                                                      --  var id = ""
  varRandoms <- newSTRef randoms                                            --  var randoms = randoms
  while (pure True) $ do                                                    --  while (true) {
    randoms <- liftBase $ readSTRef varRandoms                              --
    let bytes = take step randoms                                           --    const bytes = randoms.take(step)
    liftBase $ writeSTRef varRandoms (drop step randoms)                    --    randoms = randoms.drop(step)
    foreach [0..step] $ \i -> do                                            --    for (var i = 0; i < step; i++) {
      let byte = (bytes !! i) .&. mask                                      --      const byte = bytes[i] & mask
      when (length alphabet > byte) $ do                                    --      if (alphabet[byte]) {
        liftBase $ modifySTRef varId (\id -> id ++ [alphabet !! byte])      --        id += alphabet[byte]
                                                                            --      }
        id <- liftBase $ readSTRef varId                                    --
        when (length id == size) exit                                       --      if (id.length === size) break
                                                                            --    }
    id <- liftBase $ readSTRef varId                                        --
    when (length id == size) exit                                           --    if (id.length === size) break
                                                                            --  }
  readSTRef varId                                                           --  return id

-- |
-- = Pure generator function
--
-- After our observation we ended up with a simple sieve algorithm on a stream
-- of @randoms@. Compared to 'generatorST' this program is pure, it lacks the
-- state. Now looking at the code you can tell that the program is a
-- composition of functions 'filter', 'map', and 'take'.
--
-- Although I do realize that the batching with @step@ in the original JS
-- implementation was probably an optimization to minimize the interactions with
-- RNG. In our case I'm very doubtful that it would have any effect on the
-- Haksell program.
--
-- == Test
--
-- As a sanity check, to verify that we preserve the algorithm after all the
-- transformations, we test that all functions return the same result on the
-- identical inputs. We can notice here that 'generatorIO' is also an idempotent
-- function, but that wasn't clear from the beginning given its type signature.
--
-- >>> generatorIO (const $ pure [100..200]) url 21
-- "yzABCDEFGHIJKLMNOPQRS"
--
-- >>> generatorST [100..200] url 21
-- "yzABCDEFGHIJKLMNOPQRS"
--
-- >>> generator [100..200] url 21
-- "yzABCDEFGHIJKLMNOPQRS"
generator :: [Int] -> String -> Int -> String
generator randoms alphabet size = take size $ go randoms
  where
    mask = (2 `rotate` truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1
    go   = map (\i -> alphabet !! (i .&. mask))
      . filter (\i -> length alphabet > i .&. mask)

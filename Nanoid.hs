-- |
-- Nanoid package is an implementation of [ai/nanoid](https://github.com/ai/nanoid)
-- in Haskell.
--
-- /Nanoid is a tiny, secure, URL-friendly, unique string ID generator/
--
-- >>> generate [10..20] url 5
-- "89abc"
--
-- >>> generate <$> randomsIO <*> pure "abcd" <*> pure 4
-- "dabb"
--
-- >>> generate <$> randomsIO <*> pure url <*> pure 21
-- "1xfg7ez1WL7r6jinFzTsy"
--
-- The core nanoid generator function
-- <https://github.com/ai/nanoid/blob/f2dc36fc83785f0d132f364769cb6e0f6ba7f083/format.js>
-- is the point of our interest. The goal of this project is not to implement
-- the UID generator in Haskell, but to show that purely functional language is
-- as practical as an imperative one, and the translation of an imperative
-- algorithm to a purely functional language is a straightforward and almost
-- mechanical process.
--
-- The technique which I used is not new and was described in
-- [Lazy functional state threads](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.144.2237)
-- paper.
module Nanoid where

import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Trans.Loop
import Data.Bits
import Data.IORef
import Data.STRef

import qualified System.Random as RNG

-- |
-- = Stateful generator
--
-- Function 'generateIO' is an implementation of the ai/nanoid JS function in
-- Haskell
-- <https://github.com/ai/nanoid/blob/f2dc36fc83785f0d132f364769cb6e0f6ba7f083/format.js>
-- The idea is to show how imperative algorithm can be replicated in a
-- functional language using ideas from the paper, and my goal here was to achieve
-- line-to-line correspondence. There's one change compared to JS function when
-- we are breaking loop twice instead of an early return,
--
-- @
--     if (id.length === size) break
-- @
--
-- but I'm okay with it as far as it doesn't change the computational
-- complexity.
--
-- You can see that every line of Haskell code has its JS counterpart in the
-- comments. To achieve this lines correspondence I ended up using
-- 'control-monad-loop' package which offers loop constructions with early
-- termination. Function runs in @IO@ and uses 'liftIO' to glue custom
-- constructions from `control-monad-loop' package. The early return was the
-- only difficulty. The rest of the translation was pretty straightforward.
--
--   * JS constant - @let@ binding
--   * JS variable - 'Data.IORef' mutable container
--   * if without else clause - 'when' function
--
-- When implemented in Haskell we immediately see the problem with
-- 'generateIO'. It is a God-function with infinite powers, it runs in IO and
-- can perform arbitrary effects (even to launch the missiles, as they say). It
-- is definitely overpowered for a tiny UID generator. Moreover, it is hard to
-- tell what is happening inside because there is a lot of manipulations with
-- state and indexes which is hard to follow.
--
-- Looking at the signature we can tell that @IO@ is required because of
-- the stateful @random@ function it takes as first argument.
--
-- @
--     random :: Int -> IO [Int]
-- @
--
-- To eliminate @IO@ we can leverage the laziness feature of the Haskell
-- language and pass an infinite stream of integers instead. This takes us
-- forward to the 'generateST' implementation.
--
-- == Example
--
-- Module "Nanoid" provides 'randomnIO' and 'url' helpers for convenience
--
-- >>> generateIO randomnIO url 21
-- "bd3xzujGEwjzP_91sXIf2"
generateIO :: (Int -> IO [Int]) -> String -> Int -> IO String
generateIO random alphabet size = do                                       --  function (random, alphabet, size)
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
randomnIO :: Int -> IO [Int]
randomnIO n = replicateM n RNG.randomIO

-- | Example alphabet
url :: String
url = "_~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


-- |
-- = Referentially trasparent generator
--
-- Instead of effectful @random@ function 'generateST' takes an infinite stream
-- of integers @randoms :: [Int]@. The function still mutates the state inside
-- but is hidden inside the 'ST' monad, and looking at the type signature we can
-- tell that function is at least referentially transparent. By referential
-- transparency here I mean that while function has a state, this state doesn't
-- affect the result. This function behaves as a pure when observed from the
-- outside.
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
-- 'generateST' function is much better than 'generateIO' because we are sure it
-- cannot do anything except returning the result, cannot affect the outside
-- world. The algorithm itself is still a mess, but due to the referential
-- transparency, we can reason about it. We can forget the implementation
-- details and compose it with other functions being guided just by its type.
--
-- So far this function is a mechanical translation of the imperative code. As a
-- conclusion, I wanted to come up with more idiomatic Haskell program that
-- implements the same algorithm. And when I realized that this program is just
-- a pipeline that filters and transforms the input stream of integers, the
-- implementation became very simple. Here we transition to the 'generate'
-- function.
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
-- >>> (\rs -> generateST rs url 21) <$> randoms <$> getStdGen
-- "u~0IlMNwisEzQGF2jJ_7T"
generateST :: [Int] -> String -> Int -> String
generateST randoms alphabet size = runST $ do                              --  function (randoms, alphabet, size)
  let mask = (2 `rotate`
        truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1  --  const mask = (2 << Math.log(alphabet.length - 1) / Math.LN2) - 1
  let step = ceiling (1.6 * fromIntegral mask * fromIntegral size
                       / fromIntegral (length alphabet))                    --  const step = Math.ceil(1.6 * mask * size / alphabet.length)

  varId <- newSTRef ""                                                      --  var id = ''
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

-- | Return a stream random integers
randomsIO :: IO [Int]
randomsIO = RNG.randoms <$> RNG.getStdGen

-- |
-- = Pure generator
--
-- After our observation, we ended up with a simple sieve algorithm on a stream
-- of @randoms@. Compared to 'generateST' this program lacks the state, and we
-- can call it pure. Now looking at the code, it is much easier to tell that
-- this function is doing, because the whole program is a composition of three
-- functions 'filter', 'map' and 'take'.
--
-- Although I do realize that this implementation lacks batching in `step's, I
-- can assume that in the original JS program it was an optimization to minimize
-- the interactions with RNG. In our case, I'm very doubtful that it would have
-- any effect on the Haskell program.
--
-- == Test
--
-- As a sanity check, to verify that we preserve the algorithm after all the
-- transformations, we test that all functions return the same result on the
-- identical inputs. We can notice here that 'generateIO' is also an idempotent
-- function, but that wasn't clear from the beginning given its type signature.
--
-- >>> generateIO (const $ pure [100..200]) url 21
-- "yzABCDEFGHIJKLMNOPQRS"
--
-- >>> generateST [100..200] url 21
-- "yzABCDEFGHIJKLMNOPQRS"
--
-- >>> generate [100..200] url 21
-- "yzABCDEFGHIJKLMNOPQRS"
--
-- = Summary
--
-- I'm happy that we achieved all the goals set. First, we were able to
-- translate every line of the imperative algorithm into a functional
-- counterpart. The process was straightforward and almost mechanical. Second,
-- using the features of the Haskell language, after the two iterations we ended
-- up with a highly readable pure function which implements the same algorithm.
generate :: [Int] -> String -> Int -> String
generate randoms alphabet size = take size $ go randoms
  where
    mask = (2 `rotate` truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1
    go   = map (\i -> alphabet !! (i .&. mask))
      . filter (\i -> length alphabet > i .&. mask)

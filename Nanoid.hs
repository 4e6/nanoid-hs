-- | Nanoid package is a carbon copy of the ai/nanoid repository
-- https://github.com/ai/nanoid/blob/f2dc36fc83785f0d132f364769cb6e0f6ba7f083/format.js
--
-- This is an example of how imperative program can be replicated in a purely functional language
-- The exact technique is described in paper "Lazy functional state threads"
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

url :: String
url = "_~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

generatorIO :: (Int -> IO [Int]) -> String -> Int -> IO String
generatorIO random alphabet size = do                                       --  module.exports = function (random, alphabet, size) {
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
        when (length id == size) exit                                       --      if (id.length === size) return id
                                                                            --    }
    id <- liftIO $ readIORef varId                                          --
    when (length id == size) exit                                           --
                                                                            --  }
  readIORef varId

randIO :: Int -> IO [Int]
randIO n = replicateM n randomIO


generatorST :: [Int] -> String -> Int -> String
generatorST randoms alphabet size = runST $ do                              --  module.exports = function (randoms, alphabet, size) {
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
      let byte = (bytes !! i) .&. mask                                      --      var byte = bytes[i] & mask
      when (length alphabet > byte) $ do                                    --      if (alphabet[byte]) {
        liftBase $ modifySTRef varId (\id -> id ++ [alphabet !! byte])      --        id += alphabet[byte]
                                                                            --      }
        id <- liftBase $ readSTRef varId                                    --
        when (length id == size) exit                                       --      if (id.length === size) return id
                                                                            --    }
    id <- liftBase $ readSTRef varId                                        --
    when (length id == size) exit                                           --
                                                                            --  }
  readSTRef varId


generator :: Int -> String -> [Int] -> String
generator size alphabet = take size . go
  where
    mask = (2 `rotate` truncate (log (fromIntegral (length alphabet - 1)) / log 2.0)) - 1
    go   = map (\i -> alphabet !! (i .&. mask))
      . filter (\i -> length alphabet > i .&. mask)


{-
We can also check that all three implementations give the same result:

    > generatorIO (const $ pure [100..200]) url 21
      "yzABCDEFGHIJKLMNOPQRS"

    > generatorST [100..200] url 21
      "yzABCDEFGHIJKLMNOPQRS"

    > generator 21 url [100..200]
      "yzABCDEFGHIJKLMNOPQRS"

-}

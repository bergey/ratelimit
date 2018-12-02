{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Text (Text)
import           System.Clock

import qualified Data.Text as T
import qualified Data.Text.IO as T

wait :: TimeSpec
wait = fromInteger (1 * 10^9) -- nanoseconds

formatTimeSpec_ms :: TimeSpec -> Text
formatTimeSpec_ms (TimeSpec s ns) = showT s <> T.singleton '.' <> T.justifyLeft 3 '0' (showT (ns `div` 10^6)) <> T.singleton 's'

showT :: Show s => s -> Text
showT = T.pack . show

main :: IO ()
main = do
    start <- getTime Monotonic
    echoAfter start (start + wait)

echoAfter :: TimeSpec -> TimeSpec -> IO ()
echoAfter start t = do
    line <- T.getLine
    now <- getTime Monotonic
    if now > t then do
        T.putStrLn (formatTimeSpec_ms (now - start) <> ": " <> line)
        echoAfter start (now + wait)
        else echoAfter start t

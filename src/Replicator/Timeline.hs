module Replicator.Timeline (
    seconds,
    timestamp
) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)


seconds :: IO Integer
seconds = fmap (round' . realToFrac) getPOSIXTime where
    round' = round :: Double -> Integer

timestamp :: Integer -> String -> IO String
timestamp zero msg = do
    t <- seconds
    return $ mark (t - zero) ++ " " ++ msg
    where mark s = printf "%02u:%02u:%02u" h m' s' where
            (m, s') = s `divMod` 60
            (h, m') = m `divMod` 60

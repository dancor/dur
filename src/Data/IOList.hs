module Data.IOList where

import Control.Applicative
import Control.Applicative
import Prelude hiding (sequence)

data IOList a = Empty | IOValue a (IO (IOList a))

toList :: IOList a -> IO [a]
toList Empty = return []
toList (IOValue x ysM) = (x:) <$> (toList =<< ysM)

sequence :: [IO a] -> IO (IOList a)
sequence [] = return Empty
sequence (x:xs) = (flip IOValue $ sequence xs) <$> x

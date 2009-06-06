-- run the unix du utility, recursing into large subdirectories,
-- to find large files

-- TODO:
-- - it's only showing dirs again..
-- - distinguish dirs with trailing slash
-- - get laziness to work right (introduced IOList for this, but not working)
-- - check behavior on links
-- - try to support "show top N"-like functionality?

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import FUtil
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Process
import qualified Data.Map as M

data Du = Du (Int, String) [Du] deriving Show
data IOList a = Empty | IOValue a (IO (IOList a))

data Options = Options {
  optFileMinSize :: Int,
  optDirMinSize :: Int
}

-- in KB
defOpts :: Options
defOpts = Options {
  optFileMinSize = 1000,
  optDirMinSize = 1000
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "f" ["file-min-size"]
    (ReqArg (\ a o -> o {optFileMinSize = read a}) "KB")
    "Only show files >= this size",
  Option "d" ["dir-min-size"]
    (ReqArg (\ a o -> o {optDirMinSize = read a}) "KB")
    "Only show/descend into directories >= this size"
  ]

du :: [String] -> IO [(Int, String)]
du files = do
  let duLineProc s = first read . second tail . break (== '\t') $ s
  (_pIn, pOut, pErr, pId) <-
    runInteractiveProcess "du" ("-s":files) (Just ".") Nothing
  waitForProcess pId
  -- FIXME: handle err
  c <- hGetContents pOut
  let ls = lines c
  return . map duLineProc $ ls

killDotSlash ('.':'/':rest) = rest
killDotSlash rest = rest

findir :: String -> IO [String]
findir path = do
  -- FIXME: handle newlines in file names
  (_pIn, pOut, pErr, pId) <- runInteractiveProcess "find"
    [path, "-mindepth", "1", "-maxdepth", "1", "-type", "d"] (Just ".") Nothing
  -- FIXME: handle err
  c <- hGetContents pOut
  return $ lines c

duRecurse :: Int -> Int -> (Int, String) -> IO Du
duRecurse descendSize minSize k@(size, file) = do
  dus <- if size >= descendSize
    then unIOList =<< dur descendSize minSize file
    else return []
  return $ Du k dus

unIOList :: IOList a -> IO [a]
unIOList Empty = return []
unIOList (IOValue x ysIO) = liftM (x:) (unIOList =<< ysIO)

sequenceList :: [IO a] -> IO (IOList a)
sequenceList [] = return Empty
sequenceList (x:xs) = (flip IOValue $ sequenceList xs) <$> x

dur :: Int -> Int -> String -> IO (IOList Du)
dur descendSize minSize path = do
  fs <- findir path
  if null fs then return Empty else do
    dus <- du fs
    let l = filter ((>= minSize) . fst) . reverse $ sort dus
    sequenceList $ map (duRecurse descendSize minSize) l

duShow :: Int -> Du -> String
duShow n (Du (size, file) dus) = interlines $ [replicate n ' ' ++ show size ++
  "\t" ++ killDotSlash file] ++ map (duShow (n + 1)) dus

main :: IO ()
main = do
  (opts, args) <- doArgs "usage" defOpts options
  flip mapM_ (if null args then ["."] else args) $ \ path -> do
    dus <- dur (optDirMinSize opts) (optFileMinSize opts) path
    -- FIXME: find a way to print lines as they are found, not all at end
    dus' <- unIOList dus
    putStrLn . interlines $ map (duShow 0) dus'

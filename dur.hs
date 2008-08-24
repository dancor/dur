-- run the unix du utility, recursing into large subdirectories, 
-- to find large files

-- TODO:
-- - distinguish dirs with trailing slash
-- - get laziness to work right (introduced IOList for this, but not working)
-- - check behavior on links
-- - try to support "show top N"-like functionality?

import Control.Arrow
import Control.Monad
import Data.List
import qualified Data.Map as M
import System.Environment
import System.Process
import System.IO

data Du = Du (Int, String) [Du] deriving Show
data IOList a = Empty | IOValue a (IO (IOList a))

-- in K
defDescendSize = 1000000
defMinSize = 1000

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

interlines = intercalate "\n"

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

unIOList Empty = return []
unIOList (IOValue x ysIO) = do
  ys <- ysIO
  xs <- unIOList ys
  return (x:xs)

sequenceList :: [IO a] -> IO (IOList a)
sequenceList [] = return Empty
sequenceList (x:xs) = do
  y <- x
  return $ IOValue y (sequenceList xs)

dur :: Int -> Int -> String -> IO (IOList Du)
dur descendSize minSize path = do
  fs <- findir path
  if null fs then return Empty else do
    dus <- du fs
    print path
    let l = filter ((>= minSize) . fst) . reverse $ sort dus
    sequenceList $ map (duRecurse descendSize minSize) l

duShow :: Int -> Du -> String
duShow n (Du (size, file) dus) = interlines $ [replicate n ' ' ++ show size ++ 
  "\t" ++ killDotSlash file] ++ map (duShow (n + 1)) dus

main = do
  args <- getArgs
  [descendSize, minSize] <- case length args of
    0 -> return [defDescendSize, defMinSize]
    1 -> return [read $ head args, defMinSize]
    2 -> return $ map read args
    _ -> error "usage"
  dus <- dur descendSize minSize "."
  -- FIXME: find a way to print lines as they are found, not all at end
  dus' <- unIOList dus
  putStrLn . interlines . map (duShow 0) $ dus'

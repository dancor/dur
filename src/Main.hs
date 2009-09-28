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
import Control.Monad.Maybe
import Data.Function
import Data.Graph
import Data.List
import Data.Tree
import FUtil
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
import qualified Data.IOList as IOL
import qualified Data.Map as M

data Options = Options {
  optFileMinSize :: Integer,
  optDirMinSize :: Integer
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

data DuNodeType = File | Dir Bool  -- had dir been explored
  deriving (Show, Eq)

data DuNode = DuNode {
  duSize :: Integer,  -- in 1K blocks
  duName :: String,
  duType :: DuNodeType
  }
  deriving (Show, Eq)

duDir :: Options -> String -> IO [DuNode]
duDir opts path = do
  fs <- filter (\ x -> x /= "." && x /= "..") <$> getDirectoryContents path
  -- TODO: .du.out caching?
  (_pIn, pOut, pErr, pId) <-
    runInteractiveProcess "du" ("-s":fs) (Just path) Nothing
  waitForProcess pId
  -- FIXME: handle err
  sizesAndFs <- reverse . sortBy (compare `on` fst) .
    filter ((>= optFileMinSize opts) . fst) . map ((read *** tail) .
    break (== '\t')) . lines <$> hGetContents pOut
  flip mapM sizesAndFs $ \ (s, f) -> DuNode s f <$>
    ifM (doesDirectoryExist $ path </> f) (return $ Dir False) (return File)

drawTreeShort :: Tree String -> String
drawTreeShort = unlines . map head . splitN 2 . lines . drawTree

duNodePretty :: DuNode -> [Char]
duNodePretty (DuNode s n t) =
  show s ++ "\t" ++ n ++ if t == File || n == "/" then "" else "/"

expandTree :: Options -> String -> Tree DuNode -> MaybeT IO (Tree DuNode)
expandTree opts path (Node (DuNode s n t) forest) = do
  when (s < optDirMinSize opts) $ fail ""
  case t of
    File -> fail ""
    Dir False ->
      io $ treeify (DuNode s n $ Dir True) <$> duDir opts (path </> n)
    Dir True -> Node (DuNode s n t) <$> expandForest opts (path </> n) forest

expandForest :: Options -> String -> [Tree DuNode] -> MaybeT IO [Tree DuNode]
expandForest _ _ [] = fail ""
expandForest opts path (t:rest) = do
  tNewMb <- io . runMaybeT $ expandTree opts path t
  maybe ((t:) <$> expandForest opts path rest) (return . (:rest)) tNewMb

expandLoop :: Options -> String -> Tree DuNode -> IO ()
expandLoop opts path tree = do
  putStrLn . drawTreeShort . fmap duNodePretty $ tree
  hFlush stdout
  tMb <- runMaybeT $ expandTree opts path tree
  case tMb of
    Just t -> expandLoop opts path t
    Nothing -> return ()

treeify :: a -> [a] -> Tree a
treeify a = Node a . map (flip Node [])

main :: IO ()
main = do
  (opts, args) <- doArgs "usage" defOpts options
  let
    d = case args of
      [] -> "."
      [argD] -> argD
      _ -> error "usage"
  duRes <- duDir opts d
  expandLoop opts d $
    treeify (DuNode (sum $ map duSize duRes) d $ Dir True) duRes


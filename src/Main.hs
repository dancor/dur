import Control.Applicative
import Data.Char
import Data.Word
import qualified Data.ByteString as B
import Data.ByteString.Nums.Careless.Int

newline, tab :: Word8
newline = fromIntegral (ord '\n')
tab     = fromIntegral (ord '\t')

main :: IO ()
main = B.interact (B.concat . map (<> B.singleton newline) . 
    filter ((>= 100000) . strict_unsigned . fst . B.break (== tab)) .
    B.split newline)

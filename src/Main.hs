import Control.Applicative
import Data.Char
import qualified Data.ByteString as BS
import Data.ByteString.Nums.Careless.Int

fI = fromIntegral
nl = fI $ ord '\n'
tb = fI $ ord '\t'

main = BS.interact (BS.concat . map (`BS.append` BS.singleton nl) . 
  filter ((>= 100000) . strict_unsigned . fst . BS.breakByte tb) .
  BS.split nl)

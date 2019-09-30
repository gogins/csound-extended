import Csound
import Control.Exception
import Control.DeepSeq
import System.Info
import Foreign
import Foreign.C.Types
import System.Posix.DynamicLinker

main = do
    print csoundLib

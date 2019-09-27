import Csound
import Control.Exception
import Control.DeepSeq
import System.Info
import Foreign
import Foreign.C.Types
import System.Posix.DynamicLinker

main = do
    csoundAPI <- loadCsound "/usr/lib/libcsound64.so" [RTLD_GLOBAL]
    print csoundAPI

    --csoundInstance :: IntPtr 
    csoundInstance <- csoundAPI.csoundCreate Null

    --print csoundInstance
    return 0

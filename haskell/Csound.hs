{-|
Module      : Csound
Description : Haskell FFI interface to Csound, including play for Euterpea.
Copyright   : (c) Michael Gogins, 2019.
License     : LGPL-v2.1
Maintainer  : michael.gogins@gmail.com
Stability   : experimental
Portability : POSIX

This module implements a dynamic FFI binding to the core of the Csound API on
POSIX systems. The module includes a play function for rendering Euterpea Music 
objects.
-}

{-# LANGUAGE DataKinds, QuasiQuotes, EmptyDataDecls, ExtendedDefaultRules, ForeignFunctionInterface #-}
module Csound (libCsound, 
    csoundCompileCsdText,
    csoundCreate,
    csoundDestroy,
    csoundPerform,
    csoundReadScore,
    csoundSetControlChannel,
    csoundSetOption,
    csoundStart,
    csoundStop,
    --playCsound
    ) where
--import Euterpea
--import Euterpea.Music.Note.Music
--import Euterpea.Music.Note.MoreMusic
--import Euterpea.Music.Note.Performance
import Control.Exception
import Control.DeepSeq
import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe
import System.Info
import System.Posix.DynamicLinker
import Text.RawString.QQ

-- Dynamically load the Csound shared library. We assume that it is built for 
-- 64 bit CPU architecture.

libCsound :: DL
libCsound = unsafePerformIO $ dlopen "libcsound64.so" [RTLD_LAZY, RTLD_GLOBAL]

-- Follow this pattern for the other Csound API functions.
-- 'unsafePerformIO' in effect removes the 'IO' qualifier from the return 
-- value. We don't plan to change such values so I think that's OK.
-- We use an implicit type cast of unsigned long int (64 bits) for pointers.

csoundCreateAddress = unsafePerformIO $ dlsym libCsound "csoundCreate"
type CsoundCreate = Word64 -> IO Word64

-- 'unsafe' _is_ unsafe but, I think OK here and should quite speed things up.
-- The 'ccall "dynamic"' tells Haskell how to bind the address of a foreign 
-- function to a Haskell function of the declared type.

foreign import ccall unsafe "dynamic" csoundCreateWrapper :: (FunPtr CsoundCreate) -> CsoundCreate
csoundCreate :: CsoundCreate
csoundCreate = csoundCreateWrapper csoundCreateAddress

csoundCompileCsdTextAddress = unsafePerformIO $ dlsym libCsound "csoundCompileCsdText"
type CsoundCompileCsdTextC = Word64 -> CString -> IO Word64
foreign import ccall unsafe "dynamic" csoundCompileCsdTextWrapper :: (FunPtr CsoundCompileCsdTextC) -> CsoundCompileCsdTextC
csoundCompileCsdTextC :: CsoundCompileCsdTextC
csoundCompileCsdTextC = csoundCompileCsdTextWrapper csoundCompileCsdTextAddress
csoundCompileCsdText csound csd = withCString csd $ \ccsd -> csoundCompileCsdTextC csound ccsd

csoundPerformAddress = unsafePerformIO $ dlsym libCsound "csoundPerform"
type CsoundPerform = Word64 -> IO Word64
foreign import ccall unsafe "dynamic" csoundPerformWrapper :: (FunPtr CsoundPerform) -> CsoundPerform
csoundPerform :: CsoundPerform
csoundPerform = csoundPerformWrapper csoundPerformAddress

csoundStartAddress = unsafePerformIO $ dlsym libCsound "csoundStart"
type CsoundStart = Word64 -> IO Word64
foreign import ccall unsafe "dynamic" csoundStartWrapper :: (FunPtr CsoundStart) -> CsoundStart
csoundStart :: CsoundStart
csoundStart = csoundStartWrapper csoundStartAddress

csoundSetOptionAddress = unsafePerformIO $ dlsym libCsound "csoundSetOption"
type CsoundSetOptionC = Word64 -> CString -> IO Word64
foreign import ccall unsafe "dynamic" csoundSetOptionWrapper :: (FunPtr CsoundSetOptionC) -> CsoundSetOptionC
csoundSetOptionC :: CsoundSetOptionC
csoundSetOptionC = csoundSetOptionWrapper csoundSetOptionAddress
csoundSetOption csound option = withCString option $ \coption -> csoundSetOptionC csound coption

csoundStopAddress = unsafePerformIO $ dlsym libCsound "csoundStop"
type CsoundStop = Word64 -> IO Word64
foreign import ccall unsafe "dynamic" csoundStopWrapper :: (FunPtr CsoundStop) -> CsoundStop
csoundStop :: CsoundStop
csoundStop = csoundStopWrapper csoundStopAddress

csoundReadScoreAddress = unsafePerformIO $ dlsym libCsound "csoundReadScore"
type CsoundReadScoreC = Word64 -> CString -> IO Word64
foreign import ccall unsafe "dynamic" csoundReadScoreWrapper :: (FunPtr CsoundReadScoreC) -> CsoundReadScoreC
csoundReadScoreC :: CsoundReadScoreC
csoundReadScoreC = csoundReadScoreWrapper csoundReadScoreAddress
csoundReadScore csound score = withCString score $ \cscore -> csoundReadScoreC csound cscore

csoundSetControlChannelAddress = unsafePerformIO $ dlsym libCsound "csoundSetControlChannel"
type CsoundSetControlChannelC = Word64 -> CString -> Double -> IO ()
foreign import ccall unsafe "dynamic" csoundSetControlChannelWrapper :: (FunPtr CsoundSetControlChannelC) -> CsoundSetControlChannelC
csoundSetControlChannelC :: CsoundSetControlChannelC
csoundSetControlChannelC = csoundSetControlChannelWrapper csoundSetControlChannelAddress
csoundSetControlChannel csound name value = withCString name $ \cname -> csoundSetControlChannelC csound cname value

csoundDestroyAddress = unsafePerformIO $ dlsym libCsound "csoundDestroy"
type CsoundDestroy = Word64 -> IO Word64
foreign import ccall unsafe "dynamic" csoundDestroyWrapper :: (FunPtr CsoundDestroy) -> CsoundDestroy
csoundDestroy :: CsoundDestroy
csoundDestroy = csoundDestroyWrapper csoundDestroyAddress

{-- 
First we define a subset of the Csound API for Haskell. The functions that we 
need to call are more or less the same as the ones in csound.node. We will 
pass only an opaque Csound pointer, ints, doubles, and strings to Csound. 
Some strings will be multi-line literals, which can be implemented in Haskell 
using quasiquotes.

Usage: Load the Csound shared library, create an instance of Csound, 
and pass that instance to all other Csound functions.

See https://stackoverflow.com/questions/25093052/calling-a-c-function-in-haskell-comparing-to-python
https://www.schoolofhaskell.com/user/icelandj/dynamically-link-against-library-in-ghci
https://stackoverflow.com/questions/997738/haskell-ffi-calling-funptrs
https://rosettacode.org/wiki/Call_a_function_in_a_shared_library#Haskell
https://stackoverflow.com/questions/15203847/calling-a-c-function-that-is-referenced-by-a-c-pointer-to-that-function-with-has

csoundCleanup
csoundCompileCsd
csoundCompileCsdText
csoundCompileOrc
csoundCreate
csoundDestroy
csoundEvalCode
csoundGetControlChannel
csoundGetKsmps
csoundGetMetadata
csoundGetNchnls
csoundGetScoreTime
csoundGetVersion
csoundGetSr
csoundInputMessage
csoundIsScorePending
csoundMessage
csoundPerform
csoundPerformAndPostProcess
csoundReadScore
csoundReset
csoundRewindScore
csoundScoreEvent
csoundSetControlChannel
csoundSetMessageCallback
csoundSetMetadata
csoundSetOption
csoundSetOutput
csoundSetScorePending
csoundStart
csoundStop

Next we create a Csound play function for Euterpea.

The pattern for play functions is:

> playStrict :: (Performable a, NFData a) => PlayParams -> Music a -> IO ()
> playStrict p m = m `deepseq`
>     let x = toMidi (fst $ perfDur (pmap p) (ctxt p) m) defUpm 
>     in  x `deepseq` playM' (devID p) x

Note, deepseq performs a deep traversal of the structure m, here a Music.
NFData is simply the class of fully evaluatable types.

Here, rather than send to a MIDI device in real time, we will write i statements 
to a Csound score, and perform that score using the csd and options. Csound sorts 
the score.

toIStatement :: Event a -> IO (String)
toIStatement e = do 
    lyne <- printf "i s% 9.4f% 9.4f% 9.4f% 9.4f%%\n" (eInstName e) (eTime e) (eDurT e) (ePitch e) (eVol e)
    print lyne

-- E.g. playCsound music csdText [options]
playCsound :: (Performable a, NFData a) => Music a -> String -> [String] -> IO ()
playCsound csd music = music `deepseq`
    -- first in tuple, of that performance duration with perfmap of p, context of p, default user patch map.
    let i_statement = toIStatement (fst $ perfDur (pmap p) (ctxt p) m) defUpm 
    in i_statement `deepseq` print' x
    -- Append i to a list.
    -- return the concatenation of the list.
    
--}


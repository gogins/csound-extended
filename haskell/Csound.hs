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
    csoundCreate,
    csoundCompileCsdText,
    csoundPerform,
    csoundStart
    ) where
import Euterpea
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
type CsoundCreate = CULong -> IO CULong

-- 'unsafe' _is_ unsafe but, I think OK here and should quite speed things up.
-- The 'ccall "dynamic"' tells Haskell how to bind the address of a foreign 
-- function to a Haskell function of the declared type.

foreign import ccall unsafe "dynamic" csoundCreateWrapper :: (FunPtr CsoundCreate) -> CsoundCreate
csoundCreate :: CsoundCreate
csoundCreate = csoundCreateWrapper csoundCreateAddress

csoundCompileCsdTextAddress = unsafePerformIO $ dlsym libCsound "csoundCompileCsdText"
type CsoundCompileCsdTextC = CULong -> CString -> IO CULong
foreign import ccall unsafe "dynamic" csoundCompileCsdTextWrapper :: (FunPtr CsoundCompileCsdTextC) -> CsoundCompileCsdTextC
csoundCompileCsdTextC :: CsoundCompileCsdTextC
csoundCompileCsdTextC = csoundCompileCsdTextWrapper csoundCompileCsdTextAddress
--type CsoundCompileCsdText = CULong -> String -> IO CULong
csoundCompileCsdText csound csd = withCString csd $ \ccsd -> csoundCompileCsdTextC csound ccsd

csoundPerformAddress = unsafePerformIO $ dlsym libCsound "csoundPerform"
type CsoundPerform = CULong -> IO CULong
foreign import ccall unsafe "dynamic" csoundPerformWrapper :: (FunPtr CsoundPerform) -> CsoundPerform
csoundPerform :: CsoundPerform
csoundPerform = csoundPerformWrapper csoundPerformAddress

csoundStartAddress = unsafePerformIO $ dlsym libCsound "csoundStart"
type CsoundStart = CULong -> IO CULong
foreign import ccall unsafe "dynamic" csoundStartWrapper :: (FunPtr CsoundStart) -> CsoundStart
csoundStart :: CsoundStart
csoundStart = csoundStartWrapper csoundStartAddress


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
--}

    
{--
foreign import ccall "csoundCleanup" xxx :: xxx -> IO xxx
foreign import ccall "csoundCompileCsd
foreign import ccall "csoundCompileCsdText
foreign import ccall "csoundCompileOrc
foreign import ccall "csoundCreate
foreign import ccall "csoundDestroy
foreign import ccall "csoundEvalCode
foreign import ccall "csoundGetControlChannel
foreign import ccall "csoundGetKsmps
foreign import ccall "csoundGetMetadata
foreign import ccall "csoundGetNchnls
foreign import ccall "csoundGetScoreTime
foreign import ccall "csoundGetVersion
foreign import ccall "csoundGetSr
foreign import ccall "csoundInputMessage
foreign import ccall "csoundIsScorePending
foreign import ccall "csoundMessage
foreign import ccall "csoundPerform
foreign import ccall "csoundPerformAndPostProcess
foreign import ccall "csoundReadScore
foreign import ccall "csoundReset
foreign import ccall "csoundRewindScore
foreign import ccall "csoundScoreEvent
foreign import ccall "csoundSetControlChannel
foreign import ccall "csoundSetMessageCallback
foreign import ccall "csoundSetMetadata
foreign import ccall "csoundSetOption
foreign import ccall "csoundSetOutput
foreign import ccall "csoundSetScorePending
foreign import ccall "csoundStart
foreign import ccall "csoundStop
--}

{--
Next we create Csound player for Euterpea.

The IStatement type takes a note duration (Dur), a pitch number (AbsPitch), 
and a volume from 0-127 (Volume) and returns a Vivid synthesizer. The 
VInstr type mirrors the Instr type used in Euterpea's offline sound 
synthesis.

> type Params = [Double]
> type VInstr = Dur -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]

> data ReleaseType = 
>     FixedDuration -- sound duration unaffected by note duration (percussive, frees itself)
>     | Internal -- sound duration handled within synth def with envelopes (frees itself)
>     | External -- fade out and free expected to be handled externally to synth def
>     deriving (Eq, Show, Ord, Enum)

> data SynthInfo = SynthInfo {
>     synthDef :: VInstr, -- sound generation functionforeign import ccall "csound
>     releaseTime :: Dur, -- what is the expected release time after note offforeign import ccall "csound?
>     releaseType :: ReleaseType -- does the sound depend on note durationforeign import ccall "csound? 
>     }

> toSynth :: SynthInfo -> AbsPitch -> Volume -> Params -> SynthDef '["gate", "fadeSecs"]
> toSynth e ap v p = (synthDef e) (releaseTime e) ap v pforeign import ccall "csound

Notes on ReleaseType:
- Synthesizers using FixedDuration are expected to NOT free themselves via envelopes. 
  They will be freed by the playback algorithm. Freeing internally with envelopes 

Lookup table type for synthesizers by an Instrumentname. Note that the 
durDependent field should be True for sounds with a sustain region. 
Percussive sounds with no sustain that are primarily release-based should 
have durDependent=False to avoid excessive cycles being spent on the sound.

> type SynthTable = [(InstrumentName, SynthInfo)]

A default sound to use if no other synthesizer is defined. The 
default sound is a sine wave at a given note's frequency with 
a maximum amplitude of 0.3. The note's duration is ignored, 
meaning that the SynthInfo will have a releaseType of FixedDuration.

> defaultSound :: VInstr
> defaultSound _ ap _ _ = sd (1 ::I "gate", 0 ::I "fadeSecs") $ do
>    s <- 0.3 ~* sinOsc (freq_ $ midiCPS ap)
>    e <- envGen (env 1.0 [(0.0,0.25)] Curve_Linear) FreeEnclosing
>    out 0 [s ~* e, s ~* e]

> defaultSynth = SynthInfo defaultSound 0.25 FixedDuration

Playback functions for sending to Vivid synths. Sequentially infinite foreign import ccall "csound
Euterpea values are permitted and Ctrl+C then Enter will exit out of the 
cycle.
   
> playV :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
> playV t m = onException (playMEvs t 0 $ perform m) cmdPeriod

Euterpea's PlayParams can also be used to provide a customized performance
algorithm. The playVC function allows use of the strict parameter in the
PlayParams datatype. The playVS function is just a shorthand way of doing this.

> instance NFData MEvent where
>     rnf (MEvent t i ap d v params) = 
>         rnf t `seq` rnf i `seq` rnf ap `seq` rnf d `seq` rnf v `seq` rnf params

> playVC :: (ToMusic1 a) => SynthTable -> PlayParams -> Music a -> IO ()
> playVC t pp m = 
>     let x = (perfAlg pp . toMusic1) m
>     in  if strict pp then deepseq x $ onException (playMEvs t 0 x) cmdPeriod
>         else onException (playMEvs t 0 x) cmdPeriod
    
> playVS :: (ToMusic1 a) => SynthTable -> Music a -> IO ()
> playVS t = playVC t defParams{strict=True}

Supporting definitions for handling of MEvents in the functions above.

> playEvent :: VividAction m => SynthTable -> MEvent -> m()
> playEvent insts me = 
>     let x = lookup (eInst me) insts
>         eSyn = maybe defaultSynth id x
>         sd = toSynth eSyn (ePitch me) (eVol me) (eParams me)
>         waitTime = if releaseType eSyn == Internal then eDur me + releaseTime eSyn else
>                    if releaseType eSyn == External then eDur me else releaseTime eSyn
>     in  if releaseType eSyn == External then do
>             s0 <- synth sd ()
>             wait $ fromRational waitTime
>             set s0 (fromRational (releaseTime eSyn) :: I "fadeSecs")
>             release s0
>             wait $ releaseTime eSyn
>         else do -- for fixed and internal release cases
>             s0 <- synth sd ()
>             wait (fromRational waitTime)
>             -- Note: call to free removed. Synth defs are expected to do this themselves now.
     
> playMEvs :: VividAction m => SynthTable -> PTime -> [MEvent] -> m ()
> playMEvs insts cTime [] = return ()
> playMEvs insts cTime [me] = fork $ do
>     wait $ fromRational (eTime me - cTime)
>     playEvent insts me
> playMEvs insts cTime (me1:me2:mevs) = do
>     wait $ fromRational (eTime me1 - cTime)
>     fork $ playEvent insts me1 
>     playMEvs insts (eTime me1) (me2:mevs)

Writing to a WAV file

> writeWavV :: (ToMusic1 a) => FilePath -> SynthTable -> Music a -> IO ()
> writeWavV outFile t m = 
>     if os == "mingw32" then writeNRTWin outFile (playMEvs t 0 $ perform m)
>     else writeNRT outFile (playMEvs t 0 $ perform m)
--}
<CsoundSynthesizer>
<CsLicense>

Author: Michael Gogins
License: Lesser GNU General Public License version 2

This file demonstrates a module system for Csound.

</CsLicense>
<CsOptions>
-d -m163 -odac
</CsOptions>
<CsInstruments>

; Initialize the global variables.

sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1

; Connect up instruments and effects to create a signal flow graph.

connect "STKBowed",     "outleft",      "ReverbSC",     "inleft"
connect "STKBowed",     "outright",     "ReverbSC",     "inright"

connect "Harpsichord",  "outleft",     "ReverbSC",     	"inleft"
connect "Harpsichord",  "outright",    "ReverbSC",     	"inright"

connect "ReverbSC", 	"outleft",     "Compressor",    "inleft"
connect "ReverbSC", 	"outright",    "Compressor",    "inright"

connect "Compressor",   "outleft",     "MasterOutput",  "inleft"
connect "Compressor",   "outright",    "MasterOutput",  "inright"

; Turn on the "effect" units in the signal flow graph.

alwayson "ReverbSC"
alwayson "Compressor"
alwayson "MasterOutput"

#include "Harpsichord.inc"
#include "STKBowed.inc"
#include "ReverbSC.inc"
#include "Compressor.inc"
#include "MasterOutput.inc"

; Override default values of control channels.

gk_Reverb_feedback init .9
gk_Compressor_threshhold init .6

</CsInstruments>
<CsScore>

; Not necessary to activate "effects" or create f-tables in the score!
; Overlapping notes to create new instances of instruments.

i 1 1 5 60 85 .25
i 1 2 5 64 80 .25
i 2 3 5 67 75 .75
i 2 4 5 71 70 .75
e 10
</CsScore>
</CsoundSynthesizer>

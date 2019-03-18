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

sr = 44100
ksmps = 100
nchnls = 2
0dbfs = 1

; Connect up instruments and effects to create a signal flow graph.

connect "STKBowed",     "outleft",      "Reverb",     	"inleft"
connect "STKBowed",     "outright",     "Reverb",     	"inright"

connect "Harpsichord",  "outleft",     "Reverb",     	"inleft"
connect "Harpsichord",  "outright",    "Reverb",     	"inright"

connect "Reverb", 		"outleft",     "Compressor",    "inleft"
connect "Reverb", 		"outright",    "Compressor",    "inright"

connect "Compressor",   "outleft",     "MasterOutput",  "inleft"
connect "Compressor",   "outright",    "MasterOutput",  "inright"

; Turn on the "effect" units in the signal flow graph.

alwayson "Controllers"
alwayson "Reverb"
alwayson "Compressor"
alwayson "MasterOutput"

#include "Harpsichord.inc"
#include "STKBowed.inc"
#include "Reverb.inc"
#include "Compressor.inc"
#include "MasterOutput.inc"

; Set default values of control channels,
; this is necessary in case the user of this orchestra does not
; otherwise set these values.

chnset .95, "gk_Reverb_feedback"

instr Controllers
gk_STKBowed_level chnget "gk_STKBowed_level"
gk_STKBowed_level chnget "gk_STKBowed_bow_pressure"
gk_STKBowed_level chnget "gk_STKBowed_vibrato_level"
gk_Harpsichord_level chnget "gk_Harpsichord_level"
gk_Reverb_feedback chnget "gk_Reverb_feedback"
gk_MasterOutput_level chnget "gk_MasterOutput_level"
endin

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

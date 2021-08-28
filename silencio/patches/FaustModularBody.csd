<CsoundSynthesizer>
<CsOptions>
-d -odac -m165
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1

gi_faust_compiled faustcompile {{

declare name "ModularInterpInstrMidi";
declare description "String instrument with a modular body";
declare license "MIT";
declare copyright "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan";

import("stdfaust.lib");

process = pm.modularInterpInstr_ui_MIDI <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_faust_compiled

gi_faust_compiled1 faustcompile {{

declare name "ModularInterpInstrMidi";
declare description "String instrument with a modular body";
declare license "MIT";
declare copyright "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan";

import("stdfaust.lib");

process = pm.modularInterpInstr_ui_MIDI <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_faust_compiled1

gk_FaustModularBody_shape chnexport "gk_FaustModularBody_shape", 3
gk_FaustModularBody_scale chnexport "gk_FaustModularBody_scale", 3
gk_FaustModularBody_pluck_position chnexport "gk_FaustModularBody_pluck_position", 3
gk_FaustModularBody_gain chnexport "gk_FaustModularBody_gain", 3
gk_FaustModularBody_shape init .95
gk_FaustModularBody_scale init .25
gk_FaustModularBody_pluck_position init .95
gk_FaustModularBody_gain init .75

instr FaustModularBody
i_faust_dsp faustdsp gi_faust_compiled
k_frequency cpsmidinn p4
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "shape", gk_FaustModularBody_shape
faustctl i_faust_dsp, "scale", gk_FaustModularBody_scale
faustctl i_faust_dsp, "pluckPosition", gk_FaustModularBody_pluck_position
faustctl i_faust_dsp, "gain", gk_FaustModularBody_gain
k_gain = p5 / 127
faustctl i_faust_dsp, "outGain", k_gain
faustctl i_faust_dsp, "gate", k(1)
a_left, a_right faustplay i_faust_dsp
outs a_left, a_right
endin


</CsInstruments>
<CsScore>
i1 1 5 36 80
i1 2 5 52 70
i1 3 5 55 73
i1 4 5 86 100
</CsScore>
</CsoundSynthesizer>



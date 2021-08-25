<CsoundSynthesizer>
<CsOptions>
-d -odac -m165
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1.5

gi_faust_compiled faustcompile {{
declare name "GermanChurchBell";
declare description "German church bell physical model.";
declare license "MIT";
declare copyright "(c)Romain Michon, CCRMA (Stanford University), GRAME";

import("stdfaust.lib");

process = pm.germanBell_ui <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\" -lv 1", 1

print gi_faust_compiled

instr 1
i_faust_dsp faustdsp gi_faust_compiled
faustctl i_faust_dsp, "strikePosition", 2
faustctl i_faust_dsp, "strikeCutOff", 5000
faustctl i_faust_dsp, "strikeSharpness", .5
faustctl i_faust_dsp, "gain", .5
faustctl i_faust_dsp, "gate", k(1)

a_left, a_right faustplay i_faust_dsp
outs a_left, a_right
endin


</CsInstruments>
<CsScore>
i1 0 5 5 150
</CsScore>
</CsoundSynthesizer>



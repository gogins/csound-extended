gi_FaustJPReverb_compiled faustcompile {{

declare name "jprev";
declare version "0.1";
declare author "GRAME";
declare description "Jprev demo application.";

import("stdfaust.lib");

process = dm.jprev_demo;	

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustJPReverb_compiled

gk_FaustJPReverb_level chnexport "gk_FaustJPReverb_level", 3
gk_FaustJPReverb_earlyDiff chnexport "gk_FaustJPReverb_earlyDiff", 3
gk_FaustJPReverb_size chnexport "gk_FaustJPReverb_size", 3
gk_FaustJPReverb_t60 chnexport "gk_FaustJPReverb_t60", 3
gk_FaustJPReverb_damp chnexport "gk_FaustJPReverb_damp", 3
gk_FaustJPReverb_lowX chnexport "gk_FaustJPReverb_lowX", 3
gk_FaustJPReverb_midX chnexport "gk_FaustJPReverb_midX", 3
gk_FaustJPReverb_highX chnexport "gk_FaustJPReverb_highX", 3
gk_FaustJPReverb_lowBand chnexport "gk_FaustJPReverb_lowBand", 3
gk_FaustJPReverb_highBand chnexport "gk_FaustJPReverb_highBand", 3
gk_FaustJPReverb_mDepth chnexport "gk_FaustJPReverb_mDepth", 3
gk_FaustJPReverb_mFreq chnexport "gk_FaustJPReverb_mFreq", 3

gk_FaustJPReverb_level init 0
gk_FaustJPReverb_earlyDiff init .707
gk_FaustJPReverb_size init .83
gk_FaustJPReverb_t60 init 8.8
gk_FaustJPReverb_damp init .699
gk_FaustJPReverb_lowX init 1
gk_FaustJPReverb_midX init 1
gk_FaustJPReverb_highX init 1
gk_FaustJPReverb_lowBand init 500
gk_FaustJPReverb_highBand init 2000
gk_FaustJPReverb_mDepth init .1
gk_FaustJPReverb_mFreq init 2
gk_FaustJPReverb_midi_dynamic_range init 20

instr FaustJPReverb
a_inleft inleta "inleft"
a_inright inleta "inright"
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustJPReverb_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 110
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustJPReverb_level)
i_faust_dsp faustdsp gi_FaustJPReverb_compiled
k_frequency cpsmidinn i_midi_key
k_gain = i_midi_velocity
faustctl i_faust_dsp, "earlyDiff", gk_FaustJPReverb_earlyDiff
faustctl i_faust_dsp, "size", gk_FaustJPReverb_size
faustctl i_faust_dsp, "t60", gk_FaustJPReverb_t60
faustctl i_faust_dsp, "damp", gk_FaustJPReverb_damp
faustctl i_faust_dsp, "lowX", gk_FaustJPReverb_lowX
faustctl i_faust_dsp, "midX", gk_FaustJPReverb_midX
faustctl i_faust_dsp, "highX", gk_FaustJPReverb_highX
faustctl i_faust_dsp, "lowBand", gk_FaustJPReverb_lowBand
faustctl i_faust_dsp, "highBand", gk_FaustJPReverb_highBand
faustctl i_faust_dsp, "mDepth", gk_FaustJPReverb_mDepth
faustctl i_faust_dsp, "mFreq", gk_FaustJPReverb_mFreq
a_left, a_right faustplay i_faust_dsp, a_inleft, a_inright
k_gain = ampdb(gk_FaustJPReverb_level)
outleta "outleft", a_left * k_gain
outleta "outright", a_right * k_gain
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, i_midi_key, p5, p7, active(p1)
endin




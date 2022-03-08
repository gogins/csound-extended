gi_FaustGreyholeReverb_compiled faustcompile {{

declare name "greyhole";
declare version "0.1";
declare author "GRAME";
declare description "Greyhole demo application.";

import("stdfaust.lib");
dm = library("demos.lib");

process = dm.greyhole_demo;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustGreyholeReverb_compiled

gk_FaustGreyholeReverb_level chnexport "gk_FaustGreyholeReverb_level", 3
gk_FaustGreyholeReverb_delayTime chnexport "gk_FaustGreyholeReverb_delayTime", 3
gk_FaustGreyholeReverb_damping chnexport "gk_FaustGreyholeReverb_damping", 3
gk_FaustGreyholeReverb_size chnexport "gk_FaustGreyholeReverb_size", 3
gk_FaustGreyholeReverb_diffusion chnexport "gk_FaustGreyholeReverb_diffusion", 3
gk_FaustGreyholeReverb_feedback chnexport "gk_FaustGreyholeReverb_feedback", 3
gk_FaustGreyholeReverb_modDepth chnexport "gk_FaustGreyholeReverb_modDepth", 3
gk_FaustGreyholeReverb_modFreq chnexport "gk_FaustGreyholeReverb_modFreq", 3
gk_FaustGreyholeReverb_midi_dynamic_range chnexport "gk_FaustGreyholeReverb_midi_dynamic_range", 3

gk_FaustGreyholeReverb_level init 0
gk_FaustGreyholeReverb_delayTime init .2
gk_FaustGreyholeReverb_damping init 0
gk_FaustGreyholeReverb_size init 1
gk_FaustGreyholeReverb_diffusion init .5
gk_FaustGreyholeReverb_feedback init .9
gk_FaustGreyholeReverb_modDepth init .1
gk_FaustGreyholeReverb_modFreq init 2
gk_FaustGreyholeReverb_midi_dynamic_range init 12

instr FaustGreyholeReverb
a_inleft inleta "inleft"
a_inright inleta "inright"
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustGreyholeReverb_midi_dynamic_range)
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
k_gain = ampdb(gk_FaustGreyholeReverb_level)
i_faust_dsp faustdsp gi_FaustGreyholeReverb_compiled
k_frequency cpsmidinn i_midi_key
k_gain = i_midi_velocity
faustctl i_faust_dsp, "delayTime", gk_FaustGreyholeReverb_delayTime
faustctl i_faust_dsp, "damping", gk_FaustGreyholeReverb_damping
faustctl i_faust_dsp, "size", gk_FaustGreyholeReverb_size
faustctl i_faust_dsp, "diffusion", gk_FaustGreyholeReverb_diffusion
faustctl i_faust_dsp, "feedback", gk_FaustGreyholeReverb_feedback
faustctl i_faust_dsp, "modDepth", gk_FaustGreyholeReverb_modDepth
faustctl i_faust_dsp, "modFreq", gk_FaustGreyholeReverb_modFreq
a_left, a_right faustplay i_faust_dsp, a_inleft, a_inright
k_gain = ampdb(gk_FaustGreyholeReverb_level)
outleta "outleft", a_left * k_gain
outleta "outright", a_right * k_gain
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, i_midi_key, p5, p7, active(p1)
endin




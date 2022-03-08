gi_FaustGermanBell_compiled faustcompile {{

declare name "GermanChurchBell";
declare description "German church bell physical model.";
declare license "MIT";
declare copyright "(c)Romain Michon, CCRMA (Stanford University), GRAME";

import("stdfaust.lib");

process = pm.germanBell_ui <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustGermanBell_compiled

gk_FaustGermanBell_level chnexport "gk_FaustGermanBell_level", 3
gk_FaustGermanBell_pan chnexport "gk_FaustGermanBell_pan", 3
gk_FaustGermanBell_strikePosition chnexport "gk_FaustGermanBell_strikePosition", 3
gk_FaustGermanBell_strikeCutOff chnexport "gk_FaustGermanBell_strikeCutOff", 3
gk_FaustGermanBell_strikeSharpness chnexport "gk_FaustGermanBell_strikeSharpness", 3
gk_FaustGermanBell_gain chnexport "gk_FaustGermanBell_gain", 3
gk_FaustGermanBell_level init .5
gk_FaustGermanBell_pan init .5
gk_FaustGermanBell_strikePosition init 4
gk_FaustGermanBell_strikeCutOff init 4411
gk_FaustGermanBell_strikeSharpness init 4.49
gk_FaustGermanBell_gain init .49
gk_FaustGermanBell_midi_dynamic_range init 12
instr FaustGermanBell
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustGermanBell_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 124
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustGermanBell_level)
i_faust_dsp faustdsp gi_FaustGermanBell_compiled
k_frequency cpsmidinn i_midi_key
k_gain = i_midi_velocity
faustctl i_faust_dsp, "strikePosition", gk_FaustGermanBell_strikePosition
faustctl i_faust_dsp, "strikeCutOff", gk_FaustGermanBell_strikeCutOff
faustctl i_faust_dsp, "strikeSharpness", gk_FaustGermanBell_strikeSharpness
faustctl i_faust_dsp, "gain", gk_FaustGermanBell_gain
faustctl i_faust_dsp, "gate", k(1)
a_left, a_right faustplay i_faust_dsp
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_left + a_right
a_signal = a_signal * i_amplitude * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin



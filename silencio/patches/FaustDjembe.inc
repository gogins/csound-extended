gi_FaustDjembe_compiled faustcompile {{

declare name "ModularInterpInstrMidi";
declare description "String instrument with a modular body";
declare license "MIT";
declare copyright "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan";

import("stdfaust.lib");

process = pm.djembe_ui_MIDI <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustDjembe_compiled

gk_FaustDjembe_level chnexport "gk_FaustDjembe_level", 3
gk_FaustDjembe_freq chnexport "gk_FaustDjembe_freq", 3
gk_FaustDjembe_strikePosition chnexport "gk_FaustDjembe_strikePosition", 3
gk_FaustDjembe_strikeSharpness chnexport "gk_FaustDjembe_strikeSharpness", 3
gk_FaustDjembe_gain chnexport "gk_FaustDjembe_gain", 3
gk_FaustDjembe_level init 0
gk_FaustDjembe_gain init .75
gk_FaustDjembe_strikePosition init .5
gk_FaustDjembe_strikeSharpness init .5
gk_FaustDjembe_midi_dynamic_range init 12
instr FaustDjembe
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
; Reflect pitch into the usable range for this instrument.
while i_midi_key < 24 do
    i_midi_key = i_midi_key + 12
od
while i_midi_key > 96 do
    i_midi_key = i_midi_key - 12
od
i_midi_dynamic_range = i(gk_FaustDjembe_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 57
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustDjembe_gain)
i_faust_dsp faustdsp gi_FaustDjembe_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "strikePosition", gk_FaustDjembe_strikePosition
faustctl i_faust_dsp, "strikeSharpness", gk_FaustDjembe_strikeSharpness
faustctl i_faust_dsp, "gain", gk_FaustDjembe_gain
k_gain = i_midi_velocity
faustctl i_faust_dsp, "gate", k(1)
a_left, a_right faustplay i_faust_dsp
a_signal = a_left + a_right
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking 

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

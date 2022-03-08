gi_FaustModularBody_compiled faustcompile {{

declare name "ModularInterpInstrMidi";
declare description "String instrument with a modular body";
declare license "MIT";
declare copyright "(c)Romain Michon & John Granzow, CCRMA (Stanford University), GRAME, University of Michigan";

import("stdfaust.lib");

process = pm.modularInterpInstr_ui_MIDI <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustModularBody_compiled

gk_FaustModularBody_level chnexport "gk_FaustModularBody_level", 3
gk_FaustModularBody_shape chnexport "gk_FaustModularBody_shape", 3
gk_FaustModularBody_scale chnexport "gk_FaustModularBody_scale", 3
gk_FaustModularBody_pluck_position chnexport "gk_FaustModularBody_pluck_position", 3
gk_FaustModularBody_gain chnexport "gk_FaustModularBody_gain", 3
gk_FaustModularBody_otuGain chnexport "gk_FaustModularBody_outGain", 3

gk_FaustModularBody_level init 0
gk_FaustModularBody_shape init .95
gk_FaustModularBody_scale init .25
gk_FaustModularBody_pluck_position init .95
gk_FaustModularBody_gain init .75
gk_FaustModularBody_outGain init .5
gk_FaustModularbody_midi_dynamic_range init 20

instr FaustModularBody
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release
i_instrument = p1
i_time = p2
i_midi_key = p4
; Reflect pitch into the usable range for this instrument.
while i_midi_key < 30 do
    i_midi_key = i_midi_key + 12
od
while i_midi_key > 77 do
    i_midi_key = i_midi_key - 12
od
i_midi_dynamic_range = i(gk_FaustModularbody_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 88
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustModularBody_gain)
i_faust_dsp faustdsp gi_FaustModularBody_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
faustctl i_faust_dsp, "shape", gk_FaustModularBody_shape
faustctl i_faust_dsp, "scale", gk_FaustModularBody_scale
faustctl i_faust_dsp, "pluckPosition", gk_FaustModularBody_pluck_position
faustctl i_faust_dsp, "gain", gk_FaustModularBody_gain
k_gain = i_midi_velocity
faustctl i_faust_dsp, "outGain", gk_FaustModularBody_outGain
faustctl i_faust_dsp, "gate", k(1)
a_left, a_right faustplay i_faust_dsp
a_signal = a_left + a_right
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain

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



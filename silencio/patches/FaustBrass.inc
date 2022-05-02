gi_FaustBrass_compiled faustcompile {{

declare name "BrassMIDI";
declare description "Simple MIDI-controllable brass instrument physical model with physical parameters.";
declare license "MIT";
declare copyright "(c)Romain Michon, CCRMA (Stanford University), GRAME";

import("stdfaust.lib");

process = pm.brass_ui_MIDI <: _,_;

}}, "--import-dir \"/home/mkg/faust/libraries\"", 0

print gi_FaustBrass_compiled

gk_FaustBrass_level chnexport "gk_FaustBrass_level", 3
gk_FaustBrass_freq chnexport "gk_FaustBrass_freq", 3
gk_FaustBrass_gain chnexport "gk_FaustBrass_gain", 3
gk_FaustBrass_envAttack chnexport "gk_FaustBrass_envAttack", 3
gk_FaustBrass_lipsTension chnexport "gk_FaustBrass_lipsTension", 3
gk_FaustBrass_mute chnexport "gk_FaustBrass_mute", 3
gk_FaustBrass_vibratoFreq chnexport "gk_FaustBrass_vibratoFreq", 3
gk_FaustBrass_vibratoGain chnexport "gk_FaustBrass_vibratoGain", 3
gk_FaustBrass_outGain chnexport "gk_FaustBrass_outGain", 3

gk_FaustBrass_level init 0
gk_FaustBrass_gain init .75
gk_FaustBrass_envAttack init 1
gk_FaustBrass_lipsTension init .5
gk_FaustBrass_mute init .5
gk_FaustBrass_vibratoFreq init 4
gk_FaustBrass_vibratoGain init .5
gk_FaustBrass_outGain init .5
gk_FaustBrass_midi_dynamic_range init 20

instr FaustBrass
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
i_midi_dynamic_range = i(gk_FaustBrass_midi_dynamic_range)
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
k_gain = ampdb(gk_FaustBrass_gain)
i_faust_dsp faustdsp gi_FaustBrass_compiled
k_frequency cpsmidinn i_midi_key
faustctl i_faust_dsp, "freq", k_frequency
k_gain = i_midi_velocity
faustctl i_faust_dsp, "gain", gk_FaustBrass_gain
faustctl i_faust_dsp, "envAttack", gk_FaustBrass_envAttack
faustctl i_faust_dsp, "lipsTension", gk_FaustBrass_lipsTension
faustctl i_faust_dsp, "mute", gk_FaustBrass_mute
faustctl i_faust_dsp, "vibratoFreq", gk_FaustBrass_vibratoFreq
faustctl i_faust_dsp, "vibratoGain", gk_FaustBrass_vibratoGain
faustctl i_faust_dsp, "outGain", gk_FaustBrass_outGain
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

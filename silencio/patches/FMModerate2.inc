gk_FMModerate2_level chnexport "gk_FMModerate2_level", 3
gi_FMModerate2_carrier chnexport "gi_FMModerate2_carrier", 3
gi_FMModerate2_modulator chnexport "gi_FMModerate2_modulator", 3
gi_FMModerate2_fmamplitude chnexport "gi_FMModerate2_fmamplitude", 3
gi_FMModerate2_index chnexport "gi_FMModerate2_index", 3

gk_FMModerate2_level init 0
gi_FMModerate2_carrier init 1
gi_FMModerate2_modulator init 4
gi_FMModerate2_fmamplitude init 9
gi_FMModerate2_index init 2

gi_FMModerate2_cosine ftgen 0, 0, 65537, 11, 1

instr FMModerate2
; Author: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 85
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FMModerate2_level)
iattack = 0.002
isustain = p3
idecay = 1.5
irelease = 0.05
icarrier = gi_FMModerate2_carrier
imodulator = gi_FMModerate2_modulator
ifmamplitude = gi_FMModerate2_fmamplitude
index = gi_FMModerate2_index
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, -8.0, 1.0, idecay, -8.0, 0.025, isustain, 0.0, 0.025, irelease, 7.0, 0.0
aindex = aindenv * index * ifmamplitude
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, gi_FMModerate2_cosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, gi_FMModerate2_cosine; Plus amplitude correction.
a_signal = (aouta + aoutb) * aindenv
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
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

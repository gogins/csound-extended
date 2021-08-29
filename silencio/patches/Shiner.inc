gk_Shiner_midi_dynamic_range chnexport "gk_Shiner_midi_dynamic_range", 3 ;  127
gk_Shiner_attack chnexport "gk_Shiner_attack", 3 ;  .0125
gk_Shiner_release chnexport "gk_Shiner_release", 3 ;  .0125
gk_Shiner_level chnexport "gk_Shiner_level", 3 ;  0.5

gk_Shiner_midi_dynamic_range init 20
gk_Shiner_attack init .0125
gk_Shiner_release init .0125
gk_Shiner_level init 0.5

instr Shiner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Shiner_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 80
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Shiner_level)

iattack = i(gk_Shiner_attack)
idecay = i(gk_Shiner_release)
isustain = p3 - i(gk_Shiner_attack)
xtratim iattack + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
gk_Harmonics = 1 * 20
asignal vco2 kenvelope * 4, ihertz, 12
kgain = ampdb(gk_Shiner_level) * .5
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_signal = asignal * kgain * adamping

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

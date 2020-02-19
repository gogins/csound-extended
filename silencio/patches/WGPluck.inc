gk_WGPluck_midi_dynamic_range init 127
gk_WGPluck_level init 0
gk_WGPluck_pick init .5
gk_WGPluck_reflection init .5
gi_WGPluck_pluck init 0
gi_WGPluck_damping init 10
gi_WGPluck_filter init 1000
gi_WGPluck_sine ftgen 0, 0, 65536, 10, 1, 0, 1
instr WGPluck
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_WGPluck_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 77
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_WGPluck_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
k_amplitude = 1
a_excitation poscil 1, 1, gi_WGPluck_sine
a_signal wgpluck i_frequency, i_amplitude, gk_WGPluck_pick, gi_WGPluck_pluck, gi_WGPluck_damping, gi_WGPluck_filter, a_excitation
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
;printks "WGPluck      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

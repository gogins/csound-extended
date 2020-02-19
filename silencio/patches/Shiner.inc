gk_Shiner_midi_dynamic_range init 127
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
i_level_correction = 92
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
aleft, aright pan2 asignal, k_space_left_to_right
;printks2 "master gain:", kgain
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

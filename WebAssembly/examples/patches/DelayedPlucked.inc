gk_DelayedPlucked_midi_dynamic_range init 127
gk_DelayedPlucked_level init 0
instr DelayedPlucked
; Author: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_DelayedPlucked_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 89
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_DelayedPlucked_level)
iattack = 0.006
idecay = 1.5
xtratim iattack + idecay
isustain = p3
irelease = 0.05
xtratim iattack + isustain + irelease
ihertz = i_frequency
ioctave = octcps(i_frequency)
 ; Detuning of strings by 4 cents each way.
idetune = 4.0 / 1200.0
ihertzleft = cpsoct(ioctave + idetune)
ihertzright = cpsoct(ioctave - idetune)
isine ftgenonce 0, 0, 65537, 10, 1
icosine ftgenonce 0, 0, 65537, 11, 1
igenleft = isine
igenright = icosine
kvibrato poscil 1.0 / 120.0, 7.0, icosine
a_envelope transegr 0.0, iattack, -7.0, 1.0, idecay, -7.0, 0.5, isustain, 0.0, 0.125, irelease, -7.0, 0.0
ag pluck 1, cpsoct(ioctave + kvibrato), i_frequency, igenleft, 1
agleft pluck 1, ihertzleft, i_frequency, igenleft, 1
agright pluck 1, ihertzright, i_frequency, igenright, 1
imsleft = 0.2 * 1000
imsright = 0.21 * 1000
adelayleft vdelay ag, imsleft, imsleft + 100
adelayright vdelay ag, imsright, imsright + 100
asignal = a_envelope * (agleft + adelayleft + agright + adelayright)
; Highpass filter to exclude speaker cone excursions.
asignal1 butterhp asignal, 32.0
asignal2 balance asignal1, asignal
a_signal = asignal
i_attack = .01
i_sustain = p3
i_release = 0.1
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.7
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

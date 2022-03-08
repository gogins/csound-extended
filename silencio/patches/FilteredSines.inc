gk_FilteredSines_level chnexport "gk_FilteredSines_level", 3
gi_FilteredSines_attack chnexport "gi_FilteredSines_attack", 3
gi_FilteredSines_release chnexport "gi_FilteredSines_release", 3

gk_FilteredSines_level init 0
gi_FilteredSines_attack init 1
gi_FilteredSines_release init 1

gi_FilteredSines_bergeman ftgen 0, 0, 65537, 10, .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2 ; Bergeman f1

instr FilteredSines
; Author: Michael Bergeman
; Modified by: Michael Gogins
xtratim gi_FilteredSines_attack + gi_FilteredSines_release
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 166
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FilteredSines_level)
kHz = k(i_frequency)
koctave = octcps(kHz)
iattack init gi_FilteredSines_attack
isustain init p3
irelease init gi_FilteredSines_release
idb = 1.5
ip5 = gi_FilteredSines_bergeman
ip3 = 5.0
ip6 = 0.9
ip7 = 1.4
kp8 = cpsoct(koctave - .01)
kp9 = cpsoct(koctave + .01)
isc = idb * .333
k1 linseg 40, ip3, 800, p3, 800, 0.06, 0.0
k2 linseg 440, ip3, 220, p3, 220, 0.06, 0.0
k3 linseg 0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
k4 linseg 800, ip3, 40, p3, 40, 0.06, 0.0
k5 linseg 220, ip3, 440, p3, 440, 0.06, 0.0
k6 linseg isc, ip6, p3, ip7, p3, 0.06, 0.0
k7 linseg 0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
a5 poscil k3, kp8, ip5
a6 poscil k3, kp8 * 0.999, ip5
a7 poscil k3, kp8 * 1.001, ip5
a1 = a5 + a6 + a7
a8 poscil k6, kp9, ip5
a9 poscil k6, kp9 * 0.999, ip5
a10 poscil k6, kp9 * 1.001, ip5
a11 = a8 + a9 + a10
a2 butterbp a1, k1, 40
a3 butterbp a2, k5, k2 * 0.8
a4 balance a3, a1
a12 butterbp a11, k4, 40
a13 butterbp a12, k2, k5 * 0.8
a14 balance a13, a11
a15 reverb2 a4, 5, 0.3
a16 reverb2 a4, 4, 0.2
a17 = (a15 + a4) * k7
a18 = (a16 + a4) * k7
a_signal = (a17 + a18)
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim (i_attack + i_release)
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.88

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

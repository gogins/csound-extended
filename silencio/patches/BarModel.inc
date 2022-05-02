gk_BarModel_level chnexport "gk_BarModel_level", 3
gk_BarModel_kbcL chnexport "gk_BarModel_kbcL", 3
gk_BarModel_kbcR chnexport "gk_BarModel_kbcR", 3
gi_BarModel_iK chnexport "gi_BarModel_iK", 3
gi_BarModel_ib chnexport "gi_BarModel_ib", 3
gk_BarModel_kscan chnexport "gk_BarModel_kscan", 3
gi_BarModel_iT30 chnexport "gi_BarModel_iT30", 3
gi_BarModel_ipos chnexport "gi_BarModel_ipos", 3 
gi_BarModel_ivel chnexport "gi_BarModel_ivel", 3 
gi_BarModel_iwid chnexport "gi_BarModel_iwid", 3
gk_BarModel_midi_dynamic_range chnexport "gk_BarModel_midi_dynamic_range", 3

gk_BarModel_level init 0
gk_BarModel_kbcL init 1
gk_BarModel_kbcR init 1
gi_BarModel_iK init 0
gi_BarModel_ib init .001
gk_BarModel_kscan init .23
gi_BarModel_iT30 init 5
gi_BarModel_ipos init 0.3 
gi_BarModel_ivel init 1000 
gi_BarModel_iwid init 0.05
gk_BarModel_midi_dynamic_range init 20

gi_BarModel_sine ftgen 0, 0, 65537, 10, 1, 0, 1

instr BarModel
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_BarModel_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 120
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_BarModel_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
k_amplitude = 1
; ares     barmodel    kbcL, kbcR, iK, ib,    kscan, iT30, ipos, ivel, iwid
; a_signal barmodel    1,    1,    p4, 0.001, 0.23,  5,    p5,    p6,   p7
; a_signal barmodel gk_BarModel_kbcL, gk_BarModel_kbcR, gi_BarModel_iK, gi_BarModel_ib, gk_BarModel_kscan, gi_BarModel_iT30, gi_BarModel_ipos, gi_BarModel_ivel, gi_BarModel_iwid
// May not be correct.
k_scan = i_frequency
a_signal barmodel gk_BarModel_kbcL, gk_BarModel_kbcR, gi_BarModel_iK, gi_BarModel_ib, k_scan, gi_BarModel_iT30, gi_BarModel_ipos, gi_BarModel_ivel, gi_BarModel_iwid
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
;printks "BarModel      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

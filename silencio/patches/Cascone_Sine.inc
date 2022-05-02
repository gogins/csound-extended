gk_Cascone_Sine_level chnexport "gk_Cascone_Sine_level", 3
gi_Cascone_Sine_attack chnexport "gi_Cascone_Sine_attack", 3
gi_Cascone_Sine_release chnexport "gi_Cascone_Sine_release", 3
gk_Cascone_Sine_midi_dynamic_range chnexport "gk_Cascone_Sine_midi_dynamic_range", 3

gk_Cascone_Sine_level init 0
gi_Cascone_Sine_attack init 0.003
gi_Cascone_Sine_release init 0.01
gk_Cascone_Sine_midi_dynamic_range init 20

gi_Cascone_Sine_f10 ftgen 0, 0, 512, 7, 0, 50, 1, 50, .5, 300, .5, 112, 0
gi_Cascone_Sine_f11 ftgen 0, 0, 65537, 10, 1 ;SINE WAVE hi-res

instr Cascone_Sine
//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
//
// NOTE: I don't believe this was designed to play pitched sounds.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_Sine_attack + gi_Cascone_Sine_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_Sine_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 75
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Cascone_Sine_level)

;===============================================
; INSTRUMENT 3 - a sinewave instrument
;===============================================
;p1    p2   p3   p4  p5   p6
;instr strt dur  frq amp  kpan
;i3    0    .125 100 2000 1
ak1 poscil3 1, 1/p3, gi_Cascone_Sine_f10 ; ADSR
a_signal poscil3 ak1, i_frequency, gi_Cascone_Sine_f11 ; SINE

// VERY simple! Just a tiny bit of harmonics and a decent envelope.

a_declicking linsegr 0, gi_Cascone_Sine_attack, 1, i_sustain, 1, gi_Cascone_Sine_release, 0
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
printks "Cascone_Sine i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





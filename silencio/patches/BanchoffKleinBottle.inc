gk_BanchoffKleinBottle_level chnexport "gk_BanchoffKleinBottle_level", 3
gi_BanchoffKleinBottle_attack chnexport "gi_BanchoffKleinBottle_attack", 3
gi_BanchoffKleinBottle_release chnexport "gi_BanchoffKleinBottle_release", 3
gk_BanchoffKleinBottle_midi_dynamic_range chnexport "gk_BanchoffKleinBottle_midi_dynamic_range", 3

gk_BanchoffKleinBottle_level init 0
gi_BanchoffKleinBottle_attack init 0.005
gi_BanchoffKleinBottle_release init 0.01
gk_BanchoffKleinBottle_midi_dynamic_range init 127

gi_BanchoffKleinBottle_sine ftgen 0, 0, 65537, 10, 1
gi_BanchoffKleinBottle_cosine ftgen 0, 0, 65537, 11, 1

instr BanchoffKleinBottle
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
xtratim gi_BanchoffKleinBottle_attack + gi_BanchoffKleinBottle_release
a_declicking linsegr 0, gi_BanchoffKleinBottle_attack, 1, p3, 1, gi_BanchoffKleinBottle_release, 0
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
xtratim gi_BanchoffKleinBottle_attack + gi_BanchoffKleinBottle_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_BanchoffKleinBottle_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 93
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_BanchoffKleinBottle_level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; p1 p2 p3 p4 p5 p6 p7
; Start Dur Amp Frqc U V 
; i 4 32 6 6000 6.00 3 2
; i 4 36 4 . 5.11 5.6 0.4
; i 4 + 4 . 6.05 2 8.5
; i 4 . 2 . 6.02 4 5
; i 4 . 2 . 6.02 5 0.5
iHz = i_frequency
ifqc init iHz
ip4 init i_amplitude
iu init 5 ; p6
iv init 0.5 ; p7
irt2 init sqrt(2)
aampenv linseg 0, 0.02, ip4, p3 - 0.04, ip4, 0.02, 0
 ; Cosines
acosu oscili 1, iu * ifqc, gi_BanchoffKleinBottle_cosine
acosu2 oscili 1, iu * ifqc / 2, gi_BanchoffKleinBottle_cosine
acosv oscili 1, iv * ifqc, gi_BanchoffKleinBottle_cosine
 ; Sines
asinu oscili 1, iu * ifqc, gi_BanchoffKleinBottle_sine
asinu2 oscili 1, iu * ifqc / 2, gi_BanchoffKleinBottle_sine
asinv oscili 1, iv * ifqc, gi_BanchoffKleinBottle_sine
 ; Compute X and Y
ax = acosu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
ay = asinu * (acosu2 * (irt2 + acosv) + asinu2 * asinv * acosv)
 ; Low frequency rotation in spherical coordinates z, phi, theta.
klfsinth oscili 1, 4, gi_BanchoffKleinBottle_sine
klfsinph oscili 1, 1, gi_BanchoffKleinBottle_sine
klfcosth oscili 1, 4, gi_BanchoffKleinBottle_cosine
klfcosph oscili 1, 1, gi_BanchoffKleinBottle_cosine
aox = -ax * klfsinth + ay * klfcosth
aoy = -ax * klfsinth * klfcosph - ay * klfsinth * klfcosph + klfsinph
aoutleft = aampenv * aox
aoutright = aampenv * aoy
a_signal = aox + aoy
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
#end
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "BanchoffKleinB i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

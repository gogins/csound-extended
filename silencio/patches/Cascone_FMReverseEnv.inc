gk_Cascone_FMReverseEnv_level chnexport "gk_Cascone_FMReverseEnv_level", 3
gi_Cascone_FMReverseEnv_attack chnexport "gi_Cascone_FMReverseEnv_attack", 3
gi_Cascone_FMReverseEnv_release chnexport "gi_Cascone_FMReverseEnv_release", 3
gk_Cascone_FMReverseEnv_kcar chnexport "gk_Cascone_FMReverseEnv_kcar", 3
gk_Cascone_FMReverseEnv_kmod chnexport "gk_Cascone_FMReverseEnv_kmod", 3
gk_Cascone_FMReverseEnv_kndx chnexport "gk_Cascone_FMReverseEnv_kndx", 3
gk_Cascone_FMReverseEnv_midi_dynamic_range chnexport "gk_Cascone_FMReverseEnv_midi_dynamic_range", 3

gk_Cascone_FMReverseEnv_level init 0
gi_Cascone_FMReverseEnv_attack init 0.004
gi_Cascone_FMReverseEnv_release init 0.01
gk_Cascone_FMReverseEnv_kcar init .25
gk_Cascone_FMReverseEnv_kmod init 1.33
gk_Cascone_FMReverseEnv_kndx init 9.7
gk_Cascone_FMReverseEnv_midi_dynamic_range init 20

gi_Cascone_FMReverseEnv_f11 ftgen 0, 0, 65537, 10, 1 ;SINE WAVE hi-res
gi_Cascone_FMReverseEnv_f18 ftgen 0, 0, 512, 5, 1, 512, 256 ;reverse exp env

instr Cascone_FMReverseEnv
//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
// NOTE: This is not designed to play particular pitches.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_FMReverseEnv_attack + gi_Cascone_FMReverseEnv_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_FMReverseEnv_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 93
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Cascone_FMReverseEnv_level)

;======================================================
; INSTRUMENT 5 - FM w/reverse env
;======================================================
; CHANGES TO INSTR 5
;____________ _______________________
;make A arg in fposcil3 = 10
;put fposcil3 out into osc w/f18
;the effect I want is a cascade of short b'wards FM sounds that go from
;right to left...subtle yet present...like a flock of metal birds
;========================================================================
;p1    p2   p3   p4   p5   p6   p7   p8 p9 p10
;instr strt dur  frq  car  mod  kpan kndx kamp rvbsnd
;i 5   61     .8 4500 3.25 1.10 0    9.7  4    .09
;i 5   61.85 .   5040 2.3  2.25 1    8.3  4    .
;i 5   62.00 1   6340 3.3  1.35 0    8.2  4    .
kcps = i_frequency
kcar = gk_Cascone_FMReverseEnv_kcar
kmod = gk_Cascone_FMReverseEnv_kmod
kndx = gk_Cascone_FMReverseEnv_kndx 
kamp = i_amplitude
;kcar line 2,p3*.9,0
;kenv poscil3 3,1/p3,10
afm foscili kamp,kcps,kcar,kmod,kndx,gi_Cascone_FMReverseEnv_f11 ; f11 = HIRES SINE WAVE
afm1 poscil3 afm,1/p3,gi_Cascone_FMReverseEnv_f18
afm2 = afm1*400 ; THIS INCERASES THE GAIN OF THE FOSCILI OUTx400
a_signal = afm2
a_declicking linsegr 0, gi_Cascone_FMReverseEnv_attack, 1, i_sustain, 1, gi_Cascone_FMReverseEnv_release, 0
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
;printks "Cascone_FMReverseEnv i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





gk_Cascone_ThreeBranch_level chnexport "gk_Cascone_ThreeBranch_level", 3
gi_Cascone_ThreeBranch_attack chnexport "gi_Cascone_ThreeBranch_attack", 3
gi_Cascone_ThreeBranch_release chnexport "gi_Cascone_ThreeBranch_release", 3
gk_Cascone_ThreeBranch_kfreq1 chnexport "gk_Cascone_ThreeBranch_kfreq1", 3
gk_Cascone_ThreeBranch_kfreq2 chnexport "gk_Cascone_ThreeBranch_kfreq2", 3
gk_Cascone_ThreeBranch_kamp2 chnexport "gk_Cascone_ThreeBranch_kamp2", 3
gi_Cascone_ThreeBranch_ifrq chnexport "gi_Cascone_ThreeBranch_ifrq", 3
gk_Cascone_ThreeBranch_midi_dynamic_range chnexport "gk_Cascone_ThreeBranch_midi_dynamic_range", 3

gk_Cascone_ThreeBranch_level init 0
gi_Cascone_ThreeBranch_attack init 0.003
gi_Cascone_ThreeBranch_release init 0.01
gk_Cascone_ThreeBranch_kfreq1 init 60
gk_Cascone_ThreeBranch_kfreq2 init 50
gk_Cascone_ThreeBranch_kamp2 init 34
gi_Cascone_ThreeBranch_ifrq init 70
gk_Cascone_ThreeBranch_midi_dynamic_range init 20

gi_Cascone_ThreeBranch_f3  ftgen 0, 0, 65537, 9, 10, 1, 0, 16, 1.5, 0, 22, 2, 0, 23, 1.5, 0 ;inharm wave
gi_Cascone_ThreeBranch_f4  ftgen 0, 0, 65537, 9, 1, 1, 0 ;sine
gi_Cascone_ThreeBranch_f8  ftgen 0, 0, 512, 5, 256, 512, 1 ;exp env
gi_Cascone_ThreeBranch_f10 ftgen 0, 0, 512, 7, 0, 50, 1, 50, .5, 300, .5, 112, 0
gi_Cascone_ThreeBranch_f11 ftgen 0, 0, 65537, 10, 1 ;SINE WAVE hi-res
gi_Cascone_ThreeBranch_f17 ftgen 0, 0, 1024, 7, .5, 256, 1, 256, .5, 256, 0, 256, .5 ;triangle with offset

instr Cascone_ThreeBranch
//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
//
// NOTE: I don't believe this was designed to play pitched sounds.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_ThreeBranch_attack + gi_Cascone_ThreeBranch_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_ThreeBranch_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 130
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Cascone_ThreeBranch_level)

;==================================
; INSTRUMENT 1 - three branch instr
;==================================
;straight line
; f.losin namp| ring mod__________| noisefrq
;p1    p2   p3  p4   p5   p6     p7     p8    p9
;instr strt dur freq amp  kfreq1 kfreq2 kamp2 nfrq
;i1    5.00 40  200  6000 60     34     50    70
i1 = i_amplitude*.3
i2 = i_frequency*.98
i3 = 1/p3
i4 = i_amplitude*.6
i5 = i_frequency
kfreq1 = gk_Cascone_ThreeBranch_kfreq1
kfreq2 = gk_Cascone_ThreeBranch_kfreq2
kamp2 = gk_Cascone_ThreeBranch_kamp2
;=============================================
; 1 - noise branch
;=============================================
a1 randi i4, gi_Cascone_ThreeBranch_ifrq ;i4 WAS p5
a1 poscil3 a1, i3, gi_Cascone_ThreeBranch_f10
a1 poscil3 a1, 3000, gi_Cascone_ThreeBranch_f11 ;a1 IS THE NOISE OUTPUT
;===========================================
; 2 - RM branch
;===========================================
akamp1 linen kamp2, p3*.2, p3, p3*.2
asig1 poscil3 akamp1, gk_Cascone_ThreeBranch_kfreq1, gi_Cascone_ThreeBranch_f11 ; AMP IS CONTROLLED BY LINEN, FREQ IS CONTROLLED BY p6
asig2 poscil3 kamp2, gk_Cascone_ThreeBranch_kfreq2, gi_Cascone_ThreeBranch_f3 ; AMP IS CONTROLLED BY p8, FREQ IS CONTROLLED BY p7
aosc2 = asig1*asig2
a2 = aosc2*.085 ; THE OUTPUT a2 IS SCALED
;================================
; 3 - low sine branch
;================================
k3 poscil3 i4, i3, gi_Cascone_ThreeBranch_f8 ; f8 = EXP ENV
a3 poscil3 k3, i5, gi_Cascone_ThreeBranch_f4 ; f4 = SINE WAVE (LO RES)
a3 = a3*.5 ; a3 PROVIDES THE LOW SINE TONE
;output to filter, reverb and panning
;=====================================
iamp = i(gk_Cascone_ThreeBranch_kamp2)*.4
aout = a1+a2+a3
kcf linseg 0,p3/2,850,p3/2,0 ; THIS CONTROLS THE FILTER FRQ
akpan poscil3 1,0.1,gi_Cascone_ThreeBranch_f17 ; TRIANGLE WITH OFFSET (0-1) CONTROLS PANNING
alp butterlp aout, kcf ; THREE BRANCHES ARE MIXED & FED THROUGH BUTTERLP
akenv linen iamp,p3*.8,p3,p3*.2 ; THIS IS THE MAIN ENV ON THE OUTPUT
alpout = akenv*alp
a_signal = alpout
a_declicking linsegr 0, gi_Cascone_ThreeBranch_attack, 1, i_sustain, 1, gi_Cascone_ThreeBranch_release, 0
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
;printks "Cascone_ThreeBranch i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





gk_Cascone_Water_level chnexport "gk_Cascone_Water_level", 3
gi_Cascone_Water_attack chnexport "gi_Cascone_Water_attack", 3
gi_Cascone_Water_release chnexport "gi_Cascone_Water_release", 3
gk_Cascone_Water_krt chnexport "gk_Cascone_Water_krt", 3
gi_Cascone_Water_isd chnexport "gi_Cascone_Water_isd", 3
gk_Cascone_Water_midi_dynamic_range chnexport "gk_Cascone_Water_midi_dynamic_range", 3

gk_Cascone_Water_level init 0
gi_Cascone_Water_attack init 0.003
gi_Cascone_Water_release init 0.01
gk_Cascone_Water_krt init 7
gi_Cascone_Water_isd init .456
gk_Cascone_Water_midi_dynamic_range init 20

gi_Cascone_Water_f10 ftgen 0, 0, 512, 7, 0, 50, 1, 50, .5, 300, .5, 112, 0
gi_Cascone_Water_f11 ftgen 0, 0, 65537, 10, 1 ;SINE WAVE hi-res
gi_Cascone_Water_f14 ftgen 0, 0, 512, 7, 1, 17, 1, 0, 0, 495 ;pulse for S&H clk oscinstr Cascone_Water

//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
//////////////////////////////////////////////
instr Cascone_Water
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_Water_attack + gi_Cascone_Water_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_Water_midi_dynamic_range)
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
k_gain = ampdb(gk_Cascone_Water_level)

;=================================================================
; INSTRUMENT 9 -- WATER
;=================================================================
;  p1 p2   p3  p4    p5  p6
;i    strt dur iseed amp krt
;i 9  120  40  .3    2.5 60
krt = gk_Cascone_Water_krt ; THIS IS THE FRQ OF THE RANDH OUTPUT & CLK OSC
isd = gi_Cascone_Water_isd ; p4 HOLDS THE VALUE OF THE SEED OF RANDH UG
krn randh 10000,krt,isd ; NOISE INPUT TO S&H
kclk poscil3 100,krt,gi_Cascone_Water_f14 ; KCLK CLOCKS THE S&H -- f14 IS A DUTY CYCLE WAVE
ksh samphold krn, kclk ; S&H
a2 poscil3 2, 100,gi_Cascone_Water_f11 ;; SINE OSC (11) CONTROLLED BY S&H;;;AMP=600
ksh = ksh*.50
a4 reson a2,ksh,50 ; FILTER WITH S&H CONTROLING THE Fc
a3 poscil3 a4,1/p3,gi_Cascone_Water_f10 ; f10=ADSR -- a3 IS THE OUTPUT
a_signal = a3*.15
a_declicking linsegr 0, gi_Cascone_Water_attack, 1, i_sustain, 1, gi_Cascone_Water_release, 0
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
;printks "Cascone_Water i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





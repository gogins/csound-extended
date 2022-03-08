gk_Cascone_RissetCascadeHarmonics_level chnexport "gk_Cascone_RissetCascadeHarmonics_level", 3
gi_Cascone_RissetCascadeHarmonics_attack chnexport "gi_Cascone_RissetCascadeHarmonics_attack", 3
gi_Cascone_RissetCascadeHarmonics_release chnexport "gi_Cascone_RissetCascadeHarmonics_release", 3
gi_Cascone_RissetCascadeHarmonics_offset chnexport "gi_Cascone_RissetCascadeHarmonics_offset", 3
gk_Cascone_RissetCascadeHarmonics_midi_dynamic_range chnexport "gk_Cascone_RissetCascadeHarmonics_midi_dynamic_range", 3

gk_Cascone_RissetCascadeHarmonics_level init 0
gi_Cascone_RissetCascadeHarmonics_attack init 0.003
gi_Cascone_RissetCascadeHarmonics_release init 0.01
gi_Cascone_RissetCascadeHarmonics_offset init .03
gk_Cascone_RissetCascadeHarmonics_midi_dynamic_range init 20

gi_Cascone_RissetCascadeHarmonics_f20 ftgen 0, 0, 65537, 10, 1, 0, 0, 0, .7, .7, .7, .7, .7, .7 ;approaching square

instr Cascone_RissetCascadeHarmonics
//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_RissetCascadeHarmonics_attack + gi_Cascone_RissetCascadeHarmonics_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_RissetCascadeHarmonics_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 61.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Cascone_RissetCascadeHarmonics_level)

;======================================================
; INSTR 8 -- CASCADE HARMONICS
; [borrowed instr from Risset]
;======================================================
;instr start dur freq amp offset
;  p1  p2    p3  p4   p5   p6
;i 8   80    80  93   375 .03 ;.075
i1 = 1*gi_Cascone_RissetCascadeHarmonics_offset ; INIT VALUES CORRESPOND TO FREQ.
i2 = 2*gi_Cascone_RissetCascadeHarmonics_offset ; OFFSETS FOR OSCILLATORS BASED ON ORIGINAL p6
i3 = 3*gi_Cascone_RissetCascadeHarmonics_offset
i4 = 4*gi_Cascone_RissetCascadeHarmonics_offset
ip4 = i_frequency
ampenv linen i_amplitude,30,p3,30 ; ENVELOPE
a1 poscil3 ampenv,ip4,   gi_Cascone_RissetCascadeHarmonics_f20
a2 poscil3 ampenv,ip4+i1,gi_Cascone_RissetCascadeHarmonics_f20 ; NINE OSCILLATORS WITH THE SAME AMPENV
a3 poscil3 ampenv,ip4+i2,gi_Cascone_RissetCascadeHarmonics_f20 ; AND WAVEFORM, BUT SLIGHTLY DIFFERENT
a4 poscil3 ampenv,ip4+i3,gi_Cascone_RissetCascadeHarmonics_f20 ; FREQUENCIES TO CREATE THE BEATING EFFECT
a5 poscil3 ampenv,ip4+i4,gi_Cascone_RissetCascadeHarmonics_f20
a6 poscil3 ampenv,ip4-i1,gi_Cascone_RissetCascadeHarmonics_f20 ; p4 = fREQ OF FUNDAMENTAL (Hz)
a7 poscil3 ampenv,ip4-i2,gi_Cascone_RissetCascadeHarmonics_f20 ; p5 = AMP
a8 poscil3 ampenv,ip4-i3,gi_Cascone_RissetCascadeHarmonics_f20 ; p6 = INITIAL OFFSET OF FREQ - .03 Hz
a9 poscil3 ampenv,ip4-i4,gi_Cascone_RissetCascadeHarmonics_f20
a_signal = a1+a2+a3+a4+a5+a6+a7+a8+a9

a_declicking linsegr 0, gi_Cascone_RissetCascadeHarmonics_attack, 1, i_sustain, 1, gi_Cascone_RissetCascadeHarmonics_release, 0
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
printks "Cascone_RissetCascadeHarmonics i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





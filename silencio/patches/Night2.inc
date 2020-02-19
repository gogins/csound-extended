gk_Night2_level init 0
gi_Night2_attack init 0.003
gi_Night2_release init 0.01
gi_Night2_balance init .6
gk_Night2_midi_dynamic_range init 127
gi_Night2_sine ftgen 0, 0, 65537, 10, 1
gi_Night2_sigmoid ftgen 0, 0, 65537, 19, .5, 1, 270, 1
instr Night2
//////////////////////////////////////////////
// Instrument by Volkov
// Adapted by Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Night2_attack + gi_Night2_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Night2_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Night2_level)
iamp = i_amplitude 
inote = i_frequency
ibal = gi_Night2_balance 
if1 = 0
if2 = 0 
ind1 = 35
ind2 = 30
k1 linseg 0,p3/2,iamp,p3/2, 0
//  ares pluck kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]
a3 pluck k1,inote*.997,ind1,gi_Night2_sine,1 
a2 pluck k1,inote*1.003,ind1,gi_Night2_sine,1 
; a1 pluck k1,inote,ind1,if1,1 
aplk1 = a2+a3
k2 linseg 0,p3*.66,iamp*1.2,p3/3,0
a6 pluck k2,inote*.995,ind2,gi_Night2_sine,1 
a5 pluck k2,inote*1.005,ind2,gi_Night2_sine,1 
; a4 pluck k2,inote,ind2,if2,1 
aplk2 = a5+a6
a_signal = aplk1+aplk2
a_declicking linsegr 0, gi_Night2_attack, 1, i_sustain, 1, gi_Night2_release, 0
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "Night2 i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

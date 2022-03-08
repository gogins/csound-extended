gi_TerrainMappedLead_Modulator_sine ftgen 0, 0, 65537, 10, 1
opcode TerrainMappedLead_Modulator, a, iii
iamp, ifqc, iphase xin
akrad1 poscil3 iamp, ifqc, gi_TerrainMappedLead_Modulator_sine, iphase
akout = akrad1 + 2 * iamp 
xout akout
endop

gk_TerrainMappedLead_level chnexport "gk_TerrainMappedLead_level", 3 ;  0
gi_TerrainMappedLead_attack chnexport "gi_TerrainMappedLead_attack", 3 ;  0.003
gi_TerrainMappedLead_petals chnexport "gi_TerrainMappedLead_petals", 3 ;  2.99
gi_TerrainMappedLead_release chnexport "gi_TerrainMappedLead_release", 3 ;  0.01
gk_TerrainMappedLead_midi_dynamic_range chnexport "gk_TerrainMappedLead_midi_dynamic_range", 3 ;  20

gk_TerrainMappedLead_level init 0
gi_TerrainMappedLead_attack init 0.003
gi_TerrainMappedLead_petals init 2.99
gi_TerrainMappedLead_release init 0.01
gk_TerrainMappedLead_midi_dynamic_range init 20

gi_TerrainMappedLead_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
gi_TerrainMappedLead_sine ftgen 0, 0, 65537, 10, 1

instr TerrainMappedLead
//////////////////////////////////////////////
// Author: Hans Mikelson
// Adapted by: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_TerrainMappedLead_attack + gi_TerrainMappedLead_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_TerrainMappedLead_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 69
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_TerrainMappedLead_level)

;-----------------------------------------------------------
; Z=X-1/12*X^3-1/4*Y^2
; Lead 1
;-----------------------------------------------------------
idur = p3 ; Duration
iamp = i_amplitude ; Amplitude
ifqc = i_frequency ; Convert pitch to frequency
;imodch = p6 ; Control channel input
ipetal = gi_TerrainMappedLead_petals ; Number of petals on the rose curve
;kmod1 zkr imodch ; Read control channel
akmod1 TerrainMappedLead_Modulator 1,    .5,    0
akdclick linseg 0, .002, iamp, idur-.004, iamp, .002, 0 ; De-Click envelope
kpfade linseg 0, .2, 0, .2, .01, idur-.4, .005 ; Vibrato fade
kplfo poscil3 kpfade, 6, gi_TerrainMappedLead_sine ; Vibrato
kfqc = (1+kplfo)*ifqc ; Modulate the pitch
arose poscil3 akmod1, ipetal*kfqc, gi_TerrainMappedLead_sine ; Rose Curve
ax poscil3 arose, kfqc, gi_TerrainMappedLead_sine ; Convert from polar to
ay poscil3 arose, kfqc, gi_TerrainMappedLead_sine, .25 ; rectangular coordinates
az = ax-ax*ax*ax/12-ay*ay/4+.5 ; Compute the surface
adcblk butterhp az, 20 ; Remove the DC component
a_signal = adcblk * akdclick

a_declicking linsegr 0, gi_TerrainMappedLead_attack, 1, i_sustain, 1, gi_TerrainMappedLead_release, 0
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
;printks "TerrainMappedLead i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





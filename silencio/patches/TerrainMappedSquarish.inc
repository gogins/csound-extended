gi_TerrainMappedSquarish_Modulator_sine ftgen 0, 0, 65537, 10, 1
opcode TerrainMappedSquarish_Modulator, a, iii
iamp, ifqc, iphase xin
akrad1 poscil3 iamp, ifqc, gi_TerrainMappedSquarish_Modulator_sine, iphase
akout = akrad1 + 2 * iamp 
xout akout
endop

gk_TerrainMappedSquarish_level chnexport "gk_TerrainMappedSquarish_level", 3 ;  0
gi_TerrainMappedSquarish_attack chnexport "gi_TerrainMappedSquarish_attack", 3 ;  0.003
gi_TerrainMappedSquarish_release chnexport "gi_TerrainMappedSquarish_release", 3 ;  0.01
gi_TerrainMappedSquarish_petals chnexport "gi_TerrainMappedSquarish_petals", 3 ;  2.01
gi_TerrainMappedSquarish_detune chnexport "gi_TerrainMappedSquarish_detune", 3 ;  0.005
gk_TerrainMappedSquarish_midi_dynamic_range chnexport "gk_TerrainMappedSquarish_midi_dynamic_range", 3 ;  20

gk_TerrainMappedSquarish_level init 0
gi_TerrainMappedSquarish_attack init 0.003
gi_TerrainMappedSquarish_release init 0.01
gi_TerrainMappedSquarish_petals init 2.01
gi_TerrainMappedSquarish_detune init 0.005
gk_TerrainMappedSquarish_midi_dynamic_range init 20

gi_TerrainMappedSquarish_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
gi_TerrainMappedSquarish_sine ftgen 0, 0, 65537, 10, 1

instr TerrainMappedSquarish
//////////////////////////////////////////////
// Author: Hans Mikelson
// Adapted by: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_TerrainMappedSquarish_attack + gi_TerrainMappedSquarish_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_TerrainMappedSquarish_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 72.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_TerrainMappedSquarish_level)

;-----------------------------------------------------------
; Z=-5*X/(X^2+Y^2+1)
; Squarish Lead 2
;-----------------------------------------------------------
idur = p3 ; Duration
iamp = i_amplitude ; Amplitude
ifqc = i_frequency ; Convert pitch to frequency
;imodch = p6 ; Control channel
ipetal = gi_TerrainMappedSquarish_petals ; Number of petals on the rose curve
idetun = gi_TerrainMappedSquarish_detune ; Detune amount
ifqc1 = ifqc*(1+idetun) ; Add some chorusing
ifqc2 = ifqc*(1-idetun*.9) ; and stereo detuning
;kmod1 zkr imodch ; Read from the modulation control channel
akomd1 TerrainMappedSquarish_Modulator 1, .5, 0
akdclick linseg 0, .002, iamp, idur-.004, iamp, .002, 0 ; De-Click envelope
arose poscil3 akomd1+1, ipetal*ifqc, gi_TerrainMappedSquarish_sine ; Rose Curve
ax poscil3 arose, ifqc, gi_TerrainMappedSquarish_sine ; Convert from polar to
ay poscil3 arose, ifqc, gi_TerrainMappedSquarish_sine, .25 ; rectangular coordinates
az = -5*ax/(ax*ax+ay*ay+1) ; Compute the surface for oscillator 1
arose1 poscil3 akomd1+1, ipetal*ifqc1, gi_TerrainMappedSquarish_sine ; Rose Curve
ax1 poscil3 arose1, ifqc1, gi_TerrainMappedSquarish_sine ; Convert from polar to
ay1 poscil3 arose1, ifqc1, gi_TerrainMappedSquarish_sine, .25 ; rectangular coordinates
az1 = -5*ax1/(ax1*ax1+ay1*ay1+1) ; Compute the surface for oscillator 2
arose2 poscil3 akomd1+1, ipetal*ifqc2, gi_TerrainMappedSquarish_sine ; Rose Curve
ax2 poscil3 arose2, ifqc2, gi_TerrainMappedSquarish_sine ; Convert from polar to
ay2 poscil3 arose2, ifqc2, gi_TerrainMappedSquarish_sine, .25 ; rectangular coordinates
az2 = -5*ax2/(ax2*ax2+ay2*ay2+1) ; Compute the surface for oscillator 3
adcblkl butterhp (az+az1)/2, 20 ; Block DC
adcblkr butterhp (az+az2)/2, 20 ; Block DC
aleft = adcblkl*akdclick
aright = adcblkr*akdclick 
a_signal = aleft + aright
a_declicking linsegr 0, gi_TerrainMappedSquarish_attack, 1, i_sustain, 1, gi_TerrainMappedSquarish_release, 0
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
;printks "TerrainMappedSquarish i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





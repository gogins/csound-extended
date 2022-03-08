gi_TerrainModulator_sine ftgen 0, 0, 65537, 10, 1
opcode TerrainModulator, a, iii
iamp, ifqc, iphase xin
akrad1 poscil3 iamp, ifqc, gi_TerrainModulator_sine, iphase
akout = akrad1 + 2 * iamp 
xout akout
endop

gk_TerrainMappedBass_level chnexport "gk_TerrainMappedBass_level", 3 ;  0
gi_TerrainMappedBass_attack chnexport "gi_TerrainMappedBass_attack", 3 ;  0.003
gi_TerrainMappedBass_release chnexport "gi_TerrainMappedBass_release", 3 ;  0.01
gk_TerrainMappedBass_midi_dynamic_range chnexport "gk_TerrainMappedBass_midi_dynamic_range", 3 ;  20

gk_TerrainMappedBass_level init 0
gi_TerrainMappedBass_attack init 0.003
gi_TerrainMappedBass_release init 0.01
gk_TerrainMappedBass_midi_dynamic_range init 20

gi_TerrainMappedBass_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
gi_TerrainMappedBass_sine ftgen 0, 0, 65537, 10, 1

instr TerrainMappedBass
//////////////////////////////////////////////
// Author: Hans Mikelson
// Adapted by: Michael Gogins
//////////////////////////////////////////////
xtratim gi_TerrainMappedBass_attack + gi_TerrainMappedBass_release
i_instrument = p1
i_time = p2
i_sustain = p3
// For a "bass" this seems appropriate.
if p3 < .5 then
  p3 = .5
endif
i_midi_key = p4
i_midi_dynamic_range = i(gk_TerrainMappedBass_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 70.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_TerrainMappedBass_level)

; Terrain Mapped Bass
; z=sqrt(x^4+y^4-x^2-y^2-x^2Y^2+1)
;-----------------------------------------------------------
idur = p3 ; Duration
iamp = i_amplitude ; Amplitude
aamp1 init iamp ; Used to balance the signal
ifqc = i_frequency ; Convert pitch to frequency
; Sta Dur Amp Fqc Phase OutKCh
akrad1 TerrainModulator 2.5, .5, 0
akrad2 TerrainModulator 2.5, .6, .25
ibndtab = gi_TerrainMappedBass_pitch_bend_table ; Pitch bend table
kamp linsegr 0, .01, 2, .1, 1.5, idur-.21, 1, .1, 0 ; Amp Envelope
kbend poscil3 1, 1/idur, ibndtab ; Pitch bend
kfqc = kbend*ifqc ; Adjust frequency by pitch bend
asin1 poscil3 akrad1, kfqc, gi_TerrainMappedBass_sine ; Y component of circle radius 1 
acos1 poscil3 akrad1, kfqc, gi_TerrainMappedBass_sine, .25 ; X component of circle radius 1
asin2 poscil3 akrad2, kfqc, gi_TerrainMappedBass_sine, .5 ; Y component of circle radius 2
acos2 poscil3 akrad2, kfqc, gi_TerrainMappedBass_sine, .75 ; X component of circle radius 2
ax1 = asin1+.35 ; Offset the circle centers a bit
ay1 = acos1-.80 ; so the waves are assymmetrical.
ax2 = asin2+.5
ay2 = acos2-.20
axsq1 = ax1*ax1 ; Compute X^2 and Y^2 in advance
aysq1 = ay1*ay1 ; to save time.
axsq2 = ax2*ax2
aysq2 = ay2*ay2
; Compute the surface
az1 = sqrt(axsq1*axsq1+aysq1*aysq1-axsq1-aysq1-axsq1*aysq1+1)
az2 = sqrt(axsq2*axsq2+aysq2*aysq2-axsq2-aysq2-axsq2*aysq2+1)
abal1 balance az1, aamp1 ; Balance the signal
abal2 balance az2, aamp1 ; Balance the signal
aout1 butterhp abal1, 10 ; DC blocking filter
aout2 butterhp abal2, 10 ; DC blocking filter
aleft = aout1 * kamp
aright = aout2 * kamp
a_signal = aleft + aright
a_declicking linsegr 0, gi_TerrainMappedBass_attack, 1, i_sustain, 1, gi_TerrainMappedBass_release, 0
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
;printks "TerrainMappedBass i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





gi_TerrainMappedPulsar_Modulator_sine ftgen 0, 0, 65537, 10, 1
opcode TerrainMappedPulsar_Modulator, a, iii
iamp, ifqc, iphase xin
akrad1 poscil3 iamp, ifqc, gi_TerrainMappedPulsar_Modulator_sine, iphase
akout = akrad1 + 2 * iamp 
xout akout
endop

gk_TerrainMappedPulsar_level chnexport "gk_TerrainMappedPulsar_level", 3 ;  0
gi_TerrainMappedPulsar_attack chnexport "gi_TerrainMappedPulsar_attack", 3 ;  0.003
gi_TerrainMappedPulsar_release chnexport "gi_TerrainMappedPulsar_release", 3 ;  0.01
gi_TerrainMappedPulsar_petals chnexport "gi_TerrainMappedPulsar_petals", 3 ;  2.01
gi_TerrainMappedPulsar_detune chnexport "gi_TerrainMappedPulsar_detune", 3 ;  0.005
gk_TerrainMappedPulsar_midi_dynamic_range chnexport "gk_TerrainMappedPulsar_midi_dynamic_range", 3 ;  20

gk_TerrainMappedPulsar_level init 0
gi_TerrainMappedPulsar_attack init 0.003
gi_TerrainMappedPulsar_release init 0.01
gi_TerrainMappedPulsar_petals init 2.01
gi_TerrainMappedPulsar_detune init 0.005
gk_TerrainMappedPulsar_midi_dynamic_range init 20

gi_TerrainMappedPulsar_pitch_bend_table ftgen 0, 0, 1024, -7, 1, 1024, 1 
gi_TerrainMappedPulsar_sine ftgen 0, 0, 65537, 10, 1
gi_TerrainMappedPulsar_amp ftgen 0, 0, 1024, -7, 0, 256, 1, 512, 1, 256, 0
instr TerrainMappedPulsar
//////////////////////////////////////////////
// Author: Hans Mikelson
// Adapted by: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_TerrainMappedPulsar_attack + gi_TerrainMappedPulsar_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_TerrainMappedPulsar_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 66.25
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_TerrainMappedPulsar_level)

;-----------------------------------------------------------
; Pulsar
; z=sqrt((1-x^2)*((1-y)+y*cos(1/x)))
;-----------------------------------------------------------
idur = p3 ; Duration
iamp = i_amplitude ; Amplitude
ifqc = i_frequency ; Frequency
iamptab = gi_TerrainMappedPulsar_amp ; Fader table
;kmod zkr imodch ; Read the modulation
akmod TerrainMappedPulsar_Modulator .15, .50, 0 
akamp poscil3 iamp, 1/idur, iamptab ; Amplitude Fader
aka = .01+akmod ; Cardioid A
akb = .1+akmod*.2 ; Cardioid B
acardi poscil3 akb, ifqc, gi_TerrainMappedPulsar_sine ; Cardioid Curve
ax1 poscil3 aka+acardi, ifqc, gi_TerrainMappedPulsar_sine ; Convert from polar to
ax = ax1
ay poscil3 aka+acardi, ifqc, gi_TerrainMappedPulsar_sine, .25 ; rectangular coordinates
az = sqrt((1-ax*ax)*((1-ay)+ay*cos(1/(ax*ax+.01))))-1 ; Compute the surface
aout butterhp az, 20 ; Remove DC
aleft = aout*akamp
aright = aout*akamp 
a_signal = aleft + aright
a_declicking linsegr 0, gi_TerrainMappedPulsar_attack, 1, i_sustain, 1, gi_TerrainMappedPulsar_release, 0
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
;printks "TerrainMappedPulsar i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





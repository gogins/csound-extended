; Fill isize tables of size isize to create the surface.
; Generate tables to control the strength of partials generated.
gi_WaveTerrainTables_initialized init 0
; strength for brass partials (all) was 4
gi_WaveTerrainTables_size init 256
gi_WaveTerrainTables_sine ftgen 0, 0, 65537, 10, 1
gi_WaveTerrainTables_brass_partials ftgen 0, 0, gi_WaveTerrainTables_size, 9, .25, 1, 90
;strength for clarinet partials (1, 3, 5, 7) was 3
gi_WaveTerrainTables_clarinet_partials ftgen 3, 0, gi_WaveTerrainTables_size, 9, .25, 1, 0
gi_WaveTerrainTables_index init 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
opcode WaveTerrainTables, 0, 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gisize = 256;number and size of the tables
;gifn0 = 301
krow init 0
loop:
irow = i(krow)
ip1 tablei irow, gi_WaveTerrainTables_clarinet_partials
ip2 tablei irow, gi_WaveTerrainTables_brass_partials
;iftnum = gifn0+i(krow)
gi_WaveTerrainTables_index ftgen 0, 0, gi_WaveTerrainTables_size+1 , 13, 1, 1, 0, .6*ip1+ip2, -.7*ip1, -.8*ip1-.7*ip2, ip1, .8*ip1+.4*ip2, -.6*ip1, -.4*ip1-.1*ip2
krow = krow + 1
if krow >= gi_WaveTerrainTables_size goto end
reinit loop
end:
gi_WaveTerrainTables_index -= gi_WaveTerrainTables_size
endop

gk_WaveTerrain_level chnexport "gk_WaveTerrain_level", 3 ;  0
gi_WaveTerrain_attack chnexport "gi_WaveTerrain_attack", 3 ;  0.003
gi_WaveTerrain_release chnexport "gi_WaveTerrain_release", 3 ;  0.01
gk_WaveTerrain_midi_dynamic_range chnexport "gk_WaveTerrain_midi_dynamic_range", 3 ;  20

gk_WaveTerrain_level init 0
gi_WaveTerrain_attack init 0.003
gi_WaveTerrain_release init 0.01
gk_WaveTerrain_midi_dynamic_range init 20

gi_WaveTerrain_sine ftgen 0, 0, 65537, 10, 1

instr WaveTerrain
//////////////////////////////////////////////
//  Wave Terrain Synthesis by Josep Comajuncosas
//  Adapted by Michael Gogins
/////////////////////////////////////////////
if gi_WaveTerrainTables_initialized == 0 then
  gi_WaveTerrainTables_initialized = 1
  WaveTerrainTables
endif
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_WaveTerrain_attack + gi_WaveTerrain_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_WaveTerrain_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
//  Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
//  AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 155
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_WaveTerrain_level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Orbit & waveform generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
inote = i_frequency
iamp  = 10000
; set an ascending orbit to index the surface
kportamento linseg -1, .1, .1, .1, 0, p3-.2, 0; add some portamento to the attack
kvibamp oscil1i 0, .05, p3, gi_WaveTerrainTables_clarinet_partials; add vibrato to the clarinet
kvib oscili kvibamp, 5, gi_WaveTerrainTables_sine
kamp linseg 0, .03, 1, p3-1.3, .8, 1, .5, .2, 0
kx oscili .5*kamp*(1+kvib), inote*(1+.05*kportamento+.02*kvibamp), gi_WaveTerrainTables_sine; sine
ky expon .99, p3, .01
; map the orbit through the surface
; table indexes
kfndown  = int(ky*gi_WaveTerrainTables_size) + gi_WaveTerrainTables_index 
kfnup    = int(1+ky*gi_WaveTerrainTables_size) + gi_WaveTerrainTables_index
kndx     = kx; normalized 0 to 1
igoto end
;table read
azdown tableikt kndx,kfndown, 1, .5, 1
azup   tableikt kndx,kfnup,   1, .5, 1
;linear interpolation
ay upsamp frac(ky*gi_WaveTerrainTables_size)
az = (1-ay)*azdown + ay*azup
;final output & endin
a_signal = iamp*az*kamp
end:
a_declicking linsegr 0, gi_WaveTerrain_attack, 1, i_sustain, 1, gi_WaveTerrain_release, 0
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
;printks "WaveTerrain      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

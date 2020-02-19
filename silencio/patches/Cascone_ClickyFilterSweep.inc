gk_Cascone_ClickyFilterSweep_level init 0
gi_Cascone_ClickyFilterSweep_attack init 0.003
gi_Cascone_ClickyFilterSweep_release init 0.01
gk_Cascone_ClickyFilterSweep_midi_dynamic_range init 127
gi_Cascone_ClickyFilterSweep_f2 ftgen 0, 0, 512, 5, 4096, 512, 1 ;exp env
gi_Cascone_ClickyFilterSweep_f8 ftgen 0, 0, 512, 5, 256, 512, 1 ;exp env
gi_Cascone_ClickyFilterSweep_f10 ftgen 0, 0, 512, 7, 0, 50, 1, 50, .5, 300, .5, 112, 0 ;ADSR
instr Cascone_ClickyFilterSweep
//////////////////////////////////////////////
// Author: Kim Cascone
// Adapted by: Michael Gogins
// NOTE: This is not designed to play particular pitches.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_Cascone_ClickyFilterSweep_attack + gi_Cascone_ClickyFilterSweep_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_Cascone_ClickyFilterSweep_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 150.5
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Cascone_ClickyFilterSweep_level)

;==================================================
; INSTRUMENT 6 - clicky filter sweep w/pan
; take a noise source and bp filter
; pass to amp => lp filter
; then pan across stereo field 
;=================================================
aclk = p3*4.3 ; THIS IS THE FRQ FOR THE FILTER AND ADSR
; [THIS COMMENT ABOVE DIDN'T HAVE A ";" BEFORE IT
; AND MIGHT HAVE NOT HAD AN EFFECT ON THE CODE] 2/16
;arnd randi 7000, 5000 ; NOT USING THIS NOISE SOURCE::USING PULSE INSTEAD
apls poscil3 7000,aclk,gi_Cascone_ClickyFilterSweep_f2 ; THIS GENERATES A SMALL SPIKE SHAPED LIKE A EXP ENV f2
abp butterbp apls,2500,200 ; THIS FILTERS THE SPIKE SO ITS MORE CLICKY SOUNDING
abp = abp*3 ; THIS BOOSTS THE LEVEL OF THE FILTER OUT
anoise poscil3 abp,aclk,gi_Cascone_ClickyFilterSweep_f8 ; THIS GIVES THE FILTERED SIGNAL THE SAME ENV AS THE WAVEFORM 
akswp line 1800,p3,180 ; THIS CONTROLS THE RESON FILTER::STRT frq=1800, END frq=180
;kswp expon 2600,p3,300
afilt reson anoise,akswp,20 ; RESON CREATES A FILTER SWEEP
;afilt = afilt*.4
afilt2 poscil3 afilt,1/p3,gi_Cascone_ClickyFilterSweep_f10 ; THIS ENVELOPES THE FILTER OUTPUT
akpan line 0,p3*.8,1 ; THIS IS USED FOR THE PANNING OF THE OUTPUT
afilt2 = afilt2*.05 ; tHIS SCALES THE OUTPUT OF THE FILTER 
a_signal = afilt2

a_declicking linsegr 0, gi_Cascone_ClickyFilterSweep_attack, 1, i_sustain, 1, gi_Cascone_ClickyFilterSweep_release, 0
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
printks "Cascone_ClickyFilterSweep i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin





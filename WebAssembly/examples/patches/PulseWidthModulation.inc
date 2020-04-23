gk_PulseWidthModulation_level init 0
gi_PulseWidthModulation_attack init 0.003
gi_PulseWidthModulation_release init 0.01
gk_PulseWidthModulation_midi_dynamic_range init 127
gi_PulseWidthModulation_sine ftgen 0, 0, 65537, 10, 1
instr PulseWidthModulation
//////////////////////////////////////////////
//  Instrument definition patch template.
//  Author: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_PulseWidthModulation_attack + gi_PulseWidthModulation_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_PulseWidthModulation_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
//  Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
//  AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 221.2
i_normalization = ampdb(-i_level_correction)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_PulseWidthModulation_level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; coded by Josep M Comajuncosas / jan98
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
iamp = 10000; intended max. amplitude
kenv linen 1, .2,p3,.2; volume envelope
kfreq = i_frequency
ibound = sr/4; set it to sr/2 for true BL square wave
kratio line .01, p3, .99; fractional width of the pulse part of a cycle
apulse1 buzz 1, kfreq, ibound/kfreq, gi_PulseWidthModulation_sine
; comb filter to give pulse width modulation
atemp delayr 1/20
apulse2 deltapi kratio/kfreq
delayw apulse1
avpw = apulse1 - apulse2
apwmdc integ avpw
apwm = apwmdc + kratio - .5
a_signal = apwm*iamp*kenv

a_declicking linsegr 0, gi_PulseWidthModulation_attack, 1, i_sustain, 1, gi_PulseWidthModulation_release, 0
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
;printks "PulseWidthModulation      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

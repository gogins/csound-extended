gk_RampSaw_level init 0
gi_RampSaw_attack init 0.003
gi_RampSaw_release init 0.01
gk_RampSaw_midi_dynamic_range init 127
gi_RampSaw_sine ftgen 0, 0, 65537, 10, 1
instr RampSaw
//////////////////////////////////////////////
//  Instrument definition patch template.
//  Author: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_RampSaw_attack + gi_RampSaw_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_RampSaw_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
//  Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
//  AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 129
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_RampSaw_level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; band limited ramp/saw modulation
; coded by Josep M Comajuncosas / jan98
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
iamp = 10000; intended max. amplitude
kenv linen 1, .2,p3,.2; volume envelope
kfreq = i_frequency
kfreqn = kfreq/sr
ibound = sr/4; set it to sr/2 for true BL square wave
kratio line .01, p3, .99
kcorr = 2*kfreqn/(kratio*(1-kratio))
apulse1 buzz 1, kfreq, ibound/kfreq, gi_RampSaw_sine, .5
atemp delayr 1/20
apulse2 deltapi kratio/kfreq
delayw apulse1
avpw = apulse1 - apulse2
apwmdc integ avpw
apwm atone apwmdc,1
apwm = apwm*kcorr
arsmdc tone apwm,5; leaky integrator
arsm atone arsmdc, 5;this is convenient to  remove DC
a_signal = arsm*iamp*kenv*.5

a_declicking linsegr 0, gi_RampSaw_attack, 1, i_sustain, 1, gi_RampSaw_release, 0
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
;printks "RampSaw      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

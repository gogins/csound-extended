gk_ChebyshevPoly_level init 0
gk_ChebyshevPoly_Coefficient1 init .5
gk_ChebyshevPoly_Coefficient2 init 1
gk_ChebyshevPoly_Coefficient3 init 0
gk_ChebyshevPoly_Coefficient4 init .1
gk_ChebyshevPoly_Coefficient5 init .01
gk_ChebyshevPoly_Coefficient6 init .01
gk_ChebyshevPoly_Coefficient7 init .01
gk_ChebyshevPoly_Coefficient8 init .1
gk_ChebyshevPoly_Coefficient9 init .01
gk_ChebyshevPoly_Coefficient10 init .05
gi_ChebyshevPoly_attack init 0.003
gi_ChebyshevPoly_release init 0.01
gk_ChebyshevPoly_midi_dynamic_range init 127
gi_ChebyshevPoly_sine ftgen 0, 0, 65536, 10, 1
instr ChebyshevPoly
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = p3
xtratim gi_ChebyshevPoly_attack + gi_ChebyshevPoly_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_ChebyshevPoly_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 78.3
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ChebyshevPoly_level)
if p3 > 0 then
iattack init p3 / 4.0
idecay init p3 / 4.0
else
iattack init 1 / 4.0
idecay init 1 / 4.0
endif
aenvelope transegr 0.0, iattack / 2.0, 2.5, i_amplitude / 2.0, iattack / 2.0, -2.5, i_amplitude, i_sustain, 0.0, i_amplitude, idecay / 2.0, 2.5, i_amplitude / 2.0, idecay / 2.0, -2.5, 0.
a_signal poscil3 1, i_frequency, gi_ChebyshevPoly_sine
a_signal chebyshevpoly a_signal, 0, gk_ChebyshevPoly_Coefficient1, gk_ChebyshevPoly_Coefficient2, gk_ChebyshevPoly_Coefficient3, gk_ChebyshevPoly_Coefficient4, gk_ChebyshevPoly_Coefficient5, gk_ChebyshevPoly_Coefficient6, gk_ChebyshevPoly_Coefficient7, gk_ChebyshevPoly_Coefficient8, gk_ChebyshevPoly_Coefficient9, gk_ChebyshevPoly_Coefficient10
a_signal *= aenvelope
a_declicking linsegr 0, gi_ChebyshevPoly_attack, 1, i_sustain, 1, gi_ChebyshevPoly_release, 0
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
;printks "ChebyshevPoly i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

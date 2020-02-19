gk_Droner_partial1 init .5
gk_Droner_partial2 init .05
gk_Droner_partial3 init .1
gk_Droner_partial4 init .2
gk_Droner_partial5 init .1
gk_Droner_partial6 init 0
gk_Droner_partial7 init 0
gk_Droner_partial8 init 0
gk_Droner_partial9 init 0
gk_Droner_partial10 init 0
gk_Droner_level init 0
gi_Droner_waveform init 0
instr Droner
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = -20 + 98 + 4
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Droner_level)
k1 = gk_Droner_partial1
k2 = gk_Droner_partial2
k3 = gk_Droner_partial3
k4 = gk_Droner_partial4
k5 = gk_Droner_partial5
k6 = gk_Droner_partial6
k7 = gk_Droner_partial7
k8 = gk_Droner_partial8
k9 = gk_Droner_partial9
k10 = gk_Droner_partial10
iwaveform = gi_Droner_waveform
iattack = .5
idecay = .5
xtratim iattack + idecay
isustain = p3
aenvelope transegr 0.0, iattack / 2.0, 1.5, 1 / 2.0, iattack / 2.0, -1.5, 1, isustain, 0.0, 1, idecay / 2.0, 1.5, 1 / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
isine ftgenonce 0, 0, 65537, 10, 1, 0, .02
if iwaveform == 0 goto i_waveform_0
if iwaveform == 1 goto i_waveform_1
if iwaveform == 2 goto i_waveform_2
i_waveform_0:
asignal poscil3 1, ihertz, isine
goto i_waveform_endif
i_waveform_1:
asignal vco2 1, ihertz, 8 ; integrated saw
goto i_waveform_endif
i_waveform_2:
asignal vco2 1, ihertz, 12 ; triangle
i_waveform_endif:
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
adeclicking linsegr 0, .004, 1, p3 - .014, 1, .1, 0
a_signal = asignal * adeclicking
a_signal = a_signal * i_amplitude * k_gain * 1.4
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
endin

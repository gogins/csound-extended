gk_Internals_mod_amp chnexport "gk_Internals_mod_amp", 3
gk_Internals_mod_hz chnexport "gk_Internals_mod_hz", 3
gk_Internals_level chnexport "gk_Internals_level", 3
gk_Internals_pan chnexport "gk_Internals_pan", 3
gS_Internals_mod_waveform chnexport "gS_Internals_mod_waveform", 3
gS_Internals_waveform chnexport "gS_Internals_waveform", 3
gk_Internals_k1 chnexport "gk_Internals_k1", 3
gk_Internals_k2 chnexport "gk_Internals_k2", 3
gk_Internals_k3 chnexport "gk_Internals_k3", 3
gk_Internals_k4 chnexport "gk_Internals_k4", 3
gk_Internals_k5 chnexport "gk_Internals_k5", 3
gk_Internals_k6 chnexport "gk_Internals_k6", 3
gk_Internals_k7 chnexport "gk_Internals_k7", 3
gk_Internals_k8 chnexport "gk_Internals_k8", 3

gk_Internals_1_pan init 0.878
gk_Internals_1_k8 init 0.1211
gk_Internals_1_k7 init 0.0
gk_Internals_1_k6 init 0.6829
gk_Internals_1_k5 init 0.5691
gk_Internals_1_k4 init 0.2846
gk_Internals_1_k3 init 0.5528
gk_Internals_1_k2 init 0.4797
gk_Internals_1_k1 init 0.6829
gk_Internals_1_mod_hz init 0.0
gk_Internals_1_mod_amp init 0.0
gS_Internals_1_mod_waveform init "Triangle"
gS_Internals_1_waveform init "Chebyshev"
gk_Internals_1_level init 0

gi_Internals_sine ftgen 0, 0, 65537, 10, 1, 0, .02

instr Internals
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
k_space_left_to_right = p7
k1 = gk_Internals_k1
k2 = gk_Internals_k2
k3 = gk_Internals_k3
k4 = gk_Internals_k4
k5 = gk_Internals_k5
k6 = gk_Internals_k6
k7 = gk_Internals_k7
k8 = gk_Internals_k8
i_amplitude = ampdb(i_midi_velocity)
i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
xtratim i_attack + i_release
ak_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
; print i_frequency
i_waveform init 0
if strcmp(gS_Internals_waveform, "Sine") == 0 then
i_waveform = 0
endif
if strcmp(gS_Internals_waveform, "Sawtooth") == 0 then
i_waveform = 1
endif
if strcmp(gS_Internals_waveform, "Triangle") == 0 then
i_waveform = 2
endif
if strcmp(gS_Internals_waveform, "Chebyshev") == 0 then
i_waveform = 3 
endif
if i_waveform == 0 then
a_signal poscil3 1, i_frequency, gi_Internals_sine
endif
if i_waveform == 1 then
a_signal vco2 1, i_frequency, 8
endif
if i_waveform == 2 then
a_signal vco2 1, i_frequency, 12
endif
if i_waveform == 3 then
a_signal poscil3 1, i_frequency, gi_Internals_sine
a_signal chebyshevpoly a_signal, 0, k1, k2, k3, k4, k5, k6, k7, k8
endif
i_mod_waveform init 12
if strcmp(gS_Internals_mod_waveform, "Triangle") == 0 then
i_mod_waveform init 12
endif 
if strcmp(gS_Internals_mod_waveform, "Sawtooth") == 0 then
i_mod_waveform init 4
endif
if strcmp(gS_Internals_mod_waveform, "Square") == 0 then
i_mod_waveform init 10
endif 
; prints   "Csound: gS_Internals_waveform:     %s\\n", gS_Internals_waveform
; prints   "Csound: gS_Internals_mod_waveform: %s\\n", gS_Internals_mod_waveform
; prints   "Csound: i_mod_waveform:              %9.4f\\n", i_mod_waveform
; printks2 "Csound: gk_Internals_mod_amp:      %9.4f\\n", gk_Internals_mod_amp
; printks2 "Csound: gk_Internals_mod_hz:       %9.4f\\n", gk_Internals_mod_hz
; printks2 "Csound: gk_Internals_level:        %9.4f\\n", gk_Internals_level
; printks2 "Csound: gk_Internals_k1:           %9.4f\\n", gk_Internals_k1
; printks2 "Csound: gk_Internals_k2:           %9.4f\\n", gk_Internals_k2
; printks2 "Csound: gk_Internals_k3:           %9.4f\\n", gk_Internals_k3
; printks2 "Csound: gk_Internals_k4:           %9.4f\\n", gk_Internals_k4
; printks2 "Csound: gk_Internals_k5:           %9.4f\\n", gk_Internals_k5
; printks2 "Csound: gk_Internals_k6:           %9.4f\\n", gk_Internals_k6
; printks2 "Csound: gk_Internals_k7:           %9.4f\\n", gk_Internals_k7
; printks2 "Csound: gk_Internals_k8:           %9.4f\\n", gk_Internals_k8
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 177 
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Internals_level)
a_modulator vco2 gk_Internals_mod_amp, gk_Internals_mod_hz, 12
a_vdelay vdelay3 a_signal, a_modulator, 4
a_vdelay = a_vdelay * ak_envelope * 10
a_output = k_gain * a_vdelay * i_amplitude
a_left, a_right pan2 a_output, k_space_left_to_right
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right

#ifdef USE_SPATIALIZATION
a_signal = a_left + a_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_left
outleta "outright", a_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

// These must be initialized in the orc header before any #includes:
// gi_Fluidsynth_engine fluidEngine 0, 0
// gi_Fluidsynth_soundfont fluidLoad "Steinway_C.sf2", gi_Fluidsynth_engine, 1
// fluidProgramSelect gi_Fluidsynth_engine, 0, gi_Fluidsynth_soundfont, 0, 1
// fluidInfo gi_Fluidsynth_engine
gk_FluidsynthOut_level init 0
gk_FluidsynthOut_front_to_back init 0
gk_FluidsynthOut_left_to_right init 0.5
gk_FluidsynthOut_bottom_to_top init 0

instr FluidsynthOut
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
i_overall_amps = -19
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FluidsynthOut_level)
//fluidProgramSelect gi_Fluidsynth_engine, 0, gi_Fluidsynth_soundfont, 0, 1
a_out_left, a_out_right fluidOut gi_Fluidsynth_engine
a_signal = a_out_left + a_out_right
a_signal *= i_amplitude
a_signal *= k_gain
a_out_left, a_out_right pan2 a_signal, gk_FluidsynthOut_left_to_right

#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_FluidsynthOut_front_to_back, gk_FluidsynthOut_left_to_right, gk_FluidsynthOut_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

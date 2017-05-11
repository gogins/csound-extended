gk_Piano_level init 0
gk_Piano_front_to_back init 0
gk_Piano_left_to_right init 0
gk_Piano_bottom_to_top init 0
instr PianoOut
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
i_overall_amps = -27
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Piano_level)
giFluidsynth fluidEngine 0, 0
giFluidSteinway fluidLoad "Piano Steinway Grand Model C (21,738KB).sf2", giFluidsynth, 1
fluidProgramSelect giFluidsynth, 0, giFluidSteinway, 0, 1
a_out_left, a_out_right fluidOut giFluidsynth
a_out_left = a_out_left * i_amplitude * k_gain
a_out_right = a_out_right * i_amplitude * k_gain
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_Piano_front_to_back, gk_Piano_left_to_right, gk_Piano_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "PianoOutFluids i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

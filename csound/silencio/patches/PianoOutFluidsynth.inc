gk_Piano_level init 0
instr PianoOut
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_pan = p6
i_depth = p7
i_height = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
iamps = 0
i_normalization = ampdb(-iamps - 12)
i_amplitude = ampdb(i_midi_velocity) / 32767.0 * i_normalization
k_gain = ampdb(gk_Piano_level)
giFluidsynth fluidEngine 0, 0
giFluidSteinway fluidLoad "Piano Steinway Grand Model C (21,738KB).sf2", giFluidsynth, 1
fluidProgramSelect giFluidsynth, 0, giFluidSteinway, 0, 1
aoutleft, aoutright fluidOut giFluidsynth
outleta "outleft", aoutleft * k_gain * i_amplitude
outleta "outright", aoutright * k_gain * i_amplitude
prints "PianoOutFluids i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p6
endin

giFluidsynth init 0
giFluidSteinway init 0
instr PianoNote
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_pan = p6
i_depth = p7
i_height = p8
i_phase = p9
; Use channel assigned in fluidLoad.
i_channel = 0
prints "PianoNoteFluid i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
fluidNote giFluidsynth, i_channel, i_midi_key, i_midi_velocity
endin

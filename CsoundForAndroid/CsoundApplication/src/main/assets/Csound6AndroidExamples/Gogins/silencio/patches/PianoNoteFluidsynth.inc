giFluidsynth init 0
giFluidSteinway init 0
instr PianoNote
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
; Use channel assigned in fluidLoad.
i_channel = 0
prints "PianoNoteFluid i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
fluidNote giFluidsynth, i_channel, i_midi_key, i_midi_velocity
endin

// These must be initialized to 0 in the orc header before any #includes.
// gi_Fluidsynth_engine fluidEngine 0, 0
// gi_Fluidsynth_soundfont fluidLoad "Steinway_C.sf2", gi_Fluidsynth_engine, 1
// fluidProgramSelect gi_Fluidsynth_engine, 0, gi_Fluidsynth_soundfont, 0, 1
// fluidInfo gi_Fluidsynth_engine

instr FluidsynthNote
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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
fluidNote gi_Fluidsynth, i_channel, i_midi_key, i_midi_velocity
endin

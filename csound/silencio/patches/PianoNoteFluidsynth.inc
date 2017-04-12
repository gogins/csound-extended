giFluidsynth init 0
giFluidSteinway init 0

instr PianoNote
i_instrument = p1
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
i_phase = p6
i_pan = p7
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
; Use channel assigned in fluidLoad.
ichannel = 0
;;;;;;;;;;;;;;;;;;i
prints "PianoNoteFl i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
fluidNote giFluidsynth, ichannel, i_midikey, i_midivelocity
endin

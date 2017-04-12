giPianoteq init 0

instr PianoNote
i_instrument = p1
i_channel = 1
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
;;;;;;;;;;;;;;; ;;;i
prints "PianoNotePt i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\n", i_pitch_correction
vstnote giPianoteq, i_channel, i_midikey, i_midivelocity, i_duration
endin

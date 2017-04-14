gk_Piano_level init 0
instr PianoOut
k_gain = ampdb(gk_Piano_level)
iamps = -30
i_normalization = ampdb(-iamps - 12)
i_amplitude = ampdb(80) / 32767.0 * i_normalization
giPianoteq vstinit "C:/Program_Files/Steinberg/VSTPlugins/Pianoteq5_x64", 0
; Should be "D4 Daily Practice".
vstprogset giPianoteq, 0
; Sustain off.
vstparamset giPianoteq, 0, 0
; Reverb off.
vstparamset giPianoteq, 72, 0
vstinfo giPianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0.0
ainright init 0.0
aoutleft, aoutright vstaudiog giPianoteq, ainleft, ainright
; printks "vstaudiog: %9.4f %9.4f\n", 0.5, aoutleft, aoutright
outleta "outleft", aoutleft * k_gain * i_amplitude
outleta "outright", aoutright * k_gain * i_amplitude
prints "PianoOutPt     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Piano_level init 0
instr PianoOut
k_gain = ampdb(gk_Piano_level)
i_overall_amps = 60
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(80) * i_normalization
giPianoteq vstinit "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
; Should be "D4 Daily Practice".
vstprogset giPianoteq, 0
; Sustain off.
vstparamset giPianoteq, 0, 0
; Reverb off.
vstparamset giPianoteq, 72, 0
; vstinfo giPianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0.0
ainright init 0.0
aoutleft, aoutright vstaudiog giPianoteq, ainleft, ainright
; printks "vstaudiog: %9.4f %9.4f\n", 0.5, aoutleft, aoutright
a_out_left = aoutleft * k_gain * i_amplitude
a_out_right = aoutright * k_gain * i_amplitude
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_Piano_front_to_back, gk_Piano_left_to_right, gk_Piano_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
outleta "outleft", a_out_left * k_gain * i_amplitude
outleta "outright", a_out_right * k_gain * i_amplitude
#endif
prints  "PianoOutPt     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\n", 0.5, aoutleft, aoutright, gk_Piano_level
endin

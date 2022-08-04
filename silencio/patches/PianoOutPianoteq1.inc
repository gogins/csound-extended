// This must be initialized in the orc header before any #includes.

; gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0

gk_PianoOutPianoteq_level chnexport "gk_PianoOutPianoteq_level", 3 ;  0
gi_PianoOutPianoteq_print chnexport "gi_PianoOutPianoteq_print", 3 ;  1
gk_PianoOutPianoteq_front_to_back chnexport "gk_PianoOutPianoteq_front_to_back", 3 ;  0
gk_PianoOutPianoteq_left_to_right chnexport "gk_PianoOutPianoteq_left_to_right", 3 ;  0.5
gk_PianoOutPianoteq_bottom_to_top chnexport "gk_PianoOutPianoteq_bottom_to_top", 3 ;  0

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 0.5
gk_PianoOutPianoteq_bottom_to_top init 0

instr PianoOutPianoteq
prints "starting piano"
vstprogset gi_Pianoteq, 1
; Sustain off.
vstparamset gi_Pianoteq, 6, 0
; Reverb switch off.
vstparamset gi_Pianoteq, 93, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 87
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
k_1 rms aoutleft
k_2 rms aoutright

kx = gk_PianoOutPianoteq_front_to_back
ky = gk_PianoOutPianoteq_left_to_right
kz = gk_PianoOutPianoteq_bottom_to_top

#ifdef USE_SPATIALIZATION
absignal[] init 16
aspatialreverbsend init 0
; opcode Spatialize, a[]a, akkk
absignal, aspatialreverbsend Spatialize a_signal, kx, ky, kz
outletv "outbformat", absignal
outleta "out", aspatialreverbsend
#else
a_out_left, a_out_right pan2 a_signal, ky
outleta "outleft", a_out_left
outleta "outright", a_out_right
#end
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

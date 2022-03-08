gk_OrganOutOrganteq_level chnexport "gk_OrganOutOrganteq_level", 3 ;  0
gi_OrganOutOrganteq_print chnexport "gi_OrganOutOrganteq_print", 3 ;  1
gk_OrganOutOrganteq_front_to_back chnexport "gk_OrganOutOrganteq_front_to_back", 3 ;  0
gk_OrganOutOrganteq_left_to_right chnexport "gk_OrganOutOrganteq_left_to_right", 3 ;  0.5
gk_OrganOutOrganteq_bottom_to_top chnexport "gk_OrganOutOrganteq_bottom_to_top", 3 ;  0

gk_OrganOutOrganteq_level init 0
gi_OrganOutOrganteq_print init 1
gk_OrganOutOrganteq_front_to_back init 0
gk_OrganOutOrganteq_left_to_right init 0.5
gk_OrganOutOrganteq_bottom_to_top init 0

instr OrganOutOrganteq
; By default, Organteq .fxp  preset files are saved thus:
; /home/mkg/.local/share/Modartt/Organteq/Presets/My Presets/Church (copy).fxp
; However, vst4cs doesn't load .fxp files, only .fxb preset bank files.
; However again, Organtec doesn't save .fxb files, only .fxp files.
; The vst4cs program change opcode does not seem to work except with loaded .fxb files.
; vstprogset gi_Organteq, 3
; vstmidiout gi_Organteq, 192, 5, 15, 0
; vstmidiout gi_Organteq, 192, 6, 15, 0
; So, the only thing to do is to set each parameter right here. Unfortunately,
; not all the parameters in the GUI are available from code.
; Reverb control.
vstparamset gi_Organteq, 4, 0
; Tutti (test), it works.
; vstparamset gi_Organteq, 6, 1

; Set up all stops...

; Keyboard 1 -- Pedale

vstparamset gi_Organteq, 33, 1
vstparamset gi_Organteq, 34, 0
vstparamset gi_Organteq, 35, 0
vstparamset gi_Organteq, 36, 0
vstparamset gi_Organteq, 37, 1
vstparamset gi_Organteq, 38, 0
vstparamset gi_Organteq, 39, 0
vstparamset gi_Organteq, 40, 0
vstparamset gi_Organteq, 41, 0
vstparamset gi_Organteq, 42, 0

; Keyboard 2 -- Positif

vstparamset gi_Organteq, 43, 0
vstparamset gi_Organteq, 44, 1
vstparamset gi_Organteq, 45, 0
vstparamset gi_Organteq, 46, 0
vstparamset gi_Organteq, 47, 1
vstparamset gi_Organteq, 48, 0
vstparamset gi_Organteq, 49, 0
vstparamset gi_Organteq, 50, 0
vstparamset gi_Organteq, 51, 1
vstparamset gi_Organteq, 52, 0

; Keyboard 3 -- Grand Orgue

vstparamset gi_Organteq, 53, 0
vstparamset gi_Organteq, 54, 1
vstparamset gi_Organteq, 55, 1
vstparamset gi_Organteq, 56, 0
vstparamset gi_Organteq, 57, 0 
vstparamset gi_Organteq, 58, 0
vstparamset gi_Organteq, 59, 0
vstparamset gi_Organteq, 60, 0
vstparamset gi_Organteq, 61, 0
vstparamset gi_Organteq, 62, 0

; Keyboard 4 - Recit 

vstparamset gi_Organteq, 63, 1
vstparamset gi_Organteq, 64, 1
vstparamset gi_Organteq, 65, 0
vstparamset gi_Organteq, 66, 0
vstparamset gi_Organteq, 67, 0
vstparamset gi_Organteq, 68, 0
vstparamset gi_Organteq, 69, 0
vstparamset gi_Organteq, 70, 1
vstparamset gi_Organteq, 71, 0
vstparamset gi_Organteq, 72, 0

k_gain = ampdb(gk_OrganOutOrganteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_OrganOutOrganteq_print == 1 then
  vstinfo gi_Organteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Organteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_OrganOutOrganteq_left_to_right
; printks "vstaudio:       %9.4f   %9.4f\\n", 0.5, aoutleft, aoutright

#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_OrganOutOrganteq_front_to_back, gk_OrganOutOrganteq_left_to_right, gk_OrganOutOrganteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "OrganOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Organ_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

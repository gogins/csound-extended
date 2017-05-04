gk_Rhodes_level init 0
instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
if p3 == -1 then
  p3 = 1000000
endif
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
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 83
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Rhodes_level)
iattack = 0.002
isustain = p3
irelease = 0.05
iindex = 4
icrossfade = 3
ivibedepth = 0.2
iviberate = 6
isine ftgenonce 0, 0, 65536, 10, 1
icosine ftgenonce 0, 0, 65536, 11, 1
icookblank ftgenonce 0, 0, 65536, 10, 0 ; Blank wavetable for some Cook FM opcodes.
ifn1 = isine
ifn2 = icosine
ifn3 = isine
ifn4 = icookblank
ivibefn = isine
asignal fmrhode i_amplitude, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 asignal * a_declick * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "Rhodes         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

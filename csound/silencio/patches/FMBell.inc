gk_FMBell_level init 0
instr FMBell
; Authors: John ffitch, Michael Gogins
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
i_overall_amps = 90
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMBell_level)
kc1 = 5
kc2 = 5
kvdepth = 0.025
kvrate = 5.1
icosine ftgenonce 0, 0, 65536, 11, 1
ifn1 = icosine
ifn2 = icosine
ifn3 = icosine
ifn4 = icosine
ivfn = icosine
aout fmbell i_amplitude, i_frequency, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
aenv transegr 0.0, .003, -6, 1.0, 9, -6, 0
aout = aout * aenv
asignal = aout * k_gain
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "FMBell         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

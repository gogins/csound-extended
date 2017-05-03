gk_HeavyMetal_level init 0
instr HeavyMetal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Author: Perry Cook and John ffitch
; Adapted by: Michael Gogins
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
i_overall_amps = (135 - 67)
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_HeavyMetal_level)
iindex = 1.1
icrossfade = 2
ivibedepth = 0.02
iviberate = 4.8
isine ftgenonce 0, 0, 65536, 10, 1
icosine ftgenonce 0, 0, 65536, 11, 1
iexponentialrise ftgenonce 0, 0, 65536, 5, .001, 513, 1 ; Exponential rise.
ithirteen ftgenonce 0, 0, 65536, 9, 1, .3, 0
ifn1 = isine
ifn2 = iexponentialrise
ifn3 = ithirteen
ifn4 = isine
ivibefn = icosine
iattack = 0.003
idecay = 3.0
isustain = p3
irelease = 0.05
adecay transeg 0.0, iattack, -4, 1.0, idecay, -4, 0.1, isustain, -4, 0.1, irelease, -4, 0.0
asignal fmmetal 1.0, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = asignal * adeclick * i_amplitude * k_gain
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "HeavyMetal     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

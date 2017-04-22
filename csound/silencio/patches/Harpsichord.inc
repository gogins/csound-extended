gk_Harpsichord_level init 0
gk_Harpsichord_pick init .075
gk_Harpsichord_reflection init .5
gk_Harpsichord_pluck init .75
instr Harpsichord
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
i_overall_amps = 77.7
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Harpsichord_level)
gk_Harpsichord_pan = .5
iattack = .002
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
k_amplitude = 1
apluck pluck 1, kHz, iHz, 0, 1
iharptable ftgenonce 0, 0, 65536, 7, -1, 1024, 1, 1024, -1
aharp poscil 1, kHz, iharptable
aharp2 balance apluck, aharp
asignal	= (apluck + aharp2) * i_amplitude * aenvelope * k_gain
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = asignal * adeclick
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "Harpsichord    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

gk_Guitar_level init 0
instr Guitar
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
i_overall_amps = 80 ; To start - change to observed value.
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Guitar_level)
acomp pluck i_amplitude, 440.0, 440.0, 0, 1, .1
i_frequency2 = i_frequency / 2.0
kHz = k(i_frequency)
asig pluck i_amplitude, kHz, i_frequency2, 0, 1, .1
aenvelope transeg 1.0, 20.0, -5.0, 0.25
af1 reson asig, 110, 80
af2 reson asig, 220, 100
af3 reson asig, 440, 80
asignal1 balance 0.6 * af1 + af2 + 0.6 * af3 + 0.4 * asig, acomp
asignal2 = asignal1 * aenvelope
asignal dcblock2 asignal2
isustain_ = p3
iattack_ = .003
irelease_ = .01
p3 = isustain_ + iattack_ + irelease_
adeclick linsegr 0, iattack_, 1, isustain_, 1, irelease_, 0
aleft, aright pan2 asignal * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "Guitar         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

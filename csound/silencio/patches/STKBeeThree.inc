gk_STKBeeThree_level init 0
instr STKBeeThree
; Authors: Michael Gogins
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
i_overall_amps = 70
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBeeThree_level)
asignal STKBeeThree i_frequency, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
; ares phaser1 asig, kfreq, kord, kfeedback [, iskip]
aphased phaser1 asignal, 8000, 16, .2, .9
iattack = .002
isustain = p3
irelease = 0.1
p3 = iattack + isustain + irelease
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 aphased * i_amplitude * a_declick * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "STKBeeThree    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

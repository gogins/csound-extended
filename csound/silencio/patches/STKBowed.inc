gk_STKBowed_vibrato_level init 2.8
gk_STKBowed_bow_pressure init 110
gk_STKBowed_bow_position init 20
gk_STKBowed_vibrato_frequency init 50.2
gk_STKBowed_level init 0
instr STKBowed
; Author: Michael Gogins
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
; Adjust the following value until "overall amps" at the end of solo performance is about -6 dB.
i_overall_amps = 70
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_STKBowed_level)
asignal STKBowed i_frequency, 1.0, 1, gk_STKBowed_vibrato_level, 2, gk_STKBowed_bow_pressure, 4, gk_STKBowed_bow_position, 11, gk_STKBowed_vibrato_frequency
iattack = .005
isustain = p3
irelease = 0.1
p3 = iattack + isustain + irelease
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 asignal * i_amplitude * a_declick * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "STKBowed       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

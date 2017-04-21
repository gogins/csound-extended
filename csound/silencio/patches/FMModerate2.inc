gk_FMModerate2_level init 0
instr FMModerate2
if p3 == -1 then
  p3 = 3600
endif
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
i_overall_amps = 83
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FMModerate2_level)
iattack = 0.002
isustain = p3
idecay = 1.5
irelease = 0.05
p3 = iattack + isustain + irelease
icarrier = 1
imodulator = 4
ifmamplitude = 9
index = 2
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, 9.0, 1.0, idecay, 10.0, 0.025, isustain, 0.0, 0.025, irelease, 7.0, 0.0
aindex = aindenv * index * ifmamplitude
icosine ftgenonce 0, 0, 65536, 11, 1
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, icosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, icosine; Plus amplitude correction.
afmout = (aouta + aoutb) * aindenv
aleft, aright pan2 afmout * i_amplitude * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "FMModerate2    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p6
endin

gk_FMModerate_level init 0
instr FMModerate
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
i_overall_amps = 80 ; To start - change to observed value.
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModerate_level)
iattack = 0.002
isustain = p3
idecay = 1.5
irelease = 0.05
p3 = iattack + idecay + isustain + irelease
icarrier = 1
imodulator = 1
ifmamplitude = 8
index = 5.4
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, -7.0, 1.0, idecay, -10.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex = aindenv * index * ifmamplitude
icosine ftgenonce 0, 0, 65536, 11, 1
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, icosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, icosine
asignal = (aouta + aoutb) * aindenv * i_amplitude
aleft, aright pan2 asignal * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "FMModerate     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

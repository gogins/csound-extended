gk_Xing_level init 0
instr Xing
; Author: Andrew Horner
if p3 == -1 then
  p3 = 100000
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
k_gain = ampdb(gk_Xing_level)
isine ftgenonce 0, 0, 65536, 10, 1
iinstrument = p1
istarttime = p2
ioctave = p4
idur = p3
kfreq = kHz
iamp = 1
inorm = 32310
aamp1 linseg 0,.001,5200,.001,800,.001,3000,.0025,1100,.002,2800,.0015,1500,.001,2100,.011,1600,.03,1400,.95,700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0
adevamp1 linseg 0, .05, .3, idur - .05, 0
adev1 poscil adevamp1, 6.7, isine, .8
amp1 = aamp1 * (1 + adev1)
aamp2 linseg 0,.0009,22000,.0005,7300,.0009,11000,.0004,5500,.0006,15000,.0004,5500,.0008,2200,.055,7300,.02,8500,.38,5000,.5,300,.5,73,.5,5.,5,0,1,1
adevamp2 linseg 0,.12,.5,idur-.12,0
adev2 poscil adevamp2, 10.5, isine, 0
amp2 = aamp2 * (1 + adev2)
aamp3 linseg 0,.001,3000,.001,1000,.0017,12000,.0013,3700,.001,12500,.0018,3000,.0012,1200,.001,1400,.0017,6000,.0023,200,.001,3000,.001,1200,.0015,8000,.001,1800,.0015,6000,.08,1200,.2,200,.2,40,.2,10,.4,0,1,0
adevamp3 linseg 0, .02, .8, idur - .02, 0
adev3 poscil adevamp3, 70, isine ,0
amp3 = aamp3 * (1 + adev3),
awt1 poscil amp1, kfreq, isine
awt2 poscil amp2, 2.7 * kfreq, isine
awt3 poscil amp3, 4.95 * kfreq, isine
asig = awt1 + awt2 + awt3
arel linenr 1,0, idur, .06
asignal = asig * arel * (iamp / inorm) * i_amplitude
aleft, aright Pan p7, asignal
p3, aleft, aright Declick 0.005, p3, 0.3, aleft, aright
outleta "outleft", aleft
outleta "outright", aright
endin


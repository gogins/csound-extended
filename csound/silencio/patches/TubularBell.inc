gk_TubularBell_level init 0
gi_TubularBell_crossfade init 2
gi_TubularBell_vibrato_depth init .33333
gi_TubularBell_vibrato_rate init 5
instr TubularBell
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
i_overall_amps = 73
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_TubularBell_level)
iattack = 0.002
isustain = p3
irelease = 0.05
iindex = 1
isine ftgenonce 0, 0, 65536, 10, 1
icosine ftgenonce 0, 0, 65536, 11, 1
icook3 ftgenonce 0, 0, 65536, 10, 1, .4, 0.2, 0.1, 0.1, .05
ifn1 = isine
ifn2 = icook3
ifn3 = isine
ifn4 = isine
ivibefn = icosine
asignal fmbell 1, i_frequency, iindex, gi_TubularBell_crossfade, gi_TubularBell_vibrato_depth, gi_TubularBell_vibrato_rate, ifn1, ifn2, ifn3, ifn4, ivibefn
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 asignal * a_declick * k_gain * i_amplitude, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "TubularBell    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

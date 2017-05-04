gk_FMModulatedChorus_level init 0
instr FMModulatedChorus
; Author: Thomas Kung
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
i_frequency = cpsmidinn(i_midi_key)
i_overall_amps = 80 + 2.90 + 2.15 + 3; To start - adjust by adding observed value.
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModulatedChorus_level)
kHz = k(i_frequency)
iattack = 0.25
isustain = p3
irelease = 0.3333333
p3 = iattack + isustain + irelease
ip6 = 0.3
ip7 = 2.2
ishift = 4.0 / 12000.0
kpch = kHz
koct = octcps(kHz)
isine ftgenonce 0, 0, 65536, 10, 1
icosine ftgenonce 0, 0, 65536, 11, 1
iln ftgenonce 0, 0, 65536, -12, 20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
aadsr linen 1.0, iattack, irelease, 0.01
amodi linseg 0, iattack, 5, p3, 2, irelease, 0
 ; r moves from ip6 to ip7 in p3 secs.
amodr linseg ip6, p3, ip7
a1 = amodi * (amodr - 1 / amodr) / 2
 ; a1*2 is argument normalized from 0-1.
a1ndx = abs(a1 * 2 / 20)
a2 = amodi * (amodr + 1 / amodr) / 2
 ; Look up table is in f43, normalized index.
a3 tablei a1ndx, iln, 1
 ; Cosine
ao1 poscil a1, kpch, icosine
a4 = exp(-0.5 * a3 + ao1)
 ; Cosine
ao2 poscil a2 * kpch, kpch, icosine
 ; Final output left
aleft poscil a4, ao2 + cpsoct(koct + ishift), isine
 ; Final output right
aright poscil a4, ao2 + cpsoct(koct - ishift), isine
adamping linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = aleft + aright
asignal = asignal * adamping * i_amplitude * k_gain
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "FMModChorus    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin

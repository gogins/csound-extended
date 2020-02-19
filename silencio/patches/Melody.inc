gk_Melody_midi_dynamic_range init 127
gk_Melody_level init 0
gi_Melody_chebyshev ftgen 0, 0, 65537, -7, -1, 150, 0.1, 110, 0, 252, 0
gi_Melody_sine ftgen 0, 0, 65537, 10, 1
gi_Melody_cook3 ftgen 0, 0, 65537, 10, 1, .4, 0.2, 0.1, 0.1, .05
instr Melody
; Author: Jon Nelson
; Adapted by: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Melody_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 72
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Melody_level)
ip3 init 3.0
iattack = 0.05
isustain = p3
irelease = 0.1
xtratim iattack + irelease
ip6 = gi_Melody_chebyshev
i1 = i_frequency
k100 randi 1,10
k101 poscil 1, 5 + k100, gi_Melody_sine
ak102 linseg 0, .5, 1, p3, 1
k100 = i1 + (k101 * ak102)
; Envelope for driving oscillator.
; k1 linenr 0.5, ip3 * .3, ip3 * 2, 0.01
k1 linseg 0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2 line 1, p3, .5
k2 linseg 1.0, ip3, .5, isustain, .5, irelease, 0
k1 = k2 * k1
; Amplitude envelope.
k10 expseg 0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
k10 = (k10 - .0001)
; Power to partials.
k20 linseg 1.485, iattack, 1.5, (isustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
a1 poscil k1, k100 - .025, gi_Melody_cook3
; Tables a1 to fn13, others normalize,
a2 tablei a1, ip6, 1, .5
a3 balance a2, a1
; Try other waveforms as well.
a4 foscili 1, k100 + .04, 1, 2.005, k20, gi_Melody_sine
a5 poscil 1, k100, gi_Melody_sine
a6 = ((a3 * .1) + (a4 * .1) + (a5 * .8)) * k10
a7 comb a6, .5, 1 / i1
a8 = (a6 * .9) + (a7 * .1)
asignal balance a8, a1
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 asignal * i_amplitude * a_declick * k_gain, k_space_left_to_right
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

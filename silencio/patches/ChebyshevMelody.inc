gk_ChebyshevMelody_midi_dynamic_range init 127
gk_ChebyshevMelody_level init 0
gi_ChebyshevMelody_chebyshev ftgen 0, 0, 65536, -7, -1, 150, 0.1, 110, 0, 252, 0
gi_ChebyshevMelody_sine ftgen 0, 0, 65536, 10, 1
gi_ChebyshevMelody_cook3 ftgen 0, 0, 65536, 10, 1, .4, 0.2, 0.1, 0.1, .05
instr ChebyshevMelody
///////////////////////////////////////////////////////
// Original by Jon Nelson.
// Adapted by Michael Gogins.
///////////////////////////////////////////////////////
pset 0, 0, 3600
i_instrument = p1
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
i_phase = p6
i_pan = p7
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
iHz = cpsmidinn(i_midikey)
iamplitude = ampdb(i_midivelocity) * 7.
iattack = .01
isustain = p3
irelease = .01
xtratim iattack + irelease
ip6 = gi_ChebyshevMelody_chebyshev
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
kHz = k(iHz)
idB = i_midivelocity
i1 = iHz
k100 randi 1,0.05
ak101 poscil 1, 5 + k100, gi_ChebyshevMelody_sine
ak102 linseg 0, .5, 1, p3, 1
k100 = i1 + (ak101 * ak102)
; Envelope for driving oscillator.
ip3 init 3.0
; k1 linenr 0.5, ip3 * .3, ip3 * 2, 0.01
ak1 linseg 0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2 line 1, p3, .5
ak2 linseg 1.0, ip3, .5, isustain, .5, irelease, 0
ak1 = ak2 * ak1
; Amplitude envelope.
ak10 expseg 0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
ak10 = (ak10 - .0001)
; Power to partials.
k20 linseg 1.485, iattack, 1.5, (isustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
a1 poscil ak1, k100 - .25, gi_ChebyshevMelody_cook3
; Tables a1 to fn13, others normalize,
a2 tablei a1, ip6, 1, .5
a3 balance a2, a1
; Try other waveforms as well.
a4 foscili 1, k100 + .04, 1, 2.000, k20, gi_ChebyshevMelody_sine
a5 poscil 1, k100, gi_ChebyshevMelody_sine
a6 = ((a3 * .1) + (a4 * .1) + (a5 * .8)) * ak10
a7 comb a6, .5, 1 / i1
a8 = (a6 * .9) + (a7 * .1)
asignal balance a8, a1
asignal = asignal * iamplitude
aoutleft, aoutright	pan2 asignal * adeclick, i_pan
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endin
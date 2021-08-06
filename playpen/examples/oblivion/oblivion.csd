<Cabbage> bounds(0, 0, 0, 0)
form size(500, 300), caption("Untitled")
hslider bounds(20, 24, 409, 50) channel("gk_ZakianFlute_level") range(-50, 50, 0, 1, 0.001) text("Flute level") valueTextBox(1) textColour(0, 0, 0, 255) fontColour(0, 0, 0, 255) colour(233, 135, 135, 255) alpha(0.5)
</Cabbage>

<CsoundSynthesizer>
<CsLicense>
"Oblivion,"" by Astor Piazzola
Arranged for Csound by Michael Gogins
</CsLicense>
<CsOptions>
-odac:plughw:2,0 -m195 -d
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1

gi_ampmidicurve_dynamic_range init .375
gi_ampmidicurve_exponent init 5

prealloc "ZakianFlute", 4
prealloc "Guitar", 4
prealloc "Harpsichord", 4
prealloc "YiString", 4
prealloc "Bower", 4

connect "Guitar", "outleft", "ReverbSC", "inleft"
connect "Guitar", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "YiString", "outleft", "ReverbSC", "inleft"
connect "YiString", "outright", "ReverbSC", "inright"
connect "Bower", "outleft", "ReverbSC", "inleft"
connect "Bower", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

alwayson "ReverbSC"
alwayson "MasterOutput"

gk_overlap init .0125

;gk_ZakianFlute_level init -4
gk_ZakianFlute_level chnexport "gk_ZakianFlute_level",3
gk_ZakianFlute_pan init (2 / 7 - .5)
gi_ZakianFLute_seed init .5
gif2 ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gif26 ftgen 0, 0, 65536, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gif27 ftgen 0, 0, 65536, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gif28 ftgen 0, 0, 65536, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gif29 ftgen 0, 0, 65536, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gif30 ftgen 0, 0, 65536, -10, 1229, 16, 34, 57, 32
gif31 ftgen 0, 0, 65536, -10, 163, 31, 1, 50, 31
gif32 ftgen 0, 0, 65536, -10, 4128, 883, 354, 79, 59, 23
gif33 ftgen 0, 0, 65536, -10, 1924, 930, 251, 50, 25, 14
gif34 ftgen 0, 0, 65536, -10, 94, 6, 22, 8
gif35 ftgen 0, 0, 65536, -10, 2661, 87, 33, 18
gif36 ftgen 0, 0, 65536, -10, 174, 12
gif37 ftgen 0, 0, 65536, -10, 314, 13
giwtsin ftgen 0, 0, 65536, 10, 1
instr ZakianFlute
; Author: Lee Zakian
; Adapted by: Michael Gogins
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p1/6
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
k_gain = ampdb(gk_ZakianFlute_level)
iattack = .002
isustain = p3
irelease = .3
xtratim iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
ip3 = (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4 overall amplitude scaling factor
ip4 init i_amplitude
; p5 pitch in Hertz (normal pitch range: C4-C7)
ip5 init iHz
; p6 percent vibrato depth, recommended values in range [-1., +1.]
ip6 init 0.5
; 0.0 -> no vibrato
; +1. -> 1% vibrato depth, where vibrato rate increases slightly
; -1. -> 1% vibrato depth, where vibrato rate decreases slightly
; p7 attack time in seconds
; recommended value: .12 for slurred notes, .06 for tongued notes
; (.03 for short notes)
ip7 init .08
; p8 decay time in seconds
; recommended value: .1 (.05 for short notes)
ip8 init .08
; p9 overall brightness / filter cutoff factor
; 1 -> least bright / minimum filter cutoff frequency (40 Hz)
; 9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9 init 5
; initial variables
iampscale = ip4 ; overall amplitude scaling factor
ifreq = ip5 ; pitch in Hertz
ivibdepth = abs(ip6*ifreq/100.0) ; vibrato depth relative to fundamental frequency
iattack = ip7 * (1.1 - .2*gi_ZakianFLute_seed) ; attack time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947) ; reset gi_ZakianFLute_seed
idecay = ip8 * (1.1 - .2*gi_ZakianFLute_seed) ; decay time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifiltcut tablei ip9, gif2 ; lowpass filter cutoff frequency
iattack = (iattack < 6/kr ? 6/kr : iattack) ; minimal attack length
idecay = (idecay < 6/kr ? 6/kr : idecay) ; minimal decay length
isustain = p3 - iattack - idecay
p3 = (isustain < 5/kr ? iattack+idecay+5/kr : p3) ; minimal sustain length
isustain = (isustain < 5/kr ? 5/kr : isustain)
iatt = iattack/6
isus = isustain/4
idec = idecay/6
iphase = gi_ZakianFLute_seed ; use same phase for all wavetables
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
; vibrato block
; kvibdepth linseg .1, .8*p3, 1, .2*p3, .7
kvibdepth linseg .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth = kvibdepth* ivibdepth ; vibrato depth
kvibdepthr randi .1*kvibdepth, 5, gi_ZakianFLute_seed ; up to 10% vibrato depth variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibdepth = kvibdepth + kvibdepthr
ivibr1 = gi_ZakianFLute_seed ; vibrato rate
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ivibr2 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
if ip6 < 0 goto vibrato1
kvibrate linseg 2.5+ivibr1, p3, 4.5+ivibr2 ; if p6 positive vibrato gets faster
 goto vibrato2
vibrato1:
ivibr3 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
vibrato2:
kvibrater randi .1*kvibrate, 5, gi_ZakianFLute_seed ; up to 10% vibrato rate variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate = kvibrate + kvibrater
kvib oscili kvibdepth, kvibrate, giwtsin
ifdev1 = -.03 * gi_ZakianFLute_seed ; frequency deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev2 = .003 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev3 = -.0015 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev4 = .012 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq = kHz * (1 + kfreqr) + kvib
if ifreq < 427.28 goto range1 ; (cpspch(8.08) + cpspch(8.09))/2
if ifreq < 608.22 goto range2 ; (cpspch(9.02) + cpspch(9.03))/2
if ifreq < 1013.7 goto range3 ; (cpspch(9.11) + cpspch(10.00))/2
goto range4
; wavetable amplitude envelopes
range1: ; for low range tones
kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, \
0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
0.172, idec, 0.053, idec, 0
kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, \
-0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
-0.035, idec, -0.020, idec, 0
kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, \
0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, \
0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, \
0.010, idec, 0.013, idec, 0
iwt1 = gif26 ; wavetable numbers
iwt2 = gif27
iwt3 = gif28
inorm = 3949
goto end
range2: ; for low mid-range tones
kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
0.037, idec, 0.012, idec, 0
kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, \
1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, \
-1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
-0.170, idec, -0.065, idec, 0
kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, \
0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, \
12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
0.174, idec, -0.027, idec, 0
iwt1 = gif29
iwt2 = gif30
iwt3 = gif31
inorm = 27668.2
goto end
range3: ; for high mid-range tones
kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
idec, 0.031, idec, 0
kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
-0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
-0.003, idec, -0.023, idec, 0
kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, \
0.167, idec, -0.059, idec, 0
iwt1 = gif32
iwt2 = gif33
iwt3 = gif34
inorm = 3775
goto end
range4: ; for high range tones
kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, \
0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, \
1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, \
idec, 0.028, idec, 0
kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, \
0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, \
-0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, \
-0.071, idec, -0.019, idec, 0
kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, \
0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, \
0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, \
-0.024, idec, 0.002, idec, 0
iwt1 = gif35
iwt2 = gif36
iwt3 = gif37
inorm = 4909.05
goto end
end:
kampr1 randi .02*kamp1, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp1 = kamp1 + kampr1
kampr2 randi .02*kamp2, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp2 = kamp2 + kampr2
kampr3 randi .02*kamp3, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp3 = kamp3 + kampr3
awt1 poscil kamp1, kfreq, iwt1, iphase ; wavetable lookup
awt2 poscil kamp2, kfreq, iwt2, iphase
awt3 poscil kamp3, kfreq, iwt3, iphase
asig = awt1 + awt2 + awt3
asig = asig*(iampscale/inorm)
kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0 ; lowpass filter for brightness control
afilt tone asig, kcut
a_signal balance afilt, asig
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, p1/6
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "ZakianFlute    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_Guitar_level init 8
instr Guitar
; Michael Gogins
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p1/6
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
k_gain = ampdb(gk_Guitar_level)
acomp pluck i_amplitude, 440.0, 440.0, 0, 1, .1
i_frequency2 = i_frequency / 2.0
kHz = k(i_frequency)
iattack = 0.004
isustain = p3
irelease = 0.05
p3 = iattack + isustain + irelease
asigcomp pluck 1.0, 440, 440, 0, 1
asig pluck 1.0, i_frequency, i_frequency, 0, 1
af1 reson asig, 110, 80
af2 reson asig, 220, 100
af3 reson asig, 440, 80
aout balance 0.6 * af1 + af2 + 0.6 * af3 + 0.4 * asig, asigcomp
aexp expseg 1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
aenv = aexp - 1.0
a_signal = aout * aenv
a_declicking linsegr 0, iattack, 1, isustain, 1, irelease, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, p1/6
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "Guitar         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_YiString_level init 6
gk_YiString_reverb_send init .5
gk_YiString_chorus_send init .5
gi_YiString_overlap init .1
instr YiString
 //////////////////////////////////////////////
 // Original by Steven Yi.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p1/6
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
k_gain = ampdb(gk_YiString_level)
iattack = gi_YiString_overlap
isustain = p3
idecay = gi_YiString_overlap
p3 = iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, i_amplitude / 2.0, iattack / 2.0, -1.5, i_amplitude, isustain, 0.0, i_amplitude, idecay / 2.0, 1.5, i_amplitude / 2.0, idecay / 2.0, -1.5, 0
;ampenv = madsr:a(1, 0.1, 0.95, 0.5)
asignal = vco2(1, i_frequency)
asignal = moogladder(asignal, 6000, 0.1)
a_signal = asignal * aenvelope
i_attack = .002
i_release = 0.01
i_sustain = p3 - (i_attack + i_release)
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, p1/6
outleta "outleft", a_out_left * gk_YiString_reverb_send
outleta "outright", a_out_right * gk_YiString_reverb_send
outleta "chorusleft", a_out_left * gk_YiString_chorus_send
outleta "chorusright", a_out_right * gk_YiString_chorus_send
;printks "YiString         %9.4f  %9.4f\n", 0.5, a_out_left, a_out_right
#endif
prints  "YiString       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_Bower_level init 20
gk_Bower_pressure init 4.2
gisine ftgen 0,0,65536,10,1
instr Bower
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = (4 / 7 - .5)
iamp ampmidicurve ivelocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
krat rspline 0.006,0.988,1,2
kvibf = 4.5
kvibamp = 0
iminfreq = 30
aSig wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,gisine,iminfreq
aleft, aright pan2 aSig / 7, p1/6
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Bower_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Bower          i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_Harpsichord_level init 0
gk_Harpsichord_pick init .275
gk_Harpsichord_reflection init .75
gk_Harpsichord_pluck init .5
giharptable ftgen 0, 0, 65536, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
if p3 == -1 goto indefinite
goto non_indefinite
indefinite:
  p3 = 1000000
non_indefinite:
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = .2
k_space_bottom_to_top = p8
i_phase = p9
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
k_amplitude = 1
apluck pluck 1, kHz, iHz, 0, 1
aharp poscil 1, kHz, giharptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2)
i_attack = .002
i_sustain = p3
i_release = 0.01
p3 = i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, p1/6
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
; printks "Harpsichord      %9.4f   %9.4f\n", 0.5, a_out_left, a_out_right
prints "Harpsichord    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
kpbend    pchbend   2
printks2 "pchbend %9.4f\n", kpbend
kmodw     midictrl  1
printks2 "kmodw   %9.4f\n", kmodw
kctl6     midictrl  6
printks2 "kctl6   %9.4f\n", kctl6
kctl4     midictrl  4
printks2 "kctl4   %9.4f\n", kctl4
kctl5     midictrl  5
printks2 "kctl5   %9.4f\n", kctl5
kafter    aftouch   1
printks2 "kafter  %9.4f\n", kafter

endin

gk_Reverb_feedback init 0.85
gi_Reverb_delay_modulation init 0.05
gk_Reverb_frequency_cutoff init 15000
instr ReverbSC
aleftout init 0
arightout init 0
aleft inleta "inleft"
aright inleta "inright"
; aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]
aleftout, arightout reverbsc aleft, aright, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
outleta "outleft", aleftout
outleta "outright", arightout
prints "ReverbSC       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p1/6, active(p1)
endin

gk_MasterOutput_level init -15
gS_MasterOutput_filename init ""
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\n", k_gain
iamp init 1
iattack init .01
idecay init 10
isustain = 2400 - (iattack + idecay)
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain * aenvelope, aright * k_gain * aenvelope
; We want something that will play on my phone.
i_amplitude_adjustment = ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto has_filename
goto non_has_filename
has_filename:
prints sprintf("Output filename: %s\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
non_has_filename:
prints "MasterOutput   i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p1/6, active(p1)
kstatus, kchan, kdata1, kdata2 midiin
;printf "          midi in s %4d c %4d %4d %4d\n", kdata2, kstatus, kchan, kdata1, kdata2
endin

</CsInstruments>
<CsScore>

; track 1

t	    0.0000	   76.0
i 1	    1.5000	    0.4979	 60	 49
i 1	    2.0000	    0.4979	 62	 49
i 1	    2.5000	    0.4979	 63	 49
i 1	    3.0000	    0.9979	 65	 49
i 1	    5.5000	    0.4979	 60	 49
i 1	    6.0000	    0.4979	 62	 49
i 1	    6.5000	    0.4979	 63	 49
i 1	    7.0000	    0.9979	 65	 49
i 1	    9.5000	    0.4979	 60	 49
i 1	   10.0000	    0.4979	 62	 49
i 1	   10.5000	    0.4979	 63	 49
i 1	   11.0000	    0.9979	 65	 49
i 1	   13.5000	    0.4979	 60	 49
i 1	   14.0000	    0.4979	 62	 49
i 1	   14.5000	    0.4979	 63	 49
i 1	   15.0000	    0.9979	 65	 49
i 1	   17.5000	    0.4979	 60	 49
i 1	   18.0000	    0.4979	 62	 49
i 1	   18.5000	    0.4979	 63	 49
i 1	   19.0000	    0.9979	 65	 49
i 1	   21.5000	    0.4979	 60	 49
i 1	   22.0000	    0.4979	 62	 49
i 1	   22.5000	    0.4979	 63	 49
i 1	   23.0000	    0.9979	 65	 49
i 1	   25.5000	    0.4979	 60	 49
i 1	   26.0000	    0.4979	 62	 49
i 1	   26.5000	    0.4979	 63	 49
i 1	   27.0000	    0.9979	 65	 49
i 1	   29.5000	    0.4979	 60	 49
i 1	   30.0000	    0.4979	 62	 49
i 1	   30.5000	    0.4979	 63	 49
i 1	   31.0000	    0.9979	 65	 49
i 1	   32.0000	    1.9979	 68	 49
i 1	   34.0000	    1.9979	 68	 49
i 1	   36.0000	    1.9979	 67	 49
i 1	   38.0000	    1.9979	 67	 49
i 1	   40.0000	    1.9979	 65	 49
i 1	   42.0000	    1.9979	 66	 49
i 1	   44.0000	    1.9979	 67	 49
i 1	   46.0000	    0.4979	 67	 49
i 1	   46.5000	    0.4979	 67	 49
i 1	   47.0000	    0.2479	 68	 49
i 1	   47.2500	    0.2479	 67	 49
i 1	   47.5000	    0.2479	 66	 49
i 1	   47.7500	    0.2479	 67	 49
i 1	   48.0000	    4.6646	 79	 49
i 1	   52.6667	    0.6646	 77	 49
i 1	   53.3333	    0.6646	 75	 49
i 1	   54.0000	    0.7479	 74	 49
i 1	   54.7500	    0.2479	 72	 49
i 1	   55.0000	    0.2479	 74	 49
i 1	   55.2500	    0.2479	 75	 49
i 1	   55.5000	    0.2479	 74	 49
i 1	   55.7500	    0.2479	 72	 49
i 1	   56.0000	    4.9979	 75	 49
i 1	   61.0000	    0.4979	 74	 49
i 1	   61.5000	    0.4979	 72	 49
i 1	   62.0000	    1.4979	 70	 49
i 1	   63.5000	    0.4979	 68	 49
i 1	   64.0000	    2.9979	 72	 49
i 1	   67.0000	    0.2479	 70	 49
i 1	   67.2500	    0.2479	 72	 49
i 1	   67.5000	    0.2479	 70	 49
i 1	   67.7500	    0.2479	 68	 49
i 1	   68.0000	    1.4979	 70	 49
i 1	   69.5000	    0.4979	 73	 49
i 1	   70.0000	    0.4979	 73	 49
i 1	   70.5000	    0.9979	 72	 49
i 1	   71.5000	    0.2479	 70	 49
i 1	   71.7500	    0.2479	 70	 49
i 1	   72.0000	    1.4979	 70	 49
i 1	   73.5000	    0.2479	 68	 49
i 1	   73.7500	    0.2479	 70	 49
i 1	   74.0000	    1.9979	 68	 49
i 1	   76.0000	    1.9979	 68	 49
i 1	   79.2500	    0.2479	 72	 96
i 1	   79.5000	    0.2479	 77	 96
i 1	   79.7500	    0.2479	 79	 96
i 1	   80.0000	    0.4979	 79	 96
i 1	   80.5000	    0.9979	 80	 96
i 1	   81.5000	    0.2479	 79	 96
i 1	   81.7500	    0.2479	 77	 96
i 1	   82.0000	    1.9979	 80	 96
i 1	   84.5000	    0.4979	 79	 96
i 1	   85.0000	    0.2479	 82	 96
i 1	   85.2500	    0.2479	 80	 96
i 1	   85.5000	    0.2479	 79	 96
i 1	   85.7500	    0.2479	 77	 96
i 1	   86.0000	    1.4979	 77	 96
i 1	   87.5000	    0.2479	 75	 96
i 1	   87.7500	    0.2479	 77	 96
i 1	   88.0000	    0.2479	 75	 96
i 1	   88.2500	    0.2479	 72	 96
i 1	   88.5000	    0.4979	 72	 96
i 1	   89.0000	    2.9979	 72	 96
i 1	   94.0000	    0.7479	 77	 96
i 1	   94.7500	    0.7479	 75	 96
i 1	   95.5000	    0.2479	 74	 96
i 1	   95.7500	    0.2479	 72	 96
i 1	   96.0000	    2.9979	 66	 96
i 1	   99.0000	    0.4979	 66	 96
i 1	   99.5000	    0.4979	 66	 96
i 1	  100.0000	    1.9979	 67	 96
i 1	  102.0000	    0.4979	 68	 96
i 1	  102.5000	    0.9979	 67	 96
i 1	  103.5000	    0.2479	 66	 96
i 1	  103.7500	    0.2479	 67	 96
i 1	  104.0000	    5.9979	 72	 96
i 1	  110.0000	    0.4979	 72	 96
i 1	  110.5000	    0.4979	 79	 96
i 1	  111.0000	    0.4979	 84	 96
i 1	  111.5000	    0.4979	 86	 96
i 1	  112.0000	    2.9979	 87	 96
i 1	  115.0000	    0.4979	 84	 96
i 1	  115.5000	    0.4979	 86	 96
i 1	  116.0000	    0.6646	 87	 96
i 1	  116.6667	    0.6646	 86	 96
i 1	  117.3333	    0.6646	 89	 96
i 1	  118.0000	    0.9979	 82	 96
i 1	  119.0000	    0.9979	 80	 96
i 1	  120.0000	    0.1375	 79	 96
i 1	  120.1396	    0.1375	 80	 96
i 1	  120.2812	    0.1375	 81	 96
i 1	  120.4250	    0.1375	 82	 96
i 1	  120.5646	    0.1375	 83	 96
i 1	  120.7062	    0.1375	 84	 96
i 1	  120.8500	    0.1479	 85	 96
i 1	  121.0000	    2.9979	 86	 96
i 1	  124.0000	    0.6646	 86	 96
i 1	  124.6667	    0.6646	 84	 96
i 1	  125.3333	    0.6646	 87	 96
i 1	  126.0000	    0.9979	 80	 96
i 1	  127.0000	    0.9979	 79	 96
i 1	  128.0000	    0.1375	 77	 96
i 1	  128.1396	    0.1375	 78	 96
i 1	  128.2812	    0.1375	 79	 96
i 1	  128.4250	    0.1375	 80	 96
i 1	  128.5646	    0.1375	 81	 96
i 1	  128.7062	    0.1375	 82	 96
i 1	  128.8500	    0.1479	 83	 96
i 1	  129.0000	    2.9979	 84	 96
i 1	  132.0000	    0.6646	 84	 96
i 1	  132.6667	    0.6646	 83	 96
i 1	  133.3333	    0.6646	 86	 96
i 1	  134.0000	    0.9979	 79	 96
i 1	  135.0000	    0.9979	 77	 96
i 1	  136.0000	    1.4979	 75	 96
i 1	  137.5000	    1.4979	 77	 96
i 1	  139.0000	    0.9979	 79	 96
i 1	  140.0000	    3.9979	 79	 96
i 1	  160.0000	    3.9979	 60	 33
i 1	  164.0000	    3.9979	 65	 33
i 1	  168.0000	    1.4979	 67	 33
i 1	  169.5000	    1.4979	 68	 33
i 1	  171.0000	    0.9979	 67	 33
i 1	  172.0000	    1.4979	 70	 33
i 1	  173.5000	    1.4979	 68	 33
i 1	  175.0000	    0.9979	 67	 33
i 1	  177.5000	    0.4979	 60	 33
i 1	  178.0000	    0.4979	 62	 33
i 1	  178.5000	    0.4979	 63	 33
i 1	  179.0000	    0.9979	 65	 33
i 1	  181.5000	    0.4979	 60	 33
i 1	  182.0000	    0.4979	 62	 33
i 1	  182.5000	    0.4979	 63	 33
i 1	  183.0000	    0.9979	 65	 33
i 1	  185.5000	    0.4979	 60	 33
i 1	  186.0000	    0.4979	 62	 33
i 1	  186.5000	    0.4979	 63	 33
i 1	  187.0000	    0.9979	 65	 33
i 1	  189.5000	    0.4979	 60	 33
i 1	  190.0000	    0.4979	 62	 33
i 1	  190.5000	    0.4979	 63	 33
i 1	  191.0000	    0.9979	 65	 33
i 1	  192.0000	    1.9979	 68	 33
i 1	  194.0000	    1.9979	 68	 33
i 1	  196.0000	    1.9979	 67	 33
i 1	  198.0000	    1.9979	 67	 33
i 1	  200.0000	    1.9979	 65	 33
i 1	  202.0000	    1.9979	 66	 33
i 1	  204.0000	    2.9979	 67	 33
i 1	  207.2500	    0.2479	 67	 49
i 1	  207.5000	    0.2479	 67	 49
i 1	  207.7500	    0.2479	 68	 49
i 1	  208.0000	    0.4979	 79	 49
i 1	  208.5000	    0.4979	 79	 49
i 1	  209.0000	    2.9979	 79	 49
i 1	  212.0000	    0.6646	 79	 49
i 1	  212.6667	    0.6646	 77	 49
i 1	  213.3333	    0.6646	 75	 49
i 1	  214.0000	    0.7479	 74	 49
i 1	  214.7500	    0.2479	 72	 49
i 1	  215.0000	    0.2479	 74	 49
i 1	  215.2500	    0.2479	 72	 49
i 1	  215.5000	    0.2479	 71	 49
i 1	  215.7500	    0.2479	 72	 49
i 1	  216.0000	    0.4979	 75	 49
i 1	  216.5000	    0.4979	 75	 49
i 1	  217.0000	    2.9979	 75	 49
i 1	  220.5000	    0.4979	 74	 49
i 1	  221.0000	    0.2479	 77	 49
i 1	  221.2500	    0.2479	 75	 49
i 1	  221.5000	    0.2479	 74	 49
i 1	  221.7500	    0.2479	 72	 49
i 1	  222.0000	    0.4979	 70	 49
i 1	  222.5000	    0.9979	 68	 49
i 1	  223.5000	    0.2479	 67	 49
i 1	  223.7500	    0.2479	 68	 49
i 1	  224.0000	    0.4979	 72	 49
i 1	  224.5000	    1.4979	 72	 49
i 1	  226.0000	    0.4979	 72	 49
i 1	  226.5000	    0.4979	 70	 49
i 1	  227.0000	    0.4979	 70	 49
i 1	  227.5000	    0.4979	 70	 49
i 1	  228.0000	    0.4979	 68	 49
i 1	  228.5000	    0.4979	 67	 49
i 1	  229.0000	    0.9979	 67	 49
i 1	  230.0000	    0.4979	 65	 49
i 1	  230.5000	    0.9979	 65	 49
i 1	  231.5000	    0.4979	 67	 49
i 1	  232.0000	    0.4979	 70	 49
i 1	  232.5000	    0.2479	 68	 49
i 1	  232.7500	    0.2479	 67	 49
i 1	  233.0000	    2.9979	 68	 49
i 1	  236.0000	    1.9979	 68	 49
i 1	  239.2500	    0.2479	 72	 49
i 1	  239.5000	    0.2479	 77	 49
i 1	  239.7500	    0.2479	 79	 49
i 1	  240.0000	    1.4979	 82	 96
i 1	  241.5000	    0.2479	 80	 96
i 1	  241.7500	    0.2479	 79	 96
i 1	  242.0000	    1.9979	 80	 96
i 1	  244.5000	    0.4979	 79	 96
i 1	  245.0000	    0.2479	 82	 96
i 1	  245.2500	    0.2479	 80	 96
i 1	  245.5000	    0.2479	 79	 96
i 1	  245.7500	    0.2479	 77	 96
i 1	  246.0000	    0.2479	 79	 96
i 1	  246.2500	    0.2479	 77	 96
i 1	  246.5000	    0.2479	 75	 96
i 1	  246.7500	    0.2479	 74	 96
i 1	  247.0000	    0.2479	 72	 96
i 1	  247.2500	    0.2479	 74	 96
i 1	  247.5000	    0.2479	 75	 96
i 1	  247.7500	    0.2479	 77	 96
i 1	  248.0000	    0.4979	 79	 96
i 1	  248.5000	    0.4979	 72	 96
i 1	  249.0000	    2.9979	 72	 96
i 1	  252.0000	    1.9979	 72	 96
i 1	  254.0000	    0.4979	 77	 96
i 1	  254.5000	    0.9979	 75	 93
i 1	  255.5000	    0.2479	 74	 86
i 1	  255.7500	    0.2479	 72	 84
i 1	  256.0000	    2.9979	 78	 82
i 1	  259.0000	    0.4979	 78	 61
i 1	  259.5000	    0.4979	 78	 58
i 1	  260.0000	    2.9979	 79	 54
i 1	  263.0000	    0.9979	 91	 33
i 1	  264.0000	    3.9979	 96	 33
i 1	  268.0000	    3.9979	 96	 33
i 1	  272.0000	    3.9979	 96	 33
i 1	  276.0000	    3.9979	 96	 33
i 1	  280.0000	    0.0542	 96	 33
i 1	  280.0562	    0.0542	 95	 33
i 1	  280.1146	    0.0542	 94	 33
i 1	  280.1729	    0.0542	 93	 33
i 1	  280.2312	    0.0542	 92	 33
i 1	  280.2896	    0.0542	 91	 33
i 1	  280.3500	    0.0542	 90	 33
i 1	  280.4062	    0.0542	 89	 33
i 1	  280.4646	    0.0542	 88	 33
i 1	  280.5229	    0.0542	 87	 33
i 1	  280.5813	    0.0542	 86	 33
i 1	  280.6396	    0.0542	 85	 33
i 1	  280.7000	    0.0542	 84	 33
i 1	  280.7563	    0.0542	 83	 33
i 1	  280.8146	    0.0542	 82	 33
i 1	  280.8729	    0.0542	 81	 33
i 1	  280.9313	    0.0542	 80	 33
i 1	  280.9896	    0.0542	 79	 33
i 1	  281.0500	    0.0542	 78	 33
i 1	  281.1063	    0.0542	 77	 33
i 1	  281.1646	    0.0542	 76	 33
i 1	  281.2229	    0.0542	 75	 33
i 1	  281.2813	    0.0542	 74	 33
i 1	  281.3396	    0.0542	 73	 33
i 1	  281.4000	    0.0542	 72	 33
i 1	  281.4563	    0.0542	 71	 33
i 1	  281.5146	    0.0542	 70	 33
i 1	  281.5729	    0.0542	 69	 33
i 1	  281.6313	    0.0542	 68	 33
i 1	  281.6896	    0.0542	 67	 33
i 1	  281.7500	    0.0542	 66	 33
i 1	  281.8063	    0.0542	 65	 33
i 1	  281.8646	    0.0542	 64	 33
i 1	  281.9229	    0.0708	 63	 33
i 1	  282.0000	    0.1208	 62	 33
i 1	  282.1229	    0.1208	 62	 33
i 1	  282.2479	    0.1208	 62	 33
i 1	  282.3708	    0.1208	 62	 33
i 1	  282.4958	    0.1208	 62	 33
i 1	  282.6188	    0.1208	 62	 33
i 1	  282.7438	    0.1208	 62	 33
i 1	  282.8667	    0.1208	 62	 33
i 1	  282.9917	    0.1208	 62	 33
i 1	  283.1146	    0.1208	 62	 33
i 1	  283.2396	    0.1208	 62	 33
i 1	  283.3625	    0.1208	 62	 33
i 1	  283.4875	    0.1208	 62	 33
i 1	  283.6104	    0.1208	 62	 33
i 1	  283.7354	    0.1208	 62	 33
i 1	  283.8583	    0.1208	 62	 33

; track 2

i 2	    0.5000	    0.4979	 55	 49
i 2	    1.0000	    2.9979	 56	 49
i 2	    4.5000	    0.4979	 55	 49
i 2	    5.0000	    2.9979	 56	 49
i 2	    8.5000	    0.4979	 55	 49
i 2	    9.0000	    2.9979	 56	 49
i 2	   12.5000	    0.4979	 55	 49
i 2	   13.0000	    2.9979	 56	 49
i 2	   16.5000	    0.4979	 55	 49
i 2	   17.0000	    2.9979	 56	 49
i 2	   20.5000	    0.4979	 55	 49
i 2	   21.0000	    2.9979	 56	 49
i 2	   24.5000	    0.4979	 55	 49
i 2	   25.0000	    2.9979	 56	 49
i 2	   28.5000	    0.4979	 55	 49
i 2	   29.0000	    2.9979	 56	 49
i 2	   32.5000	    0.4979	 63	 49
i 2	   33.0000	    0.4979	 62	 49
i 2	   33.5000	    0.4979	 63	 49
i 2	   34.5000	    0.4979	 63	 49
i 2	   35.0000	    0.9979	 62	 49
i 2	   36.5000	    0.4979	 62	 49
i 2	   37.0000	    0.4979	 60	 49
i 2	   37.5000	    0.4979	 62	 49
i 2	   38.5000	    0.4979	 62	 49
i 2	   39.0000	    0.4979	 60	 49
i 2	   39.5000	    0.4979	 62	 49
i 2	   40.5000	    0.4979	 62	 49
i 2	   41.0000	    0.4979	 60	 48
i 2	   41.5000	    0.4979	 62	 47
i 2	   42.5000	    0.4979	 62	 45
i 2	   43.0000	    0.4979	 60	 43
i 2	   43.5000	    0.4979	 62	 42
i 2	   44.5000	    0.4979	 60	 40
i 2	   45.0000	    0.4979	 62	 38
i 2	   45.5000	    0.4979	 59	 37
i 2	   46.0000	    0.9979	 63	 36
i 2	   47.0000	    0.9979	 62	 33
i 2	   49.5000	    0.4979	 60	 33
i 2	   50.0000	    0.4979	 62	 33
i 2	   50.5000	    0.4979	 63	 33
i 2	   51.0000	    0.9979	 65	 33
i 2	   53.5000	    0.4979	 60	 33
i 2	   54.0000	    0.4979	 62	 33
i 2	   54.5000	    0.4979	 63	 33
i 2	   55.0000	    0.9979	 65	 33
i 2	   57.5000	    0.4979	 60	 33
i 2	   58.0000	    0.4979	 62	 33
i 2	   58.5000	    0.4979	 63	 33
i 2	   59.0000	    0.9979	 65	 33
i 2	   61.5000	    0.4979	 60	 33
i 2	   62.0000	    0.4979	 62	 33
i 2	   62.5000	    0.4979	 63	 33
i 2	   63.0000	    0.9979	 65	 33
i 2	   64.0000	    0.9979	 65	 33
i 2	   65.0000	    0.9979	 67	 48
i 2	   66.0000	    0.9979	 64	 64
i 2	   67.0000	    0.9979	 65	 80
i 2	   68.0000	    1.9979	 68	 80
i 2	   70.0000	    1.9979	 67	 80
i 2	   72.0000	    0.9979	 60	 80
i 2	   73.0000	    0.9979	 62	 80
i 2	   74.0000	    0.9979	 63	 80
i 2	   75.0000	    0.9979	 65	 80
i 2	   76.0000	    1.4979	 67	 80
i 2	   77.5000	    0.2479	 65	 80
i 2	   77.7500	    0.2479	 63	 80
i 2	   78.0000	    0.9979	 62	 80
i 2	   79.0000	    0.9979	 60	 80
i 2	   80.0000	    0.9979	 65	 80
i 2	   81.0000	    1.9979	 67	 80
i 2	   83.0000	    0.9979	 65	 80
i 2	   84.0000	    0.9979	 68	 80
i 2	   85.0000	    1.9979	 67	 80
i 2	   87.0000	    0.9979	 65	 80
i 2	   88.0000	    0.9979	 63	 80
i 2	   89.0000	    1.9979	 65	 80
i 2	   91.0000	    0.9979	 63	 80
i 2	   92.0000	    0.9979	 67	 80
i 2	   93.0000	    1.9979	 65	 80
i 2	   95.0000	    0.9979	 63	 80
i 2	   96.0000	    0.9979	 62	 80
i 2	   97.0000	    1.9979	 63	 80
i 2	   99.0000	    0.9979	 62	 80
i 2	  100.0000	    0.9979	 62	 80
i 2	  101.0000	    1.9979	 63	 80
i 2	  103.0000	    0.9979	 62	 80
i 2	  104.0000	    1.4979	 67	 80
i 2	  105.5000	    1.4979	 68	 80
i 2	  107.0000	    0.9979	 67	 80
i 2	  108.0000	    1.4979	 70	 80
i 2	  109.5000	    1.4979	 68	 80
i 2	  111.0000	    0.9979	 67	 80
i 2	  112.0000	    2.9979	 72	 96
i 2	  115.0000	    0.4979	 68	 96
i 2	  115.5000	    0.4979	 70	 96
i 2	  116.0000	    0.6646	 72	 96
i 2	  116.6667	    0.6646	 70	 96
i 2	  117.3333	    0.6646	 74	 96
i 2	  118.0000	    0.9979	 67	 96
i 2	  119.0000	    0.9979	 65	 96
i 2	  120.0000	    0.1375	 63	 96
i 2	  120.1396	    0.1375	 64	 96
i 2	  120.2813	    0.1375	 65	 96
i 2	  120.4250	    0.1375	 66	 96
i 2	  120.5646	    0.1375	 67	 96
i 2	  120.7063	    0.1375	 68	 96
i 2	  120.8500	    0.1479	 69	 96
i 2	  121.0000	    2.9979	 70	 96
i 2	  124.0000	    0.6646	 70	 96
i 2	  124.6667	    0.6646	 68	 96
i 2	  125.3333	    0.6646	 72	 96
i 2	  126.0000	    0.9979	 65	 96
i 2	  127.0000	    0.9979	 63	 96
i 2	  128.0000	    0.1625	 62	 96
i 2	  128.1646	    0.1625	 63	 96
i 2	  128.3313	    0.1625	 64	 96
i 2	  128.5000	    0.1625	 65	 96
i 2	  128.6646	    0.1625	 66	 96
i 2	  128.8312	    0.1625	 67	 96
i 2	  129.0000	    2.9979	 68	 96
i 2	  132.0000	    0.6646	 68	 96
i 2	  132.6667	    0.6646	 67	 96
i 2	  133.3333	    0.6646	 71	 96
i 2	  134.0000	    0.9979	 63	 96
i 2	  135.0000	    0.9979	 62	 96
i 2	  136.0000	    1.4979	 60	 96
i 2	  137.5000	    1.4979	 60	 96
i 2	  139.0000	    0.9979	 60	 96
i 2	  140.0000	    2.9979	 61	 96
i 2	  143.0000	    0.2479	 68	 96
i 2	  143.2500	    0.2479	 67	 96
i 2	  143.5000	    0.2479	 66	 96
i 2	  143.7500	    0.2479	 67	 96
i 2	  144.0000	    2.9979	 75	 49
i 2	  147.0000	    0.4979	 72	 49
i 2	  147.5000	    0.4979	 74	 49
i 2	  148.0000	    0.6646	 75	 49
i 2	  148.6667	    0.6646	 74	 49
i 2	  149.3333	    0.6646	 77	 49
i 2	  150.0000	    0.9979	 70	 49
i 2	  151.0000	    0.9979	 68	 49
i 2	  152.0000	    0.9979	 67	 49
i 2	  153.0000	    2.9979	 74	 49
i 2	  156.0000	    0.6646	 74	 49
i 2	  156.6667	    0.6646	 72	 49
i 2	  157.3333	    0.6646	 75	 49
i 2	  158.0000	    0.9979	 68	 49
i 2	  159.0000	    0.9979	 67	 49
i 2	  160.0000	    3.9979	 66	 49
i 2	  164.0000	    2.9979	 67	 49
i 2	  167.0000	    0.4979	 67	 49
i 2	  167.5000	    0.4979	 67	 49
i 2	  168.0000	    5.9979	 72	 49
i 2	  176.5000	    0.4979	 55	 33
i 2	  177.0000	    2.9979	 56	 33
i 2	  180.5000	    0.4979	 55	 33
i 2	  181.0000	    2.9979	 56	 33
i 2	  184.5000	    0.4979	 55	 33
i 2	  185.0000	    2.9979	 56	 33
i 2	  188.5000	    0.4979	 55	 33
i 2	  189.0000	    2.9979	 56	 33
i 2	  192.5000	    0.4979	 63	 33
i 2	  193.0000	    0.4979	 62	 33
i 2	  193.5000	    0.4979	 63	 33
i 2	  194.5000	    0.4979	 63	 33
i 2	  195.0000	    0.9979	 62	 33
i 2	  196.5000	    0.4979	 62	 33
i 2	  197.0000	    0.4979	 60	 33
i 2	  197.5000	    0.4979	 62	 33
i 2	  198.5000	    0.4979	 62	 33
i 2	  199.0000	    0.4979	 60	 33
i 2	  199.5000	    0.4979	 62	 33
i 2	  200.5000	    0.4979	 62	 33
i 2	  201.0000	    0.4979	 60	 33
i 2	  201.5000	    0.4979	 62	 33
i 2	  202.5000	    0.4979	 62	 33
i 2	  203.0000	    0.4979	 60	 33
i 2	  203.5000	    0.4979	 62	 33
i 2	  204.5000	    0.4979	 60	 33
i 2	  205.0000	    0.4979	 62	 33
i 2	  205.5000	    0.4979	 59	 33
i 2	  206.0000	    0.9979	 63	 33
i 2	  207.0000	    0.9979	 62	 33
i 2	  209.5000	    0.4979	 60	 33
i 2	  210.0000	    0.4979	 62	 33
i 2	  210.5000	    0.4979	 63	 33
i 2	  211.0000	    0.9979	 65	 33
i 2	  213.5000	    0.4979	 60	 33
i 2	  214.0000	    0.4979	 62	 33
i 2	  214.5000	    0.4979	 63	 33
i 2	  215.0000	    0.9979	 65	 33
i 2	  217.5000	    0.4979	 60	 33
i 2	  218.0000	    0.4979	 62	 33
i 2	  218.5000	    0.4979	 63	 33
i 2	  219.0000	    0.9979	 65	 33
i 2	  221.5000	    0.4979	 60	 33
i 2	  222.0000	    0.4979	 62	 33
i 2	  222.5000	    0.4979	 63	 33
i 2	  223.0000	    0.9979	 65	 33
i 2	  224.0000	    0.9979	 65	 33
i 2	  225.0000	    0.9979	 67	 33
i 2	  226.0000	    0.9979	 64	 33
i 2	  227.0000	    0.9979	 65	 33
i 2	  228.0000	    1.9979	 68	 33
i 2	  230.0000	    1.9979	 67	 33
i 2	  232.0000	    0.9979	 60	 33
i 2	  233.0000	    0.9979	 62	 33
i 2	  234.0000	    0.9979	 63	 33
i 2	  235.0000	    0.9979	 65	 33
i 2	  236.0000	    1.4979	 67	 33
i 2	  237.5000	    0.2479	 65	 33
i 2	  237.7500	    0.2479	 63	 33
i 2	  238.0000	    0.9979	 62	 33
i 2	  239.0000	    0.9979	 60	 33
i 2	  240.0000	    0.9979	 65	 80
i 2	  241.0000	    1.9979	 67	 80
i 2	  243.0000	    0.9979	 65	 80
i 2	  244.0000	    0.9979	 68	 80
i 2	  245.0000	    1.9979	 67	 80
i 2	  247.0000	    0.9979	 65	 80
i 2	  248.0000	    0.9979	 63	 80
i 2	  249.0000	    1.9979	 65	 80
i 2	  251.0000	    0.9979	 63	 80
i 2	  252.0000	    0.9979	 67	 80
i 2	  253.0000	    1.9979	 65	 80
i 2	  255.0000	    0.9979	 63	 74
i 2	  256.0000	    0.9979	 62	 71
i 2	  257.0000	    1.9979	 63	 68
i 2	  259.0000	    0.9979	 62	 62
i 2	  260.0000	    0.9979	 62	 59
i 2	  261.0000	    1.9979	 63	 56
i 2	  263.0000	    0.9979	 62	 49
i 2	  265.5000	    0.4979	 60	 49
i 2	  266.0000	    0.4979	 62	 49
i 2	  266.5000	    0.4979	 63	 49
i 2	  267.0000	    0.4979	 65	 49
i 2	  267.5000	    0.4979	 67	 49
i 2	  269.5000	    0.4979	 60	 49
i 2	  270.0000	    0.4979	 62	 49
i 2	  270.5000	    0.4979	 63	 49
i 2	  271.0000	    0.4979	 65	 49
i 2	  271.5000	    0.4979	 67	 49
i 2	  273.5000	    0.4979	 60	 49
i 2	  274.0000	    0.4979	 62	 49
i 2	  274.5000	    0.4979	 63	 49
i 2	  275.0000	    0.4979	 65	 49
i 2	  275.5000	    0.4979	 67	 49
i 2	  277.5000	    0.4979	 60	 49
i 2	  278.0000	    0.4979	 62	 49
i 2	  278.5000	    0.4979	 63	 49
i 2	  279.0000	    0.4979	 65	 49
i 2	  279.5000	    0.4979	 67	 49
i 2	  280.0000	    3.9979	 63	 33

; track 3

i 3	   16.0000	    4.6646	 67	 64
i 3	   20.6667	    0.6646	 65	 64
i 3	   21.3333	    0.6646	 63	 64
i 3	   22.0000	    1.4979	 62	 64
i 3	   23.5000	    0.4979	 60	 64
i 3	   24.0000	    5.4979	 63	 64
i 3	   29.5000	    0.2479	 62	 64
i 3	   29.7500	    0.2479	 60	 64
i 3	   30.0000	    1.4979	 58	 64
i 3	   31.5000	    0.4979	 56	 64
i 3	   32.0000	    2.9979	 60	 64
i 3	   35.0000	    0.2479	 58	 64
i 3	   35.2500	    0.2479	 60	 64
i 3	   35.5000	    0.2479	 58	 64
i 3	   35.7500	    0.2479	 56	 64
i 3	   36.0000	    1.4979	 58	 64
i 3	   37.5000	    0.4979	 60	 64
i 3	   38.0000	    0.4979	 60	 64
i 3	   38.5000	    0.4979	 58	 64
i 3	   39.0000	    0.4979	 56	 64
i 3	   39.5000	    0.4979	 55	 64
i 3	   40.0000	    1.4979	 56	 64
i 3	   41.5000	    0.2479	 55	 53
i 3	   41.7500	    0.2479	 53	 51
i 3	   42.0000	    0.9979	 51	 49
i 3	   43.0000	    0.9979	 50	 41
i 3	   44.0000	    3.9979	 55	 33
i 3	   48.5000	    0.4979	 55	 33
i 3	   49.0000	    2.9979	 56	 33
i 3	   52.5000	    0.4979	 55	 33
i 3	   53.0000	    2.9979	 56	 33
i 3	   56.5000	    0.4979	 55	 33
i 3	   57.0000	    2.9979	 56	 33
i 3	   60.5000	    0.4979	 55	 33
i 3	   61.0000	    2.9979	 56	 33
i 3	   64.0000	    1.4979	 61	 33
i 3	   65.5000	    1.4979	 61	 33
i 3	   67.0000	    0.9979	 61	 33
i 3	   68.0000	    1.4979	 61	 33
i 3	   69.5000	    1.4979	 61	 33
i 3	   71.0000	    0.9979	 61	 33
i 3	   72.0000	    1.4979	 60	 33
i 3	   73.5000	    1.4979	 60	 33
i 3	   75.0000	    0.9979	 60	 33
i 3	   76.0000	    1.4979	 60	 33
i 3	   77.5000	    1.4979	 60	 33
i 3	   79.0000	    0.9979	 60	 33
i 3	   80.0000	    1.4979	 60	 80
i 3	   81.5000	    1.4979	 60	 80
i 3	   83.0000	    0.9979	 60	 80
i 3	   84.0000	    1.4979	 62	 80
i 3	   85.5000	    1.4979	 62	 80
i 3	   87.0000	    0.9979	 58	 80
i 3	   88.0000	    1.4979	 55	 80
i 3	   89.5000	    1.4979	 55	 80
i 3	   91.0000	    0.9979	 55	 80
i 3	   92.0000	    1.4979	 60	 80
i 3	   93.5000	    1.4979	 60	 80
i 3	   95.0000	    0.9979	 60	 80
i 3	   96.0000	    1.4979	 66	 80
i 3	   97.5000	    1.4979	 66	 80
i 3	   99.0000	    0.9979	 66	 80
i 3	  100.0000	    1.4979	 65	 80
i 3	  101.5000	    1.4979	 65	 80
i 3	  103.0000	    0.9979	 65	 80
i 3	  104.0000	    1.4979	 63	 80
i 3	  105.5000	    1.4979	 63	 80
i 3	  107.0000	    0.9979	 63	 80
i 3	  108.0000	    1.4979	 63	 80
i 3	  109.5000	    1.4979	 63	 80
i 3	  111.0000	    0.9979	 63	 80
i 3	  112.0000	    2.9979	 68	 96
i 3	  115.0000	    0.4979	 65	 96
i 3	  115.5000	    0.4979	 67	 96
i 3	  116.0000	    0.6646	 68	 96
i 3	  116.6667	    0.6646	 67	 96
i 3	  117.3333	    0.6646	 70	 96
i 3	  118.0000	    0.9979	 62	 96
i 3	  119.0000	    0.9979	 60	 96
i 3	  120.0000	    0.1063	 58	 96
i 3	  120.1083	    0.1063	 59	 96
i 3	  120.2188	    0.1063	 60	 96
i 3	  120.3292	    0.1063	 61	 96
i 3	  120.4396	    0.1063	 62	 96
i 3	  120.5500	    0.1063	 63	 96
i 3	  120.6604	    0.1063	 64	 96
i 3	  120.7708	    0.1063	 65	 96
i 3	  120.8813	    0.1125	 66	 96
i 3	  121.0000	    2.9979	 67	 96
i 3	  124.0000	    0.6646	 67	 96
i 3	  124.6667	    0.6646	 65	 96
i 3	  125.3333	    0.6646	 68	 96
i 3	  126.0000	    0.9979	 60	 96
i 3	  127.0000	    0.9979	 58	 96
i 3	  128.0000	    0.1062	 56	 96
i 3	  128.1083	    0.1062	 57	 96
i 3	  128.2188	    0.1062	 58	 96
i 3	  128.3292	    0.1062	 59	 96
i 3	  128.4396	    0.1062	 60	 96
i 3	  128.5500	    0.1062	 61	 96
i 3	  128.6604	    0.1062	 62	 96
i 3	  128.7708	    0.1062	 63	 96
i 3	  128.8812	    0.1125	 64	 96
i 3	  129.0000	    2.9979	 65	 96
i 3	  132.0000	    0.6646	 65	 96
i 3	  132.6667	    0.6646	 65	 96
i 3	  133.3333	    0.6646	 67	 96
i 3	  134.0000	    0.9979	 60	 96
i 3	  135.0000	    0.9979	 59	 96
i 3	  136.0000	    1.4979	 55	 96
i 3	  137.5000	    1.4979	 56	 96
i 3	  139.0000	    0.9979	 55	 96
i 3	  140.0000	    3.9979	 58	 96
i 3	  144.0000	    0.4979	 56	 33
i 3	  144.5000	    0.9979	 63	 33
i 3	  145.5000	    0.4979	 56	 33
i 3	  146.0000	    1.9979	 63	 33
i 3	  148.0000	    0.4979	 56	 33
i 3	  148.5000	    0.9979	 65	 33
i 3	  149.5000	    0.4979	 56	 33
i 3	  150.0000	    1.9979	 62	 33
i 3	  152.0000	    0.4979	 51	 33
i 3	  152.5000	    0.9979	 58	 33
i 3	  153.5000	    0.4979	 55	 33
i 3	  154.0000	    1.9979	 62	 33
i 3	  156.0000	    0.4979	 56	 33
i 3	  156.5000	    0.9979	 63	 33
i 3	  157.5000	    0.4979	 60	 33
i 3	  158.0000	    0.9979	 65	 33
i 3	  159.0000	    0.9979	 63	 33
i 3	  160.0000	    3.9979	 56	 33
i 3	  164.0000	    3.9979	 59	 33
i 3	  168.0000	    1.4979	 63	 33
i 3	  169.5000	    1.4979	 63	 33
i 3	  171.0000	    0.9979	 63	 33
i 3	  172.0000	    1.4979	 63	 33
i 3	  173.5000	    1.4979	 63	 33
i 3	  175.0000	    0.9979	 63	 33
i 3	  176.0000	    1.4979	 48	 33
i 3	  177.5000	    0.4979	 60	 33
i 3	  178.0000	    0.9979	 60	 33
i 3	  179.0000	    0.9979	 48	 33
i 3	  180.0000	    1.4979	 48	 33
i 3	  181.5000	    0.4979	 60	 33
i 3	  182.0000	    0.9979	 60	 33
i 3	  183.0000	    0.9979	 48	 33
i 3	  184.0000	    1.4979	 48	 33
i 3	  185.5000	    0.4979	 60	 33
i 3	  186.0000	    0.9979	 60	 33
i 3	  187.0000	    0.9979	 48	 33
i 3	  188.0000	    1.4979	 48	 33
i 3	  189.5000	    0.4979	 60	 33
i 3	  190.0000	    0.9979	 60	 33
i 3	  191.0000	    0.9979	 48	 33
i 3	  192.0000	    1.4979	 53	 33
i 3	  193.5000	    0.4979	 53	 33
i 3	  194.0000	    0.9979	 58	 33
i 3	  195.0000	    0.9979	 58	 33
i 3	  196.0000	    1.4979	 51	 33
i 3	  197.5000	    0.4979	 51	 33
i 3	  198.0000	    0.9979	 56	 33
i 3	  199.0000	    0.9979	 56	 33
i 3	  200.0000	    1.4979	 50	 33
i 3	  201.5000	    0.4979	 62	 33
i 3	  202.0000	    0.9979	 62	 33
i 3	  203.0000	    0.9979	 50	 33
i 3	  204.0000	    1.4979	 55	 33
i 3	  205.5000	    0.4979	 56	 33
i 3	  206.0000	    0.4979	 55	 33
i 3	  206.5000	    0.4979	 53	 33
i 3	  207.0000	    0.4979	 51	 33
i 3	  207.5000	    0.4979	 50	 33
i 3	  208.0000	    0.4979	 48	 33
i 3	  208.5000	    0.4979	 55	 33
i 3	  209.0000	    2.9979	 56	 33
i 3	  212.5000	    0.4979	 55	 33
i 3	  213.0000	    2.9979	 56	 33
i 3	  216.5000	    0.4979	 55	 33
i 3	  217.0000	    2.9979	 56	 33
i 3	  220.5000	    0.4979	 55	 33
i 3	  221.0000	    2.9979	 56	 33
i 3	  224.0000	    1.4979	 61	 33
i 3	  225.5000	    1.4979	 61	 33
i 3	  227.0000	    0.9979	 61	 33
i 3	  228.0000	    1.4979	 61	 33
i 3	  229.5000	    1.4979	 61	 33
i 3	  231.0000	    0.9979	 61	 33
i 3	  232.0000	    1.4979	 60	 33
i 3	  233.5000	    1.4979	 60	 33
i 3	  235.0000	    0.9979	 60	 33
i 3	  236.0000	    1.4979	 60	 33
i 3	  237.5000	    1.4979	 60	 33
i 3	  239.0000	    0.9979	 60	 33
i 3	  240.0000	    1.4979	 60	 80
i 3	  241.5000	    1.4979	 60	 80
i 3	  243.0000	    0.9979	 60	 80
i 3	  244.0000	    1.4979	 62	 80
i 3	  245.5000	    1.4979	 62	 80
i 3	  247.0000	    0.9979	 58	 80
i 3	  248.0000	    1.4979	 55	 80
i 3	  249.5000	    1.4979	 55	 80
i 3	  251.0000	    0.9979	 55	 80
i 3	  252.0000	    1.4979	 60	 80
i 3	  253.5000	    1.4979	 60	 80
i 3	  255.0000	    0.9979	 60	 76
i 3	  256.0000	    1.4979	 66	 72
i 3	  257.5000	    1.4979	 66	 67
i 3	  259.0000	    0.9979	 66	 63
i 3	  260.0000	    1.4979	 65	 59
i 3	  261.5000	    1.4979	 65	 54
i 3	  263.0000	    0.9979	 65	 49
i 3	  264.5000	    0.4979	 55	 49
i 3	  265.0000	    2.9979	 56	 49
i 3	  268.5000	    0.4979	 55	 49
i 3	  269.0000	    2.9979	 56	 49
i 3	  272.5000	    0.4979	 55	 49
i 3	  273.0000	    2.9979	 56	 49
i 3	  276.5000	    0.4979	 55	 49
i 3	  277.0000	    2.9979	 56	 49
i 3	  280.0000	    3.9979	 55	 33

; track 4

i 4	    0.0000	    1.4979	 36	 49
i 4	    1.5000	    0.4979	 48	 49
i 4	    2.0000	    0.9979	 48	 49
i 4	    3.0000	    0.9979	 36	 49
i 4	    4.0000	    1.4979	 36	 49
i 4	    5.5000	    0.4979	 48	 49
i 4	    6.0000	    0.9979	 48	 49
i 4	    7.0000	    0.9979	 36	 49
i 4	    8.0000	    1.4979	 36	 49
i 4	    9.5000	    0.4979	 48	 49
i 4	   10.0000	    0.9979	 48	 49
i 4	   11.0000	    0.9979	 36	 49
i 4	   12.0000	    1.4979	 36	 49
i 4	   13.5000	    0.4979	 48	 49
i 4	   14.0000	    0.9979	 48	 49
i 4	   15.0000	    0.9979	 36	 49
i 4	   16.0000	    1.4979	 36	 49
i 4	   17.5000	    0.4979	 48	 49
i 4	   18.0000	    0.9979	 48	 49
i 4	   19.0000	    0.9979	 36	 49
i 4	   20.0000	    1.4979	 36	 49
i 4	   21.5000	    0.4979	 48	 49
i 4	   22.0000	    0.9979	 48	 49
i 4	   23.0000	    0.9979	 36	 49
i 4	   24.0000	    1.4979	 36	 49
i 4	   25.5000	    0.4979	 48	 49
i 4	   26.0000	    0.9979	 48	 49
i 4	   27.0000	    0.9979	 36	 49
i 4	   28.0000	    1.4979	 36	 49
i 4	   29.5000	    0.4979	 48	 49
i 4	   30.0000	    0.9979	 48	 49
i 4	   31.0000	    0.9979	 36	 49
i 4	   32.0000	    1.4979	 41	 49
i 4	   33.5000	    0.4979	 41	 49
i 4	   34.0000	    0.9979	 46	 49
i 4	   35.0000	    0.9979	 46	 49
i 4	   36.0000	    1.4979	 39	 49
i 4	   37.5000	    0.4979	 39	 49
i 4	   38.0000	    0.9979	 44	 49
i 4	   39.0000	    0.9979	 44	 49
i 4	   40.0000	    1.4979	 38	 49
i 4	   41.5000	    0.4979	 50	 46
i 4	   42.0000	    0.9979	 50	 45
i 4	   43.0000	    0.9979	 38	 43
i 4	   44.0000	    1.4979	 43	 41
i 4	   45.5000	    0.4979	 44	 38
i 4	   46.0000	    0.4979	 43	 37
i 4	   46.5000	    0.4979	 41	 36
i 4	   47.0000	    0.4979	 39	 35
i 4	   47.5000	    0.4979	 38	 33
i 4	   48.0000	    1.4979	 36	 33
i 4	   49.5000	    0.4979	 48	 33
i 4	   50.0000	    0.9979	 48	 33
i 4	   51.0000	    0.9979	 36	 33
i 4	   52.0000	    1.4979	 36	 33
i 4	   53.5000	    0.4979	 48	 33
i 4	   54.0000	    0.9979	 48	 33
i 4	   55.0000	    0.9979	 36	 33
i 4	   56.0000	    1.4979	 36	 33
i 4	   57.5000	    0.4979	 48	 33
i 4	   58.0000	    0.9979	 48	 33
i 4	   59.0000	    0.9979	 36	 33
i 4	   60.0000	    1.4979	 36	 33
i 4	   61.5000	    0.4979	 48	 33
i 4	   62.0000	    0.9979	 48	 33
i 4	   63.0000	    0.9979	 36	 33
i 4	   64.0000	    1.4979	 43	 33
i 4	   65.5000	    0.4979	 55	 33
i 4	   66.0000	    0.9979	 55	 33
i 4	   67.0000	    0.9979	 43	 33
i 4	   68.0000	    1.4979	 36	 33
i 4	   69.5000	    0.4979	 48	 33
i 4	   70.0000	    0.9979	 48	 33
i 4	   71.0000	    0.9979	 36	 33
i 4	   72.0000	    1.4979	 41	 33
i 4	   73.5000	    0.4979	 53	 33
i 4	   74.0000	    0.9979	 53	 33
i 4	   75.0000	    0.9979	 41	 33
i 4	   76.0000	    1.4979	 39	 33
i 4	   77.5000	    0.4979	 51	 33
i 4	   78.0000	    0.9979	 51	 33
i 4	   79.0000	    0.9979	 39	 33
i 4	   80.0000	    1.4979	 38	 80
i 4	   81.5000	    0.4979	 50	 80
i 4	   82.0000	    0.9979	 50	 80
i 4	   83.0000	    0.9979	 38	 80
i 4	   84.0000	    1.4979	 43	 80
i 4	   85.5000	    0.4979	 55	 80
i 4	   86.0000	    0.9979	 55	 80
i 4	   87.0000	    0.9979	 50	 80
i 4	   88.0000	    1.4979	 48	 80
i 4	   89.5000	    0.4979	 60	 80
i 4	   90.0000	    0.9979	 60	 80
i 4	   91.0000	    0.9979	 48	 80
i 4	   92.0000	    1.4979	 46	 80
i 4	   93.5000	    0.4979	 58	 80
i 4	   94.0000	    0.9979	 58	 80
i 4	   95.0000	    0.9979	 46	 80
i 4	   96.0000	    1.4979	 45	 80
i 4	   97.5000	    0.4979	 57	 80
i 4	   98.0000	    0.9979	 57	 80
i 4	   99.0000	    0.9979	 45	 80
i 4	  100.0000	    1.4979	 43	 80
i 4	  101.5000	    0.4979	 55	 80
i 4	  102.0000	    0.9979	 55	 80
i 4	  103.0000	    0.9979	 43	 80
i 4	  104.0000	    1.4979	 36	 80
i 4	  105.5000	    0.4979	 48	 80
i 4	  106.0000	    0.9979	 48	 80
i 4	  107.0000	    0.9979	 36	 80
i 4	  108.0000	    1.4979	 36	 80
i 4	  109.5000	    0.4979	 48	 80
i 4	  110.0000	    0.9979	 48	 80
i 4	  111.0000	    0.9979	 36	 80
i 4	  112.0000	    1.4979	 41	 96
i 4	  113.5000	    0.4979	 51	 96
i 4	  114.0000	    0.9979	 51	 96
i 4	  115.0000	    0.9979	 41	 96
i 4	  116.0000	    1.4979	 46	 96
i 4	  117.5000	    0.4979	 58	 96
i 4	  118.0000	    0.9979	 58	 96
i 4	  119.0000	    0.9979	 46	 96
i 4	  120.0000	    1.4979	 39	 96
i 4	  121.5000	    0.4979	 51	 96
i 4	  122.0000	    0.9979	 51	 96
i 4	  123.0000	    0.9979	 46	 96
i 4	  124.0000	    1.4979	 44	 96
i 4	  125.5000	    0.4979	 56	 96
i 4	  126.0000	    0.9979	 56	 96
i 4	  127.0000	    0.9979	 44	 96
i 4	  128.0000	    1.4979	 38	 96
i 4	  129.5000	    0.4979	 50	 96
i 4	  130.0000	    0.9979	 50	 96
i 4	  131.0000	    0.9979	 38	 96
i 4	  132.0000	    1.4979	 43	 96
i 4	  133.5000	    0.4979	 55	 96
i 4	  134.0000	    0.9979	 55	 96
i 4	  135.0000	    0.9979	 43	 96
i 4	  136.0000	    1.4979	 36	 96
i 4	  137.5000	    0.4979	 36	 96
i 4	  138.0000	    1.4979	 38	 96
i 4	  139.5000	    0.4979	 38	 96
i 4	  140.0000	    1.4979	 39	 96
i 4	  141.5000	    0.4979	 39	 96
i 4	  142.0000	    1.4979	 40	 96
i 4	  143.5000	    0.4979	 40	 96
i 4	  144.0000	    1.4979	 41	 33
i 4	  145.5000	    0.4979	 51	 33
i 4	  146.0000	    0.9979	 51	 33
i 4	  147.0000	    0.9979	 41	 33
i 4	  148.0000	    1.4979	 46	 33
i 4	  149.5000	    0.4979	 58	 33
i 4	  150.0000	    0.9979	 58	 33
i 4	  151.0000	    0.9979	 46	 33
i 4	  152.0000	    1.4979	 39	 33
i 4	  153.5000	    0.4979	 51	 33
i 4	  154.0000	    0.9979	 51	 33
i 4	  155.0000	    0.9979	 46	 33
i 4	  156.0000	    1.4979	 44	 33
i 4	  157.5000	    0.4979	 56	 33
i 4	  158.0000	    0.9979	 56	 33
i 4	  159.0000	    0.9979	 44	 33
i 4	  160.0000	    1.4979	 38	 33
i 4	  161.5000	    0.4979	 50	 33
i 4	  162.0000	    0.9979	 50	 33
i 4	  163.0000	    0.9979	 38	 33
i 4	  164.0000	    1.4979	 43	 33
i 4	  165.5000	    0.4979	 55	 33
i 4	  166.0000	    0.9979	 55	 33
i 4	  167.0000	    0.9979	 43	 33
i 4	  168.0000	    1.4979	 36	 33
i 4	  169.5000	    0.4979	 48	 33
i 4	  170.0000	    0.9979	 48	 33
i 4	  171.0000	    0.9979	 36	 33
i 4	  172.0000	    1.9979	 36	 33
i 4	  175.0000	    0.9979	 55	 49
i 4	  176.0000	    4.4979	 67	 49
i 4	  180.5000	    0.4979	 65	 49
i 4	  181.0000	    0.3313	 67	 49
i 4	  181.3333	    0.3313	 65	 49
i 4	  181.6667	    0.3313	 63	 49
i 4	  182.0000	    1.4979	 62	 49
i 4	  183.5000	    0.4979	 60	 49
i 4	  184.0000	    4.4979	 63	 49
i 4	  188.5000	    0.4979	 62	 49
i 4	  189.0000	    0.4979	 65	 49
i 4	  189.5000	    0.4979	 63	 49
i 4	  190.0000	    0.4979	 62	 49
i 4	  190.5000	    0.9979	 60	 49
i 4	  191.5000	    0.2479	 58	 49
i 4	  191.7500	    0.2479	 56	 49
i 4	  192.0000	    2.9979	 60	 49
i 4	  195.0000	    0.2479	 58	 49
i 4	  195.2500	    0.2479	 60	 49
i 4	  195.5000	    0.2479	 58	 49
i 4	  195.7500	    0.2479	 56	 49
i 4	  196.0000	    1.9979	 58	 49
i 4	  198.0000	    0.4979	 60	 49
i 4	  198.5000	    0.9979	 58	 49
i 4	  199.5000	    0.2479	 56	 49
i 4	  199.7500	    0.2479	 55	 49
i 4	  200.0000	    1.4979	 56	 49
i 4	  201.5000	    0.2479	 55	 49
i 4	  201.7500	    0.2479	 53	 49
i 4	  202.0000	    0.9979	 51	 49
i 4	  203.0000	    0.9979	 50	 49
i 4	  204.0000	    3.9979	 55	 49
i 4	  208.0000	    1.4979	 36	 49
i 4	  209.5000	    0.4979	 48	 49
i 4	  210.0000	    0.9979	 48	 49
i 4	  211.0000	    0.9979	 36	 49
i 4	  212.0000	    1.4979	 36	 49
i 4	  213.5000	    0.4979	 48	 49
i 4	  214.0000	    0.9979	 48	 49
i 4	  215.0000	    0.9979	 36	 49
i 4	  216.0000	    1.4979	 36	 49
i 4	  217.5000	    0.4979	 48	 49
i 4	  218.0000	    0.9979	 48	 49
i 4	  219.0000	    0.9979	 36	 49
i 4	  220.0000	    1.4979	 36	 49
i 4	  221.5000	    0.4979	 48	 49
i 4	  222.0000	    0.9979	 48	 49
i 4	  223.0000	    0.9979	 36	 49
i 4	  224.0000	    1.4979	 43	 49
i 4	  225.5000	    0.4979	 55	 49
i 4	  226.0000	    0.9979	 55	 49
i 4	  227.0000	    0.9979	 43	 49
i 4	  228.0000	    1.4979	 36	 49
i 4	  229.5000	    0.4979	 48	 49
i 4	  230.0000	    0.9979	 48	 49
i 4	  231.0000	    0.9979	 36	 49
i 4	  232.0000	    1.4979	 41	 49
i 4	  233.5000	    0.4979	 53	 49
i 4	  234.0000	    0.9979	 53	 49
i 4	  235.0000	    0.9979	 41	 49
i 4	  236.0000	    1.4979	 39	 49
i 4	  237.5000	    0.4979	 51	 49
i 4	  238.0000	    0.9979	 51	 49
i 4	  239.0000	    0.9979	 39	 49
i 4	  240.0000	    1.4979	 38	 80
i 4	  241.5000	    0.4979	 50	 80
i 4	  242.0000	    0.9979	 50	 80
i 4	  243.0000	    0.9979	 38	 80
i 4	  244.0000	    1.4979	 43	 80
i 4	  245.5000	    0.4979	 55	 80
i 4	  246.0000	    0.9979	 55	 80
i 4	  247.0000	    0.9979	 50	 80
i 4	  248.0000	    1.4979	 48	 80
i 4	  249.5000	    0.4979	 60	 80
i 4	  250.0000	    0.9979	 60	 80
i 4	  251.0000	    0.9979	 48	 80
i 4	  252.0000	    1.4979	 46	 80
i 4	  253.5000	    0.4979	 58	 76
i 4	  254.0000	    0.9979	 58	 75
i 4	  255.0000	    0.9979	 46	 72
i 4	  256.0000	    1.4979	 45	 69
i 4	  257.5000	    0.4979	 57	 65
i 4	  258.0000	    0.9979	 57	 64
i 4	  259.0000	    0.9979	 45	 61
i 4	  260.0000	    1.4979	 43	 58
i 4	  261.5000	    0.4979	 55	 54
i 4	  262.0000	    0.9979	 55	 52
i 4	  263.0000	    0.9979	 43	 49
i 4	  264.0000	    2.9979	 36	 49
i 4	  267.0000	    0.9979	 48	 49
i 4	  268.0000	    2.9979	 46	 49
i 4	  271.0000	    0.9979	 46	 49
i 4	  272.0000	    2.9979	 44	 49
i 4	  275.0000	    0.9979	 44	 49
i 4	  276.0000	    2.9979	 43	 49
i 4	  279.0000	    0.9979	 43	 49
i 4	  280.0000	    3.9979	 36	 33
e 5
</CsScore>
</CsoundSynthesizer>

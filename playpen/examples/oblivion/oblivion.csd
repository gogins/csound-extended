
<CsoundSynthesizer>
<CsLicense>
"Oblivion," by Astor Piazzola
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
kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, 0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, 0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, 0.172, idec, 0.053, idec, 0
kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, -0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, 0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, -0.035, idec, -0.020, idec, 0
kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, 0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, 0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, 0.010, idec, 0.013, idec, 0
iwt1 = gif26 ; wavetable numbers
iwt2 = gif27
iwt3 = gif28
inorm = 3949
goto end
range2: ; for low mid-range tones
kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, 0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, 1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, 0.037, idec, 0.012, idec, 0
kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, 1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, -1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, -0.170, idec, -0.065, idec, 0
kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, 0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, 12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, 0.174, idec, -0.027, idec, 0
iwt1 = gif29
iwt2 = gif30
iwt3 = gif31
inorm = 27668.2
goto end
range3: ; for high mid-range tones
kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, 0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, 0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, idec, 0.031, idec, 0
kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, -0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, 0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, -0.003, idec, -0.023, idec, 0
kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, 0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, 1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, 0.167, idec, -0.059, idec, 0
iwt1 = gif32
iwt2 = gif33
iwt3 = gif34
inorm = 3775
goto end
range4: ; for high range tones
kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, 0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, 1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, idec, 0.028, idec, 0
kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, 0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, -0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, -0.071, idec, -0.019, idec, 0
kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, 0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, 0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, -0.024, idec, 0.002, idec, 0
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
prints "ReverbSC       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\
", p1, p2, p3, p4, p5, p1/6, active(p1)
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
i 1 0 1 24 79 0 0 0 0 4095 1 
i 14 0 0.3333333 36 63 0 0 0 0 4095 1 
i 11 0.3333333 0.3333333 43 63 0 0 0 0 4095 1 
i 14 0.3333333 0.3333333 43 63 0 0 0 0 4095 1 
i 11 0.6666667 2 44 63 0 0 0 0 4095 1 
i 14 0.6666667 2 43 63 0 0 0 0 4095 1 
i 1 1 0.3333333 36 79 0 0 0 0 4095 1 
i 7 1 0.3333333 48 63 0 0 0 0 4095 1 
i 1 1.333333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 1.333333 0.3333333 50 63 0 0 0 0 4095 1 
i 7 1.333333 1.333333 50 63 0 0 0 0 4095 1 
i 4 1.666667 0.3333333 51 63 0 0 0 0 4095 1 
i 1 2 0.6666667 24 79 0 0 0 0 4095 1 
i 4 2 0.6666667 53 63 0 0 0 0 4095 1 
i 1 2.666667 1 24 79 0 0 0 0 4095 1 
i 14 2.666667 0.3333333 36 63 0 0 0 0 4095 1 
i 11 3 0.3333333 43 63 0 0 0 0 4095 1 
i 14 3 0.3333333 43 63 0 0 0 0 4095 1 
i 11 3.333333 2 44 63 0 0 0 0 4095 1 
i 14 3.333333 2 43 63 0 0 0 0 4095 1 
i 1 3.666667 0.3333333 36 79 0 0 0 0 4095 1 
i 7 3.666667 0.3333333 48 63 0 0 0 0 4095 1 
i 1 4 0.6666667 36 79 0 0 0 0 4095 1 
i 4 4 0.3333333 50 63 0 0 0 0 4095 1 
i 7 4 1.333333 50 63 0 0 0 0 4095 1 
i 4 4.333333 0.3333333 51 63 0 0 0 0 4095 1 
i 1 4.666667 0.6666667 24 79 0 0 0 0 4095 1 
i 4 4.666667 0.6666667 53 63 0 0 0 0 4095 1 
i 1 5.333333 1 24 79 0 0 0 0 4095 1 
i 1 5.333333 3.666667 67 95 0 0 0 0 4095 1 
i 14 5.333333 0.3333333 36 63 0 0 0 0 4095 1 
i 11 5.666667 0.3333333 43 63 0 0 0 0 4095 1 
i 14 5.666667 0.3333333 43 63 0 0 0 0 4095 1 
i 11 6 2 44 63 0 0 0 0 4095 1 
i 14 6 2 43 63 0 0 0 0 4095 1 
i 1 6.333333 0.3333333 36 79 0 0 0 0 4095 1 
i 7 6.333333 0.3333333 48 63 0 0 0 0 4095 1 
i 1 6.666667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 6.666667 0.3333333 50 63 0 0 0 0 4095 1 
i 7 6.666667 1.333333 50 63 0 0 0 0 4095 1 
i 4 7 0.3333333 51 63 0 0 0 0 4095 1 
i 1 7.333333 0.6666667 24 79 0 0 0 0 4095 1 
i 4 7.333333 0.6666667 53 63 0 0 0 0 4095 1 
i 1 8 1 24 79 0 0 0 0 4095 1 
i 14 8 0.3333333 36 63 0 0 0 0 4095 1 
i 11 8.333333 0.3333333 43 63 0 0 0 0 4095 1 
i 14 8.333333 0.3333333 43 63 0 0 0 0 4095 1 
i 11 8.666667 2 44 63 0 0 0 0 4095 1 
i 14 8.666667 2 43 63 0 0 0 0 4095 1 
i 1 9 0.3333333 36 79 0 0 0 0 4095 1 
i 1 9 0.1666667 65 95 0 0 0 0 4095 1 
i 7 9 0.3333333 48 63 0 0 0 0 4095 1 
i 1 9.166667 0.1666667 63 95 0 0 0 0 4095 1 
i 1 9.333333 0.6666667 36 79 0 0 0 0 4095 1 
i 1 9.333333 1 62 95 0 0 0 0 4095 1 
i 4 9.333333 0.3333333 50 63 0 0 0 0 4095 1 
i 7 9.333333 1.333333 50 63 0 0 0 0 4095 1 
i 4 9.666667 0.3333333 51 63 0 0 0 0 4095 1 
i 1 10 0.6666667 24 79 0 0 0 0 4095 1 
i 4 10 0.6666667 53 63 0 0 0 0 4095 1 
i 1 10.33333 0.3333333 60 95 0 0 0 0 4095 1 
i 1 10.66667 1 24 79 0 0 0 0 4095 1 
i 1 10.66667 3.666667 63 95 0 0 0 0 4095 1 
i 14 10.66667 0.3333333 36 63 0 0 0 0 4095 1 
i 11 11 0.3333333 43 63 0 0 0 0 4095 1 
i 14 11 0.3333333 43 63 0 0 0 0 4095 1 
i 11 11.33333 2 44 63 0 0 0 0 4095 1 
i 14 11.33333 2 43 63 0 0 0 0 4095 1 
i 1 11.66667 0.3333333 36 79 0 0 0 0 4095 1 
i 7 11.66667 0.3333333 48 63 0 0 0 0 4095 1 
i 1 12 0.6666667 36 79 0 0 0 0 4095 1 
i 4 12 0.3333333 50 63 0 0 0 0 4095 1 
i 7 12 1.333333 50 63 0 0 0 0 4095 1 
i 4 12.33333 0.3333333 51 63 0 0 0 0 4095 1 
i 1 12.66667 0.6666667 24 79 0 0 0 0 4095 1 
i 4 12.66667 0.6666667 53 63 0 0 0 0 4095 1 
i 1 13.33333 1 24 79 0 0 0 0 4095 1 
i 14 13.33333 0.3333333 36 63 0 0 0 0 4095 1 
i 11 13.66667 0.3333333 43 63 0 0 0 0 4095 1 
i 14 13.66667 0.3333333 43 63 0 0 0 0 4095 1 
i 11 14 2 44 63 0 0 0 0 4095 1 
i 14 14 2 43 63 0 0 0 0 4095 1 
i 1 14.33333 0.3333333 36 79 0 0 0 0 4095 1 
i 1 14.33333 0.1666667 62 95 0 0 0 0 4095 1 
i 7 14.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 1 14.5 0.1666667 60 95 0 0 0 0 4095 1 
i 1 14.66667 0.6666667 36 79 0 0 0 0 4095 1 
i 1 14.66667 1 58 95 0 0 0 0 4095 1 
i 4 14.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 7 14.66667 1.333333 50 63 0 0 0 0 4095 1 
i 4 15 0.3333333 51 63 0 0 0 0 4095 1 
i 1 15.33333 0.6666667 24 79 0 0 0 0 4095 1 
i 4 15.33333 0.6666667 53 63 0 0 0 0 4095 1 
i 1 15.66667 0.3333333 56 95 0 0 0 0 4095 1 
i 1 16 1 29 79 0 0 0 0 4095 1 
i 1 16 2.333333 60 95 0 0 0 0 4095 1 
i 4 16 2.666667 56 63 0 0 0 0 4095 1 
i 11 16 2.666667 44 63 0 0 0 0 4095 1 
i 11 16 2.666667 48 63 0 0 0 0 4095 1 
i 7 16.33333 0.3333333 51 63 0 0 0 0 4095 1 
i 14 16.33333 0.3333333 51 63 0 0 0 0 4095 1 
i 7 16.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 14 16.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 1 17 0.3333333 29 79 0 0 0 0 4095 1 
i 7 17 0.3333333 51 63 0 0 0 0 4095 1 
i 14 17 0.3333333 51 63 0 0 0 0 4095 1 
i 1 17.33333 0.6666667 34 79 0 0 0 0 4095 1 
i 7 17.66667 0.3333333 51 63 0 0 0 0 4095 1 
i 14 17.66667 0.3333333 51 63 0 0 0 0 4095 1 
i 1 18 0.6666667 34 79 0 0 0 0 4095 1 
i 7 18 0.6666667 50 63 0 0 0 0 4095 1 
i 14 18 0.6666667 50 63 0 0 0 0 4095 1 
i 1 18.33333 0.1666667 58 95 0 0 0 0 4095 1 
i 1 18.5 0.1666667 56 95 0 0 0 0 4095 1 
i 1 18.66667 1 27 79 0 0 0 0 4095 1 
i 1 18.66667 2.333333 58 95 0 0 0 0 4095 1 
i 4 18.66667 2.666667 55 63 0 0 0 0 4095 1 
i 11 18.66667 2.666667 46 63 0 0 0 0 4095 1 
i 11 18.66667 2.666667 50 63 0 0 0 0 4095 1 
i 7 19 0.3333333 50 63 0 0 0 0 4095 1 
i 14 19 0.3333333 50 63 0 0 0 0 4095 1 
i 7 19.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 14 19.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 1 19.66667 0.3333333 27 79 0 0 0 0 4095 1 
i 7 19.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 14 19.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 1 20 0.6666667 32 79 0 0 0 0 4095 1 
i 7 20.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 14 20.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 1 20.66667 0.6666667 32 79 0 0 0 0 4095 1 
i 7 20.66667 0.6666667 48 63 0 0 0 0 4095 1 
i 14 20.66667 0.6666667 48 63 0 0 0 0 4095 1 
i 1 21 0.1666667 56 95 0 0 0 0 4095 1 
i 1 21.16667 0.1666667 55 95 0 0 0 0 4095 1 
i 1 21.33333 0.6666667 26 79 0 0 0 0 4095 1 
i 1 21.33333 1 56 95 0 0 0 0 4095 1 
i 4 21.33333 1.333333 53 63 0 0 0 0 4095 1 
i 11 21.33333 2.666667 44 63 0 0 0 0 4095 1 
i 11 21.33333 2.666667 48 63 0 0 0 0 4095 1 
i 7 21.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 14 21.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 1 22 0.3333333 26 79 0 0 0 0 4095 1 
i 7 22 0.3333333 48 63 0 0 0 0 4095 1 
i 14 22 0.3333333 48 63 0 0 0 0 4095 1 
i 1 22.33333 1 38 79 0 0 0 0 4095 1 
i 1 22.33333 0.1666667 55 95 0 0 0 0 4095 1 
i 7 22.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 14 22.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 1 22.5 0.1666667 53 95 0 0 0 0 4095 1 
i 1 22.66667 0.6666667 51 95 0 0 0 0 4095 1 
i 4 22.66667 1.333333 54 63 0 0 0 0 4095 1 
i 7 23 0.3333333 50 63 0 0 0 0 4095 1 
i 14 23 0.3333333 50 63 0 0 0 0 4095 1 
i 1 23.33333 0.6666667 26 79 0 0 0 0 4095 1 
i 1 23.33333 0.6666667 50 95 0 0 0 0 4095 1 
i 7 23.33333 0.6666667 48 63 0 0 0 0 4095 1 
i 14 23.33333 0.6666667 48 63 0 0 0 0 4095 1 
i 1 24 1 31 79 0 0 0 0 4095 1 
i 1 24 2.666667 55 95 0 0 0 0 4095 1 
i 4 24 2.666667 53 63 0 0 0 0 4095 1 
i 11 24 1.333333 44 63 0 0 0 0 4095 1 
i 7 24.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 14 24.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 7 24.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 14 24.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 1 25 0.3333333 31 79 0 0 0 0 4095 1 
i 7 25 0.3333333 47 63 0 0 0 0 4095 1 
i 14 25 0.3333333 47 63 0 0 0 0 4095 1 
i 1 25.33333 0.3333333 31 79 0 0 0 0 4095 1 
i 7 25.33333 1 51 63 0 0 0 0 4095 1 
i 11 25.33333 1.333333 43 63 0 0 0 0 4095 1 
i 14 25.33333 1 51 63 0 0 0 0 4095 1 
i 1 25.66667 0.3333333 29 79 0 0 0 0 4095 1 
i 1 26 0.3333333 27 79 0 0 0 0 4095 1 
i 1 26.33333 0.3333333 26 79 0 0 0 0 4095 1 
i 7 26.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 14 26.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 1 26.66667 1 24 79 0 0 0 0 4095 1 
i 1 26.66667 3.666667 67 95 0 0 0 0 4095 1 
i 14 26.66667 0.3333333 36 63 0 0 0 0 4095 1 
i 11 27 0.3333333 43 63 0 0 0 0 4095 1 
i 14 27 0.3333333 43 63 0 0 0 0 4095 1 
i 11 27.33333 2 44 63 0 0 0 0 4095 1 
i 14 27.33333 2 43 63 0 0 0 0 4095 1 
i 1 27.66667 0.3333333 36 79 0 0 0 0 4095 1 
i 7 27.66667 0.3333333 48 63 0 0 0 0 4095 1 
i 1 28 0.6666667 36 79 0 0 0 0 4095 1 
i 4 28 0.3333333 50 63 0 0 0 0 4095 1 
i 7 28 1.333333 50 63 0 0 0 0 4095 1 
i 4 28.33333 0.3333333 51 63 0 0 0 0 4095 1 
i 1 28.66667 0.6666667 24 79 0 0 0 0 4095 1 
i 4 28.66667 0.6666667 53 63 0 0 0 0 4095 1 
i 1 29.33333 1 24 79 0 0 0 0 4095 1 
i 14 29.33333 0.3333333 36 63 0 0 0 0 4095 1 
i 11 29.66667 0.3333333 43 63 0 0 0 0 4095 1 
i 14 29.66667 0.3333333 43 63 0 0 0 0 4095 1 
i 11 30 2 44 63 0 0 0 0 4095 1 
i 14 30 2 43 63 0 0 0 0 4095 1 
i 1 30.33333 0.3333333 36 79 0 0 0 0 4095 1 
i 1 30.33333 0.1666667 65 95 0 0 0 0 4095 1 
i 7 30.33333 0.3333333 48 63 0 0 0 0 4095 1 
i 1 30.5 0.1666667 63 95 0 0 0 0 4095 1 
i 1 30.66667 0.6666667 36 79 0 0 0 0 4095 1 
i 1 30.66667 1 62 95 0 0 0 0 4095 1 
i 4 30.66667 0.3333333 50 63 0 0 0 0 4095 1 
i 7 30.66667 1.333333 50 63 0 0 0 0 4095 1 
i 4 31 0.3333333 51 63 0 0 0 0 4095 1 
i 1 31.33333 0.6666667 24 79 0 0 0 0 4095 1 
i 4 31.33333 0.6666667 53 63 0 0 0 0 4095 1 
i 1 31.66667 0.3333333 60 95 0 0 0 0 4095 1 
i 1 32 1 24 79 0 0 0 0 4095 1 
i 1 32 3.666667 63 95 0 0 0 0 4095 1 
i 14 32 0.3333333 36 63 0 0 0 0 4095 1 
i 11 32.33333 0.3333333 43 63 0 0 0 0 4095 1 
i 14 32.33333 0.3333333 43 63 0 0 0 0 4095 1 
i 11 32.66667 2 44 63 0 0 0 0 4095 1 
i 14 32.66667 2 43 63 0 0 0 0 4095 1 
i 1 33 0.3333333 36 79 0 0 0 0 4095 1 
i 7 33 0.3333333 48 63 0 0 0 0 4095 1 
i 1 33.33333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 33.33333 0.3333333 50 63 0 0 0 0 4095 1 
i 7 33.33333 1.333333 50 63 0 0 0 0 4095 1 
i 4 33.66667 0.3333333 51 63 0 0 0 0 4095 1 
i 1 34 0.6666667 24 79 0 0 0 0 4095 1 
i 4 34 0.6666667 53 63 0 0 0 0 4095 1 
i 1 34.66667 1 24 79 0 0 0 0 4095 1 
i 14 34.66667 0.3333333 36 63 0 0 0 0 4095 1 
i 11 35 0.3333333 43 63 0 0 0 0 4095 1 
i 14 35 0.3333333 43 63 0 0 0 0 4095 1 
i 11 35.33333 2 44 63 0 0 0 0 4095 1 
i 14 35.33333 2 43 63 0 0 0 0 4095 1 
i 1 35.66667 0.3333333 36 79 0 0 0 0 4095 1 
i 1 35.66667 0.1666667 62 95 0 0 0 0 4095 1 
i 7 35.66667 0.3333333 48 63 0 0 0 0 4095 1 
i 1 35.83333 0.1666667 60 95 0 0 0 0 4095 1 
i 1 36 0.6666667 36 79 0 0 0 0 4095 1 
i 1 36 1 58 95 0 0 0 0 4095 1 
i 4 36 0.3333333 50 63 0 0 0 0 4095 1 
i 7 36 1.333333 50 63 0 0 0 0 4095 1 
i 4 36.33333 0.3333333 51 63 0 0 0 0 4095 1 
i 1 36.66667 0.6666667 24 79 0 0 0 0 4095 1 
i 4 36.66667 0.6666667 53 63 0 0 0 0 4095 1 
i 1 37 0.3333333 56 95 0 0 0 0 4095 1 
i 1 37.33333 1 31 79 0 0 0 0 4095 1 
i 1 37.33333 3.666667 60 95 0 0 0 0 4095 1 
i 4 37.33333 0.6666667 53 63 0 0 0 0 4095 1 
i 7 37.33333 2.666667 49 63 0 0 0 0 4095 1 
i 11 37.33333 2.666667 46 63 0 0 0 0 4095 1 
i 14 37.33333 0.6666667 41 63 0 0 0 0 4095 1 
i 4 38 0.6666667 55 63 0 0 0 0 4095 1 
i 14 38 0.6666667 43 63 0 0 0 0 4095 1 
i 1 38.33333 0.3333333 31 79 0 0 0 0 4095 1 
i 1 38.66667 0.6666667 31 79 0 0 0 0 4095 1 
i 4 38.66667 0.6666667 52 63 0 0 0 0 4095 1 
i 14 38.66667 0.6666667 40 63 0 0 0 0 4095 1 
i 1 39.33333 0.6666667 31 79 0 0 0 0 4095 1 
i 4 39.33333 0.6666667 53 63 0 0 0 0 4095 1 
i 14 39.33333 0.6666667 41 63 0 0 0 0 4095 1 
i 1 40 1 24 79 0 0 0 0 4095 1 
i 4 40 1.333333 56 63 0 0 0 0 4095 1 
i 7 40 2.666667 52 63 0 0 0 0 4095 1 
i 11 40 2.666667 49 63 0 0 0 0 4095 1 
i 14 40 1.333333 44 63 0 0 0 0 4095 1 
i 1 41 0.3333333 24 79 0 0 0 0 4095 1 
i 1 41 0.1666667 58 95 0 0 0 0 4095 1 
i 1 41.16667 0.1666667 56 95 0 0 0 0 4095 1 
i 1 41.33333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 41.33333 1 55 95 0 0 0 0 4095 1 
i 4 41.33333 1.333333 55 63 0 0 0 0 4095 1 
i 14 41.33333 1.333333 43 63 0 0 0 0 4095 1 
i 1 42 0.6666667 24 79 0 0 0 0 4095 1 
i 1 42.33333 0.3333333 53 95 0 0 0 0 4095 1 
i 1 42.66667 1 29 79 0 0 0 0 4095 1 
i 1 42.66667 4 56 95 0 0 0 0 4095 1 
i 4 42.66667 0.6666667 48 63 0 0 0 0 4095 1 
i 7 42.66667 2.666667 48 63 0 0 0 0 4095 1 
i 11 42.66667 2.666667 44 63 0 0 0 0 4095 1 
i 14 42.66667 0.6666667 36 63 0 0 0 0 4095 1 
i 4 43.33333 0.6666667 50 63 0 0 0 0 4095 1 
i 14 43.33333 0.6666667 38 63 0 0 0 0 4095 1 
i 1 43.66667 0.3333333 34 79 0 0 0 0 4095 1 
i 1 44 0.6666667 34 79 0 0 0 0 4095 1 
i 4 44 0.6666667 51 63 0 0 0 0 4095 1 
i 14 44 0.6666667 39 63 0 0 0 0 4095 1 
i 1 44.66667 0.6666667 29 79 0 0 0 0 4095 1 
i 4 44.66667 0.6666667 53 63 0 0 0 0 4095 1 
i 14 44.66667 0.6666667 41 63 0 0 0 0 4095 1 
i 1 45.33333 1 27 79 0 0 0 0 4095 1 
i 4 45.33333 1 55 63 0 0 0 0 4095 1 
i 7 45.33333 2.666667 50 63 0 0 0 0 4095 1 
i 11 45.33333 2.666667 44 63 0 0 0 0 4095 1 
i 14 45.33333 1 43 63 0 0 0 0 4095 1 
i 1 46.33333 0.3333333 36 79 0 0 0 0 4095 1 
i 4 46.33333 0.1666667 53 63 0 0 0 0 4095 1 
i 14 46.33333 0.1666667 41 63 0 0 0 0 4095 1 
i 4 46.5 0.1666667 51 63 0 0 0 0 4095 1 
i 14 46.5 0.1666667 39 63 0 0 0 0 4095 1 
i 1 46.66667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 46.66667 1.333333 50 63 0 0 0 0 4095 1 
i 14 46.66667 1.333333 38 63 0 0 0 0 4095 1 
i 1 47 0.3333333 48 95 0 0 0 0 4095 1 
i 1 47.33333 0.6666667 27 79 0 0 0 0 4095 1 
i 1 47.33333 0.3333333 53 95 0 0 0 0 4095 1 
i 1 47.66667 0.3333333 55 95 0 0 0 0 4095 1 
i 1 48 1 26 79 0 0 0 0 4095 1 
i 1 48 3.666667 56 95 0 0 0 0 4095 1 
i 4 48 0.6666667 53 63 0 0 0 0 4095 1 
i 7 48 2.666667 48 63 0 0 0 0 4095 1 
i 11 48 2.666667 44 63 0 0 0 0 4095 1 
i 14 48 0.6666667 41 63 0 0 0 0 4095 1 
i 4 48.66667 1 55 63 0 0 0 0 4095 1 
i 14 48.66667 1 43 63 0 0 0 0 4095 1 
i 1 49 0.3333333 32 79 0 0 0 0 4095 1 
i 1 49.33333 0.6666667 32 79 0 0 0 0 4095 1 
i 4 49.66667 1 53 63 0 0 0 0 4095 1 
i 14 49.66667 1 41 63 0 0 0 0 4095 1 
i 1 50 0.6666667 24 79 0 0 0 0 4095 1 
i 1 50.66667 1 23 79 0 0 0 0 4095 1 
i 4 50.66667 1 56 63 0 0 0 0 4095 1 
i 7 50.66667 2.666667 53 63 0 0 0 0 4095 1 
i 11 50.66667 2.666667 50 63 0 0 0 0 4095 1 
i 14 50.66667 2.666667 44 63 0 0 0 0 4095 1 
i 1 51.66667 0.3333333 31 79 0 0 0 0 4095 1 
i 1 51.66667 0.1666667 55 95 0 0 0 0 4095 1 
i 1 51.83333 0.1666667 53 95 0 0 0 0 4095 1 
i 1 52 0.6666667 31 79 0 0 0 0 4095 1 
i 1 52 0.6666667 51 95 0 0 0 0 4095 1 
i 1 52.66667 0.6666667 23 79 0 0 0 0 4095 1 
i 1 52.66667 0.6666667 50 95 0 0 0 0 4095 1 
i 1 53.33333 1 24 79 0 0 0 0 4095 1 
i 1 53.33333 3.666667 55 95 0 0 0 0 4095 1 
i 4 53.33333 0.6666667 51 63 0 0 0 0 4095 1 
i 7 53.33333 2.666667 55 63 0 0 0 0 4095 1 
i 11 53.33333 2.666667 48 63 0 0 0 0 4095 1 
i 14 53.33333 0.6666667 39 63 0 0 0 0 4095 1 
i 4 54 1.333333 53 63 0 0 0 0 4095 1 
i 14 54 1.333333 41 63 0 0 0 0 4095 1 
i 1 54.33333 0.3333333 31 79 0 0 0 0 4095 1 
i 1 54.66667 0.6666667 31 79 0 0 0 0 4095 1 
i 1 55.33333 0.6666667 24 79 0 0 0 0 4095 1 
i 4 55.33333 0.6666667 51 63 0 0 0 0 4095 1 
i 14 55.33333 0.6666667 39 63 0 0 0 0 4095 1 
i 1 56 1 22 79 0 0 0 0 4095 1 
i 4 56 1 55 63 0 0 0 0 4095 1 
i 7 56 2.666667 51 63 0 0 0 0 4095 1 
i 11 56 2.666667 48 63 0 0 0 0 4095 1 
i 14 56 2.666667 43 63 0 0 0 0 4095 1 
i 1 57 0.3333333 31 79 0 0 0 0 4095 1 
i 1 57 0.1666667 53 95 0 0 0 0 4095 1 
i 1 57.16667 0.1666667 51 95 0 0 0 0 4095 1 
i 1 57.33333 0.6666667 31 79 0 0 0 0 4095 1 
i 1 57.33333 1 50 95 0 0 0 0 4095 1 
i 1 58 0.6666667 22 79 0 0 0 0 4095 1 
i 1 58.33333 0.3333333 48 95 0 0 0 0 4095 1 
i 1 58.66667 1 21 79 0 0 0 0 4095 1 
i 1 58.66667 2 54 95 0 0 0 0 4095 1 
i 4 58.66667 0.6666667 50 63 0 0 0 0 4095 1 
i 7 58.66667 2.666667 50 63 0 0 0 0 4095 1 
i 11 58.66667 2.666667 48 63 0 0 0 0 4095 1 
i 14 58.66667 0.6666667 38 63 0 0 0 0 4095 1 
i 4 59.33333 0.6666667 51 63 0 0 0 0 4095 1 
i 14 59.33333 0.6666667 39 63 0 0 0 0 4095 1 
i 1 59.66667 0.3333333 30 79 0 0 0 0 4095 1 
i 1 60 0.6666667 30 79 0 0 0 0 4095 1 
i 4 60 0.6666667 51 63 0 0 0 0 4095 1 
i 14 60 0.6666667 39 63 0 0 0 0 4095 1 
i 1 60.66667 0.6666667 21 79 0 0 0 0 4095 1 
i 1 60.66667 0.6666667 50 95 0 0 0 0 4095 1 
i 4 60.66667 0.6666667 50 63 0 0 0 0 4095 1 
i 14 60.66667 0.6666667 38 63 0 0 0 0 4095 1 
i 1 61.33333 1 19 79 0 0 0 0 4095 1 
i 1 61.33333 2 55 95 0 0 0 0 4095 1 
i 4 61.33333 0.6666667 50 63 0 0 0 0 4095 1 
i 7 61.33333 2.666667 53 63 0 0 0 0 4095 1 
i 11 61.33333 2.666667 47 63 0 0 0 0 4095 1 
i 14 61.33333 0.6666667 38 63 0 0 0 0 4095 1 
i 4 62 1.333333 51 63 0 0 0 0 4095 1 
i 14 62 1.333333 39 63 0 0 0 0 4095 1 
i 1 62.33333 0.3333333 29 79 0 0 0 0 4095 1 
i 1 62.66667 0.6666667 29 79 0 0 0 0 4095 1 
i 1 63.33333 0.6666667 19 79 0 0 0 0 4095 1 
i 1 63.33333 0.6666667 55 95 0 0 0 0 4095 1 
i 4 63.33333 0.6666667 50 63 0 0 0 0 4095 1 
i 14 63.33333 0.6666667 38 63 0 0 0 0 4095 1 
i 1 64 1 24 79 0 0 0 0 4095 1 
i 1 64 4.666667 60 95 0 0 0 0 4095 1 
i 4 64 1 55 63 0 0 0 0 4095 1 
i 7 64 2.666667 51 63 0 0 0 0 4095 1 
i 11 64 5.333333 48 63 0 0 0 0 4095 1 
i 14 64 1 43 63 0 0 0 0 4095 1 
i 1 65 0.3333333 36 79 0 0 0 0 4095 1 
i 4 65 0.3333333 56 63 0 0 0 0 4095 1 
i 14 65 0.3333333 44 63 0 0 0 0 4095 1 
i 1 65.33333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 65.33333 0.6666667 56 63 0 0 0 0 4095 1 
i 14 65.33333 0.6666667 44 63 0 0 0 0 4095 1 
i 1 66 0.6666667 24 79 0 0 0 0 4095 1 
i 4 66 0.6666667 55 63 0 0 0 0 4095 1 
i 14 66 0.6666667 43 63 0 0 0 0 4095 1 
i 1 66.66667 1 24 79 0 0 0 0 4095 1 
i 4 66.66667 1 58 63 0 0 0 0 4095 1 
i 7 66.66667 2.666667 51 63 0 0 0 0 4095 1 
i 14 66.66667 1 46 63 0 0 0 0 4095 1 
i 1 67.66667 0.3333333 36 79 0 0 0 0 4095 1 
i 4 67.66667 0.3333333 56 63 0 0 0 0 4095 1 
i 14 67.66667 0.3333333 44 63 0 0 0 0 4095 1 
i 1 68 0.6666667 36 79 0 0 0 0 4095 1 
i 4 68 0.6666667 56 63 0 0 0 0 4095 1 
i 14 68 0.6666667 44 63 0 0 0 0 4095 1 
i 1 68.66667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 68.66667 0.08333333 62 95 0 0 0 0 4095 1 
i 4 68.66667 0.6666667 55 63 0 0 0 0 4095 1 
i 14 68.66667 0.6666667 43 63 0 0 0 0 4095 1 
i 1 68.75 0.08333333 63 95 0 0 0 0 4095 1 
i 1 68.83333 0.08333333 65 95 0 0 0 0 4095 1 
i 1 68.91667 0.08333333 67 95 0 0 0 0 4095 1 
i 1 69 0.08333333 68 95 0 0 0 0 4095 1 
i 1 69.08333 0.08333333 70 95 0 0 0 0 4095 1 
i 1 69.16667 0.08333333 72 95 0 0 0 0 4095 1 
i 1 69.25 0.08333333 74 95 0 0 0 0 4095 1 
i 1 69.33333 1 29 79 0 0 0 0 4095 1 
i 1 69.33333 2 75 111 0 0 0 0 4095 1 
i 4 69.33333 2 63 63 0 0 0 0 4095 1 
i 7 69.33333 2 60 63 0 0 0 0 4095 1 
i 11 69.33333 2 60 63 0 0 0 0 4095 1 
i 14 69.33333 2 44 63 0 0 0 0 4095 1 
i 1 70.33333 0.3333333 29 79 0 0 0 0 4095 1 
i 1 70.66667 0.6666667 29 79 0 0 0 0 4095 1 
i 1 71.33333 0.6666667 29 79 0 0 0 0 4095 1 
i 1 71.33333 0.3333333 72 111 0 0 0 0 4095 1 
i 4 71.33333 0.3333333 60 63 0 0 0 0 4095 1 
i 7 71.33333 0.3333333 56 63 0 0 0 0 4095 1 
i 11 71.33333 0.3333333 56 63 0 0 0 0 4095 1 
i 14 71.33333 0.3333333 41 63 0 0 0 0 4095 1 
i 1 71.66667 0.3333333 74 111 0 0 0 0 4095 1 
i 4 71.66667 0.3333333 62 63 0 0 0 0 4095 1 
i 7 71.66667 0.3333333 58 63 0 0 0 0 4095 1 
i 11 71.66667 0.3333333 58 63 0 0 0 0 4095 1 
i 14 71.66667 0.3333333 43 63 0 0 0 0 4095 1 
i 1 72 1 34 79 0 0 0 0 4095 1 
i 1 72 0.444 75 111 0 0 0 0 4095 1 
i 4 72 0.444 63 63 0 0 0 0 4095 1 
i 7 72 0.444 60 63 0 0 0 0 4095 1 
i 11 72 0.444 60 63 0 0 0 0 4095 1 
i 14 72 0.444 44 63 0 0 0 0 4095 1 
i 1 72.444 0.444 74 111 0 0 0 0 4095 1 
i 4 72.444 0.444 62 63 0 0 0 0 4095 1 
i 7 72.444 0.444 58 63 0 0 0 0 4095 1 
i 11 72.444 0.444 58 63 0 0 0 0 4095 1 
i 14 72.444 0.444 44 63 0 0 0 0 4095 1 
i 1 72.888 0.444 77 111 0 0 0 0 4095 1 
i 4 72.888 0.444 65 63 0 0 0 0 4095 1 
i 7 72.888 0.444 62 63 0 0 0 0 4095 1 
i 11 72.888 0.444 62 63 0 0 0 0 4095 1 
i 14 72.888 0.444 44 63 0 0 0 0 4095 1 
i 1 73 0.3333333 34 79 0 0 0 0 4095 1 
i 1 73.332 0.6666667 70 111 0 0 0 0 4095 1 
i 4 73.332 0.6666667 58 63 0 0 0 0 4095 1 
i 7 73.332 0.6666667 55 63 0 0 0 0 4095 1 
i 11 73.332 0.6666667 50 63 0 0 0 0 4095 1 
i 14 73.332 0.6666667 46 63 0 0 0 0 4095 1 
i 1 73.33333 0.6666667 34 79 0 0 0 0 4095 1 
i 1 73.99867 0.6666667 68 111 0 0 0 0 4095 1 
i 4 73.99867 0.6666667 56 63 0 0 0 0 4095 1 
i 7 73.99867 0.6666667 53 63 0 0 0 0 4095 1 
i 11 73.99867 0.6666667 53 63 0 0 0 0 4095 1 
i 14 73.99867 0.6666667 46 63 0 0 0 0 4095 1 
i 1 74 0.6666667 34 79 0 0 0 0 4095 1 
i 1 74.66533 0.6666667 67 111 0 0 0 0 4095 1 
i 4 74.66533 0.6666667 55 63 0 0 0 0 4095 1 
i 7 74.66533 0.6666667 51 63 0 0 0 0 4095 1 
i 11 74.66533 0.6666667 51 63 0 0 0 0 4095 1 
i 14 74.66533 0.6666667 46 63 0 0 0 0 4095 1 
i 1 74.66667 1 27 79 0 0 0 0 4095 1 
i 1 75.332 2.444 74 111 0 0 0 0 4095 1 
i 4 75.332 2.444 62 63 0 0 0 0 4095 1 
i 7 75.332 2.444 58 63 0 0 0 0 4095 1 
i 11 75.332 2.444 58 63 0 0 0 0 4095 1 
i 14 75.332 2 43 63 0 0 0 0 4095 1 
i 1 75.66667 0.3333333 27 79 0 0 0 0 4095 1 
i 1 76 0.6666667 27 79 0 0 0 0 4095 1 
i 1 76.66667 0.6666667 27 79 0 0 0 0 4095 1 
i 14 77.332 0.444 44 63 0 0 0 0 4095 1 
i 1 77.33333 1 20 79 0 0 0 0 4095 1 
i 1 77.776 0.444 72 111 0 0 0 0 4095 1 
i 4 77.776 0.444 60 63 0 0 0 0 4095 1 
i 7 77.776 0.444 56 63 0 0 0 0 4095 1 
i 11 77.776 0.444 56 63 0 0 0 0 4095 1 
i 14 77.776 0.444 44 63 0 0 0 0 4095 1 
i 1 78.22 0.444 75 111 0 0 0 0 4095 1 
i 4 78.22 0.444 63 63 0 0 0 0 4095 1 
i 7 78.22 0.444 60 63 0 0 0 0 4095 1 
i 11 78.22 0.444 60 63 0 0 0 0 4095 1 
i 14 78.22 0.444 44 63 0 0 0 0 4095 1 
i 1 78.33333 0.3333333 20 79 0 0 0 0 4095 1 
i 1 78.664 0.6666667 68 111 0 0 0 0 4095 1 
i 4 78.664 0.6666667 56 63 0 0 0 0 4095 1 
i 7 78.664 0.6666667 53 63 0 0 0 0 4095 1 
i 11 78.664 0.6666667 48 63 0 0 0 0 4095 1 
i 14 78.664 0.6666667 41 63 0 0 0 0 4095 1 
i 1 78.66667 0.6666667 20 79 0 0 0 0 4095 1 
i 1 79.33067 0.6666667 67 111 0 0 0 0 4095 1 
i 4 79.33067 0.6666667 55 63 0 0 0 0 4095 1 
i 7 79.33067 0.6666667 51 63 0 0 0 0 4095 1 
i 11 79.33067 0.6666667 51 63 0 0 0 0 4095 1 
i 14 79.33067 0.6666667 48 63 0 0 0 0 4095 1 
i 1 79.33333 0.6666667 20 79 0 0 0 0 4095 1 
i 1 79.99733 0.6666667 65 111 0 0 0 0 4095 1 
i 4 79.99733 0.6666667 53 63 0 0 0 0 4095 1 
i 7 79.99733 0.6666667 50 63 0 0 0 0 4095 1 
i 11 79.99733 0.6666667 50 63 0 0 0 0 4095 1 
i 14 79.99733 0.6666667 44 63 0 0 0 0 4095 1 
i 1 80 1 26 79 0 0 0 0 4095 1 
i 1 80.664 2.444 72 111 0 0 0 0 4095 1 
i 4 80.664 2.444 60 63 0 0 0 0 4095 1 
i 7 80.664 2.444 56 63 0 0 0 0 4095 1 
i 11 80.664 2.444 56 63 0 0 0 0 4095 1 
i 14 80.664 2.444 41 63 0 0 0 0 4095 1 
i 1 81 0.3333333 26 79 0 0 0 0 4095 1 
i 1 81.33333 0.6666667 26 79 0 0 0 0 4095 1 
i 1 82 0.6666667 26 79 0 0 0 0 4095 1 
i 1 82.66667 1 19 79 0 0 0 0 4095 1 
i 1 83.108 0.444 71 111 0 0 0 0 4095 1 
i 4 83.108 0.444 59 63 0 0 0 0 4095 1 
i 7 83.108 0.444 55 63 0 0 0 0 4095 1 
i 11 83.108 0.444 55 63 0 0 0 0 4095 1 
i 14 83.108 0.444 41 63 0 0 0 0 4095 1 
i 1 83.552 0.444 74 111 0 0 0 0 4095 1 
i 4 83.552 0.444 62 63 0 0 0 0 4095 1 
i 7 83.552 0.444 59 63 0 0 0 0 4095 1 
i 11 83.552 0.444 55 63 0 0 0 0 4095 1 
i 14 83.552 0.444 41 63 0 0 0 0 4095 1 
i 1 83.66667 0.3333333 19 79 0 0 0 0 4095 1 
i 1 83.996 0.6666667 67 111 0 0 0 0 4095 1 
i 4 83.996 0.6666667 55 63 0 0 0 0 4095 1 
i 7 83.996 0.6666667 51 63 0 0 0 0 4095 1 
i 11 83.996 0.6666667 47 63 0 0 0 0 4095 1 
i 14 83.996 0.6666667 44 63 0 0 0 0 4095 1 
i 1 84 0.6666667 19 79 0 0 0 0 4095 1 
i 1 84.66267 0.6666667 65 111 0 0 0 0 4095 1 
i 4 84.66267 0.6666667 53 63 0 0 0 0 4095 1 
i 7 84.66267 0.6666667 50 63 0 0 0 0 4095 1 
i 11 84.66267 0.6666667 47 63 0 0 0 0 4095 1 
i 14 84.66267 0.6666667 44 63 0 0 0 0 4095 1 
i 1 84.66667 0.6666667 19 79 0 0 0 0 4095 1 
i 1 85.32933 1 63 111 0 0 0 0 4095 1 
i 4 85.32933 1 51 63 0 0 0 0 4095 1 
i 7 85.32933 1 48 63 0 0 0 0 4095 1 
i 11 85.32933 1 43 63 0 0 0 0 4095 1 
i 14 85.32933 1 39 63 0 0 0 0 4095 1 
i 1 85.33333 1 24 79 0 0 0 0 4095 1 
i 1 86.32933 1 65 111 0 0 0 0 4095 1 
i 4 86.32933 1 53 63 0 0 0 0 4095 1 
i 7 86.32933 1 50 63 0 0 0 0 4095 1 
i 11 86.32933 1 44 63 0 0 0 0 4095 1 
i 14 86.32933 1 41 63 0 0 0 0 4095 1 
i 1 86.33333 0.3333333 24 79 0 0 0 0 4095 1 
i 1 86.66667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 87.32933 0.6666667 67 111 0 0 0 0 4095 1 
i 4 87.32933 0.6666667 55 63 0 0 0 0 4095 1 
i 7 87.32933 0.6666667 51 63 0 0 0 0 4095 1 
i 11 87.32933 0.6666667 46 63 0 0 0 0 4095 1 
i 14 87.32933 0.6666667 36 63 0 0 0 0 4095 1 
i 1 87.33333 0.6666667 27 79 0 0 0 0 4095 1 
i 1 87.996 1 67 111 0 0 0 0 4095 1 
i 4 87.996 2.666667 55 63 0 0 0 0 4095 1 
i 7 87.996 2 49 63 0 0 0 0 4095 1 
i 11 87.996 2.666667 46 63 0 0 0 0 4095 1 
i 14 87.996 2.666667 43 63 0 0 0 0 4095 1 
i 1 88 1 28 79 0 0 0 0 4095 1 
i 1 89 0.3333333 36 79 0 0 0 0 4095 1 
i 1 89.33333 0.6666667 36 79 0 0 0 0 4095 1 
i 1 89.996 0.1666667 68 111 0 0 0 0 4095 1 
i 1 90 0.6666667 24 79 0 0 0 0 4095 1 
i 1 90.16267 0.1666667 67 111 0 0 0 0 4095 1 
i 1 90.32933 0.1666667 66 111 0 0 0 0 4095 1 
i 1 90.496 0.1666667 67 111 0 0 0 0 4095 1 
i 1 90.66267 2 75 111 0 0 0 0 4095 1 
i 11 90.66267 2 51 63 0 0 0 0 4095 1 
i 14 90.66267 2 48 63 0 0 0 0 4095 1 
i 1 90.66667 1 29 79 0 0 0 0 4095 1 
i 1 91.66667 0.3333333 29 79 0 0 0 0 4095 1 
i 1 92 0.6666667 29 79 0 0 0 0 4095 1 
i 1 92.66267 0.3333333 72 111 0 0 0 0 4095 1 
i 11 92.66267 0.3333333 48 63 0 0 0 0 4095 1 
i 14 92.66267 0.3333333 44 63 0 0 0 0 4095 1 
i 1 92.66667 0.6666667 29 79 0 0 0 0 4095 1 
i 1 92.996 0.3333333 74 111 0 0 0 0 4095 1 
i 11 92.996 0.3333333 50 63 0 0 0 0 4095 1 
i 14 92.996 0.3333333 46 63 0 0 0 0 4095 1 
i 1 93.32933 0.444 75 111 0 0 0 0 4095 1 
i 11 93.32933 0.444 51 63 0 0 0 0 4095 1 
i 14 93.32933 0.444 48 63 0 0 0 0 4095 1 
i 1 93.33333 1 34 79 0 0 0 0 4095 1 
i 1 93.77333 0.444 74 111 0 0 0 0 4095 1 
i 11 93.77333 0.444 50 63 0 0 0 0 4095 1 
i 14 93.77333 0.444 46 63 0 0 0 0 4095 1 
i 1 94.21733 0.444 77 111 0 0 0 0 4095 1 
i 11 94.21733 0.444 53 63 0 0 0 0 4095 1 
i 14 94.21733 0.444 50 63 0 0 0 0 4095 1 
i 1 94.33333 0.3333333 34 79 0 0 0 0 4095 1 
i 1 94.66133 0.6666667 70 111 0 0 0 0 4095 1 
i 11 94.66133 0.6666667 46 63 0 0 0 0 4095 1 
i 14 94.66133 0.6666667 43 63 0 0 0 0 4095 1 
i 1 94.66667 0.6666667 34 79 0 0 0 0 4095 1 
i 1 95.328 0.6666667 68 111 0 0 0 0 4095 1 
i 11 95.328 0.6666667 44 63 0 0 0 0 4095 1 
i 14 95.328 0.6666667 41 63 0 0 0 0 4095 1 
i 1 95.33333 0.6666667 34 79 0 0 0 0 4095 1 
i 1 95.99467 0.6666667 67 111 0 0 0 0 4095 1 
i 11 95.99467 0.6666667 43 63 0 0 0 0 4095 1 
i 14 95.99467 0.6666667 39 63 0 0 0 0 4095 1 
i 1 96 1 27 79 0 0 0 0 4095 1 
i 1 96.66133 2.444 74 111 0 0 0 0 4095 1 
i 11 96.66133 2.444 50 63 0 0 0 0 4095 1 
i 14 96.66133 2.444 46 63 0 0 0 0 4095 1 
i 1 97 0.3333333 27 79 0 0 0 0 4095 1 
i 1 97.33333 0.6666667 27 79 0 0 0 0 4095 1 
i 1 98 0.6666667 27 79 0 0 0 0 4095 1 
i 1 98.66667 1 32 79 0 0 0 0 4095 1 
i 1 99.10533 0.444 72 111 0 0 0 0 4095 1 
i 11 99.10533 0.444 48 63 0 0 0 0 4095 1 
i 14 99.10533 0.444 44 63 0 0 0 0 4095 1 
i 1 99.54933 0.444 75 111 0 0 0 0 4095 1 
i 11 99.54933 0.444 51 63 0 0 0 0 4095 1 
i 14 99.54933 0.444 48 63 0 0 0 0 4095 1 
i 1 99.66667 0.3333333 32 79 0 0 0 0 4095 1 
i 1 99.99333 0.6666667 68 111 0 0 0 0 4095 1 
i 11 99.99333 0.6666667 56 63 0 0 0 0 4095 1 
i 14 99.99333 0.6666667 41 63 0 0 0 0 4095 1 
i 1 100 0.6666667 32 79 0 0 0 0 4095 1 
i 1 100.66 0.6666667 67 111 0 0 0 0 4095 1 
i 11 100.66 0.6666667 55 63 0 0 0 0 4095 1 
i 14 100.66 0.6666667 39 63 0 0 0 0 4095 1 
i 1 100.6667 0.6666667 32 79 0 0 0 0 4095 1 
i 1 101.3267 2.666667 66 111 0 0 0 0 4095 1 
i 11 101.3267 2.666667 54 63 0 0 0 0 4095 1 
i 14 101.3267 2.666667 38 63 0 0 0 0 4095 1 
i 1 101.3333 1 26 79 0 0 0 0 4095 1 
i 1 102.3333 0.3333333 26 79 0 0 0 0 4095 1 
i 1 102.6667 0.6666667 26 79 0 0 0 0 4095 1 
i 1 103.3333 0.6666667 26 79 0 0 0 0 4095 1 
i 1 103.9933 2.333333 67 111 0 0 0 0 4095 1 
i 11 103.9933 2.666667 47 63 0 0 0 0 4095 1 
i 11 103.9933 2.666667 55 63 0 0 0 0 4095 1 
i 14 103.9933 2.666667 41 63 0 0 0 0 4095 1 
i 1 104 1 31 79 0 0 0 0 4095 1 
i 1 105 0.3333333 31 79 0 0 0 0 4095 1 
i 1 105.3333 0.6666667 31 79 0 0 0 0 4095 1 
i 1 106 0.6666667 31 79 0 0 0 0 4095 1 
i 1 106.3267 0.3333333 67 111 0 0 0 0 4095 1 
i 1 106.66 4.666667 72 111 0 0 0 0 4095 1 
i 11 106.66 5.333333 48 63 0 0 0 0 4095 1 
i 14 106.66 1 43 63 0 0 0 0 4095 1 
i 4 106.6627 5.333333 60 63 0 0 0 0 4095 1 
i 7 106.6627 5.333333 51 63 0 0 0 0 4095 1 
i 1 106.6667 1 24 79 0 0 0 0 4095 1 
i 14 107.66 0.3333333 44 63 0 0 0 0 4095 1 
i 1 107.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 14 107.9933 0.6666667 44 63 0 0 0 0 4095 1 
i 1 108 0.6666667 36 79 0 0 0 0 4095 1 
i 14 108.66 0.6666667 43 63 0 0 0 0 4095 1 
i 1 108.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 14 109.3267 1 46 63 0 0 0 0 4095 1 
i 1 109.3333 1 24 79 0 0 0 0 4095 1 
i 14 110.3267 0.3333333 44 63 0 0 0 0 4095 1 
i 1 110.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 14 110.66 0.6666667 44 63 0 0 0 0 4095 1 
i 1 110.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 14 111.3267 0.6666667 43 63 0 0 0 0 4095 1 
i 1 111.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 111.9933 3.666667 79 95 0 0 0 0 4095 1 
i 14 111.9933 0.3333333 36 63 0 0 0 0 4095 1 
i 1 112 1 24 79 0 0 0 0 4095 1 
i 11 112.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 14 112.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 11 112.66 2 44 63 0 0 0 0 4095 1 
i 14 112.66 2 43 63 0 0 0 0 4095 1 
i 7 112.996 0.3333333 48 63 0 0 0 0 4095 1 
i 1 113 0.3333333 36 79 0 0 0 0 4095 1 
i 4 113.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 7 113.3293 1.333333 50 63 0 0 0 0 4095 1 
i 1 113.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 113.6627 0.3333333 51 63 0 0 0 0 4095 1 
i 4 113.996 0.6666667 53 63 0 0 0 0 4095 1 
i 1 114 0.6666667 24 79 0 0 0 0 4095 1 
i 14 114.66 0.3333333 36 63 0 0 0 0 4095 1 
i 1 114.6667 1 24 79 0 0 0 0 4095 1 
i 11 114.9933 0.3333333 43 63 0 0 0 0 4095 1 
i 14 114.9933 0.3333333 43 63 0 0 0 0 4095 1 
i 11 115.3267 2 44 63 0 0 0 0 4095 1 
i 14 115.3267 2 43 63 0 0 0 0 4095 1 
i 1 115.66 0.1666667 77 95 0 0 0 0 4095 1 
i 7 115.6627 0.3333333 48 63 0 0 0 0 4095 1 
i 1 115.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 1 115.8267 0.1666667 75 95 0 0 0 0 4095 1 
i 1 115.9933 1 74 95 0 0 0 0 4095 1 
i 4 115.996 0.3333333 50 63 0 0 0 0 4095 1 
i 7 115.996 1.333333 50 63 0 0 0 0 4095 1 
i 1 116 0.6666667 36 79 0 0 0 0 4095 1 
i 4 116.3293 0.3333333 51 63 0 0 0 0 4095 1 
i 4 116.6627 0.6666667 53 63 0 0 0 0 4095 1 
i 1 116.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 116.9933 0.3333333 72 95 0 0 0 0 4095 1 
i 1 117.3267 3.666667 75 95 0 0 0 0 4095 1 
i 14 117.3267 0.3333333 36 63 0 0 0 0 4095 1 
i 1 117.3333 1 24 79 0 0 0 0 4095 1 
i 11 117.66 0.3333333 43 63 0 0 0 0 4095 1 
i 14 117.66 0.3333333 43 63 0 0 0 0 4095 1 
i 11 117.9933 2 44 63 0 0 0 0 4095 1 
i 14 117.9933 2 43 63 0 0 0 0 4095 1 
i 7 118.3293 0.3333333 48 63 0 0 0 0 4095 1 
i 1 118.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 4 118.6627 0.3333333 50 63 0 0 0 0 4095 1 
i 7 118.6627 1.333333 50 63 0 0 0 0 4095 1 
i 1 118.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 118.996 0.3333333 51 63 0 0 0 0 4095 1 
i 4 119.3293 0.6666667 53 63 0 0 0 0 4095 1 
i 1 119.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 14 119.9933 0.3333333 36 63 0 0 0 0 4095 1 
i 1 120 1 24 79 0 0 0 0 4095 1 
i 11 120.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 14 120.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 11 120.66 2 44 63 0 0 0 0 4095 1 
i 14 120.66 2 43 63 0 0 0 0 4095 1 
i 1 120.9933 0.1666667 74 95 0 0 0 0 4095 1 
i 7 120.996 0.3333333 48 63 0 0 0 0 4095 1 
i 1 121 0.3333333 36 79 0 0 0 0 4095 1 
i 1 121.16 0.1666667 72 95 0 0 0 0 4095 1 
i 1 121.3267 1 70 95 0 0 0 0 4095 1 
i 4 121.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 7 121.3293 1.333333 50 63 0 0 0 0 4095 1 
i 1 121.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 121.6627 0.3333333 51 63 0 0 0 0 4095 1 
i 4 121.996 0.6666667 53 63 0 0 0 0 4095 1 
i 1 122 0.6666667 24 79 0 0 0 0 4095 1 
i 1 122.3267 0.3333333 68 95 0 0 0 0 4095 1 
i 1 122.66 2.333333 72 95 0 0 0 0 4095 1 
i 11 122.66 2.666667 44 63 0 0 0 0 4095 1 
i 11 122.66 2.666667 48 63 0 0 0 0 4095 1 
i 4 122.6627 2.666667 56 63 0 0 0 0 4095 1 
i 1 122.6667 1 29 79 0 0 0 0 4095 1 
i 14 122.9933 0.3333333 51 63 0 0 0 0 4095 1 
i 7 122.996 0.3333333 51 63 0 0 0 0 4095 1 
i 14 123.3267 0.3333333 50 63 0 0 0 0 4095 1 
i 7 123.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 14 123.66 0.3333333 51 63 0 0 0 0 4095 1 
i 7 123.6627 0.3333333 51 63 0 0 0 0 4095 1 
i 1 123.6667 0.3333333 29 79 0 0 0 0 4095 1 
i 1 124 0.6666667 34 79 0 0 0 0 4095 1 
i 14 124.3267 0.3333333 51 63 0 0 0 0 4095 1 
i 7 124.3293 0.3333333 51 63 0 0 0 0 4095 1 
i 14 124.66 0.6666667 50 63 0 0 0 0 4095 1 
i 7 124.6627 0.6666667 50 63 0 0 0 0 4095 1 
i 1 124.6667 0.6666667 34 79 0 0 0 0 4095 1 
i 1 124.9933 0.1666667 70 95 0 0 0 0 4095 1 
i 1 125.16 0.1666667 68 95 0 0 0 0 4095 1 
i 1 125.3267 2.333333 70 95 0 0 0 0 4095 1 
i 11 125.3267 2.666667 46 63 0 0 0 0 4095 1 
i 11 125.3267 2.666667 50 63 0 0 0 0 4095 1 
i 4 125.3293 2.666667 55 63 0 0 0 0 4095 1 
i 1 125.3333 1 27 79 0 0 0 0 4095 1 
i 14 125.66 0.3333333 50 63 0 0 0 0 4095 1 
i 7 125.6627 0.3333333 50 63 0 0 0 0 4095 1 
i 14 125.9933 0.3333333 48 63 0 0 0 0 4095 1 
i 7 125.996 0.3333333 48 63 0 0 0 0 4095 1 
i 14 126.3267 0.3333333 50 63 0 0 0 0 4095 1 
i 7 126.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 1 126.3333 0.3333333 27 79 0 0 0 0 4095 1 
i 1 126.6667 0.6666667 32 79 0 0 0 0 4095 1 
i 14 126.9933 0.3333333 50 63 0 0 0 0 4095 1 
i 7 126.996 0.3333333 50 63 0 0 0 0 4095 1 
i 14 127.3267 0.6666667 48 63 0 0 0 0 4095 1 
i 7 127.3293 0.6666667 48 63 0 0 0 0 4095 1 
i 1 127.3333 0.6666667 32 79 0 0 0 0 4095 1 
i 1 127.66 0.1666667 68 95 0 0 0 0 4095 1 
i 1 127.8267 0.1666667 67 95 0 0 0 0 4095 1 
i 1 127.9933 1 68 95 0 0 0 0 4095 1 
i 11 127.9933 2.666667 44 63 0 0 0 0 4095 1 
i 11 127.9933 2.666667 48 63 0 0 0 0 4095 1 
i 4 127.996 1.333333 53 63 0 0 0 0 4095 1 
i 1 128 0.6666667 26 79 0 0 0 0 4095 1 
i 14 128.3267 0.3333333 50 63 0 0 0 0 4095 1 
i 7 128.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 14 128.66 0.3333333 48 63 0 0 0 0 4095 1 
i 7 128.6627 0.3333333 48 63 0 0 0 0 4095 1 
i 1 128.6667 0.3333333 26 79 0 0 0 0 4095 1 
i 1 128.9933 0.1666667 67 95 0 0 0 0 4095 1 
i 14 128.9933 0.3333333 50 63 0 0 0 0 4095 1 
i 7 128.996 0.3333333 50 63 0 0 0 0 4095 1 
i 1 129 1 38 79 0 0 0 0 4095 1 
i 1 129.16 0.1666667 65 95 0 0 0 0 4095 1 
i 1 129.3267 0.6666667 63 95 0 0 0 0 4095 1 
i 4 129.3293 1.333333 54 63 0 0 0 0 4095 1 
i 14 129.66 0.3333333 50 63 0 0 0 0 4095 1 
i 7 129.6627 0.3333333 50 63 0 0 0 0 4095 1 
i 1 129.9933 0.6666667 62 95 0 0 0 0 4095 1 
i 14 129.9933 0.6666667 48 63 0 0 0 0 4095 1 
i 7 129.996 0.6666667 48 63 0 0 0 0 4095 1 
i 1 130 0.6666667 26 79 0 0 0 0 4095 1 
i 1 130.66 2.666667 67 95 0 0 0 0 4095 1 
i 11 130.66 1.333333 44 63 0 0 0 0 4095 1 
i 4 130.6627 2.666667 55 63 0 0 0 0 4095 1 
i 1 130.6667 1 31 79 0 0 0 0 4095 1 
i 14 130.9933 0.3333333 48 63 0 0 0 0 4095 1 
i 7 130.996 0.3333333 48 63 0 0 0 0 4095 1 
i 14 131.3267 0.3333333 50 63 0 0 0 0 4095 1 
i 7 131.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 14 131.66 0.3333333 47 63 0 0 0 0 4095 1 
i 7 131.6627 0.3333333 47 63 0 0 0 0 4095 1 
i 1 131.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 11 131.9933 1.333333 43 63 0 0 0 0 4095 1 
i 14 131.9933 1 51 63 0 0 0 0 4095 1 
i 7 131.996 1 51 63 0 0 0 0 4095 1 
i 1 132 0.3333333 31 79 0 0 0 0 4095 1 
i 1 132.3333 0.3333333 29 79 0 0 0 0 4095 1 
i 1 132.6667 0.3333333 27 79 0 0 0 0 4095 1 
i 14 132.9933 0.3333333 50 63 0 0 0 0 4095 1 
i 7 132.996 0.3333333 50 63 0 0 0 0 4095 1 
i 1 133 0.3333333 26 79 0 0 0 0 4095 1 
i 1 133.3267 3.110667 67 95 0 0 0 0 4095 1 
i 14 133.3267 0.3333333 36 63 0 0 0 0 4095 1 
i 1 133.3333 1 24 79 0 0 0 0 4095 1 
i 11 133.66 0.3333333 43 63 0 0 0 0 4095 1 
i 14 133.66 0.3333333 43 63 0 0 0 0 4095 1 
i 11 133.9933 2 44 63 0 0 0 0 4095 1 
i 14 133.9933 2 43 63 0 0 0 0 4095 1 
i 7 134.3293 0.3333333 48 63 0 0 0 0 4095 1 
i 1 134.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 4 134.6627 0.3333333 50 63 0 0 0 0 4095 1 
i 7 134.6627 1.333333 50 63 0 0 0 0 4095 1 
i 1 134.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 134.996 0.3333333 51 63 0 0 0 0 4095 1 
i 4 135.3293 0.6666667 53 63 0 0 0 0 4095 1 
i 1 135.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 14 135.9933 0.3333333 36 63 0 0 0 0 4095 1 
i 1 136 1 24 79 0 0 0 0 4095 1 
i 11 136.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 14 136.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 1 136.4373 0.444 65 95 0 0 0 0 4095 1 
i 11 136.66 2 44 63 0 0 0 0 4095 1 
i 14 136.66 2 43 63 0 0 0 0 4095 1 
i 1 136.8813 0.444 63 95 0 0 0 0 4095 1 
i 7 136.996 0.3333333 48 63 0 0 0 0 4095 1 
i 1 137 0.3333333 36 79 0 0 0 0 4095 1 
i 1 137.3253 0.5 62 95 0 0 0 0 4095 1 
i 4 137.3293 0.3333333 50 63 0 0 0 0 4095 1 
i 7 137.3293 1.333333 50 63 0 0 0 0 4095 1 
i 1 137.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 137.6627 0.3333333 51 63 0 0 0 0 4095 1 
i 1 137.8253 0.1666667 60 95 0 0 0 0 4095 1 
i 1 137.992 0.1666667 62 95 0 0 0 0 4095 1 
i 4 137.996 0.6666667 53 63 0 0 0 0 4095 1 
i 1 138 0.6666667 24 79 0 0 0 0 4095 1 
i 1 138.1587 0.1666667 60 95 0 0 0 0 4095 1 
i 1 138.3253 0.1666667 59 95 0 0 0 0 4095 1 
i 1 138.492 0.1666667 60 95 0 0 0 0 4095 1 
i 1 138.6587 3 63 95 0 0 0 0 4095 1 
i 14 138.66 0.3333333 36 63 0 0 0 0 4095 1 
i 1 138.6667 1 24 79 0 0 0 0 4095 1 
i 11 138.9933 0.3333333 43 63 0 0 0 0 4095 1 
i 14 138.9933 0.3333333 43 63 0 0 0 0 4095 1 
i 11 139.3267 2 44 63 0 0 0 0 4095 1 
i 14 139.3267 2 43 63 0 0 0 0 4095 1 
i 7 139.6627 0.3333333 48 63 0 0 0 0 4095 1 
i 1 139.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 4 139.996 0.3333333 50 63 0 0 0 0 4095 1 
i 7 139.996 1.333333 50 63 0 0 0 0 4095 1 
i 1 140 0.6666667 36 79 0 0 0 0 4095 1 
i 4 140.3293 0.3333333 51 63 0 0 0 0 4095 1 
i 4 140.6627 0.6666667 53 63 0 0 0 0 4095 1 
i 1 140.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 14 141.3267 0.3333333 36 63 0 0 0 0 4095 1 
i 1 141.3333 1 24 79 0 0 0 0 4095 1 
i 1 141.6587 0.3333333 62 95 0 0 0 0 4095 1 
i 11 141.66 0.3333333 43 63 0 0 0 0 4095 1 
i 14 141.66 0.3333333 43 63 0 0 0 0 4095 1 
i 1 141.992 0.1666667 65 95 0 0 0 0 4095 1 
i 11 141.9933 2 44 63 0 0 0 0 4095 1 
i 14 141.9933 2 43 63 0 0 0 0 4095 1 
i 1 142.1587 0.1666667 63 95 0 0 0 0 4095 1 
i 1 142.3253 0.1666667 62 95 0 0 0 0 4095 1 
i 7 142.3293 0.3333333 48 63 0 0 0 0 4095 1 
i 1 142.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 1 142.492 0.1666667 60 95 0 0 0 0 4095 1 
i 1 142.6587 0.3333333 58 95 0 0 0 0 4095 1 
i 4 142.6627 0.3333333 50 63 0 0 0 0 4095 1 
i 7 142.6627 1.333333 50 63 0 0 0 0 4095 1 
i 1 142.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 1 142.992 0.6666667 56 95 0 0 0 0 4095 1 
i 4 142.996 0.3333333 51 63 0 0 0 0 4095 1 
i 4 143.3293 0.6666667 53 63 0 0 0 0 4095 1 
i 1 143.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 143.6587 0.1666667 55 95 0 0 0 0 4095 1 
i 1 143.8253 0.1666667 56 95 0 0 0 0 4095 1 
i 1 143.992 2 60 95 0 0 0 0 4095 1 
i 11 143.9933 2.666667 46 63 0 0 0 0 4095 1 
i 14 143.9933 0.6666667 41 63 0 0 0 0 4095 1 
i 4 143.996 0.6666667 53 63 0 0 0 0 4095 1 
i 7 143.996 2.666667 49 63 0 0 0 0 4095 1 
i 1 144 1 31 79 0 0 0 0 4095 1 
i 14 144.66 0.6666667 43 63 0 0 0 0 4095 1 
i 4 144.6627 0.6666667 55 63 0 0 0 0 4095 1 
i 1 145 0.3333333 31 79 0 0 0 0 4095 1 
i 14 145.3267 0.6666667 40 63 0 0 0 0 4095 1 
i 4 145.3293 0.6666667 52 63 0 0 0 0 4095 1 
i 1 145.3333 0.6666667 31 79 0 0 0 0 4095 1 
i 1 145.992 0.222 60 95 0 0 0 0 4095 1 
i 14 145.9933 0.6666667 41 63 0 0 0 0 4095 1 
i 4 145.996 0.6666667 53 63 0 0 0 0 4095 1 
i 1 146 0.6666667 31 79 0 0 0 0 4095 1 
i 1 146.214 0.222 58 95 0 0 0 0 4095 1 
i 1 146.436 0.222 56 95 0 0 0 0 4095 1 
i 1 146.658 0.1666667 56 95 0 0 0 0 4095 1 
i 11 146.66 2.666667 49 63 0 0 0 0 4095 1 
i 14 146.66 1.333333 44 63 0 0 0 0 4095 1 
i 4 146.6627 1.333333 56 63 0 0 0 0 4095 1 
i 7 146.6627 2.666667 52 63 0 0 0 0 4095 1 
i 1 146.6667 1 24 79 0 0 0 0 4095 1 
i 1 146.8247 1.166667 55 95 0 0 0 0 4095 1 
i 1 147.6667 0.3333333 24 79 0 0 0 0 4095 1 
i 1 147.9913 1 53 95 0 0 0 0 4095 1 
i 14 147.9933 1.333333 43 63 0 0 0 0 4095 1 
i 4 147.996 1.333333 55 63 0 0 0 0 4095 1 
i 1 148 0.6666667 24 79 0 0 0 0 4095 1 
i 1 148.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 148.9913 0.3333333 53 95 0 0 0 0 4095 1 
i 1 149.3247 0.3333333 58 95 0 0 0 0 4095 1 
i 11 149.3267 2.666667 44 63 0 0 0 0 4095 1 
i 14 149.3267 0.6666667 36 63 0 0 0 0 4095 1 
i 4 149.3293 0.6666667 48 63 0 0 0 0 4095 1 
i 7 149.3293 2.666667 48 63 0 0 0 0 4095 1 
i 1 149.3333 1 29 79 0 0 0 0 4095 1 
i 1 149.658 0.1666667 56 95 0 0 0 0 4095 1 
i 1 149.8247 0.1666667 55 95 0 0 0 0 4095 1 
i 1 149.9913 4.166667 56 95 0 0 0 0 4095 1 
i 14 149.9933 0.6666667 38 63 0 0 0 0 4095 1 
i 4 149.996 0.6666667 50 63 0 0 0 0 4095 1 
i 1 150.3333 0.3333333 34 79 0 0 0 0 4095 1 
i 14 150.66 0.6666667 39 63 0 0 0 0 4095 1 
i 4 150.6627 0.6666667 51 63 0 0 0 0 4095 1 
i 1 150.6667 0.6666667 34 79 0 0 0 0 4095 1 
i 14 151.3267 0.6666667 41 63 0 0 0 0 4095 1 
i 4 151.3293 0.6666667 53 63 0 0 0 0 4095 1 
i 1 151.3333 0.6666667 29 79 0 0 0 0 4095 1 
i 11 151.9933 2.666667 44 63 0 0 0 0 4095 1 
i 14 151.9933 1 43 63 0 0 0 0 4095 1 
i 4 151.996 1 55 63 0 0 0 0 4095 1 
i 7 151.996 2.666667 50 63 0 0 0 0 4095 1 
i 1 152 1 27 79 0 0 0 0 4095 1 
i 14 152.9933 0.1666667 41 63 0 0 0 0 4095 1 
i 4 152.996 0.1666667 53 63 0 0 0 0 4095 1 
i 1 153 0.3333333 36 79 0 0 0 0 4095 1 
i 14 153.16 0.1666667 39 63 0 0 0 0 4095 1 
i 4 153.1627 0.1666667 51 63 0 0 0 0 4095 1 
i 14 153.3267 1.333333 38 63 0 0 0 0 4095 1 
i 4 153.3293 1.333333 50 63 0 0 0 0 4095 1 
i 1 153.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 1 154 0.6666667 27 79 0 0 0 0 4095 1 
i 1 154.158 0.1666667 60 95 0 0 0 0 4095 1 
i 1 154.3247 0.1666667 65 95 0 0 0 0 4095 1 
i 1 154.4913 0.1666667 67 95 0 0 0 0 4095 1 
i 1 154.658 1 70 95 0 0 0 0 4095 1 
i 11 154.66 2.666667 44 63 0 0 0 0 4095 1 
i 14 154.66 0.6666667 41 63 0 0 0 0 4095 1 
i 4 154.6627 0.6666667 53 63 0 0 0 0 4095 1 
i 7 154.6627 2.666667 48 63 0 0 0 0 4095 1 
i 1 154.6667 1 26 79 0 0 0 0 4095 1 
i 14 155.3267 1 43 63 0 0 0 0 4095 1 
i 4 155.3293 1 55 63 0 0 0 0 4095 1 
i 1 155.658 0.1666667 68 95 0 0 0 0 4095 1 
i 1 155.6667 0.3333333 32 79 0 0 0 0 4095 1 
i 1 155.8247 0.1666667 67 95 0 0 0 0 4095 1 
i 1 155.9913 1.666667 68 95 0 0 0 0 4095 1 
i 1 156 0.6666667 32 79 0 0 0 0 4095 1 
i 14 156.3267 1 41 63 0 0 0 0 4095 1 
i 4 156.3293 1 53 63 0 0 0 0 4095 1 
i 1 156.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 11 157.3267 2.666667 50 63 0 0 0 0 4095 1 
i 14 157.3267 2.666667 44 63 0 0 0 0 4095 1 
i 4 157.3293 1 56 63 0 0 0 0 4095 1 
i 7 157.3293 2.666667 53 63 0 0 0 0 4095 1 
i 1 157.3333 1 23 79 0 0 0 0 4095 1 
i 1 157.658 0.3333333 67 95 0 0 0 0 4095 1 
i 1 157.9913 0.3333333 70 95 0 0 0 0 4095 1 
i 1 158.3247 0.3333333 68 95 0 0 0 0 4095 1 
i 1 158.3333 0.3333333 31 79 0 0 0 0 4095 1 
i 1 158.658 0.3333333 67 95 0 0 0 0 4095 1 
i 1 158.6667 0.6666667 31 79 0 0 0 0 4095 1 
i 1 158.9913 0.6666667 65 95 0 0 0 0 4095 1 
i 1 159.3333 0.6666667 23 79 0 0 0 0 4095 1 
i 1 159.658 0.1666667 63 95 0 0 0 0 4095 1 
i 1 159.8247 0.1666667 65 95 0 0 0 0 4095 1 
i 1 159.9913 4 67 95 0 0 0 0 4095 1 
i 11 159.9933 2.666667 48 63 0 0 0 0 4095 1 
i 14 159.9933 0.6666667 39 63 0 0 0 0 4095 1 
i 4 159.996 0.6666667 51 63 0 0 0 0 4095 1 
i 7 159.996 2.666667 55 63 0 0 0 0 4095 1 
i 1 160 1 24 79 0 0 0 0 4095 1 
i 14 160.66 1.333333 41 63 0 0 0 0 4095 1 
i 4 160.6627 1.333333 53 63 0 0 0 0 4095 1 
i 1 161 0.3333333 31 79 0 0 0 0 4095 1 
i 1 161.3333 0.6666667 31 79 0 0 0 0 4095 1 
i 14 161.9933 0.6666667 39 63 0 0 0 0 4095 1 
i 4 161.996 0.6666667 51 63 0 0 0 0 4095 1 
i 1 162 0.6666667 24 79 0 0 0 0 4095 1 
i 11 162.66 2.666667 48 63 0 0 0 0 4095 1 
i 14 162.66 2.666667 43 63 0 0 0 0 4095 1 
i 4 162.6627 1 55 63 0 0 0 0 4095 1 
i 7 162.6627 2.666667 51 63 0 0 0 0 4095 1 
i 1 162.6667 1 22 79 0 0 0 0 4095 1 
i 1 163.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 1 163.9913 0.3333333 65 95 0 0 0 0 4095 1 
i 1 164 0.6666667 31 79 0 0 0 0 4095 1 
i 1 164.3247 0.6666667 63 95 0 0 0 0 4095 1 
i 1 164.6667 0.6666667 22 79 0 0 0 0 4095 1 
i 1 164.9913 0.1666667 62 95 0 0 0 0 4095 1 
i 1 165.158 0.1666667 60 95 0 0 0 0 4095 1 
i 1 165.3247 2.333333 66 95 0 0 0 0 4095 1 
i 11 165.3267 2.666667 48 63 0 0 0 0 4095 1 
i 14 165.3267 0.6666667 38 63 0 0 0 0 4095 1 
i 4 165.3293 0.6666667 50 63 0 0 0 0 4095 1 
i 7 165.3293 2.666667 50 63 0 0 0 0 4095 1 
i 1 165.3333 1 21 79 0 0 0 0 4095 1 
i 14 165.9933 0.6666667 39 63 0 0 0 0 4095 1 
i 4 165.996 0.6666667 51 63 0 0 0 0 4095 1 
i 1 166.3333 0.3333333 30 79 0 0 0 0 4095 1 
i 14 166.66 0.6666667 39 63 0 0 0 0 4095 1 
i 4 166.6627 0.6666667 51 63 0 0 0 0 4095 1 
i 1 166.6667 0.6666667 30 79 0 0 0 0 4095 1 
i 14 167.3267 0.6666667 38 63 0 0 0 0 4095 1 
i 4 167.3293 0.6666667 50 63 0 0 0 0 4095 1 
i 1 167.3333 0.6666667 21 79 0 0 0 0 4095 1 
i 1 167.658 0.3333333 66 95 0 0 0 0 4095 1 
i 1 167.9913 2 67 95 0 0 0 0 4095 1 
i 11 167.9933 2.666667 47 63 0 0 0 0 4095 1 
i 14 167.9933 0.6666667 38 63 0 0 0 0 4095 1 
i 4 167.996 0.6666667 50 63 0 0 0 0 4095 1 
i 7 167.996 2.666667 53 63 0 0 0 0 4095 1 
i 1 168 1 19 79 0 0 0 0 4095 1 
i 14 168.66 1.333333 39 63 0 0 0 0 4095 1 
i 4 168.6627 1.333333 51 63 0 0 0 0 4095 1 
i 1 169 0.3333333 29 79 0 0 0 0 4095 1 
i 1 169.3333 0.6666667 29 79 0 0 0 0 4095 1 
i 1 169.9913 0.6666667 67 95 0 0 0 0 4095 1 
i 14 169.9933 0.6666667 38 63 0 0 0 0 4095 1 
i 4 169.996 0.6666667 50 63 0 0 0 0 4095 1 
i 1 170 0.6666667 19 79 0 0 0 0 4095 1 
i 1 170.658 2.666667 60 95 0 0 0 0 4095 1 
i 11 170.66 5.333333 48 63 0 0 0 0 4095 1 
i 14 170.66 1 43 63 0 0 0 0 4095 1 
i 4 170.6627 1 55 63 0 0 0 0 4095 1 
i 7 170.6627 2.666667 51 63 0 0 0 0 4095 1 
i 1 170.6667 1 24 79 0 0 0 0 4095 1 
i 14 171.66 0.3333333 44 63 0 0 0 0 4095 1 
i 4 171.6627 0.3333333 56 63 0 0 0 0 4095 1 
i 1 171.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 14 171.9933 0.6666667 44 63 0 0 0 0 4095 1 
i 4 171.996 0.6666667 56 63 0 0 0 0 4095 1 
i 1 172 0.6666667 36 79 0 0 0 0 4095 1 
i 14 172.66 0.6666667 43 63 0 0 0 0 4095 1 
i 4 172.6627 0.6666667 55 63 0 0 0 0 4095 1 
i 1 172.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 173.3247 2.666667 60 95 0 0 0 0 4095 1 
i 1 173.3247 2.666667 72 95 0 0 0 0 4095 1 
i 14 173.3267 1 46 63 0 0 0 0 4095 1 
i 4 173.3293 1 58 63 0 0 0 0 4095 1 
i 7 173.3293 2.666667 51 63 0 0 0 0 4095 1 
i 1 173.3333 1 24 79 0 0 0 0 4095 1 
i 14 174.3267 0.3333333 44 63 0 0 0 0 4095 1 
i 4 174.3293 0.3333333 56 63 0 0 0 0 4095 1 
i 1 174.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 14 174.66 0.6666667 44 63 0 0 0 0 4095 1 
i 4 174.6627 0.6666667 56 63 0 0 0 0 4095 1 
i 1 174.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 14 175.3267 0.6666667 43 63 0 0 0 0 4095 1 
i 4 175.3293 0.6666667 55 63 0 0 0 0 4095 1 
i 1 175.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 175.9913 2 72 111 0 0 0 0 4095 1 
i 11 175.9933 2 60 63 0 0 0 0 4095 1 
i 14 175.9933 2 44 63 0 0 0 0 4095 1 
i 4 175.996 2 63 63 0 0 0 0 4095 1 
i 7 175.996 2 60 63 0 0 0 0 4095 1 
i 1 176 1 29 79 0 0 0 0 4095 1 
i 1 177 0.3333333 29 79 0 0 0 0 4095 1 
i 1 177.3333 0.6666667 29 79 0 0 0 0 4095 1 
i 1 177.9913 0.3333333 68 111 0 0 0 0 4095 1 
i 11 177.9933 0.3333333 56 63 0 0 0 0 4095 1 
i 14 177.9933 0.3333333 41 63 0 0 0 0 4095 1 
i 4 177.996 0.3333333 60 63 0 0 0 0 4095 1 
i 7 177.996 0.3333333 56 63 0 0 0 0 4095 1 
i 1 178 0.6666667 29 79 0 0 0 0 4095 1 
i 1 178.3247 0.3333333 70 111 0 0 0 0 4095 1 
i 11 178.3267 0.3333333 58 63 0 0 0 0 4095 1 
i 14 178.3267 0.3333333 43 63 0 0 0 0 4095 1 
i 4 178.3293 0.3333333 62 63 0 0 0 0 4095 1 
i 7 178.3293 0.3333333 58 63 0 0 0 0 4095 1 
i 1 178.658 0.444 72 111 0 0 0 0 4095 1 
i 11 178.66 0.444 60 63 0 0 0 0 4095 1 
i 14 178.66 0.444 44 63 0 0 0 0 4095 1 
i 4 178.6627 0.444 63 63 0 0 0 0 4095 1 
i 7 178.6627 0.444 60 63 0 0 0 0 4095 1 
i 1 178.6667 1 34 79 0 0 0 0 4095 1 
i 1 179.102 0.444 70 111 0 0 0 0 4095 1 
i 11 179.104 0.444 58 63 0 0 0 0 4095 1 
i 14 179.104 0.444 44 63 0 0 0 0 4095 1 
i 4 179.1067 0.444 62 63 0 0 0 0 4095 1 
i 7 179.1067 0.444 58 63 0 0 0 0 4095 1 
i 1 179.546 0.444 74 111 0 0 0 0 4095 1 
i 11 179.548 0.444 62 63 0 0 0 0 4095 1 
i 14 179.548 0.444 44 63 0 0 0 0 4095 1 
i 4 179.5507 0.444 65 63 0 0 0 0 4095 1 
i 7 179.5507 0.444 62 63 0 0 0 0 4095 1 
i 1 179.6667 0.3333333 34 79 0 0 0 0 4095 1 
i 1 179.99 0.6666667 67 111 0 0 0 0 4095 1 
i 11 179.992 0.6666667 50 63 0 0 0 0 4095 1 
i 14 179.992 0.6666667 46 63 0 0 0 0 4095 1 
i 4 179.9947 0.6666667 58 63 0 0 0 0 4095 1 
i 7 179.9947 0.6666667 55 63 0 0 0 0 4095 1 
i 1 180 0.6666667 34 79 0 0 0 0 4095 1 
i 1 180.6567 0.6666667 65 111 0 0 0 0 4095 1 
i 11 180.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 14 180.6587 0.6666667 46 63 0 0 0 0 4095 1 
i 4 180.6613 0.6666667 56 63 0 0 0 0 4095 1 
i 7 180.6613 0.6666667 53 63 0 0 0 0 4095 1 
i 1 180.6667 0.6666667 34 79 0 0 0 0 4095 1 
i 1 181.3233 0.6666667 63 111 0 0 0 0 4095 1 
i 11 181.3253 0.6666667 51 63 0 0 0 0 4095 1 
i 14 181.3253 0.6666667 46 63 0 0 0 0 4095 1 
i 4 181.328 0.6666667 55 63 0 0 0 0 4095 1 
i 7 181.328 0.6666667 51 63 0 0 0 0 4095 1 
i 1 181.3333 1 27 79 0 0 0 0 4095 1 
i 1 181.99 2.444 70 111 0 0 0 0 4095 1 
i 11 181.992 2.444 58 63 0 0 0 0 4095 1 
i 14 181.992 2 43 63 0 0 0 0 4095 1 
i 4 181.9947 2.444 62 63 0 0 0 0 4095 1 
i 7 181.9947 2.444 58 63 0 0 0 0 4095 1 
i 1 182.3333 0.3333333 27 79 0 0 0 0 4095 1 
i 1 182.6667 0.6666667 27 79 0 0 0 0 4095 1 
i 1 183.3333 0.6666667 27 79 0 0 0 0 4095 1 
i 14 183.992 0.444 44 63 0 0 0 0 4095 1 
i 1 184 1 20 79 0 0 0 0 4095 1 
i 1 184.434 0.444 68 111 0 0 0 0 4095 1 
i 11 184.436 0.444 56 63 0 0 0 0 4095 1 
i 14 184.436 0.444 44 63 0 0 0 0 4095 1 
i 4 184.4387 0.444 60 63 0 0 0 0 4095 1 
i 7 184.4387 0.444 56 63 0 0 0 0 4095 1 
i 1 184.878 0.444 72 111 0 0 0 0 4095 1 
i 11 184.88 0.444 60 63 0 0 0 0 4095 1 
i 14 184.88 0.444 44 63 0 0 0 0 4095 1 
i 4 184.8827 0.444 63 63 0 0 0 0 4095 1 
i 7 184.8827 0.444 60 63 0 0 0 0 4095 1 
i 1 185 0.3333333 20 79 0 0 0 0 4095 1 
i 1 185.322 0.6666667 65 111 0 0 0 0 4095 1 
i 11 185.324 0.6666667 48 63 0 0 0 0 4095 1 
i 14 185.324 0.6666667 41 63 0 0 0 0 4095 1 
i 4 185.3267 0.6666667 56 63 0 0 0 0 4095 1 
i 7 185.3267 0.6666667 53 63 0 0 0 0 4095 1 
i 1 185.3333 0.6666667 20 79 0 0 0 0 4095 1 
i 1 185.9887 0.6666667 63 111 0 0 0 0 4095 1 
i 11 185.9907 0.6666667 51 63 0 0 0 0 4095 1 
i 14 185.9907 0.6666667 48 63 0 0 0 0 4095 1 
i 4 185.9933 0.6666667 55 63 0 0 0 0 4095 1 
i 7 185.9933 0.6666667 51 63 0 0 0 0 4095 1 
i 1 186 0.6666667 20 79 0 0 0 0 4095 1 
i 1 186.6553 0.6666667 62 111 0 0 0 0 4095 1 
i 11 186.6573 0.6666667 50 63 0 0 0 0 4095 1 
i 14 186.6573 0.6666667 44 63 0 0 0 0 4095 1 
i 4 186.66 0.6666667 53 63 0 0 0 0 4095 1 
i 7 186.66 0.6666667 50 63 0 0 0 0 4095 1 
i 1 186.6667 1 26 79 0 0 0 0 4095 1 
i 1 187.322 2.444 68 111 0 0 0 0 4095 1 
i 11 187.324 2.444 56 63 0 0 0 0 4095 1 
i 14 187.324 2.444 41 63 0 0 0 0 4095 1 
i 4 187.3267 2.444 60 63 0 0 0 0 4095 1 
i 7 187.3267 2.444 56 63 0 0 0 0 4095 1 
i 1 187.6667 0.3333333 26 79 0 0 0 0 4095 1 
i 1 188 0.6666667 26 79 0 0 0 0 4095 1 
i 1 188.6667 0.6666667 26 79 0 0 0 0 4095 1 
i 1 189.3333 1 19 79 0 0 0 0 4095 1 
i 1 189.766 0.444 67 111 0 0 0 0 4095 1 
i 11 189.768 0.444 55 63 0 0 0 0 4095 1 
i 14 189.768 0.444 41 63 0 0 0 0 4095 1 
i 4 189.7707 0.444 59 63 0 0 0 0 4095 1 
i 7 189.7707 0.444 55 63 0 0 0 0 4095 1 
i 1 190.21 0.444 71 111 0 0 0 0 4095 1 
i 11 190.212 0.444 55 63 0 0 0 0 4095 1 
i 14 190.212 0.444 41 63 0 0 0 0 4095 1 
i 4 190.2147 0.444 62 63 0 0 0 0 4095 1 
i 7 190.2147 0.444 59 63 0 0 0 0 4095 1 
i 1 190.3333 0.3333333 19 79 0 0 0 0 4095 1 
i 1 190.654 0.6666667 63 111 0 0 0 0 4095 1 
i 11 190.656 0.6666667 47 63 0 0 0 0 4095 1 
i 14 190.656 0.6666667 44 63 0 0 0 0 4095 1 
i 4 190.6587 0.6666667 55 63 0 0 0 0 4095 1 
i 7 190.6587 0.6666667 51 63 0 0 0 0 4095 1 
i 1 190.6667 0.6666667 19 79 0 0 0 0 4095 1 
i 1 191.3207 0.6666667 62 111 0 0 0 0 4095 1 
i 11 191.3227 0.6666667 47 63 0 0 0 0 4095 1 
i 14 191.3227 0.6666667 44 63 0 0 0 0 4095 1 
i 4 191.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 7 191.3253 0.6666667 50 63 0 0 0 0 4095 1 
i 1 191.3333 0.6666667 19 79 0 0 0 0 4095 1 
i 1 191.9873 1 60 111 0 0 0 0 4095 1 
i 11 191.9893 1 43 63 0 0 0 0 4095 1 
i 14 191.9893 1 39 63 0 0 0 0 4095 1 
i 4 191.992 1 51 63 0 0 0 0 4095 1 
i 7 191.992 1 48 63 0 0 0 0 4095 1 
i 1 192 1 24 79 0 0 0 0 4095 1 
i 1 192.9873 1 60 111 0 0 0 0 4095 1 
i 11 192.9893 1 44 63 0 0 0 0 4095 1 
i 14 192.9893 1 41 63 0 0 0 0 4095 1 
i 4 192.992 1 53 63 0 0 0 0 4095 1 
i 7 192.992 1 50 63 0 0 0 0 4095 1 
i 1 193 0.3333333 24 79 0 0 0 0 4095 1 
i 1 193.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 193.9873 0.6666667 60 111 0 0 0 0 4095 1 
i 11 193.9893 0.6666667 46 63 0 0 0 0 4095 1 
i 14 193.9893 0.6666667 36 63 0 0 0 0 4095 1 
i 4 193.992 0.6666667 55 63 0 0 0 0 4095 1 
i 7 193.992 0.6666667 51 63 0 0 0 0 4095 1 
i 1 194 0.6666667 27 79 0 0 0 0 4095 1 
i 1 194.654 2 61 111 0 0 0 0 4095 1 
i 11 194.656 2.666667 46 63 0 0 0 0 4095 1 
i 14 194.656 2.666667 43 63 0 0 0 0 4095 1 
i 4 194.6587 2.666667 55 63 0 0 0 0 4095 1 
i 7 194.6587 2 49 63 0 0 0 0 4095 1 
i 1 194.6667 1 28 79 0 0 0 0 4095 1 
i 1 195.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 1 196 0.6666667 36 79 0 0 0 0 4095 1 
i 1 196.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 197.3207 0.3333333 56 111 0 0 0 0 4095 1 
i 11 197.3227 2 51 63 0 0 0 0 4095 1 
i 14 197.3227 2 48 63 0 0 0 0 4095 1 
i 1 197.3333 1 29 79 0 0 0 0 4095 1 
i 1 197.654 0.6666667 63 111 0 0 0 0 4095 1 
i 1 198.3207 0.3333333 56 111 0 0 0 0 4095 1 
i 1 198.3333 0.3333333 29 79 0 0 0 0 4095 1 
i 1 198.654 1.333333 63 111 0 0 0 0 4095 1 
i 1 198.6667 0.6666667 29 79 0 0 0 0 4095 1 
i 11 199.3227 0.3333333 48 63 0 0 0 0 4095 1 
i 14 199.3227 0.3333333 44 63 0 0 0 0 4095 1 
i 1 199.3333 0.6666667 29 79 0 0 0 0 4095 1 
i 11 199.656 0.3333333 50 63 0 0 0 0 4095 1 
i 14 199.656 0.3333333 46 63 0 0 0 0 4095 1 
i 1 199.9873 0.3333333 56 111 0 0 0 0 4095 1 
i 11 199.9893 0.444 51 63 0 0 0 0 4095 1 
i 14 199.9893 0.444 48 63 0 0 0 0 4095 1 
i 1 200 1 34 79 0 0 0 0 4095 1 
i 1 200.3207 0.6666667 65 111 0 0 0 0 4095 1 
i 11 200.4333 0.444 50 63 0 0 0 0 4095 1 
i 14 200.4333 0.444 46 63 0 0 0 0 4095 1 
i 11 200.8773 0.444 53 63 0 0 0 0 4095 1 
i 14 200.8773 0.444 50 63 0 0 0 0 4095 1 
i 1 200.9873 0.3333333 56 111 0 0 0 0 4095 1 
i 1 201 0.3333333 34 79 0 0 0 0 4095 1 
i 1 201.3207 1.333333 62 111 0 0 0 0 4095 1 
i 11 201.3213 0.6666667 46 63 0 0 0 0 4095 1 
i 14 201.3213 0.6666667 43 63 0 0 0 0 4095 1 
i 1 201.3333 0.6666667 34 79 0 0 0 0 4095 1 
i 11 201.988 0.6666667 44 63 0 0 0 0 4095 1 
i 14 201.988 0.6666667 41 63 0 0 0 0 4095 1 
i 1 202 0.6666667 34 79 0 0 0 0 4095 1 
i 1 202.654 0.3333333 51 111 0 0 0 0 4095 1 
i 11 202.6547 0.6666667 43 63 0 0 0 0 4095 1 
i 14 202.6547 0.6666667 39 63 0 0 0 0 4095 1 
i 1 202.6667 1 27 79 0 0 0 0 4095 1 
i 1 202.9873 0.6666667 58 111 0 0 0 0 4095 1 
i 11 203.3213 2.444 50 63 0 0 0 0 4095 1 
i 14 203.3213 2.444 46 63 0 0 0 0 4095 1 
i 1 203.654 0.3333333 55 111 0 0 0 0 4095 1 
i 1 203.6667 0.3333333 27 79 0 0 0 0 4095 1 
i 1 203.9873 1.333333 62 111 0 0 0 0 4095 1 
i 1 204 0.6666667 27 79 0 0 0 0 4095 1 
i 1 204.6667 0.6666667 27 79 0 0 0 0 4095 1 
i 1 205.3207 0.3333333 56 111 0 0 0 0 4095 1 
i 1 205.3333 1 32 79 0 0 0 0 4095 1 
i 1 205.654 0.6666667 51 111 0 0 0 0 4095 1 
i 11 205.7653 0.444 48 63 0 0 0 0 4095 1 
i 14 205.7653 0.444 44 63 0 0 0 0 4095 1 
i 11 206.2093 0.444 51 63 0 0 0 0 4095 1 
i 14 206.2093 0.444 48 63 0 0 0 0 4095 1 
i 1 206.3207 0.3333333 60 111 0 0 0 0 4095 1 
i 1 206.3333 0.3333333 32 79 0 0 0 0 4095 1 
i 11 206.6533 0.6666667 56 63 0 0 0 0 4095 1 
i 14 206.6533 0.6666667 41 63 0 0 0 0 4095 1 
i 1 206.654 0.6666667 53 111 0 0 0 0 4095 1 
i 1 206.6667 0.6666667 32 79 0 0 0 0 4095 1 
i 11 207.32 0.6666667 55 63 0 0 0 0 4095 1 
i 14 207.32 0.6666667 39 63 0 0 0 0 4095 1 
i 1 207.3207 0.6666667 51 111 0 0 0 0 4095 1 
i 1 207.3333 0.6666667 32 79 0 0 0 0 4095 1 
i 11 207.9867 2.666667 54 63 0 0 0 0 4095 1 
i 14 207.9867 2.666667 38 63 0 0 0 0 4095 1 
i 1 207.9873 2.666667 48 79 0 0 0 0 4095 1 
i 1 208 1 26 79 0 0 0 0 4095 1 
i 1 209 0.3333333 26 79 0 0 0 0 4095 1 
i 1 209.3333 0.6666667 26 79 0 0 0 0 4095 1 
i 1 210 0.6666667 26 79 0 0 0 0 4095 1 
i 11 210.6533 2.666667 47 63 0 0 0 0 4095 1 
i 11 210.6533 2.666667 55 63 0 0 0 0 4095 1 
i 14 210.6533 2.666667 41 63 0 0 0 0 4095 1 
i 1 210.654 2.333333 55 79 0 0 0 0 4095 1 
i 1 210.6667 1 31 79 0 0 0 0 4095 1 
i 1 211.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 1 212 0.6666667 31 79 0 0 0 0 4095 1 
i 1 212.6667 0.6666667 31 79 0 0 0 0 4095 1 
i 1 212.9873 0.3333333 55 79 0 0 0 0 4095 1 
i 11 213.32 5.333333 48 63 0 0 0 0 4095 1 
i 14 213.32 1 43 63 0 0 0 0 4095 1 
i 1 213.3207 4.666667 60 79 0 0 0 0 4095 1 
i 4 213.3253 5.333333 60 63 0 0 0 0 4095 1 
i 7 213.3253 5.333333 51 63 0 0 0 0 4095 1 
i 1 213.3333 1 24 79 0 0 0 0 4095 1 
i 14 214.32 0.3333333 44 63 0 0 0 0 4095 1 
i 1 214.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 14 214.6533 0.6666667 44 63 0 0 0 0 4095 1 
i 1 214.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 14 215.32 0.6666667 43 63 0 0 0 0 4095 1 
i 1 215.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 14 215.9867 1 46 63 0 0 0 0 4095 1 
i 1 216 1 24 79 0 0 0 0 4095 1 
i 14 216.9867 0.3333333 44 63 0 0 0 0 4095 1 
i 1 217 0.3333333 36 79 0 0 0 0 4095 1 
i 14 217.32 0.6666667 44 63 0 0 0 0 4095 1 
i 1 217.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 14 217.9867 0.6666667 43 63 0 0 0 0 4095 1 
i 1 218 0.6666667 24 79 0 0 0 0 4095 1 
i 14 218.6533 0.3333333 36 63 0 0 0 0 4095 1 
i 1 218.654 3 67 79 0 0 0 0 4095 1 
i 1 218.6667 1 24 79 0 0 0 0 4095 1 
i 11 218.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 14 218.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 11 219.32 2 44 63 0 0 0 0 4095 1 
i 14 219.32 2 43 63 0 0 0 0 4095 1 
i 7 219.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 1 219.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 4 219.992 0.3333333 50 63 0 0 0 0 4095 1 
i 7 219.992 1.333333 50 63 0 0 0 0 4095 1 
i 1 220 0.6666667 36 79 0 0 0 0 4095 1 
i 4 220.3253 0.3333333 51 63 0 0 0 0 4095 1 
i 4 220.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 1 220.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 14 221.32 0.3333333 36 63 0 0 0 0 4095 1 
i 1 221.3333 1 24 79 0 0 0 0 4095 1 
i 11 221.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 14 221.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 1 221.654 0.3333333 65 79 0 0 0 0 4095 1 
i 11 221.9867 2 44 63 0 0 0 0 4095 1 
i 14 221.9867 2 43 63 0 0 0 0 4095 1 
i 1 221.9873 0.222 67 79 0 0 0 0 4095 1 
i 1 222.2093 0.222 65 79 0 0 0 0 4095 1 
i 7 222.3253 0.3333333 48 63 0 0 0 0 4095 1 
i 1 222.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 1 222.4313 0.222 63 79 0 0 0 0 4095 1 
i 1 222.6533 1 62 79 0 0 0 0 4095 1 
i 4 222.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 7 222.6587 1.333333 50 63 0 0 0 0 4095 1 
i 1 222.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 222.992 0.3333333 51 63 0 0 0 0 4095 1 
i 4 223.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 1 223.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 223.6533 0.3333333 60 79 0 0 0 0 4095 1 
i 1 223.9867 3 63 79 0 0 0 0 4095 1 
i 14 223.9867 0.3333333 36 63 0 0 0 0 4095 1 
i 1 224 1 24 79 0 0 0 0 4095 1 
i 11 224.32 0.3333333 43 63 0 0 0 0 4095 1 
i 14 224.32 0.3333333 43 63 0 0 0 0 4095 1 
i 11 224.6533 2 44 63 0 0 0 0 4095 1 
i 14 224.6533 2 43 63 0 0 0 0 4095 1 
i 7 224.992 0.3333333 48 63 0 0 0 0 4095 1 
i 1 225 0.3333333 36 79 0 0 0 0 4095 1 
i 4 225.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 7 225.3253 1.333333 50 63 0 0 0 0 4095 1 
i 1 225.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 225.6587 0.3333333 51 63 0 0 0 0 4095 1 
i 4 225.992 0.6666667 53 63 0 0 0 0 4095 1 
i 1 226 0.6666667 24 79 0 0 0 0 4095 1 
i 14 226.6533 0.3333333 36 63 0 0 0 0 4095 1 
i 1 226.6667 1 24 79 0 0 0 0 4095 1 
i 1 226.9867 0.3333333 62 79 0 0 0 0 4095 1 
i 11 226.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 14 226.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 1 227.32 0.3333333 65 79 0 0 0 0 4095 1 
i 11 227.32 2 44 63 0 0 0 0 4095 1 
i 14 227.32 2 43 63 0 0 0 0 4095 1 
i 1 227.6533 0.3333333 63 79 0 0 0 0 4095 1 
i 7 227.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 1 227.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 1 227.9867 0.3333333 62 79 0 0 0 0 4095 1 
i 4 227.992 0.3333333 50 63 0 0 0 0 4095 1 
i 7 227.992 1.333333 50 63 0 0 0 0 4095 1 
i 1 228 0.6666667 36 79 0 0 0 0 4095 1 
i 1 228.32 0.6666667 60 79 0 0 0 0 4095 1 
i 4 228.3253 0.3333333 51 63 0 0 0 0 4095 1 
i 4 228.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 1 228.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 228.9867 0.1666667 58 79 0 0 0 0 4095 1 
i 1 229.1533 0.1666667 56 79 0 0 0 0 4095 1 
i 1 229.32 2 60 79 0 0 0 0 4095 1 
i 11 229.32 2.666667 44 63 0 0 0 0 4095 1 
i 11 229.32 2.666667 48 63 0 0 0 0 4095 1 
i 4 229.3253 2.666667 56 63 0 0 0 0 4095 1 
i 1 229.3333 1 29 79 0 0 0 0 4095 1 
i 14 229.6533 0.3333333 51 63 0 0 0 0 4095 1 
i 7 229.6587 0.3333333 51 63 0 0 0 0 4095 1 
i 14 229.9867 0.3333333 50 63 0 0 0 0 4095 1 
i 7 229.992 0.3333333 50 63 0 0 0 0 4095 1 
i 14 230.32 0.3333333 51 63 0 0 0 0 4095 1 
i 7 230.3253 0.3333333 51 63 0 0 0 0 4095 1 
i 1 230.3333 0.3333333 29 79 0 0 0 0 4095 1 
i 1 230.6667 0.6666667 34 79 0 0 0 0 4095 1 
i 14 230.9867 0.3333333 51 63 0 0 0 0 4095 1 
i 7 230.992 0.3333333 51 63 0 0 0 0 4095 1 
i 1 231.32 0.1666667 58 79 0 0 0 0 4095 1 
i 14 231.32 0.6666667 50 63 0 0 0 0 4095 1 
i 7 231.3253 0.6666667 50 63 0 0 0 0 4095 1 
i 1 231.3333 0.6666667 34 79 0 0 0 0 4095 1 
i 1 231.4867 0.1666667 60 79 0 0 0 0 4095 1 
i 1 231.6533 0.1666667 58 79 0 0 0 0 4095 1 
i 1 231.82 0.1666667 56 79 0 0 0 0 4095 1 
i 1 231.9867 1.333333 58 79 0 0 0 0 4095 1 
i 11 231.9867 2.666667 46 63 0 0 0 0 4095 1 
i 11 231.9867 2.666667 50 63 0 0 0 0 4095 1 
i 4 231.992 2.666667 55 63 0 0 0 0 4095 1 
i 1 232 1 27 79 0 0 0 0 4095 1 
i 14 232.32 0.3333333 50 63 0 0 0 0 4095 1 
i 7 232.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 14 232.6533 0.3333333 48 63 0 0 0 0 4095 1 
i 7 232.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 14 232.9867 0.3333333 50 63 0 0 0 0 4095 1 
i 7 232.992 0.3333333 50 63 0 0 0 0 4095 1 
i 1 233 0.3333333 27 79 0 0 0 0 4095 1 
i 1 233.32 0.3333333 60 79 0 0 0 0 4095 1 
i 1 233.3333 0.6666667 32 79 0 0 0 0 4095 1 
i 1 233.6533 0.6666667 58 79 0 0 0 0 4095 1 
i 14 233.6533 0.3333333 50 63 0 0 0 0 4095 1 
i 7 233.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 14 233.9867 0.6666667 48 63 0 0 0 0 4095 1 
i 7 233.992 0.6666667 48 63 0 0 0 0 4095 1 
i 1 234 0.6666667 32 79 0 0 0 0 4095 1 
i 1 234.32 0.1666667 56 79 0 0 0 0 4095 1 
i 1 234.4867 0.1666667 55 79 0 0 0 0 4095 1 
i 1 234.6533 1 56 79 0 0 0 0 4095 1 
i 11 234.6533 2.666667 44 63 0 0 0 0 4095 1 
i 11 234.6533 2.666667 48 63 0 0 0 0 4095 1 
i 4 234.6587 1.333333 53 63 0 0 0 0 4095 1 
i 1 234.6667 0.6666667 26 79 0 0 0 0 4095 1 
i 14 234.9867 0.3333333 50 63 0 0 0 0 4095 1 
i 7 234.992 0.3333333 50 63 0 0 0 0 4095 1 
i 14 235.32 0.3333333 48 63 0 0 0 0 4095 1 
i 7 235.3253 0.3333333 48 63 0 0 0 0 4095 1 
i 1 235.3333 0.3333333 26 79 0 0 0 0 4095 1 
i 1 235.6533 0.1666667 55 79 0 0 0 0 4095 1 
i 14 235.6533 0.3333333 50 63 0 0 0 0 4095 1 
i 7 235.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 1 235.6667 1 38 79 0 0 0 0 4095 1 
i 1 235.82 0.1666667 53 79 0 0 0 0 4095 1 
i 1 235.9867 0.6666667 51 79 0 0 0 0 4095 1 
i 4 235.992 1.333333 54 63 0 0 0 0 4095 1 
i 14 236.32 0.3333333 50 63 0 0 0 0 4095 1 
i 7 236.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 1 236.6533 0.6666667 50 79 0 0 0 0 4095 1 
i 14 236.6533 0.6666667 48 63 0 0 0 0 4095 1 
i 7 236.6587 0.6666667 48 63 0 0 0 0 4095 1 
i 1 236.6667 0.6666667 26 79 0 0 0 0 4095 1 
i 1 237.32 2.666667 55 79 0 0 0 0 4095 1 
i 11 237.32 1.333333 44 63 0 0 0 0 4095 1 
i 4 237.3253 2.666667 55 63 0 0 0 0 4095 1 
i 1 237.3333 1 31 79 0 0 0 0 4095 1 
i 14 237.6533 0.3333333 48 63 0 0 0 0 4095 1 
i 7 237.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 14 237.9867 0.3333333 50 63 0 0 0 0 4095 1 
i 7 237.992 0.3333333 50 63 0 0 0 0 4095 1 
i 14 238.32 0.3333333 47 63 0 0 0 0 4095 1 
i 7 238.3253 0.3333333 47 63 0 0 0 0 4095 1 
i 1 238.3333 0.3333333 31 79 0 0 0 0 4095 1 
i 11 238.6533 1.333333 43 63 0 0 0 0 4095 1 
i 14 238.6533 1 51 63 0 0 0 0 4095 1 
i 7 238.6587 1 51 63 0 0 0 0 4095 1 
i 1 238.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 1 239 0.3333333 29 79 0 0 0 0 4095 1 
i 1 239.3333 0.3333333 27 79 0 0 0 0 4095 1 
i 14 239.6533 0.3333333 50 63 0 0 0 0 4095 1 
i 7 239.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 1 239.6667 0.3333333 26 79 0 0 0 0 4095 1 
i 1 239.9867 3.666667 67 79 0 0 0 0 4095 1 
i 14 239.9867 0.3333333 36 63 0 0 0 0 4095 1 
i 1 240 1 24 79 0 0 0 0 4095 1 
i 11 240.32 0.3333333 43 63 0 0 0 0 4095 1 
i 14 240.32 0.3333333 43 63 0 0 0 0 4095 1 
i 11 240.6533 2 44 63 0 0 0 0 4095 1 
i 14 240.6533 2 43 63 0 0 0 0 4095 1 
i 7 240.992 0.3333333 48 63 0 0 0 0 4095 1 
i 1 241 0.3333333 36 79 0 0 0 0 4095 1 
i 4 241.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 7 241.3253 1.333333 50 63 0 0 0 0 4095 1 
i 1 241.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 241.6587 0.3333333 51 63 0 0 0 0 4095 1 
i 4 241.992 0.6666667 53 63 0 0 0 0 4095 1 
i 1 242 0.6666667 24 79 0 0 0 0 4095 1 
i 14 242.6533 0.3333333 36 63 0 0 0 0 4095 1 
i 1 242.6667 1 24 79 0 0 0 0 4095 1 
i 11 242.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 14 242.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 11 243.32 2 44 63 0 0 0 0 4095 1 
i 14 243.32 2 43 63 0 0 0 0 4095 1 
i 1 243.6533 0.1666667 65 79 0 0 0 0 4095 1 
i 7 243.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 1 243.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 1 243.82 0.1666667 63 79 0 0 0 0 4095 1 
i 1 243.9867 1 62 79 0 0 0 0 4095 1 
i 4 243.992 0.3333333 50 63 0 0 0 0 4095 1 
i 7 243.992 1.333333 50 63 0 0 0 0 4095 1 
i 1 244 0.6666667 36 79 0 0 0 0 4095 1 
i 4 244.3253 0.3333333 51 63 0 0 0 0 4095 1 
i 4 244.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 1 244.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 244.9867 0.3333333 60 79 0 0 0 0 4095 1 
i 1 245.32 3.666667 63 79 0 0 0 0 4095 1 
i 14 245.32 0.3333333 36 63 0 0 0 0 4095 1 
i 1 245.3333 1 24 79 0 0 0 0 4095 1 
i 11 245.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 14 245.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 11 245.9867 2 44 63 0 0 0 0 4095 1 
i 14 245.9867 2 43 63 0 0 0 0 4095 1 
i 7 246.3253 0.3333333 48 63 0 0 0 0 4095 1 
i 1 246.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 4 246.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 7 246.6587 1.333333 50 63 0 0 0 0 4095 1 
i 1 246.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 246.992 0.3333333 51 63 0 0 0 0 4095 1 
i 4 247.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 1 247.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 14 247.9867 0.3333333 36 63 0 0 0 0 4095 1 
i 1 248 1 24 79 0 0 0 0 4095 1 
i 11 248.32 0.3333333 43 63 0 0 0 0 4095 1 
i 14 248.32 0.3333333 43 63 0 0 0 0 4095 1 
i 11 248.6533 2 44 63 0 0 0 0 4095 1 
i 14 248.6533 2 43 63 0 0 0 0 4095 1 
i 1 248.9867 0.1666667 62 79 0 0 0 0 4095 1 
i 7 248.992 0.3333333 48 63 0 0 0 0 4095 1 
i 1 249 0.3333333 36 79 0 0 0 0 4095 1 
i 1 249.1533 0.1666667 60 79 0 0 0 0 4095 1 
i 1 249.32 1 58 79 0 0 0 0 4095 1 
i 4 249.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 7 249.3253 1.333333 50 63 0 0 0 0 4095 1 
i 1 249.3333 0.6666667 36 79 0 0 0 0 4095 1 
i 4 249.6587 0.3333333 51 63 0 0 0 0 4095 1 
i 4 249.992 0.6666667 53 63 0 0 0 0 4095 1 
i 1 250 0.6666667 24 79 0 0 0 0 4095 1 
i 1 250.32 0.3333333 56 79 0 0 0 0 4095 1 
i 1 250.6533 3.666667 60 79 0 0 0 0 4095 1 
i 11 250.6533 2.666667 46 63 0 0 0 0 4095 1 
i 14 250.6533 0.6666667 41 63 0 0 0 0 4095 1 
i 4 250.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 7 250.6587 2.666667 49 63 0 0 0 0 4095 1 
i 1 250.6667 1 31 79 0 0 0 0 4095 1 
i 14 251.32 0.6666667 43 63 0 0 0 0 4095 1 
i 4 251.3253 0.6666667 55 63 0 0 0 0 4095 1 
i 1 251.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 14 251.9867 0.6666667 40 63 0 0 0 0 4095 1 
i 4 251.992 0.6666667 52 63 0 0 0 0 4095 1 
i 1 252 0.6666667 31 79 0 0 0 0 4095 1 
i 14 252.6533 0.6666667 41 63 0 0 0 0 4095 1 
i 4 252.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 1 252.6667 0.6666667 31 79 0 0 0 0 4095 1 
i 11 253.32 2.666667 49 63 0 0 0 0 4095 1 
i 14 253.32 1.333333 44 63 0 0 0 0 4095 1 
i 4 253.3253 1.333333 56 63 0 0 0 0 4095 1 
i 7 253.3253 2.666667 52 63 0 0 0 0 4095 1 
i 1 253.3333 1 24 79 0 0 0 0 4095 1 
i 1 254.32 0.1666667 58 79 0 0 0 0 4095 1 
i 1 254.3333 0.3333333 24 79 0 0 0 0 4095 1 
i 1 254.4867 0.1666667 56 79 0 0 0 0 4095 1 
i 1 254.6533 1 55 79 0 0 0 0 4095 1 
i 14 254.6533 1.333333 43 63 0 0 0 0 4095 1 
i 4 254.6587 1.333333 55 63 0 0 0 0 4095 1 
i 1 254.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 1 255.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 255.6533 0.3333333 53 79 0 0 0 0 4095 1 
i 1 255.9867 4 56 79 0 0 0 0 4095 1 
i 11 255.9867 2.666667 44 63 0 0 0 0 4095 1 
i 14 255.9867 0.6666667 36 63 0 0 0 0 4095 1 
i 4 255.992 0.6666667 48 63 0 0 0 0 4095 1 
i 7 255.992 2.666667 48 63 0 0 0 0 4095 1 
i 1 256 1 29 79 0 0 0 0 4095 1 
i 14 256.6533 0.6666667 38 63 0 0 0 0 4095 1 
i 4 256.6587 0.6666667 50 63 0 0 0 0 4095 1 
i 1 257 0.3333333 34 79 0 0 0 0 4095 1 
i 14 257.32 0.6666667 39 63 0 0 0 0 4095 1 
i 4 257.3253 0.6666667 51 63 0 0 0 0 4095 1 
i 1 257.3333 0.6666667 34 79 0 0 0 0 4095 1 
i 14 257.9867 0.6666667 41 63 0 0 0 0 4095 1 
i 4 257.992 0.6666667 53 63 0 0 0 0 4095 1 
i 1 258 0.6666667 29 79 0 0 0 0 4095 1 
i 11 258.6533 2.666667 44 63 0 0 0 0 4095 1 
i 14 258.6533 1 43 63 0 0 0 0 4095 1 
i 4 258.6587 1 55 63 0 0 0 0 4095 1 
i 7 258.6587 2.666667 50 63 0 0 0 0 4095 1 
i 1 258.6667 1 27 79 0 0 0 0 4095 1 
i 14 259.6533 0.1666667 41 63 0 0 0 0 4095 1 
i 4 259.6587 0.1666667 53 63 0 0 0 0 4095 1 
i 1 259.6667 0.3333333 36 79 0 0 0 0 4095 1 
i 14 259.82 0.1666667 39 63 0 0 0 0 4095 1 
i 4 259.8253 0.1666667 51 63 0 0 0 0 4095 1 
i 14 259.9867 1.333333 38 63 0 0 0 0 4095 1 
i 4 259.992 1.333333 50 63 0 0 0 0 4095 1 
i 1 260 0.6666667 36 79 0 0 0 0 4095 1 
i 1 260.32 0.3333333 60 79 0 0 0 0 4095 1 
i 1 260.6533 0.3333333 65 79 0 0 0 0 4095 1 
i 1 260.6667 0.6666667 27 79 0 0 0 0 4095 1 
i 1 260.9867 0.3333333 67 79 0 0 0 0 4095 1 
i 1 261.32 3.666667 68 79 0 0 0 0 4095 1 
i 11 261.32 2.666667 44 63 0 0 0 0 4095 1 
i 14 261.32 0.6666667 41 63 0 0 0 0 4095 1 
i 4 261.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 7 261.3253 2.666667 48 63 0 0 0 0 4095 1 
i 1 261.3333 1 26 79 0 0 0 0 4095 1 
i 14 261.9867 1 43 63 0 0 0 0 4095 1 
i 4 261.992 1 55 63 0 0 0 0 4095 1 
i 1 262.3333 0.3333333 32 79 0 0 0 0 4095 1 
i 1 262.6667 0.6666667 32 79 0 0 0 0 4095 1 
i 14 262.9867 1 41 63 0 0 0 0 4095 1 
i 4 262.992 1 53 63 0 0 0 0 4095 1 
i 1 263.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 11 263.9867 2.666667 50 63 0 0 0 0 4095 1 
i 14 263.9867 2.666667 44 63 0 0 0 0 4095 1 
i 4 263.992 1 56 63 0 0 0 0 4095 1 
i 7 263.992 2.666667 53 63 0 0 0 0 4095 1 
i 1 264 1 23 79 0 0 0 0 4095 1 
i 1 264.9867 0.1666667 67 79 0 0 0 0 4095 1 
i 1 265 0.3333333 31 79 0 0 0 0 4095 1 
i 1 265.1533 0.1666667 65 79 0 0 0 0 4095 1 
i 1 265.32 0.6666667 63 79 0 0 0 0 4095 1 
i 1 265.3333 0.6666667 31 79 0 0 0 0 4095 1 
i 1 265.9867 0.6666667 62 79 0 0 0 0 4095 1 
i 1 266 0.6666667 23 79 0 0 0 0 4095 1 
i 1 266.6533 3.666667 67 79 0 0 0 0 4095 1 
i 11 266.6533 2.666667 48 63 0 0 0 0 4095 1 
i 14 266.6533 0.6666667 39 63 0 0 0 0 4095 1 
i 4 266.6587 0.6666667 51 63 0 0 0 0 4095 1 
i 7 266.6587 2.666667 55 63 0 0 0 0 4095 1 
i 1 266.6667 1 24 79 0 0 0 0 4095 1 
i 14 267.32 1.333333 41 63 0 0 0 0 4095 1 
i 4 267.3253 1.333333 53 63 0 0 0 0 4095 1 
i 1 267.6667 0.3333333 31 79 0 0 0 0 4095 1 
i 1 268 0.6666667 31 79 0 0 0 0 4095 1 
i 14 268.6533 0.6666667 39 63 0 0 0 0 4095 1 
i 4 268.6587 0.6666667 51 63 0 0 0 0 4095 1 
i 1 268.6667 0.6666667 24 79 0 0 0 0 4095 1 
i 11 269.32 2.666667 48 63 0 0 0 0 4095 1 
i 14 269.32 2.666667 43 63 0 0 0 0 4095 1 
i 4 269.3253 1 55 63 0 0 0 0 4095 1 
i 7 269.3253 2.666667 51 63 0 0 0 0 4095 1 
i 1 269.3333 1 22 79 0 0 0 0 4095 1 
i 1 270.32 0.1666667 65 79 0 0 0 0 4095 1 
i 1 270.3333 0.3333333 31 79 0 0 0 0 4095 1 
i 1 270.4867 0.1666667 63 79 0 0 0 0 4095 1 
i 1 270.6533 1 62 79 0 0 0 0 4095 1 
i 1 270.6667 0.6666667 31 79 0 0 0 0 4095 1 
i 1 271.3333 0.6666667 22 79 0 0 0 0 4095 1 
i 1 271.6533 0.3333333 60 79 0 0 0 0 4095 1 
i 1 271.9867 2 54 79 0 0 0 0 4095 1 
i 11 271.9867 2.666667 48 63 0 0 0 0 4095 1 
i 14 271.9867 0.6666667 38 63 0 0 0 0 4095 1 
i 4 271.992 0.6666667 50 63 0 0 0 0 4095 1 
i 7 271.992 2.666667 50 63 0 0 0 0 4095 1 
i 1 272 1 21 79 0 0 0 0 4095 1 
i 14 272.6533 0.6666667 39 63 0 0 0 0 4095 1 
i 4 272.6587 0.6666667 51 63 0 0 0 0 4095 1 
i 1 273 0.3333333 30 79 0 0 0 0 4095 1 
i 14 273.32 0.6666667 39 63 0 0 0 0 4095 1 
i 4 273.3253 0.6666667 51 63 0 0 0 0 4095 1 
i 1 273.3333 0.6666667 30 79 0 0 0 0 4095 1 
i 1 273.9867 0.6666667 50 79 0 0 0 0 4095 1 
i 14 273.9867 0.6666667 38 63 0 0 0 0 4095 1 
i 4 273.992 0.6666667 50 63 0 0 0 0 4095 1 
i 1 274 0.6666667 21 79 0 0 0 0 4095 1 
i 1 274.6533 2 55 79 0 0 0 0 4095 1 
i 11 274.6533 2.666667 47 63 0 0 0 0 4095 1 
i 14 274.6533 0.6666667 38 63 0 0 0 0 4095 1 
i 4 274.6587 0.6666667 50 63 0 0 0 0 4095 1 
i 7 274.6587 2.666667 53 63 0 0 0 0 4095 1 
i 1 274.6667 1 19 79 0 0 0 0 4095 1 
i 14 275.32 1.333333 39 63 0 0 0 0 4095 1 
i 4 275.3253 1.333333 51 63 0 0 0 0 4095 1 
i 1 275.6667 0.3333333 29 79 0 0 0 0 4095 1 
i 1 276 0.6666667 29 79 0 0 0 0 4095 1 
i 1 276.6533 0.6666667 55 79 0 0 0 0 4095 1 
i 14 276.6533 0.6666667 38 63 0 0 0 0 4095 1 
i 4 276.6587 0.6666667 50 63 0 0 0 0 4095 1 
i 1 276.6667 0.6666667 19 79 0 0 0 0 4095 1 
i 1 277.32 2.666667 60 79 0 0 0 0 4095 1 
i 14 277.32 0.3333333 36 63 0 0 0 0 4095 1 
i 1 277.3333 1 24 79 0 0 0 0 4095 1 
i 11 277.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 14 277.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 11 277.9867 2 44 63 0 0 0 0 4095 1 
i 14 277.9867 2 43 63 0 0 0 0 4095 1 
i 7 278.3253 0.3333333 48 63 0 0 0 0 4095 1 
i 1 278.3333 0.3333333 36 79 0 0 0 0 4095 1 
i 4 278.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 7 278.6587 1.333333 50 63 0 0 0 0 4095 1 
i 1 278.6667 0.6666667 36 79 0 0 0 0 4095 1 
i 4 278.992 0.3333333 51 63 0 0 0 0 4095 1 
i 4 279.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 1 279.3333 0.6666667 24 79 0 0 0 0 4095 1 
i 1 279.9867 2.666667 60 79 0 0 0 0 4095 1 
i 14 279.9867 0.3333333 34 63 0 0 0 0 4095 1 
i 1 280 1 22 79 0 0 0 0 4095 1 
i 11 280.32 0.3333333 43 63 0 0 0 0 4095 1 
i 14 280.32 0.3333333 43 63 0 0 0 0 4095 1 
i 11 280.6533 2 44 63 0 0 0 0 4095 1 
i 14 280.6533 2 43 63 0 0 0 0 4095 1 
i 7 280.992 0.3333333 48 63 0 0 0 0 4095 1 
i 1 281 0.3333333 34 79 0 0 0 0 4095 1 
i 4 281.3253 0.3333333 50 63 0 0 0 0 4095 1 
i 7 281.3253 1.333333 50 63 0 0 0 0 4095 1 
i 1 281.3333 0.6666667 34 79 0 0 0 0 4095 1 
i 4 281.6587 0.3333333 51 63 0 0 0 0 4095 1 
i 4 281.992 0.6666667 53 63 0 0 0 0 4095 1 
i 1 282 0.6666667 22 79 0 0 0 0 4095 1 
i 1 282.6533 2.666667 60 79 0 0 0 0 4095 1 
i 1 282.6533 2.666667 72 79 0 0 0 0 4095 1 
i 14 282.6533 0.3333333 32 63 0 0 0 0 4095 1 
i 1 282.6667 1 20 79 0 0 0 0 4095 1 
i 11 282.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 14 282.9867 0.3333333 43 63 0 0 0 0 4095 1 
i 11 283.32 2 44 63 0 0 0 0 4095 1 
i 14 283.32 2 43 63 0 0 0 0 4095 1 
i 7 283.6587 0.3333333 48 63 0 0 0 0 4095 1 
i 1 283.6667 0.3333333 32 79 0 0 0 0 4095 1 
i 4 283.992 0.3333333 50 63 0 0 0 0 4095 1 
i 7 283.992 1.333333 50 63 0 0 0 0 4095 1 
i 1 284 0.6666667 32 79 0 0 0 0 4095 1 
i 4 284.3253 0.3333333 51 63 0 0 0 0 4095 1 
i 4 284.6587 0.6666667 53 63 0 0 0 0 4095 1 
i 1 284.6667 0.6666667 20 79 0 0 0 0 4095 1 
i 1 285.32 2.666667 72 79 0 0 0 0 4095 1 
i 14 285.32 0.3333333 31 63 0 0 0 0 4095 1 
i 1 285.3333 1 19 79 0 0 0 0 4095 1 
i 11 285.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 14 285.6533 0.3333333 43 63 0 0 0 0 4095 1 
i 11 285.9867 2 44 63 0 0 0 0 4095 1 
i 14 285.9867 2 43 63 0 0 0 0 4095 1 
i 7 286.3253 0.3333333 48 63 0 0 0 0 4095 1 
i 1 286.3333 0.3333333 31 79 0 0 0 0 4095 1 
i 4 286.6587 0.3333333 50 63 0 0 0 0 4095 1 
i 7 286.6587 1.333333 50 63 0 0 0 0 4095 1 
i 1 286.6667 0.6666667 31 79 0 0 0 0 4095 1 
i 4 286.992 0.3333333 51 63 0 0 0 0 4095 1 
i 4 287.3253 0.6666667 53 63 0 0 0 0 4095 1 
i 1 287.3333 0.5 19 79 0 0 0 0 4095 1 
i 1 287.8333 0.1666667 19 79 0 0 0 0 4095 1 
i 1 287.9867 2 84 79 0 0 0 0 4095 1 
i 1 288 2.666667 24 79 0 0 0 0 4095 1 
i 11 288.6533 2 50 63 0 0 0 0 4095 1 
i 14 288.6533 2 48 63 0 0 0 0 4095 1 
i 4 288.6587 2 55 63 0 0 0 0 4095 1 
i 7 288.6587 2 51 63 0 0 0 0 4095 1 

e 5
</CsScore>
</CsoundSynthesizer>

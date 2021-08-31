
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

;gk_ZakianFlute_level init -2
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

gk_Bower_level chnexport "gk_Bower_level", 3
gk_Bower_pan chnexport "gk_Bower_pan", 3
gi_Bower_minimum_hz chnexport "gi_Bower_minimum_hz", 3
gk_Bower_bow_pressure chnexport "gk_Bower_bow_pressure", 3
gk_Bower_bow_position chnexport "gk_Bower_bow_position", 3
gk_Bower_vibrato_hz chnexport "gk_Bower_vibrato_hz", 3
gk_Bower_vibrato_amplitude chnexport "gk_Bower_vibrato_amplitude", 3
gi_Bower_sine ftgen 0,0,65537,10,1
gk_Bower_level init -30
gk_Bower_bow_pressure init 4.1
gk_Bower_bow_position init .148
gi_Bower_minimum_hz init 30
instr Bower
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_phase = p6
i_pan = p7
i_amplitude = ampdb(i_midi_velocity) * 500
i_amplitude ampmidicurve i_midi_velocity, gi_ampmidicurve_dynamic_range, gi_ampmidicurve_exponent
i_amplitude *= 50000

i_attack =  p3 * (1 / 4) * (4 / 3)
i_sustain = p3 * (1 / 2) * (4 / 3)
i_release =   p3 * (1 / 4) * (4 / 3)
p3 = i_attack + i_sustain + i_release
k_envelope transeg 0.0, i_attack / 2.0, 1.5, i_amplitude / 2.0, i_attack / 2.0, -1.5, i_amplitude, i_sustain, 0.0, i_amplitude, i_release / 2.0, 1.5, i_amplitude / 2.0, i_release / 2.0, -1.5, 0
i_frequency = cpsmidinn(i_midi_key)
kamp = k_envelope
kfreq = i_frequency
kpres = gk_Bower_bow_pressure
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
krat = gk_Bower_bow_position
kvibf = gk_Bower_vibrato_hz
kvibamp = gk_Bower_vibrato_amplitude
iminfreq = gi_Bower_minimum_hz
aSig wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,gi_Bower_sine,iminfreq
k_gain = ampdb(gk_Bower_level)
aSig = aSig * k_gain
;aSig butlp aSig,2000
;aSig pareq aSig,80,6,0.707
a_left, a_right pan2 aSig / 7, i_pan
a_damping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
a_left = a_damping * a_left
a_right = a_damping * a_right
outleta "outleft", a_left
outleta "outright", a_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
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
i 4 0 1.184211 24 48 0 0 0 0 4095 1 
i 2 0.3947368 0.3947368 43 48 0 0 0 0 4095 1 
i 2 0.7894737 2.368421 44 48 0 0 0 0 4095 1 
i 1 1.184211 0.3947368 48 48 0 0 0 0 4095 1 
i 4 1.184211 0.3947368 36 48 0 0 0 0 4095 1 
i 1 1.578947 0.3947368 50 48 0 0 0 0 4095 1 
i 4 1.578947 0.7894737 36 48 0 0 0 0 4095 1 
i 1 1.973684 0.3947368 51 48 0 0 0 0 4095 1 
i 1 2.368421 0.7894737 53 48 0 0 0 0 4095 1 
i 4 2.368421 0.7894737 24 48 0 0 0 0 4095 1 
i 4 3.157895 1.184211 24 48 0 0 0 0 4095 1 
i 2 3.552632 0.3947368 43 48 0 0 0 0 4095 1 
i 2 3.947368 2.368421 44 48 0 0 0 0 4095 1 
i 1 4.342105 0.3947368 48 48 0 0 0 0 4095 1 
i 4 4.342105 0.3947368 36 48 0 0 0 0 4095 1 
i 1 4.736842 0.3947368 50 48 0 0 0 0 4095 1 
i 4 4.736842 0.7894737 36 48 0 0 0 0 4095 1 
i 1 5.131579 0.3947368 51 48 0 0 0 0 4095 1 
i 1 5.526316 0.7894737 53 48 0 0 0 0 4095 1 
i 4 5.526316 0.7894737 24 48 0 0 0 0 4095 1 
i 4 6.315789 1.184211 24 48 0 0 0 0 4095 1 
i 2 6.710526 0.3947368 43 48 0 0 0 0 4095 1 
i 2 7.105263 2.368421 44 48 0 0 0 0 4095 1 
i 1 7.5 0.3947368 48 48 0 0 0 0 4095 1 
i 4 7.5 0.3947368 36 48 0 0 0 0 4095 1 
i 1 7.894737 0.3947368 50 48 0 0 0 0 4095 1 
i 4 7.894737 0.7894737 36 48 0 0 0 0 4095 1 
i 1 8.289474 0.3947368 51 48 0 0 0 0 4095 1 
i 1 8.684211 0.7894737 53 48 0 0 0 0 4095 1 
i 4 8.684211 0.7894737 24 48 0 0 0 0 4095 1 
i 4 9.473684 1.184211 24 48 0 0 0 0 4095 1 
i 2 9.868421 0.3947368 43 48 0 0 0 0 4095 1 
i 2 10.26316 2.368421 44 48 0 0 0 0 4095 1 
i 1 10.65789 0.3947368 48 48 0 0 0 0 4095 1 
i 4 10.65789 0.3947368 36 48 0 0 0 0 4095 1 
i 1 11.05263 0.3947368 50 48 0 0 0 0 4095 1 
i 4 11.05263 0.7894737 36 48 0 0 0 0 4095 1 
i 1 11.44737 0.3947368 51 48 0 0 0 0 4095 1 
i 1 11.84211 0.7894737 53 48 0 0 0 0 4095 1 
i 4 11.84211 0.7894737 24 48 0 0 0 0 4095 1 
i 3 12.63158 3.683684 55 63 0 0 0 0 4095 1 
i 4 12.63158 1.184211 24 48 0 0 0 0 4095 1 
i 2 13.02632 0.3947368 43 48 0 0 0 0 4095 1 
i 2 13.42105 2.368421 44 48 0 0 0 0 4095 1 
i 1 13.81579 0.3947368 48 48 0 0 0 0 4095 1 
i 4 13.81579 0.3947368 36 48 0 0 0 0 4095 1 
i 1 14.21053 0.3947368 50 48 0 0 0 0 4095 1 
i 4 14.21053 0.7894737 36 48 0 0 0 0 4095 1 
i 1 14.60526 0.3947368 51 48 0 0 0 0 4095 1 
i 1 15 0.7894737 53 48 0 0 0 0 4095 1 
i 4 15 0.7894737 24 48 0 0 0 0 4095 1 
i 4 15.78947 1.184211 24 48 0 0 0 0 4095 1 
i 2 16.18421 0.3947368 43 48 0 0 0 0 4095 1 
i 3 16.31526 0.5257895 53 63 0 0 0 0 4095 1 
i 2 16.57895 2.368421 44 48 0 0 0 0 4095 1 
i 3 16.84105 0.5257895 51 63 0 0 0 0 4095 1 
i 1 16.97368 0.3947368 48 48 0 0 0 0 4095 1 
i 4 16.97368 0.3947368 36 48 0 0 0 0 4095 1 
i 3 17.36684 1.184211 50 63 0 0 0 0 4095 1 
i 1 17.36842 0.3947368 50 48 0 0 0 0 4095 1 
i 4 17.36842 0.7894737 36 48 0 0 0 0 4095 1 
i 1 17.76316 0.3947368 51 48 0 0 0 0 4095 1 
i 1 18.15789 0.7894737 53 48 0 0 0 0 4095 1 
i 4 18.15789 0.7894737 24 48 0 0 0 0 4095 1 
i 3 18.55105 0.3947368 48 63 0 0 0 0 4095 1 
i 3 18.94579 4.342105 51 63 0 0 0 0 4095 1 
i 4 18.94737 1.184211 24 48 0 0 0 0 4095 1 
i 2 19.34211 0.3947368 43 48 0 0 0 0 4095 1 
i 2 19.73684 2.368421 44 48 0 0 0 0 4095 1 
i 1 20.13158 0.3947368 48 48 0 0 0 0 4095 1 
i 4 20.13158 0.3947368 36 48 0 0 0 0 4095 1 
i 1 20.52632 0.3947368 50 48 0 0 0 0 4095 1 
i 4 20.52632 0.7894737 36 48 0 0 0 0 4095 1 
i 1 20.92105 0.3947368 51 48 0 0 0 0 4095 1 
i 1 21.31579 0.7894737 53 48 0 0 0 0 4095 1 
i 4 21.31579 0.7894737 24 48 0 0 0 0 4095 1 
i 4 22.10526 1.184211 24 48 0 0 0 0 4095 1 
i 2 22.5 0.3947368 43 48 0 0 0 0 4095 1 
i 2 22.89474 2.368421 44 48 0 0 0 0 4095 1 
i 3 23.28789 0.1973684 50 63 0 0 0 0 4095 1 
i 1 23.28947 0.3947368 48 48 0 0 0 0 4095 1 
i 4 23.28947 0.3947368 36 48 0 0 0 0 4095 1 
i 3 23.48526 0.1973684 48 63 0 0 0 0 4095 1 
i 3 23.68263 1.184211 46 63 0 0 0 0 4095 1 
i 1 23.68421 0.3947368 50 48 0 0 0 0 4095 1 
i 4 23.68421 0.7894737 36 48 0 0 0 0 4095 1 
i 1 24.07895 0.3947368 51 48 0 0 0 0 4095 1 
i 1 24.47368 0.7894737 53 48 0 0 0 0 4095 1 
i 4 24.47368 0.7894737 24 48 0 0 0 0 4095 1 
i 3 24.86684 0.3947368 44 63 0 0 0 0 4095 1 
i 3 25.26158 2.368421 48 63 0 0 0 0 4095 1 
i 1 25.26316 1.578947 56 48 0 0 0 0 4095 1 
i 4 25.26316 1.184211 29 48 0 0 0 0 4095 1 
i 2 25.65789 0.3947368 51 48 0 0 0 0 4095 1 
i 2 26.05263 0.3947368 50 48 0 0 0 0 4095 1 
i 2 26.44737 0.3947368 51 48 0 0 0 0 4095 1 
i 4 26.44737 0.3947368 29 48 0 0 0 0 4095 1 
i 1 26.84211 1.578947 56 48 0 0 0 0 4095 1 
i 4 26.84211 0.7894737 34 48 0 0 0 0 4095 1 
i 2 27.23684 0.3947368 51 48 0 0 0 0 4095 1 
i 3 27.63 0.1973684 46 63 0 0 0 0 4095 1 
i 2 27.63158 0.7894737 50 48 0 0 0 0 4095 1 
i 4 27.63158 0.7894737 34 48 0 0 0 0 4095 1 
i 3 27.82737 0.1973684 48 63 0 0 0 0 4095 1 
i 3 28.02474 0.1973684 46 63 0 0 0 0 4095 1 
i 3 28.22211 0.1973684 44 63 0 0 0 0 4095 1 
i 3 28.41947 1.184211 46 63 0 0 0 0 4095 1 
i 1 28.42105 1.578947 55 48 0 0 0 0 4095 1 
i 4 28.42105 1.184211 27 48 0 0 0 0 4095 1 
i 2 28.81579 0.3947368 50 48 0 0 0 0 4095 1 
i 2 29.21053 0.3947368 48 48 0 0 0 0 4095 1 
i 3 29.60368 0.3947368 48 63 0 0 0 0 4095 1 
i 2 29.60526 0.3947368 50 48 0 0 0 0 4095 1 
i 4 29.60526 0.3947368 27 48 0 0 0 0 4095 1 
i 3 29.99842 0.3947368 48 63 0 0 0 0 4095 1 
i 1 30 1.578947 55 48 0 0 0 0 4095 1 
i 4 30 0.7894737 32 48 0 0 0 0 4095 1 
i 3 30.39316 0.3947368 46 63 0 0 0 0 4095 1 
i 2 30.39474 0.3947368 50 48 0 0 0 0 4095 1 
i 3 30.78789 0.3947368 44 63 0 0 0 0 4095 1 
i 2 30.78947 0.3947368 48 48 0 0 0 0 4095 1 
i 4 30.78947 0.7894737 32 48 0 0 0 0 4095 1 
i 3 31.18263 0.3947368 43 63 0 0 0 0 4095 1 
i 2 31.18421 0.3947368 50 48 0 0 0 0 4095 1 
i 3 31.57737 1.184211 44 63 0 0 0 0 4095 1 
i 1 31.57895 1.578947 53 48 0 0 0 0 4095 1 
i 4 31.57895 1.184211 26 48 0 0 0 0 4095 1 
i 2 31.97368 0.3947368 50 48 0 0 0 0 4095 1 
i 2 32.36842 0.3947368 48 48 0 0 0 0 4095 1 
i 3 32.76158 0.1973684 43 63 0 0 0 0 4095 1 
i 2 32.76316 0.3947368 50 48 0 0 0 0 4095 1 
i 4 32.76316 0.3947368 38 48 0 0 0 0 4095 1 
i 3 32.95895 0.1973684 41 63 0 0 0 0 4095 1 
i 3 33.15632 0.7894737 39 63 0 0 0 0 4095 1 
i 1 33.15789 1.578947 54 48 0 0 0 0 4095 1 
i 4 33.15789 0.7894737 38 48 0 0 0 0 4095 1 
i 2 33.55263 0.3947368 50 48 0 0 0 0 4095 1 
i 3 33.94579 0.7894737 38 63 0 0 0 0 4095 1 
i 2 33.94737 0.3947368 48 48 0 0 0 0 4095 1 
i 4 33.94737 0.7894737 26 48 0 0 0 0 4095 1 
i 2 34.34211 0.3947368 50 48 0 0 0 0 4095 1 
i 3 34.73526 3.157895 43 63 0 0 0 0 4095 1 
i 1 34.73684 1.578947 55 48 0 0 0 0 4095 1 
i 4 34.73684 1.184211 31 48 0 0 0 0 4095 1 
i 2 35.13158 0.3947368 48 48 0 0 0 0 4095 1 
i 2 35.52632 0.3947368 50 48 0 0 0 0 4095 1 
i 2 35.92105 0.3947368 47 48 0 0 0 0 4095 1 
i 4 35.92105 0.3947368 32 48 0 0 0 0 4095 1 
i 1 36.31579 0.3947368 55 48 0 0 0 0 4095 1 
i 2 36.31579 0.7894737 51 48 0 0 0 0 4095 1 
i 4 36.31579 0.3947368 31 48 0 0 0 0 4095 1 
i 1 36.71053 0.3947368 55 48 0 0 0 0 4095 1 
i 4 36.71053 0.3947368 29 48 0 0 0 0 4095 1 
i 1 37.10526 0.1973684 56 48 0 0 0 0 4095 1 
i 2 37.10526 0.7894737 50 48 0 0 0 0 4095 1 
i 4 37.10526 0.3947368 27 48 0 0 0 0 4095 1 
i 1 37.30263 0.1973684 55 48 0 0 0 0 4095 1 
i 1 37.5 0.1973684 54 48 0 0 0 0 4095 1 
i 4 37.5 0.3947368 26 48 0 0 0 0 4095 1 
i 1 37.69737 0.1973684 55 48 0 0 0 0 4095 1 
i 1 37.89474 3.683684 67 48 0 0 0 0 4095 1 
i 4 37.89474 1.184211 24 32 0 0 0 0 4095 1 
i 3 38.28789 0.3947368 43 32 0 0 0 0 4095 1 
i 3 38.68263 2.368421 44 32 0 0 0 0 4095 1 
i 2 39.07895 0.3947368 48 32 0 0 0 0 4095 1 
i 4 39.07895 0.3947368 36 32 0 0 0 0 4095 1 
i 2 39.47368 0.3947368 50 32 0 0 0 0 4095 1 
i 4 39.47368 0.7894737 36 32 0 0 0 0 4095 1 
i 2 39.86842 0.3947368 51 32 0 0 0 0 4095 1 
i 2 40.26316 0.7894737 53 32 0 0 0 0 4095 1 
i 4 40.26316 0.7894737 24 32 0 0 0 0 4095 1 
i 4 41.05263 1.184211 24 32 0 0 0 0 4095 1 
i 3 41.44579 0.3947368 43 32 0 0 0 0 4095 1 
i 1 41.57842 0.5257895 65 48 0 0 0 0 4095 1 
i 3 41.84053 2.368421 44 32 0 0 0 0 4095 1 
i 1 42.10421 0.5257895 63 48 0 0 0 0 4095 1 
i 2 42.23684 0.3947368 48 32 0 0 0 0 4095 1 
i 4 42.23684 0.3947368 36 32 0 0 0 0 4095 1 
i 1 42.63 0.5921053 62 48 0 0 0 0 4095 1 
i 2 42.63158 0.3947368 50 32 0 0 0 0 4095 1 
i 4 42.63158 0.7894737 36 32 0 0 0 0 4095 1 
i 2 43.02632 0.3947368 51 32 0 0 0 0 4095 1 
i 1 43.22211 0.1973684 60 48 0 0 0 0 4095 1 
i 1 43.41947 0.1973684 62 48 0 0 0 0 4095 1 
i 2 43.42105 0.7894737 53 32 0 0 0 0 4095 1 
i 4 43.42105 0.7894737 24 32 0 0 0 0 4095 1 
i 1 43.61684 0.1973684 63 48 0 0 0 0 4095 1 
i 1 43.81421 0.1973684 62 48 0 0 0 0 4095 1 
i 1 44.01158 0.1973684 60 48 0 0 0 0 4095 1 
i 1 44.20895 3.947368 63 48 0 0 0 0 4095 1 
i 4 44.21053 1.184211 24 32 0 0 0 0 4095 1 
i 3 44.60368 0.3947368 43 32 0 0 0 0 4095 1 
i 3 44.99842 2.368421 44 32 0 0 0 0 4095 1 
i 2 45.39474 0.3947368 48 32 0 0 0 0 4095 1 
i 4 45.39474 0.3947368 36 32 0 0 0 0 4095 1 
i 2 45.78947 0.3947368 50 32 0 0 0 0 4095 1 
i 4 45.78947 0.7894737 36 32 0 0 0 0 4095 1 
i 2 46.18421 0.3947368 51 32 0 0 0 0 4095 1 
i 2 46.57895 0.7894737 53 32 0 0 0 0 4095 1 
i 4 46.57895 0.7894737 24 32 0 0 0 0 4095 1 
i 4 47.36842 1.184211 24 32 0 0 0 0 4095 1 
i 3 47.76158 0.3947368 43 32 0 0 0 0 4095 1 
i 1 48.15632 0.3947368 62 48 0 0 0 0 4095 1 
i 3 48.15632 2.368421 44 32 0 0 0 0 4095 1 
i 1 48.55105 0.3947368 60 48 0 0 0 0 4095 1 
i 2 48.55263 0.3947368 48 32 0 0 0 0 4095 1 
i 4 48.55263 0.3947368 36 32 0 0 0 0 4095 1 
i 1 48.94579 1.184211 58 48 0 0 0 0 4095 1 
i 2 48.94737 0.3947368 50 32 0 0 0 0 4095 1 
i 4 48.94737 0.7894737 36 32 0 0 0 0 4095 1 
i 2 49.34211 0.3947368 51 32 0 0 0 0 4095 1 
i 2 49.73684 0.7894737 53 32 0 0 0 0 4095 1 
i 4 49.73684 0.7894737 24 32 0 0 0 0 4095 1 
i 1 50.13 0.3947368 56 48 0 0 0 0 4095 1 
i 1 50.52474 2.368421 60 48 0 0 0 0 4095 1 
i 3 50.52474 1.184211 49 32 0 0 0 0 4095 1 
i 2 50.52632 0.7894737 53 32 0 0 0 0 4095 1 
i 4 50.52632 1.184211 31 32 0 0 0 0 4095 1 
i 2 51.31579 0.7894737 55 32 0 0 0 0 4095 1 
i 3 51.70895 1.184211 49 32 0 0 0 0 4095 1 
i 4 51.71053 0.3947368 43 32 0 0 0 0 4095 1 
i 2 52.10526 0.7894737 52 32 0 0 0 0 4095 1 
i 4 52.10526 0.7894737 43 32 0 0 0 0 4095 1 
i 1 52.89316 0.1973684 58 48 0 0 0 0 4095 1 
i 3 52.89316 0.7894737 49 32 0 0 0 0 4095 1 
i 2 52.89474 0.7894737 53 32 0 0 0 0 4095 1 
i 4 52.89474 0.7894737 31 32 0 0 0 0 4095 1 
i 1 53.09053 0.1973684 60 48 0 0 0 0 4095 1 
i 1 53.28789 0.1973684 58 48 0 0 0 0 4095 1 
i 1 53.48526 0.1973684 56 48 0 0 0 0 4095 1 
i 1 53.68263 1.184211 58 48 0 0 0 0 4095 1 
i 3 53.68263 1.184211 49 32 0 0 0 0 4095 1 
i 2 53.68421 1.578947 56 32 0 0 0 0 4095 1 
i 4 53.68421 1.184211 24 32 0 0 0 0 4095 1 
i 1 54.86684 0.3947368 61 48 0 0 0 0 4095 1 
i 3 54.86684 1.184211 49 32 0 0 0 0 4095 1 
i 4 54.86842 0.3947368 36 32 0 0 0 0 4095 1 
i 1 55.26158 0.3947368 61 48 0 0 0 0 4095 1 
i 2 55.26316 1.578947 55 32 0 0 0 0 4095 1 
i 4 55.26316 0.7894737 36 32 0 0 0 0 4095 1 
i 1 55.65632 0.7894737 60 48 0 0 0 0 4095 1 
i 3 56.05105 0.7894737 49 32 0 0 0 0 4095 1 
i 4 56.05263 0.7894737 24 32 0 0 0 0 4095 1 
i 1 56.44579 0.1973684 58 48 0 0 0 0 4095 1 
i 1 56.64316 0.1973684 58 48 0 0 0 0 4095 1 
i 1 56.84053 1.184211 58 48 0 0 0 0 4095 1 
i 3 56.84053 1.184211 48 32 0 0 0 0 4095 1 
i 2 56.84211 0.7894737 48 32 0 0 0 0 4095 1 
i 4 56.84211 1.184211 29 32 0 0 0 0 4095 1 
i 2 57.63158 0.7894737 50 32 0 0 0 0 4095 1 
i 1 58.02474 0.1973684 56 48 0 0 0 0 4095 1 
i 3 58.02474 1.184211 48 32 0 0 0 0 4095 1 
i 4 58.02632 0.3947368 41 32 0 0 0 0 4095 1 
i 1 58.22211 0.1973684 58 48 0 0 0 0 4095 1 
i 1 58.41947 1.578947 56 48 0 0 0 0 4095 1 
i 2 58.42105 0.7894737 51 32 0 0 0 0 4095 1 
i 4 58.42105 0.7894737 41 32 0 0 0 0 4095 1 
i 3 59.20895 0.7894737 48 32 0 0 0 0 4095 1 
i 2 59.21053 0.7894737 53 32 0 0 0 0 4095 1 
i 4 59.21053 0.7894737 29 32 0 0 0 0 4095 1 
i 1 59.99842 1.578947 56 48 0 0 0 0 4095 1 
i 3 59.99842 1.184211 48 32 0 0 0 0 4095 1 
i 2 60 1.184211 55 32 0 0 0 0 4095 1 
i 4 60 1.184211 27 32 0 0 0 0 4095 1 
i 3 61.18263 1.184211 48 32 0 0 0 0 4095 1 
i 2 61.18421 0.1973684 53 32 0 0 0 0 4095 1 
i 4 61.18421 0.3947368 39 32 0 0 0 0 4095 1 
i 2 61.38158 0.1973684 51 32 0 0 0 0 4095 1 
i 2 61.57895 0.7894737 50 32 0 0 0 0 4095 1 
i 4 61.57895 0.7894737 39 32 0 0 0 0 4095 1 
i 3 62.36684 0.7894737 48 32 0 0 0 0 4095 1 
i 2 62.36842 0.7894737 48 32 0 0 0 0 4095 1 
i 4 62.36842 0.7894737 27 32 0 0 0 0 4095 1 
i 1 62.56421 0.1973684 60 95 0 0 0 0 4095 1 
i 1 62.76158 0.1973684 65 95 0 0 0 0 4095 1 
i 1 62.95895 0.1973684 67 95 0 0 0 0 4095 1 
i 1 63.15632 0.3947368 67 95 0 0 0 0 4095 1 
i 3 63.15632 1.184211 48 79 0 0 0 0 4095 1 
i 2 63.15789 0.7894737 53 79 0 0 0 0 4095 1 
i 4 63.15789 1.184211 26 79 0 0 0 0 4095 1 
i 1 63.55105 0.7894737 68 95 0 0 0 0 4095 1 
i 2 63.94737 1.578947 55 79 0 0 0 0 4095 1 
i 1 64.34053 0.1973684 67 95 0 0 0 0 4095 1 
i 3 64.34053 1.184211 48 79 0 0 0 0 4095 1 
i 4 64.34211 0.3947368 38 79 0 0 0 0 4095 1 
i 1 64.53789 0.1973684 65 95 0 0 0 0 4095 1 
i 1 64.73526 1.578947 68 95 0 0 0 0 4095 1 
i 4 64.73684 0.7894737 38 79 0 0 0 0 4095 1 
i 3 65.52474 0.7894737 48 79 0 0 0 0 4095 1 
i 2 65.52632 0.7894737 53 79 0 0 0 0 4095 1 
i 4 65.52632 0.7894737 26 79 0 0 0 0 4095 1 
i 3 66.31421 1.184211 50 79 0 0 0 0 4095 1 
i 2 66.31579 0.7894737 56 79 0 0 0 0 4095 1 
i 4 66.31579 1.184211 31 79 0 0 0 0 4095 1 
i 1 66.70895 0.3947368 67 95 0 0 0 0 4095 1 
i 1 67.10368 0.1973684 70 95 0 0 0 0 4095 1 
i 2 67.10526 1.578947 55 79 0 0 0 0 4095 1 
i 1 67.30105 0.1973684 68 95 0 0 0 0 4095 1 
i 1 67.49842 0.1973684 67 95 0 0 0 0 4095 1 
i 3 67.49842 1.184211 50 79 0 0 0 0 4095 1 
i 4 67.5 0.3947368 43 79 0 0 0 0 4095 1 
i 1 67.69579 0.1973684 65 95 0 0 0 0 4095 1 
i 1 67.89316 1.184211 65 95 0 0 0 0 4095 1 
i 4 67.89474 0.7894737 43 79 0 0 0 0 4095 1 
i 3 68.68263 0.7894737 46 79 0 0 0 0 4095 1 
i 2 68.68421 0.7894737 53 79 0 0 0 0 4095 1 
i 4 68.68421 0.7894737 38 79 0 0 0 0 4095 1 
i 1 69.07737 0.1973684 63 95 0 0 0 0 4095 1 
i 1 69.27474 0.1973684 65 95 0 0 0 0 4095 1 
i 1 69.47211 0.1973684 63 95 0 0 0 0 4095 1 
i 3 69.47211 1.184211 43 79 0 0 0 0 4095 1 
i 2 69.47368 0.7894737 51 79 0 0 0 0 4095 1 
i 4 69.47368 1.184211 36 79 0 0 0 0 4095 1 
i 1 69.66947 0.1973684 60 95 0 0 0 0 4095 1 
i 1 69.86684 0.3947368 60 95 0 0 0 0 4095 1 
i 1 70.26158 2.368421 60 95 0 0 0 0 4095 1 
i 2 70.26316 1.578947 53 79 0 0 0 0 4095 1 
i 3 70.65632 1.184211 43 79 0 0 0 0 4095 1 
i 4 70.65789 0.3947368 48 79 0 0 0 0 4095 1 
i 4 71.05263 0.7894737 48 79 0 0 0 0 4095 1 
i 3 71.84053 0.7894737 43 79 0 0 0 0 4095 1 
i 2 71.84211 0.7894737 51 79 0 0 0 0 4095 1 
i 4 71.84211 0.7894737 36 79 0 0 0 0 4095 1 
i 3 72.63 1.184211 48 79 0 0 0 0 4095 1 
i 2 72.63158 0.7894737 55 79 0 0 0 0 4095 1 
i 4 72.63158 1.184211 34 79 0 0 0 0 4095 1 
i 2 73.42105 1.578947 53 79 0 0 0 0 4095 1 
i 3 73.81421 1.184211 48 79 0 0 0 0 4095 1 
i 4 73.81579 0.3947368 46 79 0 0 0 0 4095 1 
i 1 74.20895 0.5921053 65 95 0 0 0 0 4095 1 
i 4 74.21053 0.7894737 46 79 0 0 0 0 4095 1 
i 1 74.80105 0.5921053 63 95 0 0 0 0 4095 1 
i 3 74.99842 0.7894737 48 79 0 0 0 0 4095 1 
i 2 75 0.7894737 51 79 0 0 0 0 4095 1 
i 4 75 0.7894737 34 79 0 0 0 0 4095 1 
i 1 75.39316 0.1973684 62 95 0 0 0 0 4095 1 
i 1 75.59053 0.1973684 60 95 0 0 0 0 4095 1 
i 1 75.78789 2.368421 54 95 0 0 0 0 4095 1 
i 3 75.78789 1.184211 54 79 0 0 0 0 4095 1 
i 2 75.78947 0.7894737 50 79 0 0 0 0 4095 1 
i 4 75.78947 1.184211 33 79 0 0 0 0 4095 1 
i 2 76.57895 1.578947 51 79 0 0 0 0 4095 1 
i 3 76.97211 1.184211 54 79 0 0 0 0 4095 1 
i 4 76.97368 0.3947368 45 79 0 0 0 0 4095 1 
i 4 77.36842 0.7894737 45 79 0 0 0 0 4095 1 
i 1 78.15632 0.3947368 54 95 0 0 0 0 4095 1 
i 3 78.15632 0.7894737 54 79 0 0 0 0 4095 1 
i 2 78.15789 0.7894737 50 79 0 0 0 0 4095 1 
i 4 78.15789 0.7894737 33 79 0 0 0 0 4095 1 
i 1 78.55105 0.3947368 54 95 0 0 0 0 4095 1 
i 1 78.94579 1.578947 55 95 0 0 0 0 4095 1 
i 3 78.94579 1.184211 53 79 0 0 0 0 4095 1 
i 2 78.94737 0.7894737 50 79 0 0 0 0 4095 1 
i 4 78.94737 1.184211 31 79 0 0 0 0 4095 1 
i 2 79.73684 1.578947 51 79 0 0 0 0 4095 1 
i 3 80.13 1.184211 53 79 0 0 0 0 4095 1 
i 4 80.13158 0.3947368 43 79 0 0 0 0 4095 1 
i 1 80.52474 0.3947368 56 95 0 0 0 0 4095 1 
i 4 80.52632 0.7894737 43 79 0 0 0 0 4095 1 
i 1 80.91947 0.7894737 55 95 0 0 0 0 4095 1 
i 3 81.31421 0.7894737 53 79 0 0 0 0 4095 1 
i 2 81.31579 0.7894737 50 79 0 0 0 0 4095 1 
i 4 81.31579 0.7894737 31 79 0 0 0 0 4095 1 
i 1 81.70895 0.1973684 54 95 0 0 0 0 4095 1 
i 1 81.90632 0.1973684 55 95 0 0 0 0 4095 1 
i 1 82.10368 4.736842 60 95 0 0 0 0 4095 1 
i 3 82.10368 1.184211 51 79 0 0 0 0 4095 1 
i 2 82.10526 1.184211 55 79 0 0 0 0 4095 1 
i 4 82.10526 1.184211 24 79 0 0 0 0 4095 1 
i 3 83.28789 1.184211 51 79 0 0 0 0 4095 1 
i 2 83.28947 1.184211 56 79 0 0 0 0 4095 1 
i 4 83.28947 0.3947368 36 79 0 0 0 0 4095 1 
i 4 83.68421 0.7894737 36 79 0 0 0 0 4095 1 
i 3 84.47211 0.7894737 51 79 0 0 0 0 4095 1 
i 2 84.47368 0.7894737 55 79 0 0 0 0 4095 1 
i 4 84.47368 0.7894737 24 79 0 0 0 0 4095 1 
i 3 85.26158 1.184211 51 79 0 0 0 0 4095 1 
i 2 85.26316 1.184211 58 79 0 0 0 0 4095 1 
i 4 85.26316 1.184211 24 79 0 0 0 0 4095 1 
i 3 86.44579 1.184211 51 79 0 0 0 0 4095 1 
i 2 86.44737 1.184211 56 79 0 0 0 0 4095 1 
i 4 86.44737 0.3947368 36 79 0 0 0 0 4095 1 
i 1 86.84053 0.3947368 60 95 0 0 0 0 4095 1 
i 4 86.84211 0.7894737 36 79 0 0 0 0 4095 1 
i 1 87.23526 0.3947368 67 95 0 0 0 0 4095 1 
i 1 87.63 0.3947368 72 95 0 0 0 0 4095 1 
i 3 87.63 0.7894737 51 79 0 0 0 0 4095 1 
i 2 87.63158 0.7894737 55 79 0 0 0 0 4095 1 
i 4 87.63158 0.7894737 24 79 0 0 0 0 4095 1 
i 1 88.02474 0.3947368 74 95 0 0 0 0 4095 1 
i 1 88.41947 2.368421 75 95 0 0 0 0 4095 1 
i 3 88.41947 2.368421 56 95 0 0 0 0 4095 1 
i 2 88.42105 2.368421 60 95 0 0 0 0 4095 1 
i 4 88.42105 1.184211 29 95 0 0 0 0 4095 1 
i 4 89.60526 0.3947368 39 95 0 0 0 0 4095 1 
i 4 90 0.7894737 39 95 0 0 0 0 4095 1 
i 1 90.78789 0.3947368 72 95 0 0 0 0 4095 1 
i 3 90.78789 0.3947368 53 95 0 0 0 0 4095 1 
i 2 90.78947 0.3947368 56 95 0 0 0 0 4095 1 
i 4 90.78947 0.7894737 29 95 0 0 0 0 4095 1 
i 1 91.18263 0.3947368 74 95 0 0 0 0 4095 1 
i 3 91.18263 0.3947368 55 95 0 0 0 0 4095 1 
i 2 91.18421 0.3947368 58 95 0 0 0 0 4095 1 
i 1 91.57737 0.5257895 75 95 0 0 0 0 4095 1 
i 3 91.57737 0.5257895 56 95 0 0 0 0 4095 1 
i 2 91.57895 0.5257895 60 95 0 0 0 0 4095 1 
i 4 91.57895 1.184211 34 95 0 0 0 0 4095 1 
i 1 92.10316 0.5257895 74 95 0 0 0 0 4095 1 
i 3 92.10316 0.5257895 55 95 0 0 0 0 4095 1 
i 2 92.10474 0.5257895 58 95 0 0 0 0 4095 1 
i 1 92.62895 0.5257895 77 95 0 0 0 0 4095 1 
i 3 92.62895 0.5257895 58 95 0 0 0 0 4095 1 
i 2 92.63053 0.5257895 62 95 0 0 0 0 4095 1 
i 4 92.76316 0.3947368 46 95 0 0 0 0 4095 1 
i 1 93.15474 0.7894737 70 95 0 0 0 0 4095 1 
i 3 93.15474 0.7894737 50 95 0 0 0 0 4095 1 
i 2 93.15632 0.7894737 55 95 0 0 0 0 4095 1 
i 4 93.15789 0.7894737 46 95 0 0 0 0 4095 1 
i 1 93.94421 0.7894737 68 95 0 0 0 0 4095 1 
i 3 93.94421 0.7894737 48 95 0 0 0 0 4095 1 
i 2 93.94579 0.7894737 53 95 0 0 0 0 4095 1 
i 4 93.94737 0.7894737 34 95 0 0 0 0 4095 1 
i 1 94.73368 0.7894737 67 95 0 0 0 0 4095 1 
i 3 94.73368 0.7894737 46 95 0 0 0 0 4095 1 
i 2 94.73526 0.7894737 51 95 0 0 0 0 4095 1 
i 4 94.73684 1.184211 27 95 0 0 0 0 4095 1 
i 1 95.52316 2.368421 74 95 0 0 0 0 4095 1 
i 3 95.52316 2.368421 55 95 0 0 0 0 4095 1 
i 2 95.52474 2.368421 58 95 0 0 0 0 4095 1 
i 4 95.92105 0.3947368 39 95 0 0 0 0 4095 1 
i 4 96.31579 0.7894737 39 95 0 0 0 0 4095 1 
i 4 97.10526 0.7894737 34 95 0 0 0 0 4095 1 
i 1 97.89158 0.5257895 74 95 0 0 0 0 4095 1 
i 3 97.89158 0.5257895 55 95 0 0 0 0 4095 1 
i 2 97.89316 0.5257895 58 95 0 0 0 0 4095 1 
i 4 97.89474 1.184211 32 95 0 0 0 0 4095 1 
i 1 98.41737 0.5257895 72 95 0 0 0 0 4095 1 
i 3 98.41737 0.5257895 53 95 0 0 0 0 4095 1 
i 2 98.41895 0.5257895 56 95 0 0 0 0 4095 1 
i 1 98.94316 0.5257895 75 95 0 0 0 0 4095 1 
i 3 98.94316 0.5257895 56 95 0 0 0 0 4095 1 
i 2 98.94474 0.5257895 60 95 0 0 0 0 4095 1 
i 4 99.07895 0.3947368 44 95 0 0 0 0 4095 1 
i 1 99.46895 0.7894737 68 95 0 0 0 0 4095 1 
i 3 99.46895 0.7894737 48 95 0 0 0 0 4095 1 
i 2 99.47053 0.7894737 53 95 0 0 0 0 4095 1 
i 4 99.47368 0.7894737 44 95 0 0 0 0 4095 1 
i 1 100.2584 0.7894737 67 95 0 0 0 0 4095 1 
i 3 100.2584 0.7894737 46 95 0 0 0 0 4095 1 
i 2 100.26 0.7894737 51 95 0 0 0 0 4095 1 
i 4 100.2632 0.7894737 32 95 0 0 0 0 4095 1 
i 1 101.0479 0.7894737 65 95 0 0 0 0 4095 1 
i 3 101.0479 0.7894737 44 95 0 0 0 0 4095 1 
i 2 101.0495 0.7894737 50 95 0 0 0 0 4095 1 
i 4 101.0526 1.184211 26 95 0 0 0 0 4095 1 
i 1 101.8374 2.368421 72 95 0 0 0 0 4095 1 
i 3 101.8374 2.368421 53 95 0 0 0 0 4095 1 
i 2 101.8389 2.368421 56 95 0 0 0 0 4095 1 
i 4 102.2368 0.3947368 38 95 0 0 0 0 4095 1 
i 4 102.6316 0.7894737 38 95 0 0 0 0 4095 1 
i 4 103.4211 0.7894737 26 95 0 0 0 0 4095 1 
i 1 104.2058 0.5257895 72 95 0 0 0 0 4095 1 
i 3 104.2058 0.5257895 53 95 0 0 0 0 4095 1 
i 2 104.2074 0.5257895 56 95 0 0 0 0 4095 1 
i 4 104.2105 1.184211 31 95 0 0 0 0 4095 1 
i 1 104.7316 0.5257895 71 95 0 0 0 0 4095 1 
i 3 104.7316 0.5257895 53 95 0 0 0 0 4095 1 
i 2 104.7332 0.5257895 55 95 0 0 0 0 4095 1 
i 1 105.2574 0.5257895 74 95 0 0 0 0 4095 1 
i 3 105.2574 0.5257895 55 95 0 0 0 0 4095 1 
i 2 105.2589 0.5257895 59 95 0 0 0 0 4095 1 
i 4 105.3947 0.3947368 43 95 0 0 0 0 4095 1 
i 1 105.7832 0.7894737 67 95 0 0 0 0 4095 1 
i 3 105.7832 0.7894737 48 95 0 0 0 0 4095 1 
i 2 105.7847 0.7894737 51 95 0 0 0 0 4095 1 
i 4 105.7895 0.7894737 43 95 0 0 0 0 4095 1 
i 1 106.5726 0.7894737 65 95 0 0 0 0 4095 1 
i 3 106.5726 0.7894737 47 95 0 0 0 0 4095 1 
i 2 106.5742 0.7894737 50 95 0 0 0 0 4095 1 
i 4 106.5789 0.7894737 31 95 0 0 0 0 4095 1 
i 1 107.3621 1.184211 63 95 0 0 0 0 4095 1 
i 3 107.3621 1.184211 43 95 0 0 0 0 4095 1 
i 2 107.3637 1.184211 48 95 0 0 0 0 4095 1 
i 4 107.3684 1.184211 24 95 0 0 0 0 4095 1 
i 1 108.5463 1.184211 65 95 0 0 0 0 4095 1 
i 3 108.5463 1.184211 44 95 0 0 0 0 4095 1 
i 2 108.5479 1.184211 48 95 0 0 0 0 4095 1 
i 4 108.5526 0.3947368 24 95 0 0 0 0 4095 1 
i 4 108.9474 1.184211 26 95 0 0 0 0 4095 1 
i 1 109.7305 0.7894737 67 95 0 0 0 0 4095 1 
i 3 109.7305 0.7894737 43 95 0 0 0 0 4095 1 
i 2 109.7321 0.7894737 48 95 0 0 0 0 4095 1 
i 4 110.1316 0.3947368 26 95 0 0 0 0 4095 1 
i 1 110.52 3.157895 67 95 0 0 0 0 4095 1 
i 3 110.52 3.157895 46 95 0 0 0 0 4095 1 
i 2 110.5216 2.368421 49 95 0 0 0 0 4095 1 
i 4 110.5263 1.184211 27 95 0 0 0 0 4095 1 
i 4 111.7105 0.3947368 27 95 0 0 0 0 4095 1 
i 4 112.1053 1.184211 28 95 0 0 0 0 4095 1 
i 2 112.89 0.1973684 56 95 0 0 0 0 4095 1 
i 2 113.0874 0.1973684 55 95 0 0 0 0 4095 1 
i 2 113.2847 0.1973684 54 95 0 0 0 0 4095 1 
i 4 113.2895 0.3947368 28 95 0 0 0 0 4095 1 
i 2 113.4821 0.1973684 55 95 0 0 0 0 4095 1 
i 3 113.6779 0.3947368 44 32 0 0 0 0 4095 1 
i 2 113.6795 2.368421 63 48 0 0 0 0 4095 1 
i 4 113.6842 1.184211 29 32 0 0 0 0 4095 1 
i 3 114.0726 0.7894737 51 32 0 0 0 0 4095 1 
i 3 114.8621 0.3947368 44 32 0 0 0 0 4095 1 
i 4 114.8684 0.3947368 39 32 0 0 0 0 4095 1 
i 3 115.2568 1.578947 51 32 0 0 0 0 4095 1 
i 4 115.2632 0.7894737 39 32 0 0 0 0 4095 1 
i 2 116.0479 0.3947368 60 48 0 0 0 0 4095 1 
i 4 116.0526 0.7894737 29 32 0 0 0 0 4095 1 
i 2 116.4426 0.3947368 62 48 0 0 0 0 4095 1 
i 3 116.8358 0.3947368 44 32 0 0 0 0 4095 1 
i 2 116.8374 0.5257895 63 48 0 0 0 0 4095 1 
i 4 116.8421 1.184211 34 32 0 0 0 0 4095 1 
i 3 117.2305 0.7894737 53 32 0 0 0 0 4095 1 
i 2 117.3632 0.5257895 62 48 0 0 0 0 4095 1 
i 2 117.8889 0.5257895 65 48 0 0 0 0 4095 1 
i 3 118.02 0.3947368 44 32 0 0 0 0 4095 1 
i 4 118.0263 0.3947368 46 32 0 0 0 0 4095 1 
i 2 118.4147 0.7894737 58 48 0 0 0 0 4095 1 
i 3 118.4147 1.578947 50 32 0 0 0 0 4095 1 
i 4 118.4211 0.7894737 46 32 0 0 0 0 4095 1 
i 2 119.2042 0.7894737 56 48 0 0 0 0 4095 1 
i 4 119.2105 0.7894737 34 32 0 0 0 0 4095 1 
i 2 119.9937 0.7894737 55 48 0 0 0 0 4095 1 
i 3 119.9937 0.3947368 39 32 0 0 0 0 4095 1 
i 4 120 1.184211 27 32 0 0 0 0 4095 1 
i 3 120.3884 0.7894737 46 32 0 0 0 0 4095 1 
i 2 120.7832 2.368421 62 48 0 0 0 0 4095 1 
i 3 121.1779 0.3947368 43 32 0 0 0 0 4095 1 
i 4 121.1842 0.3947368 39 32 0 0 0 0 4095 1 
i 3 121.5726 1.578947 50 32 0 0 0 0 4095 1 
i 4 121.5789 0.7894737 39 32 0 0 0 0 4095 1 
i 4 122.3684 0.7894737 34 32 0 0 0 0 4095 1 
i 2 123.1516 0.5257895 62 48 0 0 0 0 4095 1 
i 3 123.1516 0.3947368 44 32 0 0 0 0 4095 1 
i 4 123.1579 1.184211 32 32 0 0 0 0 4095 1 
i 3 123.5463 0.7894737 51 32 0 0 0 0 4095 1 
i 2 123.6774 0.5257895 60 48 0 0 0 0 4095 1 
i 2 124.2032 0.5257895 63 48 0 0 0 0 4095 1 
i 3 124.3358 0.3947368 48 32 0 0 0 0 4095 1 
i 4 124.3421 0.3947368 44 32 0 0 0 0 4095 1 
i 2 124.7289 0.7894737 56 48 0 0 0 0 4095 1 
i 3 124.7305 0.7894737 53 32 0 0 0 0 4095 1 
i 4 124.7368 0.7894737 44 32 0 0 0 0 4095 1 
i 2 125.5184 0.7894737 55 48 0 0 0 0 4095 1 
i 3 125.52 0.7894737 51 32 0 0 0 0 4095 1 
i 4 125.5263 0.7894737 32 32 0 0 0 0 4095 1 
i 2 126.3079 3.157895 54 48 0 0 0 0 4095 1 
i 1 126.3095 3.157895 48 32 0 0 0 0 4095 1 
i 3 126.3095 3.157895 44 32 0 0 0 0 4095 1 
i 4 126.3158 1.184211 26 32 0 0 0 0 4095 1 
i 4 127.5 0.3947368 38 32 0 0 0 0 4095 1 
i 4 127.8947 0.7894737 38 32 0 0 0 0 4095 1 
i 4 128.6842 0.7894737 26 32 0 0 0 0 4095 1 
i 2 129.4658 2.368421 55 48 0 0 0 0 4095 1 
i 1 129.4674 3.157895 53 32 0 0 0 0 4095 1 
i 3 129.4674 3.157895 47 32 0 0 0 0 4095 1 
i 4 129.4737 1.184211 31 32 0 0 0 0 4095 1 
i 4 130.6579 0.3947368 43 32 0 0 0 0 4095 1 
i 4 131.0526 0.7894737 43 32 0 0 0 0 4095 1 
i 2 131.8342 0.3947368 55 48 0 0 0 0 4095 1 
i 4 131.8421 0.7894737 31 32 0 0 0 0 4095 1 
i 2 132.2289 0.3947368 55 48 0 0 0 0 4095 1 
i 2 132.6237 4.736842 60 48 0 0 0 0 4095 1 
i 1 132.6253 1.184211 55 32 0 0 0 0 4095 1 
i 3 132.6253 1.184211 51 32 0 0 0 0 4095 1 
i 4 132.6316 1.184211 24 32 0 0 0 0 4095 1 
i 1 133.8095 1.184211 56 32 0 0 0 0 4095 1 
i 3 133.8095 1.184211 51 32 0 0 0 0 4095 1 
i 4 133.8158 0.3947368 36 32 0 0 0 0 4095 1 
i 4 134.2105 0.7894737 36 32 0 0 0 0 4095 1 
i 1 134.9937 0.7894737 55 32 0 0 0 0 4095 1 
i 3 134.9937 0.7894737 51 32 0 0 0 0 4095 1 
i 4 135 0.7894737 24 32 0 0 0 0 4095 1 
i 1 135.7832 1.184211 58 32 0 0 0 0 4095 1 
i 3 135.7832 1.184211 51 32 0 0 0 0 4095 1 
i 4 135.7895 1.578947 24 32 0 0 0 0 4095 1 
i 1 136.9674 1.184211 56 32 0 0 0 0 4095 1 
i 3 136.9674 1.184211 51 32 0 0 0 0 4095 1 
i 1 138.1516 0.7894737 55 32 0 0 0 0 4095 1 
i 3 138.1516 0.7894737 51 32 0 0 0 0 4095 1 
i 4 138.1579 0.7894737 43 48 0 0 0 0 4095 1 
i 3 138.9411 1.184211 36 32 0 0 0 0 4095 1 
i 4 138.9474 3.552632 55 48 0 0 0 0 4095 1 
i 2 139.3342 0.3947368 43 32 0 0 0 0 4095 1 
i 2 139.7289 2.368421 44 32 0 0 0 0 4095 1 
i 1 140.1253 0.3947368 48 32 0 0 0 0 4095 1 
i 3 140.1253 0.3947368 48 32 0 0 0 0 4095 1 
i 1 140.52 0.3947368 50 32 0 0 0 0 4095 1 
i 3 140.52 0.7894737 48 32 0 0 0 0 4095 1 
i 1 140.9147 0.3947368 51 32 0 0 0 0 4095 1 
i 1 141.3095 0.7894737 53 32 0 0 0 0 4095 1 
i 3 141.3095 0.7894737 36 32 0 0 0 0 4095 1 
i 3 142.0989 1.184211 36 32 0 0 0 0 4095 1 
i 2 142.4921 0.3947368 43 32 0 0 0 0 4095 1 
i 4 142.5 0.3947368 53 48 0 0 0 0 4095 1 
i 2 142.8868 2.368421 44 32 0 0 0 0 4095 1 
i 4 142.8947 0.2628947 55 48 0 0 0 0 4095 1 
i 4 143.1576 0.2628947 53 48 0 0 0 0 4095 1 
i 1 143.2832 0.3947368 48 32 0 0 0 0 4095 1 
i 3 143.2832 0.3947368 48 32 0 0 0 0 4095 1 
i 4 143.4205 0.2628947 51 48 0 0 0 0 4095 1 
i 1 143.6779 0.3947368 50 32 0 0 0 0 4095 1 
i 3 143.6779 0.7894737 48 32 0 0 0 0 4095 1 
i 4 143.6834 1.184211 50 48 0 0 0 0 4095 1 
i 1 144.0726 0.3947368 51 32 0 0 0 0 4095 1 
i 1 144.4674 0.7894737 53 32 0 0 0 0 4095 1 
i 3 144.4674 0.7894737 36 32 0 0 0 0 4095 1 
i 4 144.8676 0.3947368 48 48 0 0 0 0 4095 1 
i 3 145.2568 1.184211 36 32 0 0 0 0 4095 1 
i 4 145.2624 3.552632 51 48 0 0 0 0 4095 1 
i 2 145.65 0.3947368 43 32 0 0 0 0 4095 1 
i 2 146.0447 2.368421 44 32 0 0 0 0 4095 1 
i 1 146.4411 0.3947368 48 32 0 0 0 0 4095 1 
i 3 146.4411 0.3947368 48 32 0 0 0 0 4095 1 
i 1 146.8358 0.3947368 50 32 0 0 0 0 4095 1 
i 3 146.8358 0.7894737 48 32 0 0 0 0 4095 1 
i 1 147.2305 0.3947368 51 32 0 0 0 0 4095 1 
i 1 147.6253 0.7894737 53 32 0 0 0 0 4095 1 
i 3 147.6253 0.7894737 36 32 0 0 0 0 4095 1 
i 3 148.4147 1.184211 36 32 0 0 0 0 4095 1 
i 2 148.8079 0.3947368 43 32 0 0 0 0 4095 1 
i 4 148.815 0.3947368 50 48 0 0 0 0 4095 1 
i 2 149.2026 2.368421 44 32 0 0 0 0 4095 1 
i 4 149.2097 0.3947368 53 48 0 0 0 0 4095 1 
i 1 149.5989 0.3947368 48 32 0 0 0 0 4095 1 
i 3 149.5989 0.3947368 48 32 0 0 0 0 4095 1 
i 4 149.6045 0.3947368 51 48 0 0 0 0 4095 1 
i 1 149.9937 0.3947368 50 32 0 0 0 0 4095 1 
i 3 149.9937 0.7894737 48 32 0 0 0 0 4095 1 
i 4 149.9992 0.3947368 50 48 0 0 0 0 4095 1 
i 1 150.3884 0.3947368 51 32 0 0 0 0 4095 1 
i 4 150.3939 0.7894737 48 48 0 0 0 0 4095 1 
i 1 150.7832 0.7894737 53 32 0 0 0 0 4095 1 
i 3 150.7832 0.7894737 36 32 0 0 0 0 4095 1 
i 4 151.1834 0.1973684 46 48 0 0 0 0 4095 1 
i 4 151.3808 0.1973684 44 48 0 0 0 0 4095 1 
i 1 151.5726 1.578947 56 32 0 0 0 0 4095 1 
i 3 151.5726 1.184211 41 32 0 0 0 0 4095 1 
i 4 151.5782 2.368421 48 48 0 0 0 0 4095 1 
i 2 151.9658 0.3947368 51 32 0 0 0 0 4095 1 
i 2 152.3605 0.3947368 50 32 0 0 0 0 4095 1 
i 2 152.7553 0.3947368 51 32 0 0 0 0 4095 1 
i 3 152.7568 0.3947368 41 32 0 0 0 0 4095 1 
i 1 153.1516 1.578947 56 32 0 0 0 0 4095 1 
i 3 153.1516 0.7894737 46 32 0 0 0 0 4095 1 
i 2 153.5447 0.3947368 51 32 0 0 0 0 4095 1 
i 2 153.9395 0.7894737 50 32 0 0 0 0 4095 1 
i 3 153.9411 0.7894737 46 32 0 0 0 0 4095 1 
i 4 153.9466 0.1973684 46 48 0 0 0 0 4095 1 
i 4 154.1439 0.1973684 48 48 0 0 0 0 4095 1 
i 4 154.3413 0.1973684 46 48 0 0 0 0 4095 1 
i 4 154.5387 0.1973684 44 48 0 0 0 0 4095 1 
i 1 154.7305 1.578947 55 32 0 0 0 0 4095 1 
i 3 154.7305 1.184211 39 32 0 0 0 0 4095 1 
i 4 154.7361 1.578947 46 48 0 0 0 0 4095 1 
i 2 155.1237 0.3947368 50 32 0 0 0 0 4095 1 
i 2 155.5184 0.3947368 48 32 0 0 0 0 4095 1 
i 2 155.9132 0.3947368 50 32 0 0 0 0 4095 1 
i 3 155.9147 0.3947368 39 32 0 0 0 0 4095 1 
i 1 156.3095 1.578947 55 32 0 0 0 0 4095 1 
i 3 156.3095 0.7894737 44 32 0 0 0 0 4095 1 
i 4 156.315 0.3947368 48 48 0 0 0 0 4095 1 
i 2 156.7026 0.3947368 50 32 0 0 0 0 4095 1 
i 4 156.7097 0.7894737 46 48 0 0 0 0 4095 1 
i 2 157.0974 0.3947368 48 32 0 0 0 0 4095 1 
i 3 157.0989 0.7894737 44 32 0 0 0 0 4095 1 
i 2 157.4921 0.3947368 50 32 0 0 0 0 4095 1 
i 4 157.4992 0.1973684 44 48 0 0 0 0 4095 1 
i 4 157.6966 0.1973684 43 48 0 0 0 0 4095 1 
i 1 157.8884 1.578947 53 32 0 0 0 0 4095 1 
i 3 157.8884 1.184211 38 32 0 0 0 0 4095 1 
i 4 157.8939 1.184211 44 48 0 0 0 0 4095 1 
i 2 158.2816 0.3947368 50 32 0 0 0 0 4095 1 
i 2 158.6763 0.3947368 48 32 0 0 0 0 4095 1 
i 2 159.0711 0.3947368 50 32 0 0 0 0 4095 1 
i 3 159.0726 0.3947368 50 32 0 0 0 0 4095 1 
i 4 159.0782 0.1973684 43 48 0 0 0 0 4095 1 
i 4 159.2755 0.1973684 41 48 0 0 0 0 4095 1 
i 1 159.4674 1.578947 54 32 0 0 0 0 4095 1 
i 3 159.4674 0.7894737 50 32 0 0 0 0 4095 1 
i 4 159.4729 0.7894737 39 48 0 0 0 0 4095 1 
i 2 159.8605 0.3947368 50 32 0 0 0 0 4095 1 
i 2 160.2553 0.3947368 48 32 0 0 0 0 4095 1 
i 3 160.2568 0.7894737 38 32 0 0 0 0 4095 1 
i 4 160.2624 0.7894737 38 48 0 0 0 0 4095 1 
i 2 160.65 0.3947368 50 32 0 0 0 0 4095 1 
i 1 161.0463 2.368421 55 32 0 0 0 0 4095 1 
i 3 161.0463 1.184211 43 32 0 0 0 0 4095 1 
i 4 161.0518 3.157895 43 48 0 0 0 0 4095 1 
i 2 161.4395 0.3947368 48 32 0 0 0 0 4095 1 
i 2 161.8342 0.3947368 50 32 0 0 0 0 4095 1 
i 2 162.2289 0.3947368 47 32 0 0 0 0 4095 1 
i 3 162.2305 0.3947368 44 32 0 0 0 0 4095 1 
i 2 162.6237 0.7894737 51 32 0 0 0 0 4095 1 
i 3 162.6253 0.3947368 43 32 0 0 0 0 4095 1 
i 3 163.02 0.3947368 41 32 0 0 0 0 4095 1 
i 2 163.4132 0.7894737 50 32 0 0 0 0 4095 1 
i 3 163.4147 0.3947368 39 32 0 0 0 0 4095 1 
i 1 163.6121 0.1973684 55 48 0 0 0 0 4095 1 
i 1 163.8095 0.1973684 55 48 0 0 0 0 4095 1 
i 3 163.8095 0.3947368 38 32 0 0 0 0 4095 1 
i 1 164.0068 0.1973684 56 48 0 0 0 0 4095 1 
i 1 164.2042 0.3947368 67 48 0 0 0 0 4095 1 
i 3 164.2042 0.3947368 36 32 0 0 0 0 4095 1 
i 4 164.2097 1.184211 24 48 0 0 0 0 4095 1 
i 1 164.5989 0.3947368 67 48 0 0 0 0 4095 1 
i 3 164.5989 0.3947368 43 32 0 0 0 0 4095 1 
i 1 164.9937 2.368421 67 48 0 0 0 0 4095 1 
i 3 164.9937 2.368421 44 32 0 0 0 0 4095 1 
i 2 165.3868 0.3947368 48 32 0 0 0 0 4095 1 
i 4 165.3939 0.3947368 36 48 0 0 0 0 4095 1 
i 2 165.7816 0.3947368 50 32 0 0 0 0 4095 1 
i 4 165.7887 0.7894737 36 48 0 0 0 0 4095 1 
i 2 166.1763 0.3947368 51 32 0 0 0 0 4095 1 
i 2 166.5711 0.7894737 53 32 0 0 0 0 4095 1 
i 4 166.5782 0.7894737 24 48 0 0 0 0 4095 1 
i 1 167.3621 0.5257895 67 48 0 0 0 0 4095 1 
i 4 167.3676 1.184211 24 48 0 0 0 0 4095 1 
i 3 167.7568 0.3947368 43 32 0 0 0 0 4095 1 
i 1 167.8879 0.5257895 65 48 0 0 0 0 4095 1 
i 3 168.1516 2.368421 44 32 0 0 0 0 4095 1 
i 1 168.4137 0.5257895 63 48 0 0 0 0 4095 1 
i 2 168.5447 0.3947368 48 32 0 0 0 0 4095 1 
i 4 168.5518 0.3947368 36 48 0 0 0 0 4095 1 
i 1 168.9395 0.5921053 62 48 0 0 0 0 4095 1 
i 2 168.9395 0.3947368 50 32 0 0 0 0 4095 1 
i 4 168.9466 0.7894737 36 48 0 0 0 0 4095 1 
i 2 169.3342 0.3947368 51 32 0 0 0 0 4095 1 
i 1 169.5316 0.1973684 60 48 0 0 0 0 4095 1 
i 1 169.7289 0.1973684 62 48 0 0 0 0 4095 1 
i 2 169.7289 0.7894737 53 32 0 0 0 0 4095 1 
i 4 169.7361 0.7894737 24 48 0 0 0 0 4095 1 
i 1 169.9263 0.1973684 60 48 0 0 0 0 4095 1 
i 1 170.1237 0.1973684 59 48 0 0 0 0 4095 1 
i 1 170.3211 0.1973684 60 48 0 0 0 0 4095 1 
i 1 170.5184 0.3947368 63 48 0 0 0 0 4095 1 
i 4 170.5255 1.184211 24 48 0 0 0 0 4095 1 
i 1 170.9132 0.3947368 63 48 0 0 0 0 4095 1 
i 3 170.9147 0.3947368 43 32 0 0 0 0 4095 1 
i 1 171.3079 2.368421 63 48 0 0 0 0 4095 1 
i 3 171.3095 2.368421 44 32 0 0 0 0 4095 1 
i 2 171.7026 0.3947368 48 32 0 0 0 0 4095 1 
i 4 171.7097 0.3947368 36 48 0 0 0 0 4095 1 
i 2 172.0974 0.3947368 50 32 0 0 0 0 4095 1 
i 4 172.1045 0.7894737 36 48 0 0 0 0 4095 1 
i 2 172.4921 0.3947368 51 32 0 0 0 0 4095 1 
i 2 172.8868 0.7894737 53 32 0 0 0 0 4095 1 
i 4 172.8939 0.7894737 24 48 0 0 0 0 4095 1 
i 4 173.6834 1.184211 24 48 0 0 0 0 4095 1 
i 1 174.0711 0.3947368 62 48 0 0 0 0 4095 1 
i 3 174.0726 0.3947368 43 32 0 0 0 0 4095 1 
i 1 174.4658 0.1973684 65 48 0 0 0 0 4095 1 
i 3 174.4674 2.368421 44 32 0 0 0 0 4095 1 
i 1 174.6632 0.1973684 63 48 0 0 0 0 4095 1 
i 1 174.8605 0.1973684 62 48 0 0 0 0 4095 1 
i 2 174.8605 0.3947368 48 32 0 0 0 0 4095 1 
i 4 174.8676 0.3947368 36 48 0 0 0 0 4095 1 
i 1 175.0579 0.1973684 60 48 0 0 0 0 4095 1 
i 1 175.2553 0.3947368 58 48 0 0 0 0 4095 1 
i 2 175.2553 0.3947368 50 32 0 0 0 0 4095 1 
i 4 175.2624 0.7894737 36 48 0 0 0 0 4095 1 
i 1 175.65 0.7894737 56 48 0 0 0 0 4095 1 
i 2 175.65 0.3947368 51 32 0 0 0 0 4095 1 
i 2 176.0447 0.7894737 53 32 0 0 0 0 4095 1 
i 4 176.0518 0.7894737 24 48 0 0 0 0 4095 1 
i 1 176.4395 0.1973684 55 48 0 0 0 0 4095 1 
i 1 176.6368 0.1973684 56 48 0 0 0 0 4095 1 
i 1 176.8342 0.3947368 60 48 0 0 0 0 4095 1 
i 2 176.8342 0.7894737 53 32 0 0 0 0 4095 1 
i 3 176.8358 1.184211 49 32 0 0 0 0 4095 1 
i 4 176.8413 1.184211 31 48 0 0 0 0 4095 1 
i 1 177.2289 1.184211 60 48 0 0 0 0 4095 1 
i 2 177.6237 0.7894737 55 32 0 0 0 0 4095 1 
i 3 178.02 1.184211 49 32 0 0 0 0 4095 1 
i 4 178.0255 0.3947368 43 48 0 0 0 0 4095 1 
i 1 178.4132 0.3947368 60 48 0 0 0 0 4095 1 
i 2 178.4132 0.7894737 52 32 0 0 0 0 4095 1 
i 4 178.4203 0.7894737 43 48 0 0 0 0 4095 1 
i 1 178.8079 0.3947368 58 48 0 0 0 0 4095 1 
i 1 179.2026 0.3947368 58 48 0 0 0 0 4095 1 
i 2 179.2026 0.7894737 53 32 0 0 0 0 4095 1 
i 3 179.2042 0.7894737 49 32 0 0 0 0 4095 1 
i 4 179.2097 0.7894737 31 48 0 0 0 0 4095 1 
i 1 179.5974 0.3947368 58 48 0 0 0 0 4095 1 
i 1 179.9921 0.3947368 56 48 0 0 0 0 4095 1 
i 2 179.9921 1.578947 56 32 0 0 0 0 4095 1 
i 3 179.9937 1.184211 49 32 0 0 0 0 4095 1 
i 4 179.9992 1.184211 24 48 0 0 0 0 4095 1 
i 1 180.3868 0.3947368 55 48 0 0 0 0 4095 1 
i 1 180.7816 0.7894737 55 48 0 0 0 0 4095 1 
i 3 181.1779 1.184211 49 32 0 0 0 0 4095 1 
i 4 181.1834 0.3947368 36 48 0 0 0 0 4095 1 
i 1 181.5711 0.3947368 53 48 0 0 0 0 4095 1 
i 2 181.5711 1.578947 55 32 0 0 0 0 4095 1 
i 4 181.5782 0.7894737 36 48 0 0 0 0 4095 1 
i 1 181.9658 0.7894737 53 48 0 0 0 0 4095 1 
i 3 182.3621 0.7894737 49 32 0 0 0 0 4095 1 
i 4 182.3676 0.7894737 24 48 0 0 0 0 4095 1 
i 1 182.7553 0.3947368 55 48 0 0 0 0 4095 1 
i 1 183.15 0.3947368 58 48 0 0 0 0 4095 1 
i 2 183.15 0.7894737 48 32 0 0 0 0 4095 1 
i 3 183.1516 1.184211 48 32 0 0 0 0 4095 1 
i 4 183.1571 1.184211 29 48 0 0 0 0 4095 1 
i 1 183.5447 0.1973684 56 48 0 0 0 0 4095 1 
i 1 183.7421 0.1973684 55 48 0 0 0 0 4095 1 
i 1 183.9395 2.368421 56 48 0 0 0 0 4095 1 
i 2 183.9395 0.7894737 50 32 0 0 0 0 4095 1 
i 3 184.3358 1.184211 48 32 0 0 0 0 4095 1 
i 4 184.3413 0.3947368 41 48 0 0 0 0 4095 1 
i 2 184.7289 0.7894737 51 32 0 0 0 0 4095 1 
i 4 184.7361 0.7894737 41 48 0 0 0 0 4095 1 
i 2 185.5184 0.7894737 53 32 0 0 0 0 4095 1 
i 3 185.52 0.7894737 48 32 0 0 0 0 4095 1 
i 4 185.5255 0.7894737 29 48 0 0 0 0 4095 1 
i 1 186.3079 1.578947 56 48 0 0 0 0 4095 1 
i 2 186.3079 1.184211 55 32 0 0 0 0 4095 1 
i 3 186.3095 1.184211 48 32 0 0 0 0 4095 1 
i 4 186.315 1.184211 27 48 0 0 0 0 4095 1 
i 2 187.4921 0.1973684 53 32 0 0 0 0 4095 1 
i 3 187.4937 1.184211 48 32 0 0 0 0 4095 1 
i 4 187.4992 0.3947368 39 48 0 0 0 0 4095 1 
i 2 187.6895 0.1973684 51 32 0 0 0 0 4095 1 
i 2 187.8868 0.7894737 50 32 0 0 0 0 4095 1 
i 4 187.8939 0.7894737 39 48 0 0 0 0 4095 1 
i 2 188.6763 0.7894737 48 32 0 0 0 0 4095 1 
i 3 188.6779 0.7894737 48 32 0 0 0 0 4095 1 
i 4 188.6834 0.7894737 27 48 0 0 0 0 4095 1 
i 1 188.8737 0.1973684 60 48 0 0 0 0 4095 1 
i 1 189.0711 0.1973684 65 48 0 0 0 0 4095 1 
i 1 189.2684 0.1973684 67 48 0 0 0 0 4095 1 
i 1 189.4658 1.184211 70 95 0 0 0 0 4095 1 
i 2 189.4658 0.7894737 53 79 0 0 0 0 4095 1 
i 3 189.4674 1.184211 48 79 0 0 0 0 4095 1 
i 4 189.4729 1.184211 26 79 0 0 0 0 4095 1 
i 2 190.2553 1.578947 55 79 0 0 0 0 4095 1 
i 1 190.65 0.1973684 68 95 0 0 0 0 4095 1 
i 3 190.6516 1.184211 48 79 0 0 0 0 4095 1 
i 4 190.6571 0.3947368 38 79 0 0 0 0 4095 1 
i 1 190.8474 0.1973684 67 95 0 0 0 0 4095 1 
i 1 191.0447 1.578947 68 95 0 0 0 0 4095 1 
i 4 191.0518 0.7894737 38 79 0 0 0 0 4095 1 
i 2 191.8342 0.7894737 53 79 0 0 0 0 4095 1 
i 3 191.8358 0.7894737 48 79 0 0 0 0 4095 1 
i 4 191.8413 0.7894737 26 79 0 0 0 0 4095 1 
i 2 192.6237 0.7894737 56 79 0 0 0 0 4095 1 
i 3 192.6253 1.184211 50 79 0 0 0 0 4095 1 
i 4 192.6308 1.184211 31 79 0 0 0 0 4095 1 
i 1 193.0184 0.3947368 67 95 0 0 0 0 4095 1 
i 1 193.4132 0.1973684 70 95 0 0 0 0 4095 1 
i 2 193.4132 1.578947 55 79 0 0 0 0 4095 1 
i 1 193.6105 0.1973684 68 95 0 0 0 0 4095 1 
i 1 193.8079 0.1973684 67 95 0 0 0 0 4095 1 
i 3 193.8095 1.184211 50 79 0 0 0 0 4095 1 
i 4 193.815 0.3947368 43 79 0 0 0 0 4095 1 
i 1 194.0053 0.1973684 65 95 0 0 0 0 4095 1 
i 1 194.2026 0.1973684 67 95 0 0 0 0 4095 1 
i 4 194.2097 0.7894737 43 79 0 0 0 0 4095 1 
i 1 194.4 0.1973684 65 95 0 0 0 0 4095 1 
i 1 194.5974 0.1973684 63 95 0 0 0 0 4095 1 
i 1 194.7947 0.1973684 62 95 0 0 0 0 4095 1 
i 1 194.9921 0.1973684 60 95 0 0 0 0 4095 1 
i 2 194.9921 0.7894737 53 79 0 0 0 0 4095 1 
i 3 194.9937 0.7894737 46 79 0 0 0 0 4095 1 
i 4 194.9992 0.7894737 38 79 0 0 0 0 4095 1 
i 1 195.1895 0.1973684 62 95 0 0 0 0 4095 1 
i 1 195.3868 0.1973684 63 95 0 0 0 0 4095 1 
i 1 195.5842 0.1973684 65 95 0 0 0 0 4095 1 
i 1 195.7816 0.3947368 67 95 0 0 0 0 4095 1 
i 2 195.7816 0.7894737 51 79 0 0 0 0 4095 1 
i 3 195.7832 1.184211 43 79 0 0 0 0 4095 1 
i 4 195.7887 1.184211 36 79 0 0 0 0 4095 1 
i 1 196.1763 0.3947368 60 95 0 0 0 0 4095 1 
i 1 196.5711 2.368421 60 95 0 0 0 0 4095 1 
i 2 196.5711 1.578947 53 79 0 0 0 0 4095 1 
i 3 196.9674 1.184211 43 79 0 0 0 0 4095 1 
i 4 196.9729 0.3947368 48 79 0 0 0 0 4095 1 
i 4 197.3676 0.7894737 48 79 0 0 0 0 4095 1 
i 2 198.15 0.7894737 51 79 0 0 0 0 4095 1 
i 3 198.1516 0.7894737 43 79 0 0 0 0 4095 1 
i 4 198.1571 0.7894737 36 79 0 0 0 0 4095 1 
i 1 198.9395 1.578947 60 95 0 0 0 0 4095 1 
i 2 198.9395 0.7894737 55 79 0 0 0 0 4095 1 
i 3 198.9411 1.184211 48 79 0 0 0 0 4095 1 
i 4 198.9466 1.184211 34 79 0 0 0 0 4095 1 
i 2 199.7289 1.578947 53 79 0 0 0 0 4095 1 
i 3 200.1253 1.184211 48 79 0 0 0 0 4095 1 
i 4 200.1308 0.3947368 46 79 0 0 0 0 4095 1 
i 1 200.5184 0.3947368 65 95 0 0 0 0 4095 1 
i 4 200.5255 0.7894737 46 79 0 0 0 0 4095 1 
i 1 200.9132 0.7894737 63 95 0 0 0 0 4095 1 
i 2 201.3079 0.7894737 51 79 0 0 0 0 4095 1 
i 3 201.3095 0.7894737 48 79 0 0 0 0 4095 1 
i 4 201.315 0.7894737 34 79 0 0 0 0 4095 1 
i 1 201.7026 0.1973684 62 95 0 0 0 0 4095 1 
i 1 201.9 0.1973684 60 95 0 0 0 0 4095 1 
i 1 202.0974 2.368421 66 95 0 0 0 0 4095 1 
i 2 202.0974 0.7894737 50 79 0 0 0 0 4095 1 
i 3 202.0989 1.184211 54 79 0 0 0 0 4095 1 
i 4 202.1045 1.184211 33 79 0 0 0 0 4095 1 
i 2 202.8868 1.578947 51 79 0 0 0 0 4095 1 
i 3 203.2832 1.184211 54 79 0 0 0 0 4095 1 
i 4 203.2887 0.3947368 45 79 0 0 0 0 4095 1 
i 4 203.6834 0.7894737 45 79 0 0 0 0 4095 1 
i 1 204.4658 0.3947368 66 95 0 0 0 0 4095 1 
i 2 204.4658 0.7894737 50 79 0 0 0 0 4095 1 
i 3 204.4674 0.7894737 54 79 0 0 0 0 4095 1 
i 4 204.4729 0.7894737 33 79 0 0 0 0 4095 1 
i 1 204.8605 0.3947368 66 95 0 0 0 0 4095 1 
i 1 205.2553 2.368421 67 95 0 0 0 0 4095 1 
i 2 205.2553 0.7894737 50 79 0 0 0 0 4095 1 
i 3 205.2568 1.184211 53 79 0 0 0 0 4095 1 
i 4 205.2624 1.184211 31 79 0 0 0 0 4095 1 
i 2 206.0447 1.578947 51 79 0 0 0 0 4095 1 
i 3 206.4411 1.184211 53 79 0 0 0 0 4095 1 
i 4 206.4466 0.3947368 43 79 0 0 0 0 4095 1 
i 4 206.8413 0.7894737 43 79 0 0 0 0 4095 1 
i 1 207.6237 0.7894737 79 95 0 0 0 0 4095 1 
i 2 207.6237 0.7894737 50 79 0 0 0 0 4095 1 
i 3 207.6253 0.7894737 53 79 0 0 0 0 4095 1 
i 4 207.6308 0.7894737 31 79 0 0 0 0 4095 1 
i 1 208.4132 3.157895 84 32 0 0 0 0 4095 1 
i 4 208.4203 2.368421 24 48 0 0 0 0 4095 1 
i 3 208.8095 0.3947368 43 48 0 0 0 0 4095 1 
i 3 209.2042 2.368421 44 48 0 0 0 0 4095 1 
i 2 209.5974 0.3947368 48 48 0 0 0 0 4095 1 
i 2 209.9921 0.3947368 50 48 0 0 0 0 4095 1 
i 2 210.3868 0.3947368 51 48 0 0 0 0 4095 1 
i 2 210.7816 0.3947368 53 48 0 0 0 0 4095 1 
i 4 210.7887 0.7894737 36 48 0 0 0 0 4095 1 
i 2 211.1763 0.3947368 55 48 0 0 0 0 4095 1 
i 1 211.5711 3.157895 84 32 0 0 0 0 4095 1 
i 4 211.5782 2.368421 34 48 0 0 0 0 4095 1 
i 3 211.9674 0.3947368 43 48 0 0 0 0 4095 1 
i 3 212.3621 2.368421 44 48 0 0 0 0 4095 1 
i 2 212.7553 0.3947368 48 48 0 0 0 0 4095 1 
i 2 213.15 0.3947368 50 48 0 0 0 0 4095 1 
i 2 213.5447 0.3947368 51 48 0 0 0 0 4095 1 
i 2 213.9395 0.3947368 53 48 0 0 0 0 4095 1 
i 4 213.9466 0.7894737 34 48 0 0 0 0 4095 1 
i 2 214.3342 0.3947368 55 48 0 0 0 0 4095 1 
i 1 214.7289 3.157895 84 32 0 0 0 0 4095 1 
i 4 214.7361 2.368421 32 48 0 0 0 0 4095 1 
i 3 215.1253 0.3947368 43 48 0 0 0 0 4095 1 
i 3 215.52 2.368421 44 48 0 0 0 0 4095 1 
i 2 215.9132 0.3947368 48 48 0 0 0 0 4095 1 
i 2 216.3079 0.3947368 50 48 0 0 0 0 4095 1 
i 2 216.7026 0.3947368 51 48 0 0 0 0 4095 1 
i 2 217.0974 0.3947368 53 48 0 0 0 0 4095 1 
i 4 217.1045 0.7894737 32 48 0 0 0 0 4095 1 
i 2 217.4921 0.3947368 55 48 0 0 0 0 4095 1 
i 1 217.8868 3.157895 84 32 0 0 0 0 4095 1 
i 4 217.8939 2.368421 31 48 0 0 0 0 4095 1 
i 3 218.2832 0.3947368 43 48 0 0 0 0 4095 1 
i 3 218.6779 2.368421 44 48 0 0 0 0 4095 1 
i 2 219.0711 0.3947368 48 48 0 0 0 0 4095 1 
i 2 219.4658 0.3947368 50 48 0 0 0 0 4095 1 
i 2 219.8605 0.3947368 51 48 0 0 0 0 4095 1 
i 2 220.2553 0.3947368 53 48 0 0 0 0 4095 1 
i 4 220.2624 0.7894737 31 48 0 0 0 0 4095 1 
i 2 220.65 0.3947368 55 48 0 0 0 0 4095 1 
i 1 221.0447 1.578947 84 32 0 0 0 0 4095 1 
i 2 221.0447 3.157895 51 32 0 0 0 0 4095 1 
i 3 221.0463 3.157895 43 32 0 0 0 0 4095 1 
i 4 221.0518 3.157895 24 32 0 0 0 0 4095 1 
i 1 222.6237 1.578947 50 32 0 0 0 0 4095 1 

e 5
</CsScore>
</CsoundSynthesizer>

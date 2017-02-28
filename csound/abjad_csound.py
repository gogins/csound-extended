'''
C S O U N D   W I T H   A B J A D

Michael Gogins
17 February 2016

This script demontrates how to use Python to integrate Abjad from
http://projectabjad.org/ (for algorithmic scoring) with Csound from
http://csound.github.io/ (for audio rendering).

The MIDI file contains the following tracks mapped to Csound instruments:

Reading MIDI file 'abjad_csound.mid'...
 Track 0
 Track name: control track
 Track 1
 Track name: Bell Staff: [FMBell]
 Track 2
 Track name: First Violin Staff:First Violin Voice [Yi String]
 Track 3
 Track name: Second Violin Staff:Second Violin Voice [Yi String]
 Track 4
 Track name: Viola Staff:Viola Voice [Yi String]
 Track 5
 Track name: Cello Staff:Cello Voice [Yi String]
 Track 6
 Track name: Bass Staff:Bass Voice [Droner]

'''
print __doc__
import abjad
import csnd6

csd = '''
<CsoundSynthesizer>
<CsOptions>
csound -m167 --midi-key=4 --midi-velocity=5 -Fabjad_csound.mid -odac
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 300000000

massign 1, 1
massign 2, 5
massign 3, 5
massign 4, 5
massign 5, 5
massign 6, 6

connect "FMBell", "outleft", "ReverbLeft", "inleft"
connect "FMBell", "outright", "ReverbRight", "inright"
connect "Blower", "outleft", "ReverbLeft", "inleft"
connect "Blower", "outright", "ReverbRight", "inright"
connect "Bower", "outleft", "ReverbLeft", "inleft"
connect "Bower", "outright", "ReverbRight", "inright"
connect "Buzzer", "outleft", "ReverbLeft", "inleft"
connect "Buzzer", "outright", "ReverbRight", "inright"
connect "Droner", "outleft", "ReverbLeft", "inleft"
connect "Droner", "outright", "ReverbRight", "inright"
connect "Harpsichord", "outleft", "Reverb2Left", "inleft"
connect "Harpsichord", "outright", "Reverb2Right", "inright"
connect "Phaser", "outleft", "ReverbLeft", "inleft"
connect "Phaser", "outright", "ReverbRight", "inright"
connect "Sweeper", "outleft", "ReverbLeft", "inleft"
connect "Sweeper", "outright", "ReverbRight", "inright"
connect "Shiner", "outleft", "ReverbLeft", "inleft"
connect "Shiner", "outright", "ReverbRight", "inright"
connect "YiString", "outleft", "ReverbLeft", "inleft"
connect "YiString", "outright", "ReverbRight", "inright"
connect "YiString", "chorusleft", "SolinaChorus", "inleft"
connect "YiString", "chorusright", "SolinaChorus", "inright"
connect "SolinaChorus", "outleft", "ReverbLeft", "inleft"
connect "SolinaChorus", "outright", "ReverbRight", "inright"
connect "ReverbLeft", "outleft", "MasterOutput", "inleft"
connect "ReverbRight", "outright", "MasterOutput", "inright"
connect "Reverb2Left", "outleft", "MasterOutput", "inleft"
connect "Reverb2Right", "outright", "MasterOutput", "inright"

alwayson "SolinaChorus"
alwayson "ReverbLeft"
alwayson "ReverbRight"
alwayson "Reverb2Left"
alwayson "Reverb2Right"
alwayson "ParametricEQ"
alwayson "MasterOutput"

gk_overlap init .25

gaSendM,gaSendL,gaSendR init 0

gk_FMBell_level init -12
instr FMBell
insno = p1
itime = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p1 / 6
idepth = p8
iheight = p9
ipcs = p10
ihomogeneity = p11
iattack = .002
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
kHz = cpsmidinn(ikey)
kamp ampmidid p5, 6, 1
kfreq = 880
kc1 = 1
kc2 = 12
kvdepth = 0.005
kvrate = 6
asignal fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = asignal * adeclick
aleft, aright pan2 asignal, ipan
kgain = ampdb(gk_FMBell_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "FMBell      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Harpsichord_level init .25
gk_Harpsichord_pick init .75
gk_Harpsichord_reflection init .5
gk_Harpsichord_pluck init .75
instr Harpsichord
insno 		 = p1
itime 		 = p2
mididefault 100, p3
iduration 		 = p3
ikey 		 = p4
ivelocity = p5
iphase = p6
ipan = p7
idepth = p8
iheight = p9
ipcs = p10
ihomogeneity = p11
gk_Harpsichord_pan = .5
iattack = .005
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
iHz = cpsmidinn(ikey)
kHz = k(iHz)
iamplitude = ampdb(ivelocity) * 10
aenvelope 	 transeg 1.0, 20.0, -10.0, 0.05
;apluck 	 pluck 1, kHz, iHz, 0, 1
;apluck 	 pluck 1, kHz, iHz, 0, 6
k_amplitude = 1
apluck wgpluck2 i(gk_Harpsichord_pluck), k_amplitude, iHz, gk_Harpsichord_pick, gk_Harpsichord_reflection
iharptable 	 ftgenonce 0, 0, 65536, 7, -1, 1024, 1, 1024, -1
aharp 	 poscil 1, kHz, iharptable
aharp2 	 balance apluck, aharp
asignal	= (apluck + aharp2) * iamplitude * aenvelope * gk_Harpsichord_level
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = asignal * adeclick
aleft, aright pan2 asignal, ipan
kgain = ampdb(gk_Harpsichord_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Harpsichord i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_YiString_level init 16
gk_YiString_reverb_send init .5
gk_YiString_chorus_send init .5
instr Bower
 //////////////////////////////////////////////
 // Original by Steven Yi.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
insno = p1
istart = p2
mididefault p3, 100
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p1 / 6
iamp ampmidid p5, 10, 1
iattack = 0.05
idecay = 0.1
isustain = p3
p3 = iattack + isustain + idecay
aenvelope transegr 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;ampenv = madsr:a(1, 0.1, 0.95, 0.5)
asignal = vco2(1, ihertz)
asignal = moogladder(asignal, 6000, 0.1)
asignal *= aenvelope * iamp
aleft, aright pan2 asignal, ipan
kgain = ampdb(gk_YiString_level)
outleta "outleft", aleft * kgain * gk_YiString_reverb_send
outleta "outright", aright * kgain * gk_YiString_reverb_send
outleta "chorusleft", aleft * kgain * gk_YiString_chorus_send
outleta "chorusright", aright * kgain * gk_YiString_chorus_send
prints "YiString    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
kstatus, kchan, kdata1, kdata2 midiin
printf " YiString status %4d channel %4d data1 %4d data2 %4d\\n", kstatus, kstatus, kchan, kdata1, kdata2
endin

gk_Bower_level init 0.5
gk_Bower_pressure init 0.25
instr Bower
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 100
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 30
isine ftgenonce 0,0,65536,10,1
aSig wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,isine,iminfreq
aleft, aright pan2 aSig / 7, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Bower_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Bower       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 1/3
gk_Phaser_index1 init 1
gk_Phaser_index2 init 0.0125
gk_Phaser_level init 0.5
instr Phaser
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 8
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0,0,65536,10,1
khertz = ihertz
ifunction1 = isine
ifunction2 = isine
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, khertz, ifunction1, ifunction2
aleft, aright pan2 a1+a2, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft * kenvelope
aright = adamping * aright * kenvelope
kgain = ampdb(gk_Phaser_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Phaser      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_YiString_level init -12
gk_YiString_reverb_send init .5
gk_YiString_chorus_send init .5
instr YiString
 //////////////////////////////////////////////
 // Original by Steven Yi.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
insno = p1
istart = p2
iduration = p3
ikey = p4
mididefault 100, p3
ivelocity = p5
iphase = p6
ipan = p7
iamp ampmidid p5, 10, 1
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;ampenv = madsr:a(1, 0.1, 0.95, 0.5)
asignal = vco2(1, ihertz)
asignal = moogladder(asignal, 6000, 0.1)
asignal *= aenvelope
icount active p1
asignal /= icount
aleft, aright pan2 asignal, ipan
kgain = ampdb(gk_YiString_level)
outleta "outleft", aleft * kgain * gk_YiString_reverb_send
outleta "outright", aright * kgain * gk_YiString_reverb_send
outleta "chorusleft", aleft * kgain * gk_YiString_chorus_send
outleta "chorusright", aright * kgain * gk_YiString_chorus_send
prints "YiString    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Droner_partial1 init .1
gk_Droner_partial2 init .5
gk_Droner_partial3 init .1
gk_Droner_partial4 init .1
gk_Droner_partial5 init .2
gk_Droner_level init -3
instr Droner
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iamp ampmidid p5, 10, 1
iphase = p6
ipan = p1 / 6
k1 init .5
k2 init .05
k3 init .1
k4 init .2
k5 init .1
k6 init .05
k7 init .1
k8 init 0
k9 init 0
k10 init 0
k3 = gk_Droner_partial1
k4 = gk_Droner_partial2
k5 = gk_Droner_partial3
k6 = gk_Droner_partial4
k7 = gk_Droner_partial5
kwaveform init 0
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transegr 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0, 0, 65536, 10, 1, 0, .02
if kwaveform == 0 then
asignal poscil3 1, ihertz, isine
endif
if kwaveform == 1 then
asignal vco2 1, ihertz, 8 ; integrated saw
endif
if kwaveform == 2 then
asignal vco2 1, ihertz, 12 ; triangle
endif
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
asignal = asignal * kenvelope
icount active p1
asignal /= icount
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Droner_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Droner      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Sweeper_britel init 0
gk_Sweeper_briteh init 2.9
gk_Sweeper_britels init .2 / 3
gk_Sweeper_britehs init 2.5 / 2
gk_Sweeper_level init 0.5
instr Sweeper
//////////////////////////////////////////////
// Original by Iain McCurdy.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity)
gisine ftgenonce 0, 0, 65536, 10, 1
gioctfn ftgenonce 0, 0, 65536, -19, 1, 0.5, 270, 0.5
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gisine
kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gisine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gisine
a1,a2 pan2 a1, kpan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope
aleft = a1*kenv*.02
aright = a2*kenv*.02
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Sweeper_level) / 3
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Sweeper     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Buzzer_Harmonics init 15
gk_Buzzer_level init .5
instr Buzzer
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
isine ftgenonce 0, 0, 65536, 10, 1
gk_Harmonics = gk_Buzzer_Harmonics
asignal buzz kenvelope, ihertz, gk_Harmonics, isine
asignal = asignal * 3
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gk_DistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Buzzer_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Buzzer      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Shiner_level init 0.5
instr Shiner
insno = p1
istart = p2
mididefault 100, p3
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
gk_Harmonics = 1 * 20
asignal vco2 kenvelope * 4, ihertz, 12
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Shiner_level) * .5
;printks2 "master gain:", kgain
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Shiner      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Blower_grainDensity init 150
gk_Blower_grainDuration init 0.2
gk_Blower_grainAmplitudeRange init 100
gk_Blower_grainFrequencyRange init .033
gk_Blower_level init 0.5
instr Blower
 //////////////////////////////////////////////
 // Original by Hans Mikelson.
 // Adapted by Michael Gogins.
 //////////////////////////////////////////////
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
i_phase = p6
i_pan = p6
i_depth = p8
i_height = p9
i_pitchclassset = p10
i_homogeneity = p11
ifrequency = cpsmidinn(i_midikey)
iamplitude = ampdb(i_midivelocity) / 200
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; f1 0 65536 1 "hahaha.aif" 0 4 0
 ; f2 0 1024 7 0 224 1 800 0
 ; f3 0 8192 7 1 8192 -1
 ; f4 0 1024 7 0 512 1 512 0
 ; f5 0 1024 10 1 .3 .1 0 .2 .02 0 .1 .04
 ; f6 0 1024 10 1 0 .5 0 .33 0 .25 0 .2 0 .167
 ; a0 14 50
 ; p1 p2 p3 p4 p5 p6 p7 p8 p9 p10
 ; Start Dur Amp Freq GrTab WinTab FqcRng Dens Fade
 ; i1 0.0 6.5 700 9.00 5 4 .210 200 1.8
 ; i1 3.2 3.5 800 7.08 . 4 .042 100 0.8
 ; i1 5.1 5.2 600 7.10 . 4 .0320 100 0.9
 ; i1 7.2 6.6 900 8.03 . 4 .021 150 1.6
 ; i1 21.3 4.5 1000 9.00 . 4 .031 150 1.2
 ; i1 26.5 13.5 1100 6.09 . 4 .121 150 1.5
 ; i1 30.7 9.3 900 8.05 . 4 .014 150 2.5
 ; i1 34.2 8.8 700 10.02 . 4 .14 150 1.6
igrtab ftgenonce 0, 0, 65536, 10, 1, .3, .1, 0, .2, .02, 0, .1, .04
iwintab ftgenonce 0, 0, 65536, 10, 1, 0, .5, 0, .33, 0, .25, 0, .2, 0, .167
iHz = ifrequency
ihertz = iHz
ip4 = iamplitude
ip5 = iHz
ip6 = igrtab
ip7 = iwintab
ip8 = 0.033
ip8 = .002
ip9 = 150
ip9 = 100
ip10 = 1.6
ip10 = 3
idur = p3
iamp ampmidid p5, 10, 1
ifqc = iHz ; cpspch(p5)
igrtab = ip6
iwintab = ip7
ifrng = ip8
idens = ip9
ifade = ip10
igdur = 0.2
iattack = i(gk_overlap) * 2
idecay = i(gk_overlap) * 2
isustain = p3 - i(gk_overlap) * 2
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
; kamp linseg 0, ifade, 1, idur - 2 * ifade, 1, ifade, 0
kamp = kenvelope
; Amp Fqc Dense AmpOff PitchOff GrDur GrTable WinTable MaxGrDur
aoutl grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aoutr grain ip4, ifqc, gk_Blower_grainDensity, gk_Blower_grainAmplitudeRange, gk_Blower_grainFrequencyRange, gk_Blower_grainDuration, igrtab, iwintab, 5
aleft = aoutl * kamp * iamplitude
aright = aoutr * kamp * iamplitude
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Blower_level) / 2
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Blower      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

/**
 * Solina Chorus, based on Solina String Ensemble Chorus Module

 J. Haible: Triple Chorus
 http://jhaible.com/legacy/triple_chorus/triple_chorus.html

 Hugo Portillo: Solina-V String Ensemble
 http://www.native-instruments.com/en/reaktor-community/reaktor-user-library/entry/show/4525/

 Parabola tabled shape borrowed from Iain McCurdy delayStereoChorus.csd:
 http://iainmccurdy.org/CsoundRealtimeExamples/Delays/delayStereoChorus.csd

 Author: Steven Yi
 Date: 2016.05.22
 Adapted by Michael Gogins
*/
gi_solina_parabola ftgen 0, 0, 65537, 19, 0.5, 1, 180, 1
; 3 sine wave LFOs, 120 degrees out of phase
opcode sol_lfo_3, aaa, kk
kfreq, kamp xin
aphs phasor kfreq
; Funny: Function syntax does not work in this context.
a0 tablei aphs, gi_solina_parabola, 1, 0, 1
a120 tablei aphs, gi_solina_parabola, 1, 0.333, 1
a240 tablei aphs, gi_solina_parabola, 1, -0.333, 1
xout (a0 * kamp), (a120 * kamp), (a240 * kamp)
endop

opcode solina_chorus, a, akkkk
aLeft, klfo_freq1, klfo_amp1, klfo_freq2, klfo_amp2 xin
imax = 100
;; slow lfo
as1, as2, as3 sol_lfo_3 klfo_freq1, klfo_amp1
;; fast lfo
af1, af2, af3 sol_lfo_3 klfo_freq2, klfo_amp2
at1 = limit(as1 + af1 + 5, 0.0, imax)
at2 = limit(as2 + af2 + 5, 0.0, imax)
at3 = limit(as3 + af3 + 5, 0.0, imax)
a1 vdelay3 aLeft, at1, imax
a2 vdelay3 aLeft, at2, imax
a3 vdelay3 aLeft, at2, imax
xout (a1 + a2 + a3) / 3
endop

gk_SolinaChorus_chorus_lfo1_hz init .18
gk_SolinaChorus_chorus_lfo1_amp init .6
gk_SolinaChorus_chorus_lfo2_hz init 6
gk_SolinaChorus_chorus_lfo2_amp init .2
instr SolinaChorus
aleft inleta "inleft"
aright inleta "inright"
aleft solina_chorus aleft, gk_SolinaChorus_chorus_lfo1_hz, gk_SolinaChorus_chorus_lfo1_amp, gk_SolinaChorus_chorus_lfo2_hz, gk_SolinaChorus_chorus_lfo2_amp
aright solina_chorus aright, gk_SolinaChorus_chorus_lfo1_hz, gk_SolinaChorus_chorus_lfo1_amp, gk_SolinaChorus_chorus_lfo2_hz, gk_SolinaChorus_chorus_lfo2_amp
outleta "outleft", aleft
outleta "outright", aright
endin

gk_Reverb_Feedback init 0.78
gk_Delay_Modulation init 0.02

instr ReverbLeft
aleft init 0
azero init 0
aleft inleta "inleft"
aleft, aright reverbsc aleft, azero, gk_Reverb_Feedback, 15000.
outleta "outleft", aleft
prints "ReverbLeft  i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr ReverbRight
aleft init 0
azero init 0
aright inleta "inright"
aleft, aright reverbsc azero, aright, gk_Reverb_Feedback, 15000.0
outleta "outright", aright
prints "ReverbRight i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_Reverb2_Feedback init 0.82
gk_Delay2_Modulation init 0.2

instr Reverb2Left
aleft init 0
azero init 0
aleft inleta "inleft"
aleft, aright reverbsc aleft, azero, gk_Reverb2_Feedback, 15000.
outleta "outleft", aleft
prints "Reverb2Left i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

instr Reverb2Right
aleft init 0
azero init 0
aright inleta "inright"
aleft, aright reverbsc azero, aright, gk_Reverb2_Feedback, 15000.0
outleta "outright", aright
;;;;;;;;;;;;;;;;;;;;i
prints "Reverb2Righ i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_CenterHz init 200
gk_Gain init 1
gk_Q init 0.7071067 ; sqrt(.5)
instr ParametricEQ
aleft inleta "inleft"
aright inleta "inright"
aleft pareq aleft, gk_CenterHz, ampdb(gk_Gain), gk_Q, 0
aright pareq aright, gk_CenterHz, ampdb(gk_Gain), gk_Q, 0
outleta "outleft", aleft
outleta "outright", aright
;;;;;;;;;;;;;;;;;;;;i
prints "ParametrcEQ i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

gk_MasterOutput_level init .5
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
kgain = ampdb(gk_MasterOutput_level)
; printks2 "Master gain: %f\\n", kgain
iamp init 1
iattack init .01
idecay init 10
isustain = 2400 - (iattack + idecay)
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
aright butterlp aright, 16000
aleft butterlp aleft, 16000
outs aleft * kgain * aenvelope, aright * kgain * aenvelope
Sfilename init "abjad_csound.wav"
prints sprintf("Output filename: %s\\n", Sfilename)
; We want something that will play on my phone.
fout Sfilename, 18, aleft, aright
prints "MasterOutpt i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>
'''
csound = csnd6.Csound()
lilypond_file = abjad.demos.part.make_part_lilypond_file()
abjad.show(lilypond_file)
abjad.topleveltools.persist(lilypond_file).as_midi('abjad_csound.mid')
result = csound.CompileCsdText(csd)
print 'csound.CompileCsdText(csd): %d' % result
csound.Start();
csound.Perform();


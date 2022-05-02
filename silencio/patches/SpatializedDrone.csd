<CsoundSynthesizer>
<CsLicense>
Copyright (C) 2013 by Michael Gogins.
All rights reserved.
</CsLicense>
<CsOptions>
-odac -m195 -d 
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 100000

#define USE_SPATIALIZATION #1#

#include "Spatialize1.inc"

gk_BformatDecoder_SpeakerRig init 1
gk_Spatialize_SpeakerRigRadius init 5.0
gk_SpatialReverb_ReverbDecay init 0.96
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation init 4.0
gk_LocalReverbByDistance_Wet init 0.5
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz init 20000
gk_LocalReverbByDistance_RandomDelayModulation init 1.0
gk_Spatialize_Verbose init 0

gkslider1 init 0
gkslider2 init 0
gkslider3 init 0
gkslider4 init 0
gkslider5 init 0

connect "Droner", "outbformat", "BformatDecoder", "inbformat"
connect "Droner", "out", "SpatialReverb", "in"
connect "Blower", "outbformat", "BformatDecoder", "inbformat"
connect "Blower", "out", "SpatialReverb", "in"
connect "Buzzer", "outbformat", "BformatDecoder", "inbformat"
connect "Buzzer", "out", "SpatialReverb", "in"
connect "SpatialReverb", "outbformat", "BformatDecoder", "inbformat"

alwayson "SpatialReverb"
alwayson "BformatDecoder"
alwayson "Controls"

gk_Droner_Attack init 10
gk_Droner_1 init 0.5
gk_Droner_2 init 0.05
gk_Droner_3 init 0.1
gk_Droner_4 init 0.2
gk_Droner_5 init 0.1
gk_Droner_6 init 0.05
gk_Droner_7 init 0.1
gk_Droner_8 init 0.0
gk_Droner_9 init 0.0
gk_Droner_10 init 0.05
gk_Droner_x init 0.01
gk_Droner_y init 0.01
gk_Droner_z init 0.01
instr Droner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
k1 = gk_Droner_1
k2 = gk_Droner_2
k3 = gk_Droner_3
k4 = gk_Droner_4
k5 = gk_Droner_5
k6 = gk_Droner_6
k7 = gk_Droner_7
k8 = gk_Droner_8
k9 = gk_Droner_9
k10 = gk_Droner_10
iamp = ampdb(ivelocity)
iattack = i(gk_Droner_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0, 0, 65536, 10, 1, 0, .02
asignal poscil3 1, ihertz, isine
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
asignal = asignal * aenvelope * 10
absignal[] init 16
kx jspline 12, 1/5, 1/20
ky jspline 12, 1/5, 1/20
kz jspline 12, 1/5, 1/20
kx = kx - 6
ky = ky - 6
kz = kz - 6
absignal, asend Spatialize asignal, gk_Droner_x + kx, gk_Droner_y + ky, gk_Droner_z + kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Droner  i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %9.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, asignal
endin

gk_Bower_Attack = 10
instr Bower
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 200
iattack = i(gk_Bower_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
; krat rspline 0.006,0.988,0.1,0.4
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 20
isine ftgenonce 0,0,65536,10,1
asignal wgbow kamp,kfreq,kpres,krat,kvibf,kvibamp,isine,iminfreq
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, gk_Droner_x, gk_Droner_y, gk_Droner_z
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Bower  i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kx, ky, kz, asignal
endin

gk_Phaser_Attack init 10
gkratio1 init 1
gkratio2 init 1/3
gkindex1 init 1
gkindex2 init 0.0125
instr Phaser
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 8
iattack = i(gk_Phaser_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0,0,65536,10,1
khertz = ihertz
ifunction1 = isine
ifunction2 = isine
a1,a2 crosspm gkratio1, gkratio2, gkindex1, gkindex2, khertz, ifunction1, ifunction2
asignal = a1 + a2
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Phaser i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, downsamp(asignal)
endin

gk_Sweeper_Attack init 10
gkbritel init 0
gkbriteh init 2.9
gkbritels init .2 / 3
gkbritehs init 2.5 / 2
instr Sweeper
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
print insno, istart, iduration, ikey, ivelocity, iphase, ipan
iamp = ampdb(ivelocity)
gisine ftgenonce 0, 0, 65536, 10, 1
gioctfn ftgenonce 0, 0, 65536, -19, 1, 0.5, 270, 0.5
iattack = i(gk_Sweeper_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gisine
kbrite rspline gkbritel, gkbriteh, gkbritels, gkbritehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
;a1 hsboscil kamp, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
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
asignal = aleft + aright
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Sweeper i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kx, ky, kz, downsamp(asignal)
endin

gk_Buzzer_Attack init 10
instr Buzzer
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_Buzzer_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
isine ftgenonce 0, 0, 65536, 10, 1
gkHarmonics = gkslider1 * 20
asignal buzz aenvelope, ihertz, gkHarmonics, isine
asignal = asignal * 3
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
absignal[] init 16
kx jspline 6, 1/5, 1/20
ky jspline 6, 1/5, 1/20
kz jspline 6, 1/5, 1/20
absignal, asend Spatialize asignal, kx, ky, kz
outleta "out", asend
outletv "outbformat", absignal
kelapsed timeinsts
printks "Buzzer  i %7.2f t %7.2f [%7.2f] d %7.2f k %7.2f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, p4, ihertz, p5, kx, ky, kz, downsamp(asignal)
endin

gk_Shiner_Attack init 10
instr Shiner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_Shiner_Attack)
idecay = iattack
isustain = p3 - iattack
xtratim iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
print insno, istart, iduration, ikey, ihertz, ivelocity, iamp, iphase, ipan
;asignal gbuzz kenvelope, ihertz, 3, gkFirstHarmonic, gkDistortFactor, gisine
gkHarmonics = gkslider1 * 20
;asignal buzz kenvelope, ihertz, gkHarmonics, gisine
;asignal = asignal
asignal vco2 kenvelope * 4, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gkDistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
asignal = aleft + aright
kfronttoback jspline 6, 1/5, 1/20
klefttoright jspline 6, 1/5, 1/20
kbottomtotop jspline 6, 1/5, 1/20
absignal[] init 16
absignal, aspatialreverbsend Spatialize asignal, kfronttoback, klefttoright, kbottomtotop
outletv "outbformat", absignal
outleta "out", aspatialreverbsend
kelapsed timeinsts
printks "Shiner i %7.2f t %7.2f [%7.2f] d %7.2f f %7.2f v %7.2f kx %7.2f ky %7.2f kz %7.2f A %7.2f\n", 1.0, p1, p2, kelapsed, p3, ihertz, p5, kfronttoback, klefttoright, kbottomtotop, downsamp(asignal)
endin

#include "Blower.inc"

instr Controls
gk_Droner_1 invalue "gk_Droner_1"
gk_Droner_2 invalue "gk_Droner_2"
gk_Droner_3 invalue "gk_Droner_3"
gk_Droner_4 invalue "gk_Droner_4"
gk_Droner_5 invalue "gk_Droner_5"
gk_Droner_x invalue "gk_Droner_x"
gk_Droner_y invalue "gk_Droner_y"
gk_Droner_z invalue "gk_Droner_z"
gk_LocalReverbByDistance_Wet invalue "gk_LocalReverbByDistance_Wet"
gk_LocalReverbByDistance_ReverbDecay invalue "gk_LocalReverbByDistance_ReverbDecay"
gk_LocalReverbByDistance_CutoffHz invalue "gk_LocalReverbByDistance_CutoffHz"
gk_SpatialReverb_ReverbDecay invalue "gk_SpatialReverb_ReverbDecay"
gk_SpatialReverb_CutoffHz invalue "gk_SpatialReverb_CutoffHz"
gk_SpatialReverb_Gain invalue "gk_SpatialReverb_Gain"
gk_BformatDecoder_MasterLevel invalue "gk_BformatDecoder_MasterLevel"
endin

</CsInstruments>
<CsScore>

t 0 27

; p4 is just intonation in MIDI key numbers (numerator can be 0):
; [ ((numerator / denominator) * 12) + (octave * 12) + 24 ] 

; C E B
i "Droner"    0 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Droner"    0 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Droner"    0 60 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; C Ab E B
i "Blower"   30 30 [ (( 5 /  8) * 12) + (1 * 12) + 24 ] 60
; G F# B
i "Buzzer"   60 60 [ (( 2 /  3) * 12) + (1 * 12) + 24 ] 60
i "Buzzer"   60 60 [ ((32 / 45) * 12) + (2 * 12) + 24 ] 60
i "Buzzer"   60 30 [ ((15 / 28) * 12) + (3 * 12) + 24 ] 60
; G F B
i "Buzzer"   90 30 [ (( 3 /  4) * 12) + (3 * 12) + 24 ] 60
; C E B
i "Droner"  120 60 [ (( 0 /  1) * 12) + (1 * 12) + 24 ] 60
i "Droner"  120 60 [ (( 4 /  5) * 12) + (2 * 12) + 24 ] 60
i "Droner"  120 30 [ ((15 / 28) * 12) + (2 * 12) + 24 ] 60
i "Droner"  150 30 [ (( 0 /  1) * 12) + (2 * 12) + 24 ] 60
e 10.0
</CsScore>
</CsoundSynthesizer>






<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>0</x>
 <y>0</y>
 <width>832</width>
 <height>877</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="background">
  <r>0</r>
  <g>170</g>
  <b>0</b>
 </bgcolor>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_y</objectName>
  <x>20</x>
  <y>253</y>
  <width>349</width>
  <height>155</height>
  <uuid>{7bdad897-657c-4a54-9206-709cc8534ebb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>gk_Droner_x</objectName2>
  <xMin>-6.00000000</xMin>
  <xMax>6.00000000</xMax>
  <yMin>-6.00000000</yMin>
  <yMax>6.00000000</yMax>
  <xValue>-0.56733524</xValue>
  <yValue>0.58064516</yValue>
  <type>crosshair</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScope">
  <objectName/>
  <x>483</x>
  <y>371</y>
  <width>348</width>
  <height>132</height>
  <uuid>{f79d2676-2f95-4b20-94c4-f63775334ac9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <value>-255.00000000</value>
  <type>scope</type>
  <zoomx>2.00000000</zoomx>
  <zoomy>1.00000000</zoomy>
  <dispx>1.00000000</dispx>
  <dispy>1.00000000</dispy>
  <mode>0.00000000</mode>
 </bsbObject>
 <bsbObject version="2" type="BSBConsole">
  <objectName/>
  <x>16</x>
  <y>511</y>
  <width>844</width>
  <height>364</height>
  <uuid>{fe872149-a57c-419e-98c2-cfc0dd18e1fb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <font>Courier</font>
  <fontsize>8</fontsize>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_z</objectName>
  <x>20</x>
  <y>428</y>
  <width>270</width>
  <height>25</height>
  <uuid>{aa8392eb-79c0-4610-bf14-ea2357aab517}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>-6.00000000</xMin>
  <xMax>6.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.80000000</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_z</objectName>
  <x>289</x>
  <y>429</y>
  <width>80</width>
  <height>24</height>
  <uuid>{cd2891d2-5104-4b94-9c28-2ab734840e7f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.80000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>406</y>
  <width>349</width>
  <height>25</height>
  <uuid>{4103435a-f4f8-4b88-b559-d0ca1e8f58c3}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_z</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_SpatialReverb_ReverbDecay</objectName>
  <x>482</x>
  <y>192</y>
  <width>270</width>
  <height>25</height>
  <uuid>{9a2a4e0f-00d9-4de1-a941-fb023adbbd97}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.97533333</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_SpatialReverb_ReverbDecay</objectName>
  <x>751</x>
  <y>192</y>
  <width>80</width>
  <height>25</height>
  <uuid>{8f25f397-460d-4293-902e-9abc27bd95b8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.97533333</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>169</y>
  <width>349</width>
  <height>24</height>
  <uuid>{b1e5a1b4-3490-49a9-989b-ea7ac34ce9cd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_SpatialReverb_ReverbDecay</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_x</objectName>
  <x>212</x>
  <y>405</y>
  <width>80</width>
  <height>25</height>
  <uuid>{7877a6f7-d172-48ef-b2af-de8b4987fd93}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.58064516</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_y</objectName>
  <x>288</x>
  <y>406</y>
  <width>80</width>
  <height>25</height>
  <uuid>{68c0a806-f3a7-4228-b321-85d3ef2d0783}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>-0.56733524</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_LocalReverbByDistance_ReverbDecay</objectName>
  <x>482</x>
  <y>85</y>
  <width>270</width>
  <height>25</height>
  <uuid>{3d6d9dc1-4343-4ceb-a51e-e2e588cff359}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.93500000</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_LocalReverbByDistance_ReverbDecay</objectName>
  <x>751</x>
  <y>85</y>
  <width>80</width>
  <height>25</height>
  <uuid>{32a8f573-8bb7-407a-8c34-cffa4e6fff3b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.93500000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>62</y>
  <width>349</width>
  <height>24</height>
  <uuid>{f34af6b3-12c3-4fab-83d3-b3148ed7c1a6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_LocalReverbByDistance_ReverbDecay</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_LocalReverbByDistance_Wet</objectName>
  <x>482</x>
  <y>37</y>
  <width>270</width>
  <height>25</height>
  <uuid>{3aa72420-152e-470c-aa4c-95885cb010ec}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.86533333</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_LocalReverbByDistance_Wet</objectName>
  <x>751</x>
  <y>37</y>
  <width>80</width>
  <height>25</height>
  <uuid>{109c1b1a-4da5-43df-80f4-fc1e2bdf9bc7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.86533333</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>14</y>
  <width>349</width>
  <height>24</height>
  <uuid>{6afbc441-8e23-4039-aa30-787bb67d93f5}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_LocalReverbByDistance_Wet</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_1</objectName>
  <x>20</x>
  <y>38</y>
  <width>270</width>
  <height>25</height>
  <uuid>{fe8cb528-8134-4bdb-8804-6581614c35ac}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.21633333</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_1</objectName>
  <x>289</x>
  <y>38</y>
  <width>80</width>
  <height>25</height>
  <uuid>{0737efe1-e444-4dbc-87cc-141501eb8a3c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.21633333</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>15</y>
  <width>349</width>
  <height>24</height>
  <uuid>{af4a3421-8718-4d7b-b4cc-70c0af6a1444}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_1</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_2</objectName>
  <x>20</x>
  <y>85</y>
  <width>270</width>
  <height>25</height>
  <uuid>{8069fa75-04aa-4bbe-b78a-e42120a6c25d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.61966667</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_2</objectName>
  <x>289</x>
  <y>85</y>
  <width>80</width>
  <height>25</height>
  <uuid>{c33cf760-5c27-4935-8522-3f3cf84bfb69}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.61966667</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>62</y>
  <width>349</width>
  <height>24</height>
  <uuid>{499d6eb3-db5b-4a58-802f-f26dfcf6f4aa}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_2</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_3</objectName>
  <x>20</x>
  <y>133</y>
  <width>270</width>
  <height>25</height>
  <uuid>{5c5d8ca3-5a13-4448-89b7-737f1ad95e1b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.17600000</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_3</objectName>
  <x>289</x>
  <y>133</y>
  <width>80</width>
  <height>25</height>
  <uuid>{fed4fcba-a17d-4d91-b932-4b7855de7e2c}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.17600000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>110</y>
  <width>349</width>
  <height>24</height>
  <uuid>{0b0dd86b-1f8a-4f47-8ec7-910c1a8d4324}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_3</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_4</objectName>
  <x>20</x>
  <y>180</y>
  <width>270</width>
  <height>25</height>
  <uuid>{c0664f50-fa03-4b54-85a2-f50217a8af0f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.48400000</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_4</objectName>
  <x>289</x>
  <y>180</y>
  <width>80</width>
  <height>25</height>
  <uuid>{e640ffe8-cf32-4d3e-94d0-070ddcc0472b}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.48400000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>157</y>
  <width>349</width>
  <height>24</height>
  <uuid>{36cb819e-ccd9-4be9-9930-cd0ffc5b30be}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_4</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_Droner_5</objectName>
  <x>20</x>
  <y>228</y>
  <width>270</width>
  <height>25</height>
  <uuid>{781dbb98-0719-40ee-aa4a-f5f655150ebd}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>0.99000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.88000000</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_Droner_5</objectName>
  <x>289</x>
  <y>228</y>
  <width>80</width>
  <height>25</height>
  <uuid>{b30731e2-955e-4b49-80d3-143318303c84}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.88000000</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>20</x>
  <y>205</y>
  <width>349</width>
  <height>24</height>
  <uuid>{d3448446-2456-4239-bf6a-577fbd3f9228}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_Droner_5</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_SpatialReverb_CutoffHz</objectName>
  <x>482</x>
  <y>240</y>
  <width>270</width>
  <height>25</height>
  <uuid>{d2a8c363-a000-4422-8b4a-dce41b8e2981}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>20000.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>16962.96296296</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_SpatialReverb_CutoffHz</objectName>
  <x>751</x>
  <y>240</y>
  <width>80</width>
  <height>25</height>
  <uuid>{6e62f3d4-0de1-4c13-8576-35135b270f79}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>16962.96296296</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>216</y>
  <width>349</width>
  <height>24</height>
  <uuid>{dda5eb2f-47d1-4135-89b9-ddab858cfbb9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_SpatialReverb_CutoffHz</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_LocalReverbByDistance_CutoffHz</objectName>
  <x>482</x>
  <y>133</y>
  <width>270</width>
  <height>25</height>
  <uuid>{12072b36-d040-44c4-a5af-1d8b321d6742}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>20000.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>7851.85185185</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_LocalReverbByDistance_CutoffHz</objectName>
  <x>751</x>
  <y>133</y>
  <width>80</width>
  <height>25</height>
  <uuid>{4755eb2e-fd0a-46b6-a5fb-dde4c2b243a7}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>7851.85185185</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>110</y>
  <width>349</width>
  <height>24</height>
  <uuid>{29879be3-8eb3-4dbf-a190-d108f29d6fbc}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_LocalReverbByDistance_CutoffHz</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_BformatDecoder_MasterLevel</objectName>
  <x>483</x>
  <y>346</y>
  <width>270</width>
  <height>25</height>
  <uuid>{a58cc880-abec-4b04-8139-a73ffbcfbcc1}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>-60.00000000</xMin>
  <xMax>60.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>3.55555556</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_BformatDecoder_MasterLevel</objectName>
  <x>752</x>
  <y>346</y>
  <width>80</width>
  <height>25</height>
  <uuid>{a4c9442b-5953-417d-92ad-ce6b24e206f6}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>3.55555556</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>483</x>
  <y>322</y>
  <width>349</width>
  <height>24</height>
  <uuid>{7ed71425-f916-4456-8afc-9743cf11ca44}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_BformatDecoder_MasterLevel</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject version="2" type="BSBController">
  <objectName>gk_SpatialReverb_Gain</objectName>
  <x>482</x>
  <y>286</y>
  <width>270</width>
  <height>25</height>
  <uuid>{4701a0f9-a25f-46b4-a73c-8a713ecb8c6e}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>meter20</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>1.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>1.00000000</yMax>
  <xValue>0.42592593</xValue>
  <yValue>0.58750000</yValue>
  <type>fill</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable group="0" mode="both">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject version="2" type="BSBScrollNumber">
  <objectName>gk_SpatialReverb_Gain</objectName>
  <x>751</x>
  <y>286</y>
  <width>80</width>
  <height>25</height>
  <uuid>{15c7969d-aca2-4470-bced-e853cbabbe84}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <alignment>center</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <color>
   <r>0</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
  <value>0.42592593</value>
  <resolution>0.00100000</resolution>
  <minimum>-999999999999.00000000</minimum>
  <maximum>999999999999.00000000</maximum>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
  <randomizable group="0">false</randomizable>
  <mouseControl act=""/>
 </bsbObject>
 <bsbObject version="2" type="BSBLabel">
  <objectName/>
  <x>482</x>
  <y>264</y>
  <width>349</width>
  <height>24</height>
  <uuid>{2cfd3b12-f3eb-4a94-8298-1f0433d9f986}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>gk_SpatialReverb_Gain</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>255</r>
   <g>255</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>0</r>
   <g>85</g>
   <b>0</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>

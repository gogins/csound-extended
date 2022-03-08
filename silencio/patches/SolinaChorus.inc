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
a0   tablei aphs, gi_solina_parabola, 1, 0, 1
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
af1, af2, af3  sol_lfo_3 klfo_freq2, klfo_amp2
at1 = limit(as1 + af1 + 5, 0.0, imax)
at2 = limit(as2 + af2 + 5, 0.0, imax)
at3 = limit(as3 + af3 + 5, 0.0, imax)
a1 vdelay3 aLeft, at1, imax
a2 vdelay3 aLeft, at2, imax
a3 vdelay3 aLeft, at2, imax
xout (a1 + a2 + a3) / 3
endop

gk_SolinaChorus_chorus_lfo1_hz chnexport "gk_SolinaChorus_chorus_lfo1_hz", 3 ;  .18
gk_SolinaChorus_chorus_lfo1_amp chnexport "gk_SolinaChorus_chorus_lfo1_amp", 3 ;  .6
gk_SolinaChorus_chorus_lfo2_hz chnexport "gk_SolinaChorus_chorus_lfo2_hz", 3 ;  6
gk_SolinaChorus_chorus_lfo2_amp chnexport "gk_SolinaChorus_chorus_lfo2_amp", 3 ;  .2

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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


gk_Reverb_feedback init 0.975
gi_Reverb_delay_modulation init 0.875
gk_Reverb_frequency_cutoff init 15000
instr ReverbSC
adummy init 0
azero init 0
aleft init 0
aleft_zero init 0
aright init 0
aright_zero init 0
aleft inleta "inleft"
aright inleta "inright"
; aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]
aleft, adummy  reverbsc aleft, azero,  gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
adummy, aright reverbsc azero, aright, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
outleta "outleft", aleft
outleta "outright", aright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

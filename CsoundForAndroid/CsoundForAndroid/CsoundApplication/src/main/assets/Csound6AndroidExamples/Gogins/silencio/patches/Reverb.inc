gk_Reverb_feedback init 0.975
gi_Reverb_delay_modulation init 0.875
gk_Reverb_frequency_cutoff init 15000
instr Reverb
aleft init 0
aright init 0
aleft inleta "inleft"
aright inleta "inright"
; aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]
aleft, aright reverbsc aleft, aright, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
outleta "outleft", aleft
outleta "outright", aright
prints "Reverb         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endin


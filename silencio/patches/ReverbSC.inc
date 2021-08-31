gk_Reverb_feedback chnexport "gk_Reverb_feedback", 3
gk_Reverb_wet chnexport "gk_Reverb_wet", 3
gi_Reverb_delay_modulation chnexport "gi_Reverb_delay_modulation", 3
gk_Reverb_frequency_cutoff chnexport "gk_Reverb_frequency_cutoff", 3

gk_Reverb_feedback init 0.875
gk_Reverb_wet init 0.5
gi_Reverb_delay_modulation init 0.0075
gk_Reverb_frequency_cutoff init 15000

instr ReverbSC
gk_Reverb_dry = 1.0 - gk_Reverb_wet
aleftin init 0
arightin init 0
aleftout init 0
arightout init 0
aleftin inleta "inleft"
arightin inleta "inright"
aleftout, arightout reverbsc aleftin, arightin, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
aleftoutmix = aleftin * gk_Reverb_dry + aleftout * gk_Reverb_wet
arightoutmix = arightin * gk_Reverb_dry + arightout * gk_Reverb_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

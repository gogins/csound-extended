gk_Compressor_threshhold chnexport "gk_Compressor_threshhold", 3
gi_Compressor_comp1 chnexport "gi_Compressor_comp1", 3
gi_Compressor_comp2 chnexport "gi_Compressor_comp2", 3
gi_Compressor_rtime chnexport "gi_Compressor_rtime", 3
gi_Compressor_ftime chnexport "gi_Compressor_ftime", 3

gk_Compressor_threshhold init 2500
gi_Compressor_comp1 init .5
gi_Compressor_comp2 init .63
gi_Compressor_rtime init .1
gi_Compressor_ftime init .1

instr Compressor
; Author: Michael Gogins
aleftin inleta "inleft"
arightin inleta "inright"
aoutleft dam aleftin, gk_Compressor_threshhold, gi_Compressor_comp1, gi_Compressor_comp2, gi_Compressor_rtime, gi_Compressor_ftime
aoutright dam arightin, gk_Compressor_threshhold, gi_Compressor_comp1, gi_Compressor_comp2, gi_Compressor_rtime, gi_Compressor_ftime
outleta "outleft", aoutleft
outleta "outright", aoutright
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


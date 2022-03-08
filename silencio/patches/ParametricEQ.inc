gk_ParametricEQ_Frequency chnexport "gk_ParametricEQ_Frequency", 3 ;  200
gk_ParametricEQ_Gain chnexport "gk_ParametricEQ_Gain", 3 ;  1
gk_ParametricEQ_Q chnexport "gk_ParametricEQ_Q", 3 ;  0.7071067 ; sqrt(.5)
gi_ParametricEQ_Mode chnexport "gi_ParametricEQ_Mode", 3 ;  0

gk_ParametricEQ_Frequency init 200
gk_ParametricEQ_Gain init 1
gk_ParametricEQ_Q init 0.7071067 ; sqrt(.5)
gi_ParametricEQ_Mode init 0

instr ParametricEQ
aleft inleta "inleft"
aright inleta "inright"
aleft  pareq aleft,  gk_ParametricEQ_Frequency, gk_ParametricEQ_Gain, gk_ParametricEQ_Q, gi_ParametricEQ_Mode
aright pareq aright, gk_ParametricEQ_Frequency, gk_ParametricEQ_Gain, gk_ParametricEQ_Q, gi_ParametricEQ_Mode
outleta "outleft", aleft
outleta "outright", aright
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

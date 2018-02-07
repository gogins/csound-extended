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
prints "ParametricEQ   i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin

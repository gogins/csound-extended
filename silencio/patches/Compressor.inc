                instr Compressor
                ; Author: Michael Gogins
aleftin         inleta      "inleft"
arightin        inleta      "inright"
kthreshold      =           25000
icomp1          =           0.5
icomp2          =           0.763
irtime          =           0.1
iftime          =           0.1
aoutleft        dam         aleftin, kthreshold, icomp1, icomp2, irtime, iftime
aoutright       dam         arightin, kthreshold, icomp1, icomp2, irtime, iftime
                outleta     "outleft", aoutleft
                outleta     "outright", aoutright
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
                endin


                instr Compressor
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ; Author: Michael Gogins
aleftin         inleta      "leftin"
arightin        inleta      "rightin"
kthreshold      =           25000
icomp1          =           0.5
icomp2          =           0.763
irtime          =           0.1
iftime          =           0.1
aoutleft        dam         aleftin, kthreshold, icomp1, icomp2, irtime, iftime
aoutright       dam         arightin, kthreshold, icomp1, icomp2, irtime, iftime
                outleta     "outleft", aoutleft 
                outleta     "outright", aoutright 
                endin


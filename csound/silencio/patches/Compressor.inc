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
aleftout        dam         aleftin, kthreshold, icomp1, icomp2, irtime, iftime
arightout       dam         arightin, kthreshold, icomp1, icomp2, irtime, iftime
                outleta     "leftout", aleftout 
                outleta     "rightout", arightout 
                endin


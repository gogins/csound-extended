                        instr FMBell
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Authors: John ffitch, Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 10.0
kc1                     =                       5
kc2                     =                       5
kvdepth                 =                       0.025
kvrate                  =                       5.1
icosine                 ftgenonce               0, 0, 65536,    11,     1 
ifn1                    =                       icosine
ifn2                    =                       icosine
ifn3                    =                       icosine
ifn4                    =                       icosine
ivfn                    =                       icosine
aout	                fmbell	                i_amplitude, iHz, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn
aenv                    transeg                 0.0, .001, -6, 1.0, 9, -6, 0
aout                    =                       aout * aenv
aleft, aright		    Pan			            p7, aout
p3, aleft, aright	    Declick			        0.001, p3, .5, aleft, aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin

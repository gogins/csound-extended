                        instr Guitar
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 12.0
acomp                   pluck                   i_amplitude, 440.0, 440.0, 0, 1, .1
iHz                     init                    i(kHz)
iHz2                    =                       iHz / 2.0
asig                    pluck                   i_amplitude, kHz, iHz2, 0, 1, .1
aenvelope               transeg                 1.0, 20.0, -5.0, 0.25
af1                     reson                   asig, 110, 80
af2                     reson                   asig, 220, 100
af3                     reson                   asig, 440, 80
asignal1                balance                 0.6 * af1 + af2 + 0.6 * af3 + 0.4 * asig, acomp
asignal2                =                       asignal1 * aenvelope
asignal                 dcblock2                asignal2
asignal                 =                       asignal * i_amplitude
aleft, aright		    Pan			            p7, asignal
p3, aleft, aright	    Declick			        0.001, p3, 0.05, aleft, aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin   

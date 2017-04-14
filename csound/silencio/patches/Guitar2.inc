                        instr Guitar2
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 20.0
iattack                 =                       0.002
isustain                =                       p3
irelease                =                       0.05
asigcomp                pluck                   1.0, 440, 440, 0, 1
asig                    pluck                   1.0, iHz, iHz, 0, 1
af1                     reson                   asig, 110, 80
af2                     reson                   asig, 220, 100
af3                     reson                   asig, 440, 80
aout                    balance                 0.6 * af1 + af2 + 0.6 * af3 + 0.4 * asig, asigcomp
aexp                    expseg                  1.0, iattack, 2.0, isustain, 1.0, irelease, 1.0
aenv                    =                       aexp - 1.0
asignal1                =                       aout * aenv,
asignal                 dcblock2                aout
aleft, aright		    Pan			            p7, asignal * i_amplitude
p3, aleft, aright	    Declick			        0.002, p3, 0.05, aleft, aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin

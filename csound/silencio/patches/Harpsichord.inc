                        instr Harpsichord
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; James Kelley
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 11.0
aenvelope               transeg                 1.0, 20.0, -10.0, 0.05
apluck                  pluck                   iamplitude, kHz, iHz, 0, 1
iharptable              ftgenonce               0, 0, 65537,  7, -1, 1024, 1, 1024, -1
aharp                   poscil                  1, kHz, iharptable
aharp2                  balance                 apluck, aharp
asignal			        =			            (apluck + aharp2) * iamplitude * aenvelope
aleft, aright		    Pan			            p7, asignal
p3, aleft, aright	    Declick			        0.005, p3, 0.3, aleft, aright
outleta                 "leftout", aleft
outleta                 "rightout", aright
                        endin

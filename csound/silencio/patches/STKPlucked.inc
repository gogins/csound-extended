                        instr STKPlucked
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 20.0
asignal 		        STKPlucked 		        iHz, 1.0
aleft, aright		    Pan			            p7, asignal * iamplitude
p3, aleft, aright	    Declick			        0.002, p3, 0.05, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin

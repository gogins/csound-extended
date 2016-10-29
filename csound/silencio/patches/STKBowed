                        instr STKBowed
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 27.0
                                                ; Controllers: 
                                                ;   1  Vibrato Gain
                                                ;   2  Bow Pressure
                                                ;   4  Bow Position
                                                ;  11  Vibrato Frequency
                                                ; 128  Volume 
asignal 		        STKBowed 		        iHz, 1.0, 1, 1.8, 2, 120.0, 4, 50.0, 11, 20.0
aleft, aright		    Pan			            p7, asignal * iamplitude
p3, aleft, aright	    Declick			        0.03, p3, 0.05, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin


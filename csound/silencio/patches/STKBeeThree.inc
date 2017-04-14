                        instr STKBeeThree
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Authors: Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 34.0
asignal 		        STKBeeThree 		    iHz, 1.0, 1, 1.5, 2, 4.8, 4, 2.1
; ares                  phaser1                 asig, kfreq, kord, kfeedback [, iskip]
aphased                 phaser1                 asignal, 8000, 16, .2, .9
aleft, aright		    Pan			            p7, (asignal + aphased) * i_amplitude
p3, aleft, aright	    Declick			        0.003, p3, .05, aleft, aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin   

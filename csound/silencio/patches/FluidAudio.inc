                        instr FluidAudio
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
ijunk			        = 			            p1 + p2 + p3 + p4 + p5
iHz,kHz,iamplitude,idB  NoteOn                  p4, 127, p5
aleft, aright   	    fluidOut		        giFluidsynth
aleft			        = 			            iamplitude * aleft
aright			        =			            iamplitude * aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin


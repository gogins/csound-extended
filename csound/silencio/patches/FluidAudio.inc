                        instr FluidAudio
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
ijunk			        = 			            p1 + p2 + p3 + p4 + p5
iHz,kHz,i_amplitude,idB  NoteOn                  p4, 127, p5
aleft, aright   	    fluidOut		        giFluidsynth
aleft			        = 			            i_amplitude * aleft
aright			        =			            i_amplitude * aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin


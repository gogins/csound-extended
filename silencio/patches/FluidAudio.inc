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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
                        endin


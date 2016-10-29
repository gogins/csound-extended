                        instr FluidSteinwayNote
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 0
                        ; Use channel assigned in fluidload.
ichannel		        =			            0
ioffset			        =			            ((sr / 44100) - 1) * 12
ikey	 		        = 			            p4 - ioffset
ikey 			        =			            p4
                        fluidNote		        giFluidsynth, ichannel, ikey, p5
                        endin


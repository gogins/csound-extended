#ifdef ENABLE_PIANOTEQ
                        instr PianoteqAudio
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 31.0
ainleft                 init                    0.0
ainright                init                    0.0
aleft, aright           vstaudiog               giPianoteq, ainleft, ainright
aleft			              = 			                0.5 * aleft * i_amplitude
aright			            =			                  0.5 * aright * i_amplitude
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin
#end

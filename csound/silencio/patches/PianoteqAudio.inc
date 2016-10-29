#ifdef ENABLE_PIANOTEQ
                        instr PianoteqAudio
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 31.0
ainleft                 init                    0.0
ainright                init                    0.0
aleft, aright           vstaudiog               giPianoteq, ainleft, ainright
aleft			              = 			                0.5 * aleft * iamplitude
aright			            =			                  0.5 * aright * iamplitude
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin
#end

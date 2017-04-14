#ifdef ENABLE_PIANOTEQ
                        instr Pianoteq
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 0
ichan                   init                    1.0
                        vstnote                 giPianoteq, ichan, p4, p5, p3
                        endin
#end

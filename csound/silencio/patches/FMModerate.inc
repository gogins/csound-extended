                        instr FMModerate
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Authors: Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 15.0
iattack			        =			            0.002
isustain		        =			            p3
idecay				    =				        1.5
irelease		        =			            0.05
icarrier                =                       1
imodulator              =                       1
ifmamplitude            =                       8
index                   =                       5.4
ifrequencyb             =                       iHz * 1.003
icarrierb               =                       icarrier * 1.004
aindenv                 transeg                 0.0, iattack, -7.0, 1.0, idecay, -10.0, 0.025, isustain, 0.0, 0.025, irelease, -7.0, 0.0
aindex                  =                       aindenv * index * ifmamplitude
icosine                 ftgenonce               0, 0, 65536,    11,     1 
aouta                   foscili                 1.0, iHz, icarrier, imodulator, index, icosine
aoutb                   foscili                 1.0, ifrequencyb, icarrierb, imodulator, index, icosine
                        ; Plus amplitude correction.
afmout                  =                       (aouta + aoutb) * aindenv
aleft, aright		    Pan			            p7, afmout * i_amplitude
p3, aleft, aright	    Declick			        iattack, p3, irelease, aleft, aright
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin   

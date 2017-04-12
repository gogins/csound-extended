                        instr Rhodes
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Authors: Perry Cook, John ffitch, Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 15.0
iattack			        =			            0.002
isustain		        =			            p3
irelease		        =			            0.05
iindex                  =                       4
icrossfade              =                       3
ivibedepth              =                       0.2
iviberate               =                       6
isine                   ftgenonce               0, 0, 65537,    10,     1
icosine                 ftgenonce               0, 0, 65537,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
icookblank              ftgenonce               0, 0, 65537,    10,     0 ; Blank wavetable for some Cook FM opcodes.
ifn1                    =                       isine
ifn2                    =                       icosine
ifn3                    =                       isine
ifn4                    =                       icookblank
ivibefn                 =                       isine
asignal                 fmrhode                 iamplitude, kHz, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aleft, aright		    Pan			            p7, asignal
p3, aleft, aright	    Declick			        iattack, p3, irelease, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin   

                        instr TubularBell
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Authors: Perry Cook, John ffitch, Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 20.0
iindex                  =                       1
icrossfade              =                       2
ivibedepth              =                       0.2
iviberate               =                       6
isine                   ftgenonce               0, 0, 65537,    10,     1
icosine                 ftgenonce               0, 0, 65537,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
icook3                  ftgenonce               0, 0, 65537,    10,     1, .4, 0.2, 0.1, 0.1, .05
ifn1                    =                       isine
ifn2                    =                       icook3
ifn3                    =                       isine
ifn4                    =                       isine
ivibefn                 =                       icosine
asignal                 fmbell                  1.0, kHz, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aleft, aright		    Pan			            p7, asignal * iamplitude
p3, aleft, aright	    Declick			        0.005, p3, 0.05, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin   

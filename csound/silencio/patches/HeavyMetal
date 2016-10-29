                        instr HeavyMetal
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Perry Cook and John ffitch
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 29.0
iindex                  =                       1.1
icrossfade              =                       2
ivibedepth              =                       0.02
iviberate               =                       4.8
isine                   ftgenonce               0, 0, 65537,    10,     1
icosine                 ftgenonce               0, 0, 65537,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
iexponentialrise        ftgenonce               0, 0, 65537,     5,     .001, 513, 1 ; Exponential rise.
ithirteen               ftgenonce               0, 0, 65537,     9,     1, .3, 0
ifn1                    =                       isine
ifn2                    =                       iexponentialrise
ifn3                    =                       ithirteen
ifn4                    =                       isine
ivibefn                 =                       icosine
iattack                 =                       0.003
idecay                  =                       3.0
isustain                =                       p3
irelease                =                       0.05
adecay                  transeg                 0.0, iattack, -4, 1.0, idecay, -4, 0.1, isustain, -4, 0.1, irelease, -4, 0.0
asignal                 fmmetal                 1.0, kHz, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
aleft, aright		    Pan			            p7, asignal * iamplitude * adecay
p3, aleft, aright	    Declick			        0.005, p3, 0.3, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin

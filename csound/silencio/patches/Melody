                        instr Melody
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Jon Nelson
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 27
ip3                     init                    3.0
iattack			        =			            0.05
isustain		        =			            p3
irelease		        =			            0.1
ichebychev              ftgenonce               0, 0, 65537,    -7,    -1, 150, 0.1, 110, 0, 252, 0
isine                   ftgenonce               0, 0, 65537,    10,     1
icook3                  ftgenonce               0, 0, 65537,    10,     1, .4, 0.2, 0.1, 0.1, .05
ip6 			        = 			            ichebychev
                        ; Pitch.
i1                      =                       iHz
k100                    randi                   1,10
k101                    poscil                  1, 5 + k100, isine
k102                    linseg                  0, .5, 1, p3, 1
k100                    =                       i1 + (k101 * k102)
                        ; Envelope for driving oscillator.
; k1                      linenr                  0.5, ip3 * .3, ip3 * 2, 0.01
k1                      linseg                  0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2                      line                    1, p3, .5
k2                      linseg                  1.0, ip3, .5, isustain, .5, irelease, 0
k1                      =                       k2 * k1
                        ; Amplitude envelope.
k10                     expseg                  0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
k10                     =                       (k10 - .0001)
                        ; Power to partials.
k20                     linseg                  1.485, iattack, 1.5, (isustain + irelease), 1.485
                        ; a1-3 are for cheby with p6=1-4
a1                      poscil                  k1, k100 - .025, icook3
                        ; Tables a1 to fn13, others normalize,
a2                      tablei                  a1, ip6, 1, .5
a3                      balance                 a2, a1
                        ; Try other waveforms as well.
a4                      foscili                  1, k100 + .04, 1, 2.005, k20, isine
a5                      poscil                  1, k100, isine
a6                      =                       ((a3 * .1) + (a4 * .1) + (a5 * .8)) * k10
a7                      comb                    a6, .5, 1 / i1
a8                      =                       (a6 * .9) + (a7 * .1)
asignal        		    balance         	    a8, a1
aleft, aright		    Pan			            p7, asignal * iamplitude
p3, aleft, aright	    Declick			        iattack, p3, irelease, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin

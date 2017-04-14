                        instr FilteredSines
                        ; Author: Michael Bergeman
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, -60.0
p3, adamping		    Damping			        0.01, p3, 0.01
koctave			        =			            octcps(kHz)
idb			            = 			            1.5
ibergeman               ftgenonce               0, 0, 65536,    10,     .28, 1, .74, .66, .78, .48, .05, .33, 0.12, .08, .01, .54, 0.19, .08, .05, 0.16, .01, 0.11, .3, .02, 0.2 ; Bergeman f1
ip5                     =                       ibergeman
ip3                     =                       5.0
ip6                     =                       0.9
ip7                     =                       1.4
kp8                     =                       cpsoct(koctave - .01)
kp9                     =                       cpsoct(koctave + .01)
isc                     =                       idb * .333
k1                      linseg                  40, ip3, 800, p3, 800, 0.06, 0.0
k2                      linseg                  440, ip3, 220, p3, 220, 0.06, 0.0
k3                      linseg                  0.0, ip6, 800, ip7, 200.0, p3, 200, 0.06, 0.0
k4                      linseg                  800, ip3, 40, p3, 40, 0.06, 0.0
k5                      linseg                  220, ip3, 440, p3, 440, 0.06, 0.0
k6                      linseg                  isc, ip6, p3, ip7, p3, 0.06, 0.0
k7                      linseg                  0.0, ip6, 1, ip7, .3, p3, .1, 0.06, 0.0
a5                      poscil                  k3, kp8, ip5
a6                      poscil                  k3, kp8 * 0.999, ip5
a7                      poscil                  k3, kp8 * 1.001, ip5
a1                      =                       a5 + a6 + a7
a8                      poscil                  k6, kp9, ip5
a9                      poscil                  k6, kp9 * 0.999, ip5
a10                     poscil                  k6, kp9 * 1.001, ip5
a11                     =                       a8 + a9 + a10
a2                      butterbp                a1, k1, 40
a3                      butterbp                a2, k5, k2 * 0.8
a4                      balance                 a3, a1
a12                     butterbp                a11, k4, 40
a13                     butterbp                a12, k2, k5 * 0.8
a14                     balance                 a13, a11
a15                     reverb2                 a4, 5, 0.3
a16                     reverb2                 a4, 4, 0.2
a17                     =                       (a15 + a4) * k7
a18                     =                       (a16 + a4) * k7
aleft, aright		    Pan			            p7, (a17 + a18) * i_amplitude * adamping
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin   


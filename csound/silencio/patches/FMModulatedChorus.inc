                        instr FMModulatedChorus
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Thomas Kung
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,i_amplitude,idB  NoteOn                  p4, p5, 6.0
iattack 		        = 			            0.25
isustain		        =			            p3
irelease 		        = 			            0.3333333
p3, adamping		    Damping			        iattack, isustain, irelease
ip6                     =                       0.3
ip7                     =                       2.2
ishift      		    =           		    4.0 / 12000.0
kpch       		        =           		    kHz
koct        		    =           		    octcps(kHz) 
isine                   ftgenonce               0, 0, 65536,    10,     1
icosine                 ftgenonce               0, 0, 65536,    11,     1 
iln                     ftgenonce               0, 0, 65536,   -12,    20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
aadsr                   linen                   1.0, iattack, irelease, 0.01
amodi                   linseg                  0, iattack, 5, p3, 2, irelease, 0
                        ; r moves from ip6 to ip7 in p3 secs.
amodr                   linseg                  ip6, p3, ip7
a1                      =                       amodi * (amodr - 1 / amodr) / 2
                        ; a1*2 is argument normalized from 0-1.
a1ndx                   =                       abs(a1 * 2 / 20)
a2                      =                       amodi * (amodr + 1 / amodr) / 2
                        ; Look up table is in f43, normalized index.
a3                      tablei                  a1ndx, iln, 1
                        ; Cosine
ao1                     poscil                  a1, kpch, icosine
a4                      =                       exp(-0.5 * a3 + ao1)
                        ; Cosine
ao2                     poscil                  a2 * kpch, kpch, icosine
                        ; Final output left
aleft                   poscil                  a4, ao2 + cpsoct(koct + ishift), isine
                        ; Final output right
aright                  poscil                  a4, ao2 + cpsoct(koct - ishift), isine
aleft, aright		    Pan			            p7, (aleft + aright) * i_amplitude * adamping
                        outleta                 "outleft", aleft
                        outleta                 "outright", aright
                        endin

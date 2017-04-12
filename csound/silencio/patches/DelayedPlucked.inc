                        instr DelayedPlucked
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Author: Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, 18.0
iattack			        =			            0.006
idecay				    =				        1.5
isustain		        =			            p3
irelease		        =			            0.05
ihertz                  =                       iHz
ioctave			        =			            octcps(iHz)
                        ; Detuning of strings by 4 cents each way.
idetune                 =                       4.0 / 1200.0
ihertzleft              =                       cpsoct(ioctave + idetune)
ihertzright             =                       cpsoct(ioctave - idetune)
isine                   ftgenonce               0, 0, 65537,    10,     1
icosine                 ftgenonce               0, 0, 65537,    11,     1 ; Cosine wave. Get that noise down on the most widely used table!
igenleft                =                       isine
igenright               =                       icosine
kvibrato                poscil                  1.0 / 120.0, 7.0, icosine
kenvelope            	transeg                 0.0, iattack, -7.0, 1.0, idecay, -7.0, 0.5, isustain, 0.0, 0.125, irelease, -7.0, 0.0
ag                      pluck                   kenvelope, cpsoct(ioctave + kvibrato), iHz, igenleft, 1
agleft                  pluck                   kenvelope, ihertzleft,  iHz, igenleft, 1
agright                 pluck                   kenvelope, ihertzright, iHz, igenright, 1
imsleft                 =                       0.2 * 1000
imsright                =                       0.21 * 1000
adelayleft              vdelay                  ag, imsleft, imsleft + 100
adelayright             vdelay                  ag, imsright, imsright + 100
asignal                 =                       kenvelope * (agleft + adelayleft + agright + adelayright)
                        ; Highpass filter to exclude speaker cone excursions.
asignal1                butterhp                asignal, 32.0
asignal2                balance                 asignal1, asignal
aleft, aright		    Pan			            p7, asignal2 * iamplitude
p3, aleft, aright	    Declick			        0       .006, p3, 0.06, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin

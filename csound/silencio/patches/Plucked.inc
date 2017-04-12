                        instr Plucked
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; Michael Gogins
                        pset                    0, 0, 3600, 0, 0, 0, 0, 0, 0, 0, 0
iHz,kHz,iamplitude,idB  NoteOn                  p4, p5, -4.0
isine			        ftgenonce		        0, 0, 65536, 10, 1
iattack			        =			            0.002
isustain		        =			            p3
irelease		        =			            0.05
aenvelope               transeg                 1.0, 20, -12.0, 0.05
aexcite                 poscil                  1.0, 1, isine
asignal1		        wgpluck2 		        0.1, 1.0, iHz,         0.25, 0.22
asignal2		        wgpluck2 		        0.1, 1.0, iHz * 1.003, 0.20, 0.223
asignal3		        wgpluck2 		        0.1, 11, iHz * 0.997, 0.23, 0.224
apluckout               =                       (asignal1 + asignal2 + asignal3) 
apluckout		        =			            apluckout * aenvelope
aleft, aright		    Pan			            p7, apluckout * iamplitude 
p3, aleft, aright	    Declick			        iattack, p3, irelease, aleft, aright
                        outleta                 "leftout", aleft
                        outleta                 "rightout", aright
                        endin

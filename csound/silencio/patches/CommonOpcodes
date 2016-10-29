; COMMON OPCODES
; Michael Gogins
; This software is licensed under the terms of the 
; GNU Lesser General Public License.

giseed                  init                    0.5


                        opcode			        NoteOn, ikii, iii
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ; General purpose instrument control UDO.
                        ; Returns the pitch at i-rate, the pitch at k-rate
                        ; (with addition of smoothed MIDI pitch bend, if any),
                        ; decibels full scale scaled from MIDI velocity,
                        ; and the amplitude scaled such that 127 == 0 dBFS.
                        ;
                        ; If an instrument is balanced, then its solo peak 
                        ; amplitude at MIDI velocity 127 should be exactly 
                        ; 0 dBFS. If the instrument is too loud (or too soft) 
                        ; at velocity 127, set imeasuredDBFS to compensate
                        ; for the peak level reported by Csound; e.g. 
                        ; for the following messsage:
                        ;
                        ; rtevent:     T 12.257 TT 12.257 M:    +3.35    +3.60
                        ;    number of samples out of range:      511      552  
                        ;
                        ; set the imeasuredDBFS parameter in the NoteOn call 
                        ; in the instrument to -3.6. This will noprmalize the 
                        ; instrument.                      
ikey,ivelocity,imeasureddBFS xin
                        ; Convert MIDI key number to cycles per second.
iHz 		            = 			            cpsmidinn(ikey)
                        ; Modify with MIDI pitch bend, if any.
kpitchbend              pchbend                 -6.0, +6.0    
kpitchbend              =                       kpitchbend + 6.0
iinitialpb              init                    i(kpitchbend)
                        ;print                   iinitialpb
                        ; Smooth out the stepping in the MIDI control signal.
ksmoothbend             port                    kpitchbend, 0.125, iinitialpb                        
kKey                    =                       ikey + ksmoothbend
kHz                     =                       cpsmidinn(kKey)
                        ; Scale MIDI velocity to decibels.
ipower			        pow			            ivelocity / 127.0, 2.0
idecibels               =			            20.0 * log10(ipower)
imidiamplitude		    =			            ampdbfs(idecibels)
                        ; Normalize so amplitude at velocity 127 == amplitude at full scale.
inormalFS		        =			            ampdbfs(0)
imeasured127            =                       ampdbfs(-imeasureddBFS)
inormal                 =                       inormalFS / imeasured127
inormalizedamplitude    =                       imidiamplitude * inormal
                        ;print                   ivelocity, idecibels, imidiamplitude, inormal, inormalizedamplitude
                        xout			        iHz, kHz, inormalizedamplitude, idecibels
                        endop

                        opcode			        Pan, aa, ka
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
kpan, asignal		    xin
                        ;  Constant-power pan.
apan                    =                       (kpan / 2.0) + 0.5
aleft, aright           pan2                    asignal, apan
                        xout			        aleft, aright
                        endop

                        opcode			        Declick, iaa, iiiaa
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
iatt,idur,irel,a1,a2	xin
                        if (idur > 0)           then
isustain		        = 			            idur
idur			        =			            iatt + isustain + irel                        
                        else
isustain                =                       100000.0
                        endif                        
aenv			        linsegr			        0.0, iatt, 1.0, isustain, 1.0, irel, 0.0
ab1			            =			            a1 * aenv
ab2			            =			            a2 * aenv
                        xout			        idur, ab1, ab2
                        endop	

                        opcode			        Damping, ia, iii
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
iatt,idur,irel		    xin
                        if (idur > 0)           then
isustain		        = 			            idur
idur			        =			            iatt + isustain + irel                        
                        else
isustain                =                       100000.0
                        endif                        
                        ; Releasing envelope for MIDI performance.
aenv			        linsegr			        0.0, iatt, 1.0, isustain, 1.0, irel, 0.0
                        xout			        idur, aenv
                        endop						
            

            instr JackNote
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ichannel   	=			    p1 - 1
itime 	   	= 			    p2
iduration  	= 			    p3
ikey 	   	= 			    p4
ivelocity  	= 			    p5
ijunk       =               p6 + p7 + p8 + p9 + p10 + p11
            JackoNoteOut 	"midiout", ichannel, ikey, ivelocity
            ; print 		    itime, iduration, ichannel, ikey, ivelocity
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
            endin


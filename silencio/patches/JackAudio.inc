            instr JackAudio
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            JackoTransport	3, 1.0
aleft 	    JackoAudioIn	"leftin"
aright 	    JackoAudioIn 	"rightin"
aleft1 	    JackoAudioIn	"leftin1"
aright1	    JackoAudioIn 	"rightin1"
            ; Aeolus uses MIDI controller 98 to control stops. 
            ; Only 1 data value byte is used, not the 2 data 
            ; bytes often used  with NRPNs. 
            ; The format for control mode is 01mm0ggg:
            ; mm 10 to set stops, 0, ggg group (or Division, 0 based).
            ; The format for stop selection is 000bbbbb:   
            ; bbbbb for button number (0 based).
            ; Mode to enable stops for Divison I: b1100010 (98) 
            ; Mode to enable stops for Divison II: b1100011 (99) 
            JackoMidiOut          "midiout", 176, 0, 98,  98 
            JackoMidiOut          "midiout", 176, 0, 98,   6 ; Tibia
            JackoMidiOut          "midiout", 176, 0, 98,  97
            JackoMidiOut          "midiout", 176, 0, 98,   1 ; Harmonic flute 8
            JackoMidiOut          "midiout", 176, 0, 98,   7 ; Septime
            JackoMidiOut          "midiout", 176, 0, 98,   2 ; Septime
            ; Sends audio coming in from Aeolus out
            ; not only to the Jack system out (sound card),
            ; but also to the signal flow graph.
            ; Note that in freewheeling mode, "outleft"
            ; and "outright" simply go silent.
            outleta         "outleft", aleft+aleft1
            outleta         "outright", aright+aright1
            JackoAudioOut 	"outleft", aleft+aleft1
            JackoAudioOut 	"outright", aright+aright1
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
            endin

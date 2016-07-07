;; DOESN'T COPY AMPLITUDES -- USE "MIDI2CS.LISP" INSTEAD

(setf tracks (import-events "hhc.mid"))
(setf (elt midi-channel-names 9) "i1")
(events tracks "hhc.sco")

(setf tracks (import-events "snare.mid"))
(setf (elt midi-channel-names 9) "i1")
(events tracks "snare.sco")

(setf tracks (import-events "bassdr.mid"))
(setf (elt midi-channel-names 9) "i1")
(events tracks "bassdr.sco")



(setf tracks (import-events "/home/drew/midifiles/tocsound/aaa2.mid"))
(setf (elt midi-channel-names 1) "i1")
(setf (elt midi-channel-names 2) "i1")
(setf (elt midi-channel-names 3) "i1")
(setf (elt midi-channel-names 4) "i1")
(setf (elt midi-channel-names 5) "i1")
(setf (elt midi-channel-names 6) "i1")
(setf (elt midi-channel-names 7) "i1")
(setf (elt midi-channel-names 8) "i1")
(setf (elt midi-channel-names 9) "i1")
(setf (elt midi-channel-names 10) "i1")
(events tracks "/home/drew/midifiles/scos/aaa2.sco")

























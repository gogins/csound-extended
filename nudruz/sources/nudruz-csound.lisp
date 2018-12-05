    ; C O M M O N   M U S I C   C F F I   I N T E R F A C E   T O   C S O U N D
;
; Copyright (C) 2016 Michael Gogins
;
; This file belongs to Csound.
;
; This software is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.
;
; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this software; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
; This file is handwritten and should be maintained by keeping it up to date
; with regard to include/csound.h. This file is not intended to be complete
; and essentially defines a Steel Bank Common Lisp interface to a subset of
; the most useful functions in csound.h. At the present time, only pointers,
; strings, and other primitive types are used in this interface.
(in-package :cm)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defun event-to-istatement (event channel-offset velocity-scale arrangement)
"
Translates a Common Music MIDI channel event to a Csound score event
(i-statement), which is terminated with a newline. An offset, which may
be any number, is added to the MIDI channel number. After that, if the 
arrangement parameter is not nil, the existing event is remapped to a new 
instrument number and the velocity is modified. MIDI events that are 
not channel events are included, but as comments.
"
    (let 
        ((insno)
        (midikey)
        (velocity)
        (pan 0.5))
        (if (string-equal (class-name (class-of event)) "MIDI")
        (progn 
        ; (inspect event)
        (setf insno (+ channel-offset (midi-channel event)))
        (setf velocity (* velocity-scale (midi-amplitude event)))
        (setf midikey (keynum (midi-keynum event)))
        (when arrangement
            (setf pan (third (gethash insno arrangement)))
            (setf velocity (+ velocity (second (gethash insno arrangement))))
            (setf insno (first (gethash insno arrangement))))
    (format nil "i ~,6f ~,6f ~,6f ~,6f ~,6f 0 ~,6f 0 0 0 0~%" insno (object-time event)(midi-duration event)
    midikey velocity pan))
    (format nil "; ~a~%" event)
)))
(export 'event-to-istatement)

(defun replace-all (string part replacement &key (test #'char=))
"
Replaces all occurences of the string 'part' in 'string' with 'replacement',
using 'test' for character equality.
"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))
          
(defun seq-to-sco (seq &optional (channel-offset 1) (velocity-scale 127) &key (arrangement nil))
"
Translates all MIDI channel events in a Common Music 'seq' object to Csound sco text,
with an optional channel offset and velocity scaling. The arrangement 
parameter, if passed, is used to reassign the instrument numbers and 
add to/subtract from the MIDI velocities in the sequence. The arrangement 
consists of a hashtable mapping original Csound instrument numbers 
to a list '(new-inso add-velocity pan).
"
    (let 
        ((score-list (list))
        (score-text "")
        (sco-text ""))
        (progn 
            (format t "Building Csound sco from seq...~%")
            (defun curried-event-to-istatement (event)
                (event-to-istatement event channel-offset velocity-scale arrangement))
            (setq score-list (mapcar 'curried-event-to-istatement (subobjects seq)))
            (setq sco-text (format nil "~{~A~^~}" score-list))
        )
    )
)

(defun csd-to-file (name content)
"
Writes the contents of a CSD to a file, replacing the file if it exists.
"
  (with-open-file (stream name 
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create )
  (write-line content stream)))
  
  
(defun seq-to-lilypond (sequence filename fomus-parts partids-for-channels voices-for-channels &key (title nil)(subtitle nil)(composer nil))
"
Attempts to translate MIDI events in the sequence to a Lilypond score using 
Fomus (but does not always succeed). MIDI channels must be assigned to 
Lilypond part IDs and Lilypond voices in the hashtables. If the :voice 
parameter is a list, it should contain as many voices for the corresponding 
channel as there actually are in that channel.
"
    (let 
        ((fomus-events (list)))
        (progn 
            (format t "Building Lilypond score ~A from seq...~%" filename)
            (defun midi-event-to-fomus-event (event)
                (new fomus:note 
                    :partid (gethash (midi-channel event) partids-for-channels)
                    :off (object-time event)
                    :dur (midi-duration event)
                    :note (midi-keynum event)
                    :voice (gethash (midi-channel event) voices-for-channels)
                )
            )
            (setf fomus-events (mapcar 'midi-event-to-fomus-event (subobjects sequence)))
            (format t "Generated: ~d Fomus events.~%" (list-length fomus-events))            
            (setf fomus-events (remove-duplicates fomus-events 
                :test  #'(lambda (x y)
                    (equal (format nil "~A" x) (format nil "~A" y))
            )))
            (format t "Removed duplicates: ~d Fomus events.~%" (list-length fomus-events))
            (events (new seq :name "Lilypond" :subobjects fomus-events) filename :auto-voicing t :verbose 2 :auto-quantize t :view t :parts (list fomus-parts) :title title :subtitle subtitle :composer composer)
            ; CMN output does not produce a usable score, see #61.
            ;(events (new seq :name "Common Music Notation" :subobjects fomus-events) "temp.eps")
        )
    )
)

(defun seq-to-midifile (sequence filename)
"
Writes a sequence containing MIDI events to a MIDI file, replacing the file if 
it exists.
"
    (events sequence filename :play nil)
)

(defun build-csd (orc &key (sco "")(options "--midi-key=4 --midi-velocity=5 -m195 -RWdf")(output "dac"))
    (let
        ((csd "")
        (csd-template "<CsoundSynthesizer>
<CsOptions>
~A -o ~A
</CsOptions>
<CsInstruments>
~A
</CsInstruments>
<CsScore>
~A
</CsScore>
</CsoundSynthesizer>
~%")
        )
        (setq csd (format nil csd-template options output orc sco))
    )
)

(defun render-with-orc (sequence orc &key (options "--midi-key=4 --midi-velocity=5 -m195 -RWdf")
    (output "dac")(channel-offset 1)(velocity-scale 127)(csound-instance nil)
    (csd-filename "tmp-generated.csd")(arrangement nil))
    (let 
        ((csd "")
        (sco-text "")
        (result 0))
        (progn
            (setq csd (build-csd orc :options options :output output))
            (setq result (render-with-csd sequence csd :channel-offset channel-offset :velocity-scale velocity-scale :csound-instance csound-instance :csd-filename csd-filename :arrangement arrangement))
        )
    )
)
    
(defun render-with-csd (seq csd &key (channel-offset 1)(velocity-scale 127)
    (csound-instance nil)(csd-filename "temp-csd.csd")(arrangement nil))
"
Given a Common Music 'seq', translates each of its MIDI events into a Csound 
'i' statement, optionally offsetting the channel number and/or rescaling MIDI 
velocity, then renders the resulting score using the Csound 'csd'. The 
generated score is appended to the <CsScore> element of `csd`. It is 
possible to call csoundReadScore during the performance. This function returns 
the Csound object that it uses.

The csound parameter is used to call Csound if passed. This enables 
render-with-csound to be run in a separate thread of execution, and for the 
caller to control Csound instrument parameters during real time performance, 
e.g.

(setq csound (csoundCreate 0))
(setq my-thread (bt:make-thread (lambda () (render-with-csound cs csd 1 127 csound))))
(csoundSetControlChannel csound 'mychannel' myvalue)
(bt:join-thread my-thread)

A copy of the .csd file that is rendered is saved for archival purposes.
"
    (let
        ((score-list (list))
        (cs 0)
        (sco-text "")
        (result 0)
        (new-csd-text "")
        (csd-pointer 0))
        (progn
            (setq sco-text (seq-to-sco seq channel-offset velocity-scale :arrangement arrangement))
            (setq new-csd-text (replace-all csd "</CsScore>" (concatenate 'string sco-text "</CsScore>")))
            ;(format t "new-csd-text:~%~A~%" new-csd-text)
            (csd-to-file csd-filename new-csd-text)
            (setq csd-pointer (cffi:foreign-string-alloc new-csd-text))
            (if csound-instance
                (setq cs csound-instance)
                (progn
                    (setq result (csound:csoundInitialize 3))
                    (setq cs (csound:csoundCreate (cffi:null-pointer)))
                    (format t "csoundCreate returned: ~S.~%" cs)
                )
            )
            ; Not sure why, but cffi:with-foreign-string doesn't seem to work.
            (setq result (csound:csoundCompileCsdText cs csd-pointer))
            (format t "csoundCompileCsdText returned: ~D.~%" result)
            (setq result (csound:csoundStart cs))
            (format t "csoundStart returned: ~D.~%" result)
            (loop
                (setq result (csound:csoundPerformKsmps cs))
                (when (not (equal result 0)) (return))
            )
            (setq result (csound:csoundCleanup cs))
            (format t "csoundCleanup returned: ~D.~%" result)
            (sleep 5)
            (if (not csound-instance)
                (csound:csoundDestroy cs)
                (format t "csoundDestroy was called.~%")
            )
            (cffi:foreign-string-free csd-pointer)
            (format t "The Csound performance has ended: ~D.~%" result)
        )
    )
)   

(defun midifile-to-seq (midi-filename)
"
Import a standard midi file to a Common Music seq object such that notes
are not CM::MIDI-EVENT binary objects, but rather regular CM::MIDI objects 
that can be processed in Common Music.
"
    (let 
        ((raw-seq)
        (cooked-seq))
        (setf raw-seq (import-events midi-filename :tracks true :meta-exclude true))
        (setf cooked-seq (new seq :name "csound-seq"))
        (events raw-seq cooked-seq)
    )
)
    
(defun cope-events-to-seq (cope-events)
"
Translates an event list produced by David Cope's 'Computer Models of Musical 
Creativity' software into a CM::SEQ object.
"
    (let 
        ((midi-events))
        (defun cope-event-to-midi-event (event)
            (new midi 
                :time(/ (first event) 1000.)
                :keynum (second event)
                :amplitude (/ (fifth event) 127.)
                :channel (- (fourth event) 1)
                :duration (/ (third event) 1000.)
            )
        )
        (setf midi-events (mapcar 'cope-event-to-midi-event cope-events))
        ;(print midi-events)
        (format t "Translated: ~d MIDI events.~%" (list-length midi-events))
        (new seq :name "cm-seq" :subobjects midi-events)
    )
)
            
 
        



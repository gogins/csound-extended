; S T E E L   B A N K   C O M M O N   L I S P   F F I   I N T E R F A C E   T O   C S O U N D . H
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

(defun event-to-istatement (event channel-offset velocity-scale)
"
Translates a Common Music MIDI event to a Csound score event
(i-statement), which is terminated with a newline. An offset, which may
be any number, is added to the MIDI channel number.
"
    (format nil "i ~,6f ~,6f ~,6f ~,6f ~,6f 0 0 0 0 0 0~%" (+ channel-offset (midi-channel event)) (object-time event)(midi-duration event)(keynum (midi-keynum event))(* velocity-scale (midi-amplitude event)))
)
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
          
(defun seq-to-sco (seq &optional (channel-offset 1) (velocity-scale 127))
"
Translates all MIDI events in a Common Music 'seq' object to Csound sco text,
with an optional channel offset and velocity scaling.
"
    (let 
        ((score-list (list))
        (score-text "")
        (sco-text ""))
        (progn 
            (format t "Building Csound sco from seq...~%")
            (defun curried-event-to-istatement (event)
                (event-to-istatement event channel-offset velocity-scale))
            (setq score-list (mapcar 'curried-event-to-istatement (subobjects seq)))
            (setq sco-text (format nil "~{~A~^ ~}" score-list))
        )
    )
)

(defun write-to-file (name content)
  (with-open-file (stream name 
                           :direction :output
                           :if-exists :overwrite
                           :if-does-not-exist :create )
  (write-line content stream)))

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

(defun render-with-orc (sequence orc &key (options "--midi-key=4 --midi-velocity=5 -m195 -RWdf")(output "dac")(channel-offset 1) (velocity-scale 127)(csound-instance nil))
    (let 
        ((csd "")
        (sco-text "")
        (result 0))
        (progn
            (setq csd (build-csd orc :options options :output output))
            (setq result (render-with-csd sequence csd :channel-offset channel-offset :velocity-scale velocity-scale :csound-instance csound-instance))
        )
    )
)
    
(defun render-with-csd (seq csd &key (channel-offset 1)(velocity-scale 127)(csound-instance nil))
"
Given a Common Music 'seq', translates each of its MIDI events into a Csound 
'i' statement, optionally offsetting the channel number and/or rescaling MIDI 
velocity, then renders the resulting score using the Csound 'csd'. The 
generated score is appended to the <CsScore> element of `csd`. It is 
possible to call csoundReadScore during the performance. This function returns 
the Csound object that it uses.

The optional csound parameter is used to call Csound if passed. This enables
render-with-csound to be run in a separate thread of execution, and for the 
caller to control Csound instrument parameters during real time performance, e.g.

(setq csound (csoundCreate 0))
(setq my-thread (bt:make-thread (lambda () (render-with-csound cs csd 1 127 csound))))
(csoundSetControlChannel csound 'mychannel' myvalue)
(bt:join-thread my-thread)
"
    (let
        ((score-list (list))
        (cs 0)
        (sco-text "")
        (result 0)
        (new-csd-text "")
        (csd-pointer 0))
        (progn
            (setq sco-text (seq-to-sco seq channel-offset velocity-scale))
            (setq new-csd-text (replace-all csd "</CsScore>" (concatenate 'string sco-text "</CsScore>")))
            ;(format t "new-csd-text:~%~A~%" new-csd-text)
            (write-to-file "temp-generated-csd.csd" new-csd-text)
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

(defun post-process (soundfile-pathname title composer copyright &key date artist license publisher label album track)
"Given the pathname to a master-quality soundfile, normalizes the file and 
translates it to CD audio, mp3, and mp4 formats, each tagged with metadata."
)



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
(use-package :cffi)
(cffi:define-foreign-library libcsound64
    (:darwin "libcsound64.dylib")
    (:unix "libcsound64.so")
    (:windows "csound64.dll")
    (t (:default "libcsound64")))
(cffi:use-foreign-library libcsound64)

(in-package :cm)

; You can paste below here new definitions including those created
; e.g. by SWIG. Be sure to TEST any changes you make to this file!

; (cffi:defcfun ("csoundTableLength" csoundTableLength) :int
  ; (csound :pointer)
  ; (table :int))

; (cffi:defcfun ("csoundTableGet" csoundTableGet) :double
  ; (csound :pointer)
  ; (table :int)
  ; (index :int))

; (cffi:defcfun ("csoundTableSet" csoundTableSet) :void
  ; (csound :pointer)
  ; (table :int)
  ; (index :int)
  ; (valu :double))

; (cffi:defcfun ("csoundRunUtility" csoundRunUtility) :int
  ; (csound :pointer)
  ; (nayme :string)
  ; (argc :int)
  ; (argv :pointer))

; (cffi:defcfun ("csoundMessage" csoundMessage) :void
    ; (csound :pointer)
    ; (control :string)
    ; &rest)

; (defun csoundMessage (csound control &rest values)
    ; (cffi:foreign-funcall "csoundMessage" csound :pointer control :pointer &rest values :void))

(cffi:defcfun ("csoundSetControlChannel" csoundSetControlChannel) :void
  (csound :pointer)
  (name :string)
  (value :double))

(cffi:defcfun ("csoundInitialize" csoundInitialize) :int
  (flags :int))

(cffi:defcfun ("csoundCreate" csoundCreate) :pointer
  (host-data :pointer))

(cffi:defcfun ("csoundDestroy" csoundDestroy) :void
  (csound :pointer))

(cffi:defcfun ("csoundGetVersion" csoundGetVersion) :int)

(cffi:defcfun ("csoundGetAPIVersion" csoundGetAPIVersion) :int)

(cffi:defcfun ("csoundCompileOrc" csoundCompileOrc) :int
  (csound :pointer)
  (orc :string))

(cffi:defcfun ("csoundEvalCode" csoundEvalCode) :double
  (csound :pointer)
  (orc :string))

(cffi:defcfun ("csoundCompileArgs" csoundCompileArgs) :int
  (csound :pointer)
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("csoundStart" csoundStart) :int
  (csound :pointer))

(cffi:defcfun ("csoundCompile" csoundCompile) :int
  (csound :pointer)
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("csoundCompileCsd" csoundCompileCsd) :int
  (csound :pointer)
  (csd-pathname :string))

(cffi:defcfun ("csoundCompileCsdText" csoundCompileCsdText) :int
  (csound :pointer)
  (csd-text :string))

(cffi:defcfun ("csoundPerform" csoundPerform) :int
  (csound :pointer))

(cffi:defcfun ("csoundPerformKsmps" csoundPerformKsmps) :int
  (csound :pointer))

(cffi:defcfun ("csoundPerformBuffer" csoundPerformBuffer) :int
  (csound :pointer))

(cffi:defcfun ("csoundStop" csoundStop) :void
  (csound :pointer))

(cffi:defcfun ("csoundCleanup" csoundCleanup) :int
  (csound :pointer))

(cffi:defcfun ("csoundReset" csoundReset) :void
  (csound :pointer))

(cffi:defcfun ("csoundGetSr" csoundGetSr) :double
  (csound :pointer))

(cffi:defcfun ("csoundGetKr" csoundGetKr) :double
  (csound :pointer))

(cffi:defcfun ("csoundGetKsmps" csoundGetKsmps) :int32
  (csound :pointer))

(cffi:defcfun ("csoundGetNchnls" csoundGetNchnls) :int32
  (csound :pointer))

(cffi:defcfun ("csoundGetNchnlsInput" csoundGetNchnlsInput) :int32
  (csound :pointer))

(cffi:defcfun ("csoundGet0dBFS" csoundGet0dBFS) :double
  (csound :pointer))

(cffi:defcfun ("csoundGetCurrentTimeSamples" csoundGetCurrentTimeSamples) :int64
  (csound :pointer))

(cffi:defcfun ("csoundGetSizeOfMYFLT" csoundGetSizeOfMYFLT) :int)

(cffi:defcfun ("csoundGetHostData" csoundGetHostData) :pointer
  (csound :pointer))

(cffi:defcfun ("csoundSetHostData" csoundSetHostData) :void
  (csound :pointer)
  (hostData :pointer))

(cffi:defcfun ("csoundSetOption" csoundSetOption) :int
  (csound :pointer)
  (option :string))

(cffi:defcfun ("csoundGetOutputName" csoundGetOutputName) :string
  (csound :pointer))

(cffi:defcfun ("csoundSetOutput" csoundSetOutput) :void
  (csound :pointer)
  (nayme :string)
  (tipe :string)
  (format :string))

(cffi:defcfun ("csoundSetInput" csoundSetInput) :void
  (csound :pointer)
  (nayme :string))

(cffi:defcfun ("csoundSetMIDIInput" csoundSetMIDIInput) :void
  (csound :pointer)
  (nayme :string))

(cffi:defcfun ("csoundSetMIDIFileInput" csoundSetMIDIFileInput) :void
  (csound :pointer)
  (nayme :string))

(cffi:defcfun ("csoundSetMIDIOutput" csoundSetMIDIOutput) :void
  (csound :pointer)
  (nayme :string))

(cffi:defcfun ("csoundSetMIDIFileOutput" csoundSetMIDIFileOutput) :void
  (csound :pointer)
  (nayme :string))

(cffi:defcfun ("csoundSetRTAudioModule" csoundSetRTAudioModule) :void
  (csound :pointer)
  (moduule :string))

(cffi:defcfun ("csoundGetInputBufferSize" csoundGetInputBufferSize) :long
  (csound :pointer))

(cffi:defcfun ("csoundGetOutputBufferSize" csoundGetOutputBufferSize) :long
  (csound :pointer))

 (cffi:defcfun ("csoundGetInputBuffer" csoundGetInputBuffer) :pointer
   (csound :pointer))

(cffi:defcfun ("csoundGetOutputBuffer" csoundGetOutputBuffer) :pointer
  (csound :pointer))

(cffi:defcfun ("csoundGetSpin" csoundGetSpin) :pointer
  (csound :pointer))

; (cffi:defcfun ("csoundAddSpinSample" csoundAddSpinSample) :void
  ; (csound :pointer)
  ; (frayme :int)
  ; (channel :int)
  ; (sample :float))

; (cffi:defcfun ("csoundGetSpout" csoundGetSpout) :pointer
  ; (csound :pointer))

; (cffi:defcfun ("csoundGetSpoutSample" csoundGetSpoutSample) :double
  ; (csound :pointer)
  ; (frame :int)
  ; (channel :int))

(cffi:defcfun ("csoundReadScore" csoundReadScore) :int
  (csound :pointer)
  (score :string))

; (cffi:defcfun ("csoundGetScoreTime" csoundGetScoreTime) :double
  ; (csound :pointer))

; (cffi:defcfun ("csoundIsScorePending" csoundIsScorePending) :int
  ; (csound :pointer))

; (cffi:defcfun ("csoundSetScorePending" csoundSetScorePending) :void
  ; (csound :pointer)
  ; (pending :int))

; (cffi:defcfun ("csoundGetScoreOffsetSeconds" csoundGetScoreOffsetSeconds) :double
  ; (csound :pointer))

; (cffi:defcfun ("csoundSetScoreOffsetSeconds" csoundSetScoreOffsetSeconds) :void
  ; (csound :pointer)
  ; (time :double))

; (cffi:defcfun ("csoundRewindScore" csoundRewindScore) :void
  ; (csound :pointer))

; (cffi:defcfun ("csoundGetMessageLevel" csoundGetMessageLevel) :int
  ; (csound :pointer))

; (cffi:defcfun ("csoundSetMessageLevel" csoundSetMessageLevel) :void
  ; (csound :pointer)
  ; (messageLevel :int))

; (cffi:defcfun ("csoundCreateMessageBuffer" csoundCreateMessageBuffer) :void
  ; (csound :pointer)
  ; (toStdOut :int))

; (cffi:defcfun ("csoundGetFirstMessage" csoundGetFirstMessage) :string
  ; (csound :pointer))

; (cffi:defcfun ("csoundGetFirstMessageAttr" csoundGetFirstMessageAttr) :int
  ; (csound :pointer))

; (cffi:defcfun ("csoundPopFirstMessage" csoundPopFirstMessage) :void
  ; (csound :pointer))

; (cffi:defcfun ("csoundGetMessageCnt" csoundGetMessageCnt) :int
  ; (csound :pointer))

; (cffi:defcfun ("csoundDestroyMessageBuffer" csoundDestroyMessageBuffer) :void
  ; (csound :pointer))

; (cffi:defcfun ("csoundGetControlChannel" csoundGetControlChannel) :double
  ; (csound :pointer)
  ; (nayme :string)
  ; (err :pointer))

; (cffi:defcfun ("csoundGetAudioChannel" csoundGetAudioChannel) :void
  ; (csound :pointer)
  ; (name :string)
  ; (samples :pointer))

; (cffi:defcfun ("csoundSetAudioChannel" csoundSetAudioChannel) :void
  ; (csound :pointer)
  ; (name :string)
  ; (samples :pointer))

; (cffi:defcfun ("csoundGetStringChannel" csoundGetStringChannel) :void
  ; (csound :pointer)
  ; (name :string)
  ; (string :string))

; (cffi:defcfun ("csoundSetStringChannel" csoundSetStringChannel) :void
  ; (csound :pointer)
  ; (name :string)
  ; (string :string))

; (cffi:defcfun ("csoundScoreEvent" csoundScoreEvent) :int
  ; (csound :pointer)
  ; (tipe :char)
  ; (pFields :pointer)
  ; (numFields :long))

; (cffi:defcfun ("csoundScoreEventAbsolute" csoundScoreEventAbsolute) :int
  ; (csound :pointer)
  ; (type :char)
  ; (pfields :pointer)
  ; (numFields :long)
  ; (time_ofs :double))

; (cffi:defcfun ("csoundInputMessage" csoundInputMessage) :void
  ; (csound :pointer)
  ; (message :string))

; (cffi:defcfun ("csoundIsNamedGEN" csoundIsNamedGEN) :int
  ; (csound :pointer)
  ; (num :int))

; (cffi:defcfun ("csoundGetNamedGEN" csoundGetNamedGEN) :void
  ; (csound :pointer)
  ; (num :int)
  ; (name :string)
  ; (len :int))

; (cffi:defcfun ("csoundAppendOpcode" csoundAppendOpcode) :int
  ; (csound :pointer)
  ; (opname :string)
  ; (dsblksiz :int)
  ; (flags :int)
  ; (thread :int)
  ; (outypes :string)
  ; (intypes :string)
  ; (iopadr :pointer)
  ; (kopadr :pointer)
  ; (aopadr :pointer))
  
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

(defun render-with-csound (sequence csd-text &optional (channel-offset 1) (velocity-scale 127) (csound-instance nil))
"
Given a Common Music seq 'sequence', translates each of its events into a
Csound 'i' statement, optionally offsetting the channel number and/or
rescaling MIDI velocity, then renders the resulting score using 'csd-text'.
A CSD is used because it can contain any textual Csound input in one block of
raw text. The score generated from 'sequence' is appended to any <CsScore>
lines found in 'csd-text'. This is done so that Csound will quit performing
at the end of the score. It is possible to call csoundReadScore during the
performance. This function returns the Csound object that it uses.

The optional 'csound' parameter is used to call Csound if passed. This enables
'render-with-csound' to be run in a separate thread of execution, and for
the caller to control Csound instrument parameters during real time
performance, e.g.

(setq csound (csoundCreate 0))
(setq my-thread (bt:make-thread (lambda () (render-with-csound cs csd 1 127 csound))))
(sb-csound:csoundSetControlChannel csound 'mychannel' myvalue)
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
            (format t "Building Csound score...~%")
            (defun curried-event-to-istatement (event)
                (event-to-istatement event channel-offset velocity-scale))
            (setq score-list (mapcar 'curried-event-to-istatement (subobjects sequence)))
            (setq sco-text (format nil "~{~A~^ ~}" score-list))
            ;(print sco-text)
            (setq new-csd-text (replace-all csd-text "</CsScore>" (concatenate 'string sco-text "</CsScore>")))
            (format t "new-csd-text: ~A~%" new-csd-text)
            (setq csd-pointer (cffi:foreign-string-alloc new-csd-text))
            (if csound-instance
                (setq cs csound-instance)
                (progn
                    (setq cs (csoundCreate (cffi:null-pointer)))
                    (format t "csoundCreate returned: ~S.~%" cs)
                )
            )
            (setq result (csoundCompileCsdText cs new-csd-text))
            (format t "csoundCompileCsdText returned: ~D.~%" result)
            (setq result (csoundStart cs))
            (format t "csoundStart returned: ~D.~%" result)
            (loop
                (setq result (csoundPerformKsmps cs))
                (when (not (equal result 0))(return))
            )
            (setf result(csoundCleanup cs))
            (format t "csoundCleanup returned: ~D.~%" result)
            (sleep 5)
            (if (not csound)
                (csoundDestroy cs)
                (format t "csoundDestroy was called.~%")
            )
            (cffi:foreign-string-free csd-pointer)
            (format t "The Csound performance has ended: ~D.~%" result)
        )
    )
)



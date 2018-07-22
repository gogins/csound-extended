;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;; See https://github.com/csound/csound.
;;;
;;; CSOUND SYNTHESIS CALL
;;; J. Bresson, IRCAM 2005
;;;
;;; Modified by Michael Gogins in July 2018 to enable
;;; real-time audio, and to use CFFI to enable easy
;;; abort of Csound rendering.
;;;===================================================

; L I S P   C F F I   I N T E R F A C E   F O R   C S O U N D . H
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
; and essentially defines a Lisp interface to a subset of the most useful
; functions in csound.h. At the present time, only pointers, strings, and
; other primitive types are used in this interface.

(defpackage :csound
    (:use :common-lisp :cffi))
(cffi:define-foreign-library libcsound64
    (:darwin "libcsound64.dylib")
    (:unix "libcsound64.so")
    (:windows "csound64.dll")
    (t (:default "libcsound64")))
(cffi:use-foreign-library libcsound64)
(in-package :csound)

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
  
(defun file-get-contents (filename)
    (with-open-file (stream filename)
        (let ((contents (make-string (file-length stream))))
        (read-sequence contents stream) 
    contents)))
    
(in-package :om)

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))
    
(defun csound-render (sco orc out-name &optional normalize resolution)
    (let* 
        ((outpath (handle-new-file-exists
                   (corrige-sound-filename 
                    (if out-name out-name (pathname-name sco)) *om-outfiles-folder*)))
        (tmppath (handle-new-file-exists 
                   (om-make-pathname :directory outpath :name (pathname-name outpath) :type "tmp")))
        (csout (if (or normalize *normalize*) tmppath outpath)))
        (if (equal (file-namestring out-name) "dac")
            (progn
                (setf is_dac t)
                (if *csound-dac*
                    (setf csout *csound-dac*)
                    (setf csout "dac")))
            (setf is_dac nil))
            
        (om-print "======================================" "OM2Csound ::")
        (om-print "CSOUND SYNTHESIS..." "OM2Csound ::")
        (om-print "======================================" "OM2Csound ::")
        (print (format nil "orc:        ~s" orc))
        (print (format nil "sco:        ~s" sco))
        (print (format nil "is_dac:     ~D" is_dac))
        (print (format nil "csout:      ~s" csout))
        (print (format nil "normalize:  ~D" normalize))
        (print (format nil "resolution: ~D" resolution))
        
        (when (probe-file outpath)
          (print (string+ "Removing existing file: " (namestring outpath)))
          (om-delete-file outpath))
           
        (setf csd_template "<CsoundSynthesizer>
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
")
        (format t "Csound version: ~A~%" (csound::csoundGetVersion))
        (setf orc_text (csound::file-get-contents (om-path2cmdpath orc)))
        (setf sco_text (csound::file-get-contents (om-path2cmdpath sco)))
        (setf output_soundfile_path (om-path2cmdpath csout))
        (setf csd_text (format nil csd_template *flags* output_soundfile_path orc_text sco_text))
        (format t "csd_text: ~A" csd_text)
        (defparameter cs 0)
        (defparameter result 0)
        (setf cs (csound::csoundCreate (cffi:null-pointer)))
        (format t "csoundCreate returned: ~S~%" cs)
        (setf result (csound::csoundCompileCsdText cs csd_text))
        (format t "csoundCompileCsdText returned: ~D~%" result)
        (setf result (csound::csoundStart cs))
        (format t "csoundStart returned: ~D~%" result)
        (loop 
            (setf result (csound::csoundPerformKsmps cs))
            (when (not (equal result 0))(return))
        )        
        (csound::csoundCleanup cs)
        (csound::csoundReset cs)
        (unless (equal csout "dac")
            (if (null (probe-file csout))
                (om-message-dialog "!!! Error in CSound synthesis !!!"))
                (progn
                    (when (or normalize *normalize*)
                      (let ((real-out (om-normalize tmppath outpath (or normalize *normalize-level*) resolution)))
                        (if real-out 
                            (add-tmp-file tmppath)
                          (rename-file tmppath outpath)))))
        (when *delete-inter-file* 
            (clean-tmp-files))
        (om-print "END CSOUND SYNTHESIS")
        (return-from csound-render (probe-file outpath)))
    )
)

(defmethod! CSOUND-SYNTH ((sco pathname) (orc pathname) &optional (out-name nil) normalize resolution)
  :icon '(410)
  (setf *flags* *csound-defflags*)
  (if (probe-file *CSOUND-PATH*)
    (csound-render sco orc out-name normalize resolution)
    (om-beep-message (format nil "Csound not found in ~s." *CSOUND-PATH*))))

(defmethod! CSOUND-SYNTH ((sco t) (orc t) &optional out-name normalize resolution)
            (csound-synth (convert-input-to-csound sco "sco") (convert-input-to-csound orc "orc") 
                          out-name 
                          normalize resolution))

(defmethod convert-input-to-csound ((self string) &optional type)
  (pathname self))

(defmethod convert-input-to-csound ((self pathname) &optional type)
  self)

(defmethod convert-input-to-csound ((self list) &optional type)
  (let ((path (tmpfile (if type (string+ "temcsoundfile." type) "temcsoundfile"))))
    (WITH-OPEN-FILE (out path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for item in self do (write-line item out)))
    (push path *tmpparfiles*)
    path))
    
(defmethod convert-input-to-csound ((self textfile) &optional type)
   (let ((path (tmpfile (if type (string+ "temcsoundfile." type) "temcsoundfile"))))
     (when (buffer-text self)
       (om-buffer-write-file (buffer-text self) path :if-exists :supersede))
         (push path *tmpparfiles*)
         path))


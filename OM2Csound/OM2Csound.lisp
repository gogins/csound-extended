;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; See https://github.com/csound/csound.
;;;
;;; LIBRARY MAIN FILE
;;; Authors/Contributors: Laurent Pottier, Karim Haddad, Mikhail Malt, 
;;; Carlos Agon, Jean Bresson 
;;; (c) IRCAM 1993-2010
;;; Modified by Michael Gogins to use in-process CFFI
;;; bindings the Csound library, to run in a separate 
;;; thread of execution, and to render real-time audio.
;;;===================================================

(in-package :om)

(defparameter *Csound-files* '("cs-score" 
                               "cs-score-processing" 
                               "cs-orchestra" 
                               "orc-editor" 
                               "utils" 
                               "csound-synth"
                               "csound-normalize"
                               "csound-preferences"))

(mapc #'(lambda (file) (compile&load (om-relative-path '("sources") file))) *Csound-files*)

(fill-library '(("Score" 
                 (("ftable utils" nil nil (sampler pargen03 pargen05-07 pargen09 pargen15) nil))
                 nil (table i-statements make-obj-snd write-csound-score change-col-in-file) nil)
                ("Orchestra" 
                 (("instrument definition" nil nil (inst out assign-val) nil)
                  ("opcodes" nil nil (oscil line-seg linen phasor tablei rand soundin 
                                            reverb porta butterhp statement convert-pitch convert-val) nil)
                  ("converters" nil nil (convert-pitch convert-val) nil)
                  ("signal operators" nil nil (plus minus multiply divide) nil))
                 nil (write-csound-orc header) nil)
                ("Synthesize" nil nil (csound-synth csound-normalize) nil))
              )


(doc-library "OM2Csound is a library for the control of Csound synthesis from OpenMusic.

It is consituted of a 'score' editing part, an 'orchestra' editing part and a general synthesis feature.
Score and orchestra editing tools are mainly low level modules for formatting Csound instructions and writing .sco and .orc files to be read by Csound.

The synthesis feature (CSOUND-SYNTH) requires Csound to be installed and declared as an OM external (see Preferences/Extrernals).
Csound can also be selected as a sound normalizer in OM sound processing (see Preferences/Audio).

" 
             (find-library "OM2Csound"))

; (gen-lib-reference (find-library "OM2Csound"))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(set-lib-release 2.2) 

(print "
;;;===================================================
;;;
;;; OM2Csound
;;; Control of Csound in OpenMusic
;;; see www.csounds.com
;;; L. Pottier, K. Haddad, M. Malt, C. Agon, J. Bresson 
;;; IRCAM 1993-2010
;;;
;;;===================================================
")

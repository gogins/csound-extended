;;; 
;;; ASDF 3.1 compliant system definition file for Drew Krause's nudruz codebase.
;;;
;;; Michael Gogins
;;;

;; We need to do some non-ASDF stuff in here first because nudruz uses clocc.

(require 'asdf)    
(print (format t "ASDF version: ~D~%" (asdf:asdf-version)))
#+sbcl
(require 'sb-introspect)
;;; Turn off all those zillions of SBCL warnings.
(declaim #+sbcl (sb-ext:muffle-conditions style-warning))
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
#+lispworks
(push "~/.local/share/common-lisp/source/" asdf:*central-registry*)
(print (format t "ASDF central registry: ~D~%" asdf:*central-registry*))

(load "~/.local/share/common-lisp/source/clocc/clocc.fasl")
(load (translate-logical-pathname "clocc:src;cllib;base"))
(load (translate-logical-pathname "clocc:src;cllib;matrix"))
(load (translate-logical-pathname "clocc:src;cllib;gnuplot"))
; (load (translate-logical-pathname "clocc:src;cllib;octave"))
(load (translate-logical-pathname "clocc:src;cllib;iter"))
(load (translate-logical-pathname "clocc:src;cllib;stat"))
(load (translate-logical-pathname "clocc:src;cllib;rng"))
#+sbcl
(require :sb-posix)

(asdf::defsystem "nudruz"
  :description "Algorithmic composition code/library/examples from Drew Kruase."
  :long-description "Algorithmic composition code/library/examples from Drew Kruase. This code is fairly old, has not been recently maintained, and has recently been ported to Steel Bank Common Lisp, so don't count on everything working."
  :version "1.0"
  :author "Drew Krause <drkrause@mindspringcom>, ported to SBCL and OpenMusic by Michael Gogins <michael.gogins@gmail.com>"
  :licence "LGPL"
  :depends-on ("alexandria" 
    "babel" 
    "bordeaux-threads"
    "cffi"
    "cl-heredoc"
    "cm2"
    "csound"
    "rsm-mod"
    "sb-csound"
    "sb-introspect"
    "sb-posix"
    "rsm-mod"
    "screamer"
    "trivial-features")
  ;:serial t ;; the dependencies are linear.
  :components  
  (;; MKG: The strategy here is to comment out demos etc. or anything else that makes a noise,
   ;; also maybe other things that won't load, and reorder by dependency if necessary.
  ;; MKG Moved all this stuff in nudruz.asd. (:file "cminit")
  (:file "example-csd") 
  (:file "data/besthex")
  ;; MKG: Corrupts things somehow. (:file "data/chords")
  (:file "data/codes")
  (:file "data/drumpatts")
  (:file "data/ordparts")
  ; MKG: Too big, sbcl runs out of dynamic space. (:file "data/partition-table")
  (:file "nudruz")
  ;; MKG: A bunch of tests and examples. (:file "scratch")
  (:file "tonnetz")
  (:file "a-star")
  (:file "beats")
  ;; MKG: Now standard? (:file "clos")
  ;; MKG: Not using. (:file "cminit-laptop")
  ;; MKG: An example. (:file "cmn")
  (:file "combinatorics")
  (:file "crawford")
  ;; MKG: An example. (:file "cs-demo")
  ;; MKG: Obsolete, no "defobject i?"(:file "csnd")
  (:file "cyclops")
  (:file "debruijn")
  (:file "designs")
  (:file "diffs")
  ;; MKG: A test, not using. (:file "dk-screamtest")
  (:file "nondet")
  (:file "graphs")
  (:file "inflect")
  (:file "lewin")
  (:file "mathieu11alt")
  (:file "mathieu11")
  (:file "mathieu12")
  (:file "mathieugrp")
  ;; MKG: Not using, no more Csound support in CM2. (:file "midi2cs")
  ;; MKG: Not using, no more Csound support in CM2. (:file "midi2csound")
  ;; MKG: Symbols redefined. 
  (:file "midi-import")
  (:file "modes")
  ;; MKG: Not using. (:file "modes-todo")
  (:file "motive")
  ;; MKG: Not using. (:file "nondet-todo")
  (:file "oddities")
  ;; MKG: Not using. (:file "old-cminit")
  (:file "phrasing")
  (:file "pplanes")
  (:file "reger")
  (:file "rewrite")
  ;; MKG: Not using. (:file "rewrite-todo")
  ;; MKG: Not using. (:file "rhythm")
  ;; MKG: New example. (:file "sb-tonnetz-csound")
  (:file "scales-csound")
  (:file "scanons")
  (:file "selfsim")
  (:file "serialism")
  (:file "slonimsky")
  (:file "spacegrp")
  (:file "spearframes")
  (:file "spears")
  (:file "tiles")
  (:file "tiling")
  ;; MKG: Not finished, incorrect syntax. (:file "tps")
  (:file "transforms")
  ;; MKG: Example, not using. (:file "tsp-dk") 
  (:file "withclocc")
  (:file "nudruz-csound")
   ))


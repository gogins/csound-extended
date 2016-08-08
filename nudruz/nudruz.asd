;;; 
;;; ASDF 3.1 compliant system definition file for Csound.
;;;
;;; Michael Gogins
;;; 11 July 2016
;;;
(declaim #+sbcl (sb-ext:muffle-conditions style-warning))
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
(require :asdf)
(asdf::defsystem "nudruz"
  :description "Algorithmic composition code/library/examples from Drew Kruase."
  :long-description "Algorithmic composition code/library/examples from Drew Kruase. This code is fairly old, has not been recently maintained, and has recently been ported to Steel Bank Common Lisp, so don't count on everything working."
  :version "1.0"
  :author "Drew Krause <drkrause@mindspringcom>, ported by Michael Gogins <michael.gogins@gmail.com>"
  :licence "LLGPL"
  :serial nil ;; the dependencies are not linear.
  :components  
  (;; MKG: The strategy here is to comment out demos etc. or anything else that makes a noise,
   ;; also maybe other things that won't load, and reorder by dependency if necessary.
  (:file "cminit")
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
  (:file "csnd")
  (:file "cyclops")
  (:file "debruijn")
  (:file "designs")
  (:file "diffs")
  (:file "dk-screamtest")
  (:file "graphs")
  (:file "inflect")
  (:file "lewin")
  (:file "mathieu11alt")
  (:file "mathieu11")
  (:file "mathieu12")
  (:file "mathieugrp")
  (:file "midi2cs")
  (:file "midi2csound")
  (:file "midi-import")
  (:file "modes")
  (:file "modes-todo")
  (:file "motive")
  (:file "nondet")
  (:file "nondet-todo")
  (:file "oddities")
  (:file "old-cminit")
  (:file "phrasing")
  (:file "pplanes")
  (:file "reger")
  (:file "rewrite")
  (:file "rewrite-todo")
  (:file "rhythm")
  ; MKG (:file "sb-tonnetz-csound")
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
  (:file "tsp-dk")
  (:file "withclocc")
  ))


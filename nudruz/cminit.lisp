;;;; Forms to be evaluated when Common Music starts.
;;;; Assumes that Common Music has already been loaded using asdf.

(in-package :cm)

;(load (merge-pathnames "clocc.lisp" *clocc-home*))
;(load (translate-logical-pathname "clocc:src;cllib;base"))
;(load (translate-logical-pathname "clocc:src;cllib;matrix"))
;(load (translate-logical-pathname "clocc:src;cllib;gnuplot"))
; (load (translate-logical-pathname "clocc:src;cllib;octave"))
;(load (translate-logical-pathname "clocc:src;cllib;iter"))
;(load (translate-logical-pathname "clocc:src;cllib;stat"))
;(load (translate-logical-pathname "clocc:src;cllib;rng"))
;(load (translate-logical-pathname "clocc:src;screamer;screamer"))
;(load "/usr/local/src/clocc/src/defsystem-3.x/defsystem.lisp")

;(load "nondet.lisp")
;(asdf:load-system :clm)
;(asdf:load-system :cffi)

;; a-star: used for reger paths
;(load (merge-pathnames "a-star.lisp" *nudruz-home*))

; I need the following two lines for SUSE & new gtk:
;(in-package :cl-user)
;(setq *gtk-libdir* "/opt/gnome/lib/")

;(asdf:load-system :cm-gtk)


;; rsm stuff
;(asdf:load-system :rsm-queue)
;(asdf:load-system :rsm-cache)

;; .. must treat 'rsm-fuzzy' differently ..
;(load "/home/drew/Lisp/rsm-fuzzy/package.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/classes.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/constructors.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/check.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/introspect.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/util.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/vars.lisp")
;(load "/home/drew/Lisp/rsm-fuzzy/protocol.lisp")

;(load "/home/drew/Lisp/cl-statistics/cl-statistics.lisp")

;(asdf:load-system :rsm-filter)
;(asdf:load-system :rsm-rand)
;(asdf:load-system :rsm-genetic-alg)
;(asdf:load-system :rsm-gen-prog)
;(asdf:load-system :rsm-modal)
(asdf:load-system :rsm-mod)
;(asdf:load-system :rsm-mpoly)

;(asdf:load-system :bpm)
;(asdf:load-system :cl-variates)

;(asdf:load-system :series)

(asdf:load-system :trivial-features)
(asdf:load-system :alexandria)
(asdf:load-system :babel)
;(asdf:load-system :cffi)
;(asdf:load-system :cmn)


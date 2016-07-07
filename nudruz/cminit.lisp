
;;; **********************************************************************
;;; $Name:  $
;;; $Revision: 1.1.1.1 $
;;; $Date: 2003/06/25 10:45:05 $

;;; Forms to be evaluated when Common Music starts.

; using CM 2.5.0

(cd "/home/drew/Lisp")
(load "cm/src/cm.lisp")
(cm)
(cd "/home/drew/Lisp/cm")

(in-package :cm)

(setq *clocc-root* "/usr/local/src/clocc/") 
(load (concatenate 'string *clocc-root* "clocc"))
(load (translate-logical-pathname "clocc:src;cllib;base"))
(load (translate-logical-pathname "clocc:src;cllib;matrix"))
(load (translate-logical-pathname "clocc:src;cllib;gnuplot"))
; (load (translate-logical-pathname "clocc:src;cllib;octave"))
(load (translate-logical-pathname "clocc:src;cllib;iter"))
(load (translate-logical-pathname "clocc:src;cllib;stat"))
(load (translate-logical-pathname "clocc:src;cllib;rng"))
(load (translate-logical-pathname "clocc:src;screamer;screamer"))
;(load "/usr/local/src/clocc/src/defsystem-3.x/defsystem.lisp")

;(load "nondet.lisp")
;(use-system :clm)
;(use-system :cffi)

;; a-star: used for reger paths
(load "a-star.lisp")

; I need the following two lines for SUSE & new gtk:
;(in-package :cl-user)
;(setq *gtk-libdir* "/opt/gnome/lib/")

;(use-system :cm-gtk)


;; rsm stuff
;(use-system :rsm-queue)
;(use-system :rsm-cache)

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

;(use-system :rsm-filter)
;(use-system :rsm-rand)
;(use-system :rsm-genetic-alg)
;(use-system :rsm-gen-prog)
;(use-system :rsm-modal)
(use-system :rsm-mod)
;(use-system :rsm-mpoly)

;(use-system :bpm)
;(use-system :cl-variates)

;(use-system :series)

(use-system :trivial-features)
(use-system :alexandria)
(use-system :babel)
(use-system :cffi)
;(use-system :cmn)


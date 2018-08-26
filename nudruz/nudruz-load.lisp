;;; This file loads nudruz and all of its dependencies, including Common Music 
;;; and Csound, into Lisp. Some of these systems can be installed using 
;;; quicklisp, others as Linux packages, others by cloning a git repository, 
;;; others by downloading an archive, and some are git submodules in the 
;;; csound-extended repository. Systems not currently working with nudruz are 
;;; commented out. ALL systems, including nudruz itself, should have symlinks 
;;; in ~/.local/share/common-lisp/source. 
;;;
;;; NOTA BENE: If you can't or won't install one of the systems, comment out 
;;; the load form here, and comment out code that depends on that system 
;;; in the source code (especially in nudruz.asd).

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
    
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

(asdf:load-system :alexandria)
(asdf:load-system :babel)
(asdf:load-system :bordeaux-threads)
; (asdf:load-system :bpm)
#-lispworks
(asdf:load-system :cffi)
(asdf:load-system :cl-heredoc)
; (asdf:load-system :cl-variates)
; (asdf:load-system :clm)
; clocc must be loaded as clocc.fasl, it doesn't have an asd file.
(defparameter *clocc-home* #P"~/.local/share/common-lisp/source/clocc/")
(load (merge-pathnames "clocc.fasl" *clocc-home*))
; (asdf:load-system :cm-gtk)
; Available as part of extended-csound.
(asdf:load-system :cm2) 
; (asdf:load-system :cmn)
; Available as part of extended-csound.
(asdf:load-system :csound)
; (asdf:load-system :rsm-cache)
; (asdf:load-system :rsm-filter)
; (asdf:load-system :rsm-gen-prog)
; (asdf:load-system :rsm-genetic-alg)
(asdf:load-system :rsm-mod)
; (asdf:load-system :rsm-modal)
; (asdf:load-system :rsm-mpoly)
; (asdf:load-system :rsm-queue)
; (asdf:load-system :rsm-rand)
; Available as part of extended-csound.
(asdf:load-system :sb-csound)
(asdf:load-system :sb-introspect)
(asdf:load-system :screamer)
; (asdf:load-system :series)
(asdf:load-system :trivial-features)

(asdf:load-system :nudruz)

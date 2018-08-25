#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
    
; These are dependencies that can be satisfied by using quicklisp;
; they should get symlinks to package names in ~/.local/share/common-lisp/source. 
;alexandria-20170830-git
;babel-20171227-git
;bordeaux-threads-v0.8.6
;cffi_0.19.0
;cl-heredoc-20101006-git
;screamer-20150709-git
;trivial-features-20161204-git
    
(require 'asdf)    
(format t "ASDF version: ~D~%" (asdf:asdf-version))
#+sbcl
(require 'sb-introspect)
#+lispworks
(push "/home/mkg/.local/share/common-lisp/source/" asdf:*central-registry*)


;;; Load the Common Foreign Function Interface.
;(ql:quickload "cffi")
#-lispworks
(asdf:load-system :cffi)

;;; Load the cl-heredoc library, which is used for embedding arbitrary 
;;; Csound code, including quotes and escapes, into Lisp code.
;(ql:quickload "cl-heredoc")
(asdf:load-system :cl-heredoc)

;;; Load Bordeaux threads.
;(ql:quickload "bordeaux-threads")
(asdf:load-system :bordeaux-threads)

;;; Load screamer for constraint programming.
;(ql:quickload "screamer")
(asdf:load-system :screamer)

;;; Turn off all those zillions of SBCL warnings.
(declaim #+sbcl (sb-ext:muffle-conditions style-warning))
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;;; Load Common Music.
;;; NOTA BENE: _SYSTEM_ "cm2" defines _PACKAGE_ "cm" (or :cm).
;(push "/home/mkg/common-music-git/cm2/" asdf:*central-registry*)
(asdf:load-system :cm2)

;;; Load Csound's ffi wrappers.
(push "/home/mkg/csound-extended/dependencies/csound/interfaces/" asdf:*central-registry*)
(asdf:load-system :csound)
(asdf:load-system :sb-csound)

;;; Load CLOCC.
;;; I can't get it to load using asdf.
(defparameter *clocc-home* #P"/home/mkg/csound-extended/dependencies/clocc/")
(push *clocc-home* asdf:*central-registry*)
(load #P"/home/mkg/csound-extended/dependencies/clocc/clocc.fasl")

;;; Load Drew Krause's code.
;;; I have moved loading or defining these pre-requisites out of nudruz.lisp 
;;; and cminit.lisp.
;;; This one must be installed with: sudo apt-get install cl-rsm-mod
(asdf:load-system :rsm-mod)
(defparameter *nudruz-home* #P"/home/mkg/csound-extended/nudruz/sources/")
(push *nudruz-home* asdf:*central-registry*)
(asdf:load-system :nudruz)

# NUDRUZ
## Michael Gogins

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is hosted with his permission. Drew's code is 
licensed under the terms of the GNU Lesser General Public License. The code has beenedited to make it more 
usable with Steel Bank Common Lisp and the current version of Csound. 


## Changes

1. All code has been brought into the Common Music package (that is, of course, the Lisp version of Common Music available 
from svn://svn.code.sf.net/p/commonmusic/code/branches/cm2). The reason for doing this is that some functions already 
were dependent upon Common Music code, and changing this would have required making many more changes. As a result, 
when you load this code, the effect is to vastly extend the power of Common Music.
1. Residual uses of Scheme syntax and of CLisp-compatible but Steel Bank Common Lisp-incompatible syntax have been ported 
to SBCL. In particular, `(define` has been replaced wth `(defparameter` if no parameters are used, or with `(defun` if parameters 
are used.
1. Csound classes in nudruz have been replaced with the Csound FFI facility, because the use of Csound is no longer supported in the Lisp branch of Common Music (cm2).
2. In a few cases, redefined symbols have been disambiguated.
1. I have created a `nudruz.asd` system definition. It tries to include everything except for examples and files containing unfinished or unworkable code. To use `nudruz.asd` you must first load `:cl-heredoc`, `:rsm-mod`, `:cllib`, and `:screamer`. Do that, for example, by having in your `.sbclrc` file something like:

<pre>
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
    

(require 'asdf)    
(require 'sb-introspect)

;;; Load the Common Foreign Function Interface.
(ql:quickload "cffi")

;;; Load the cl-heredoc library, which is used for embedding arbitrary 
;;; Csound code, including quotes and escapes, into Lisp code.
(ql:quickload "cl-heredoc")

;;; Load Bordeaux threads.
(ql:quickload "bordeaux-threads")

;;; Load screamer for constraint programming.
(ql:quickload "screamer")

;;; Turn off all those zillions of SBCL warnings.
(declaim #+sbcl (sb-ext:muffle-conditions style-warning))
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;;; Load Common Music.
;;; NOTA BENE: SYSTEM "cm2" defines PACKAGE "cm" (or :cm).
(push "/home/mkg/csound/cm2/" asdf:*central-registry*)
(asdf:load-system :cm2)

;;; Load Csound's ffi wrappers.
(push "/home/mkg/csound/csound/interfaces/" asdf:*central-registry*)
(asdf:load-system :csound)
(asdf:load-system :sb-csound)

;;; Load Drew Krause's code.
;;; I have moved loading or defining these pre-requisites out of nudruz.lisp 
;;; and cminit.lisp.
(asdf:load-system :rsm-mod)
(defparameter *nudruz-home* #P"/home/mkg/csound/gogins.github.io/nudruz/")
(defparameter *clocc-home* #P"/home/mkg/clocc-hg/")
(push *nudruz-home* asdf:*central-registry*)
;(load "/home/mkg/csound/gogins.github.io/nudruz/nudruz.lisp")
(asdf:load-system :nudruz)
</pre>

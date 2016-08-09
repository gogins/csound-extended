# NUDRUZ
## Michael Gogins

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is hosted with his permission. Ths code is 
licensed under Drew's the terms of the GNU Lesser General Public License. The code has been  edited to make it more 
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
1. I have created a `nudruz.asd` system definition. It excludes all examples and files containing unfinished or unworkable 
code. To use `nudruz.asd` you must first load `:rsm-mod` and `:cllib`. Do that, for example, by having in  
your `.sbclrc` file something like:

<pre>
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

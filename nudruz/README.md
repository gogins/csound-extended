# NUDRUZ
## Michael Gogins

This archive of Lisp code from [Drew Krause](http://www.drew-krause.com/) is hosted with his permission. 
The code has been lightly edited to make it more usable with Steel Bank Common Lisp and the current version of Csound. 
Drew's code also is licensed under the terms of the GNU Lesser General Public License.

## Changes

1. All code has been brought into the Common Music package (that is, of course, the Lisp version of Common Music available 
from svn://svn.code.sf.net/p/commonmusic/code/branches/cm2.
1. Residual uses of Scheme syntax and of CLisp-compatible but Steel Bank Common Lisp-incompatible syntax have been ported 
to SBCL. In particular, `(define` has been replaced wth `(defparameter` if no parameters are used or `defun` if parameters 
are used.
1. Csound classes in nudruz have been replaced with the Csound FFI facility. NOTE: Csound is not supported in the 
 Common Music Lisp branch (cm2).
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

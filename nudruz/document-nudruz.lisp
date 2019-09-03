;;; This file is intended to automatically generate reference documentation 
;;; for the nudruz system from declarations and docstrings. It must be run in 
;;; the nudruz directory. I have tried a number of tools as documented herein.
;;;
;;; Nothing really works so far. The problems seem to be caused first by 
;;; nudruz being a system but not a package, i.e. defining all symbols in the 
;;; :cm package, and second by not exporting any of those symbols.
(require :asdf)
(require :cm2)
(require :nudruz)

; helambdap seems to be more flexible than the others, is crashing out on 
; missing stuff but may provide help with stubbing it out or avoiding it.
; The navigation stuff is not created properly.
; 
; This program does not document Common Music itself. There may be a problem 
; with the on-the-fly creation of Lisp objects from Scheme code.

(require :helambdap)
; It seems to be necessary to exclude files that are excluded from nudruz.asd.
(helambdap:document 
    ; This should be the system or the directory where this program is executed.
    (asdf:find-system :nudruz)
    ;#P"~/csound-extended/nudruz/"
    :subsystems '(:cm2)
    :only-documented nil
    :destination #P"~/csound-extended/nudruz/docs/"
    :exclude-directories '(#P"./sources/data/"
        #P"~/csound-extended/nudruz/examples/"
        #P"~/csound-extended/nudruz/tmp/"
        #P"~/csound-extended/nudruz/docs/")
    :exclude-files '("~/csound-extended/nudruz/sources/scratch.lisp"
        #P"~/csound-extended/nudruz/sources/cmn.lisp"
        #P"~/csound-extended/nudruz/sources/cs-demo.lisp"
        #P"~/csound-extended/nudruz/sources/csnd.lisp"
        #P"~/csound-extended/nudruz/sources/dk-screamtest.lisp"
        #P"~/csound-extended/nudruz/sources/midi2cs.lisp"
        #P"~/csound-extended/nudruz/sources/midi2csound.lisp"
        #P"~/csound-extended/nudruz/sources/modes-todo.lisp"
        #P"~/csound-extended/nudruz/sources/package.lisp"
        #P"~/csound-extended/nudruz/sources/withclocc.lisp")
    :clear-documentation-db :before 
    :documentation-title "nudruz extends cm")
    
(print "Finished.")
(quit)

; staple "works" but the index page has no contents.

(ql:quickload :staple)
(staple:generate :nudruz)
(print "Finished.")
(quit)

; coo "works" but only for systems, not for packages, thus my issues cripple 
; it.

(require :coo)
;(coo::document-system :cm2 :base-path #P"~/csound-extended/nudruz/docs")
(coo::document-system :nudruz :packages '(:cm) :base-path #P"~/csound-extended/nudruz/docs")

(print("Finished.")
(quit)

; helambdap seems to be more flexible than the others, is crashing out on 
; missing stuff but may provide help with stubbing it out or avoiding it.
; The navigation stuff is not created properly.
; 
; This program does not document Common Music itself. There may be a problem 
; with the on-the-fly creation of Lisp objects from Scheme code.

(require :helambdap)
; It seems to be necessary to exclude files that are excluded from nudruz.asd.
(helambdap:document 
    ; This should be the system or the directory where this program is executed.
    (asdf:find-system :cm2) ; Doesn't work because .scm files are not .lisp
    ;#P"~/csound-extended/nudruz/"
    :everything '1
    :destination #P"~/csound-extended/nudruz/docs/"
    :exclude-directories '(#P"./sources/data/"
        #P"~/csound-extended/nudruz/examples/"
        #P"~/csound-extended/nudruz/tmp/"
        #P"~/csound-extended/nudruz/docs/")
    :exclude-files '("~/csound-extended/nudruz/sources/scratch.lisp"
        #P"~/csound-extended/nudruz/sources/cmn.lisp"
        #P"~/csound-extended/nudruz/sources/cs-demo.lisp"
        #P"~/csound-extended/nudruz/sources/csnd.lisp"
        #P"~/csound-extended/nudruz/sources/dk-screamtest.lisp"
        #P"~/csound-extended/nudruz/sources/midi2cs.lisp"
        #P"~/csound-extended/nudruz/sources/midi2csound.lisp"
        #P"~/csound-extended/nudruz/sources/modes-todo.lisp"
        #P"~/csound-extended/nudruz/sources/package.lisp"
        #P"~/csound-extended/nudruz/sources/withclocc.lisp")
    :clear-documentation-db :before 
    :documentation-title "nudruz extends cm")

(print "Finished.")
(quit)

; tinaa doesn't work at all.

;(require :tinaa)
;(document-system 'package "cm" "docs")

; codex requires that one declare every symbol to be documented in manual.scr, 
; no good.

(require :codex)
(codex:document :nudruz :manifest-path #P"~/csound-extended/nudruz/manifest.lisp")


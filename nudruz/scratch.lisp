(in-package :cm)

;(load "cminit.lisp")

;(load "nudruz.lisp")

;(load "inflect.lisp")
;(load "designs.lisp")
;(load "modes.lisp")
;(load "rewrite.lisp")
;(load "diffs.lisp")
;(load "nondet.lisp")
;(load "graphs.lisp")
;(load "phrasing.lisp")
;(load "tonnetz.lisp")
;(load "selfsim.lisp")
;(load "spears.lisp")
;(load "spacegrp.lisp")
;(load "transforms.lisp")
;(load "serialism.lisp")
;(load "motive.lisp")
;(load "reger.lisp")
;(load "combinatorics.lisp")

;(load "data/besthex.lisp")

;; KEEPERS

(events
 (splay
  (transp
   (mapcar (lambda (x) (car (stack-by x 7))) 
	   (flatter
	    (mapcar (lambda (x) (stravrot x 'n))
		    (subsequences (randhexrow) 6))))
   45)
  (makecyc (transp (code->durs (resclassvec 3 5 7)) 2)))
 "out.mid" :play 'nil)

;; WORK

(defparameter three 3)

(* 8 3)

(events
 (new seq :name 'sevens
      :subobjects
      (list (new cmn :time 0 :expr '(meter 7 8))
	    (new cmn :time 0 :expr 'cs-major)
	    (new cmn :time 0 :note 'c4
		 :duration (rhythm 'h..)
		 :data '(pppp))
	    (new cmn :time 3.5
		 :note 'df5 :duration (rhythm 'e)
		 :data '(accent sfz))
	    (new cmn :time 4 :note 'f4
		 :duration 2
		 :data '(mp fermata))))
 "sevens.eps")

(in-package :cm)
(in-package :cmn)
(use-package :cmn)


(defun testit (stf len nts)
  ;; midi channel numbers map to staves.
  (let ((nts (new heap :notes nts))
        (rhy (new weighting
               :of (list 1 
                         (new cycle :of '(1/2 1/2))))))
    (process while (< (now) len)
             for n = (next nts)
             for r = (next rhy)
             output (new midi :time (now)
                         :duration r
                         :keynum n
                         :channel stf)
             wait r)))

;;; define staff descriptions
(defparameter staffs
  '((0 :name "Viola" :clef :alto :meter (4 4))
    (2 :name "Flute" :clef :treble :meter (4 4))))

;;; generate an .eps file
(events (list (testit 0 12 '(c3 d ef f g))
              (testit 1 12 '(c5 d ef f g)))
        "testit.eps" 
        :staffing staffs
        :size 24
        :exact true
        :title "Hiho!")

;;; another way to set score properties 
(io "duet.cmn" :staffing staffs :size 20
    :exact true :title "Duet")

;;; generate a .cmn file
(events (list (testit 0 12 '(c3 d ef f g))
              (testit 1 12 '(c5 d ef f g)))
        "duet.cmn")






(defparameter myline
  (process repeat 30
           for dur = .25
           output (multievent 'midi :keynum
                              :keynum (round (+ (now) 60))
                              :time (now)
                              :duration dur)
           wait dur))

(events myline "mtb.midi" :play 'nil)

(note :keynum 63.4)

(defparameter snarepits (copylist '(38) 216))

(defparameter s2 (rwgen sfree3rules '(1) 2))


(defparameter s3 (rwgen sfree3rules '(1) 3))

(defparameter snp (tpoints snarepits s3 3))


(defparameter cymp (tpoints (copylist '(51 R R R R R) 36) 
                      (transp s2 6 #'*) 18))


(defun dkplay3 (pitvec)
    (process
      for x in pitvec
      for dur = .25
      when (or (numberp x) (listp x));; needed for rests
      output (multievent 'midi :keynum
                         :keynum x
                         :channel 9
                         :time (now)
                         :duration dur)
      wait dur))


(events (list (dkplay3 (scf '(36 40 42) s3))
              (dkplay3 cymp)) "dkplay3.midi" :play 'nil)


;; stravrots as verticalities
(events
 (let ((rots
        (stravrot (heapvec 5) 'nest)))
   (loop for n to (- (length rots) 1) collect
         (splay (transp (nth n rots) (* 12 (+ 3 n))) 1.5)))
 "out.midi" :play 'nil)


(let ((ms
       (merge-slots
        (stravrot (heapvec 7) 'nest))))
  (matchreg-chds 
   (cons
    (list (transp (car ms) 60))
    (cdr ms))))

(merge-slots
(stravrot (heapvec 3 12 60) 'nest))

(heapvec 5 12 60)

;; STRAV-EXPAND -- returns rot verticals following pit & line
(defun strav-expand (inpit inline)
  (let ((rots (merge-slots (stravrot inline 'nest))))
    (transp rots (- inpit (car rots)))))



(events
(splay
(strav-expand 60 (heapvec 4)) 1.6)
"out.midi" :play 'nil)

(strav-expand 60 '(6 3 8))

(first '(3 4 5))


(transp '(4 (5 10) 6) 60)

;;

(defparameter entro (flatten (transp (entropy '(0 1 3 5)) 60)))

(defparameter centro (au-contraire entro 50))

(defun dkplay4 (pits)
    (process
      for x in pits
      for dur = .125
      when (or (numberp x) (listp x));; needed for rests
      output (multievent 'midi :keynum
                         :keynum x
                         :time (now)
                         :duration dur)
      wait dur))

;; !!
(events (list (dkplay4 entro) (dkplay4 centro)) "dkplay4.midi" :play 'nil)


;; !!
(events 
 (dkplay4
  (make-poly
   (transp 
    (flatten (loop for x in (shuffle (permutations (indices 5))) collect
                   (give-contour-to-mel x '(5 3 6 2 0)))) 40) '(2 1 3 1 1)))
 "dkplay4.midi" :play 'nil)

;; !!
(events 
 (dkplay4
  (smoothlist
   (make-poly
    (transp 
     (flatten (loop for y in (entropy '(0 1 3 5)) collect
                    (give-contour-to-mel '(1 2 0 3) y))) 52)
    '(2 1 3 1 1))))
 "dkplay4b.midi" :play 'nil)

;; frags
(events 
 (let ((pitlist (smoothlist 
                 (entropy (matchreg (heapvec 4 12) '(36 60 72 84))))))
   (frags
    pitlist
    (transp (loop for x in pitlist collect (length x)) .25 #'*)
    (transp (heapvec 20 17) .25 #'*)))
 "dw.midi" :play 'nil)

;; arpegg
(events 
 (splay 
  (arpegg (heapvec 200 24 60) (heapvec 3 4 2) (heapvec 10 4 3))
  .125)
 "out.midi" :play 'nil)


;; extraction, au-contraire etc

(defparameter mypits
         (norpt (play-mode (flatten (loop for x in (heapvec 20 7) collect
                        (chd-inversion '(29 31 30 27) x))) stravmode)))

(events 
 (let* ((mypits (randsteps 70 55))
        (xtract5 (extract mypits (lambda (x) (eql 5 (mod x 6)))))
        (xtract3 (extract mypits (lambda (x) (eql 3 (mod x 4))))))
   (list
    (splay mypits .25)
    (splay 
     (doubler 
      (au-contraire
       (sublinepits (slowline mypits '(2 5))) 50) 7)
     (transp (sublinedurs (slowline mypits '(2 5))) .25 #'*))
    (splay (doubler (sublinepits xtract5) -16)
           (transp (sublinedurs xtract5) .25 #'*))
    (splay (doubler (sublinepits xtract3) 8)
           (transp (sublinedurs xtract3) .25 #'*))
    ))
 "out.midi" :play 'nil)

;; voice-leading etc.
(events
 (splay 
  (bestpath (primefilt (fromto '(61 72 75) '(50 65 75)) '(0 1 3)))
  6.0)
"bp.midi" :play 'nil)

;; tonnetz
(events
 (let ((tpits (smoothlist (transp (tzrandchain '(5 15 21) 200) 50))))
   (splay tpits (durweight tpits .5)))
 "out.midi" :play 'nil)

;; entropy
(events
 (let ((pits
        (smoothlist (entropy (transp '(50 36 57) 12)))))
   (splay pits (durweight pits .25))) "out.midi")

(events
 (let ((pits
        (smoothlist (entropy (transp '(50 36 57 61) 12)))))
   (splay pits (durweight pits .25))) "out.midi")

(events
 (let ((pits
        (smoothlist 


(events
 (let ((pits
        (smoothlist 
               (matchreg-chds (entropy (transp '(50 36 57 61) 12) 'iflag)))))
   (splay pits (durweight pits .25))) "out.midi")

               (matchreg-chds (entropy (transp '(50 36 57 61) 12) 'iflag)))))
   (splay pits (durweight pits .25))) "out.midi")


;; embell-triad
(events
 (let ((pits
        (smoothlist (embell-triad (transp '(50 36 57) 12)))))
   (splay pits (durweight pits .25))) "out.midi")


;; stack-by
(defparameter sb (stack-by (heapvec 5 12 40) 5))
(events 
 (splay (transp sb 14) 1.25) "out.midi")


;; GENERAL WORK BELOW

;;; 
;; general inflection for a line

(defparameter mylist (randsteps 50 10))

(defparameter mylist '(1 2 3 5 3 4 6))


;;; NEEDS WORK
(defun un-inflect (melody)
  (let ((mlint (melint melody)))
    (melint->line (car melody)
                  (loop for x to (- (length mlint) 2) collect      
                        (cond ((and (eql (nth x mlint) 2)
                                    (eql (nth (+ x 1) mlint) -2)) 
                               1)
                              ((and (> x 0)
                                    (eql (nth x mlint) -2)
                                    (eql (nth (- x 1 mlint) 2)))
                               -1)
                              (t (nth x mlint)))))))


;;; now for some diff equations

(define-diff diff1 
  (0 1)
  ((lambda (x) (* x x))
   (lambda (x) (mod x 3)))
  (lambda (x) (mod x 12)))

;; a loop
(defparameter diff1vals '(0 1 5 11 6 1 1 2 6 9 4 2 2 3 4 10 5 3))

(define-diff diff2
  (0 1 1)
  ((lambda (x) (* x x))
   (lambda (x) (mod x 3))
   (lambda (x) (* -1 x)))
  (lambda (x) (mod x 12)))

;(diff-vals 'diff2 0 30)
(defparameter diff2vals '(0 9 1 4 1 1 9 3 4 10 10 7))

(define-diff diff3
    (1 3 4)
  ((lambda (x) 0)
   (lambda (x) x)
   (lambda (x) (* -1 x)))
  (lambda (x) (mod x 12)))

(diff-vals 'diff3 0 20)
(defparameter diff3vals '(3 4 1 9 8 11))

(define-diff diff3b
    (1 1 4)
  ((lambda (x) 0)
   (lambda (x) (* 3 x))
   (lambda (x) (* -1 x)))
  (lambda (x) (mod x 12)))

(diff-vals 'diff3b 0 30)

(defparameter diff3bvals '(1 4 11 5 4 7 5 8 7 1 8 11)) 

(floor (* 12 (sin 2)))

(defparameter d12
  (apply-across #'-
                (diff-vals 'diff1 0 100)
                (diff-vals 'diff2 0 100)))

(defparameter d12b
  (apply-across #'min
                (diff-vals 'diff1 0 100)
                (diff-vals 'diff2 0 100)))


(define-diff diff3c
    (1 1 4)
  ((lambda (x) x)
   (lambda (x) (* 3 x))
   (lambda (x) (* -1 x)))
  (lambda (x) (mod x 12)))

(diff-vals 'diff3c 0 20)


(events (splay 
;(transp diff3bvals 50) 
         (transp (modmult diff3bvals 5 16) 50)
         .25) "out.midi")

;;;;; FUN WITH CM-2.7



;; the 'ran' function
;; :type :exponential -> centered around 0; low (dec.) :a = high entropy



(defparameter ranpits
 (loop repeat 200 collect
  (+ (round (* 10
               (ran :type :gaussian))) 60)))
;  (0 0 2 2 1 6 12 7 14 13 25 12 21 13 20 12 18 7 5 4)

(defparameter ranpits
 (loop repeat 200 collect
  (+ (round (* 40
               (ran :type :beta :a 2 :b 10))) 40)))
;; a=15 b=3
;; (0 0 0 0 0 0 0 0 0 0 0 4 2 2 18 31 39 45 49 10)
;; a=5 b=3
;; (0 0 0 0 5 2 3 9 7 9 17 23 14 23 31 18 23 7 9 0)
;; a=2 b=10
;; (12 32 46 30 37 18 7 9 7 1 1 0 0 0 0 0 0 0 0 0)

(defparameter ranpits
 (loop repeat 200 collect
  (+ (round (* .5
               (ran :type :cauchy :a 't) 60)))))
;; cauchy = very large domain
;; e.g. 
;; (7 44 59 7 12 188 30 5 213 47 2418 82 27 96 205 8 15 1 
;; 11 35 116 46 35 27 2519 16 22 215 117 24 11 
;; 212 11 2 4 66 0 10 5 36 13 86 79 25 1 80)
;; (histogram ranpits 0 5000 10) = (194 2 0 0 1 1 0 1 0 0)

(defparameter ranpits
 (loop repeat 200 collect
  (+ 30 (round (* 1
               (ran :type :poisson :a 30))))))
;; a=20
;; (0 0 0 2 13 28 44 52 34 22 2 3 0 0 0 0 0 0 0 0)
;; a=30
;; (0 0 0 0 0 0 7 15 22 42 42 33 22 10 3 3 1 0 0 0)

(defparameter ranpits
 (loop repeat 200 collect
  (+ 30 (round (* 1
               (ran :type :gamma :a 30))))))
;; a=30
;; (0 0 0 0 0 0 0 21 33 33 48 35 13 10 4 2 1 0 0 0)

(histogram ranpits 30 90 20)

(defparameter dpickpatt (new random of '(3 4 5)))

(defparameter ranpits
 (loop repeat 200 collect
  (+ (round (* 5
               (ran :type :exponential :a .6))) 50)))


(events
 (list
  (splay  (norpt ranpits)  .25)
  (let ((sd 
         (slots->durs
          (slowline (norpt ranpits) 
                    (listsub '(2 4 5) '(0 1 2)
                             (rwgen sfree3rules '(1 2 0) 3))))))
    (splay
     (transpose (first sd) 15)
     (transp (second sd) .25 #'*)
     ))
  (let ((sd2
         (slots->durs
          (extract ranpits (lambda (x) 
                             (and (eql 0 (mod x 3))
                                  (keynum x :in? stravmode)))))))
    (splay 
     (mapcar (lambda (x) 
               (if (numberp x) 
                   (list x (+ x (next dpickpatt))) 
                   'r)) 
             (first sd2))
     (transp (second sd2) .25 #'*))) 
  ) "ran.midi" :play 'nil)

 (defun msd2 ()
         (slots->durs
          (extract ranpits (lambda (x) 
                             (and (eql 0 (mod x 3))
; MKG: Not sureif mapcar is part of msd2 or not.
                                  (keynum x :in? stravmode)))))

     (mapcar (lambda (x) (list x (+ x (next dpickpatt)))) (first msd2)))

;; vln=40
;; vla=41
;; cello=42
;; trem=44
;; pizz=45


(events
 (splay
  (embell-triad (randvec 3 10 60))
  (quantlist (ransegs 17 :sum 20) .25)
  )
 "out.midi" :play 'nil)

(histogram
 (ransegs 17 :type :gaussian :sum 20)
 0 10 10)

(splay (durfunc 20 rs '(round (+ 50 (* 20 (sin x))))
       rs)
"out.midi" :play 'nil)


(mapcar (lambda (x) (quantize x .25))
(ransegs 20 :min 0 :max 20))



.25))

(quantize 3.4444 .25)

(quantlist (ransegs 20 :min 0 :max 20) .25)



(merge-slots (list (transp ranpits 12)
             (transp ranpits 17)))


(doubler ranpits '(12 11))

(tintab ranpits stravmode)

;;;;;; ransegs

(mapcar #'round (ransegs 10 :min 0 :max 100))
; (0 0 5 9 25 27 37 74 78 100)

(ransegs 10 :sum 300)
; (10.133752 5.1309223 49.804874 17.160149 23.859093 
; 13.688698 21.912834 62.28282 68.71735 27.30951)

(mapcar #'round (ransegs 10 :min 0 :max 100 :type :gamma :a 15))
; (0 6 25 27 36 37 38 45 56 100)

(mapcar #'round (ransegs 10 :sum 300 :type :poisson :a 50))
; a=50 (92 25 17 8 25 8 17 50 8 50)

(loop for x from 10 to 60 by 10 collect
      (mapcar #'round (ransegs 10 :min 0 :max 100 :type :poisson :a x)))
;; [skews away from divisor as :a increases]
; a=10 (0 10 20 30 40 50 70 80 90 100) 
; a=20 (0 18 29 35 41 47 71 76 82 100) 
; a=30 (0 17 29 42 46 54 58 62 96 100) 
; a=40 (0 12 17 29 33 42 46 71 75 100)
; a=50 (0 21 25 46 54 71 79 83 88 100) 
; a=60 (0 11 28 33 39 50 56 67 72 100)

(loop for x from 1 to 6 collect
      (melint->line 0 (mapcar #'round (explsegs 10 100 x))))
;(0 10 20 30 40 50 60 70 80 90 100) 
;(0 7 15 23 32 41 51 62 74 86 99) 
;(0 6 12 19 27 36 46 57 70 84 100) 
;(0 5 11 18 26 35 45 56 69 84 101)
; (0 4 9 15 22 30 40 51 64 80 99) 
; (0 4 9 15 22 30 40 51 65 81 101))


;;; overtones, spectral, etc.

(events
 (splay
  (list
   (mapcar #'round (keynum
                    (loop for x from 1 to 20 by 2 collect (* 30 (* 2 x)))
                    :hz #t)))
  5.0) "chord.midi" :play 'nil)

;; exponential warping of overtones
(events
 (splay
  (loop for n from 1 to 10 collect
        (remove-duplicates
         (let ((myexp (explsegs 10 20 n))) ; or reversed explsegs
           (mapcar #'round (keynum
                            (loop for x from 1 to 10 collect 
                                  (* 30 (* (nth (- x 1) myexp) x)))
                            :hz #t)))))
  5.0) "chord.midi" :play 'nil)

;; fm with changing carrier
(events
 (splay 
  (loop for x from 100 to 200 by 5 collect
        (mapcar #'round
                (fm-spectrum x 1.4 4 :spectrum ':keynum)))
  5.0) "chord.midi" :play 'nil)

;; ring modulation with changing carrier
(events
 (splay 
  (loop for x from 300 downto 200 by 5 collect
        (mapcar #'round
                (rm-spectrum (list x 400) 550 :hz #t :spectrum ':keynum)))
  5.0) "chord.midi" :play 'nil)

;;; --> use strength of partials/sidebands to drive # of repetitions etc.


;;; fun with modes

;drunk
(loop repeat 50
  for r = 40
  then (drunk r 2 :low 30 :high 50) 
  collect r)

;; humayun mode (smaller than mod12) steps '(1 3 1 2 2) = (0 1 4 5 7)
(events
 (splay (play-mode (randsteps 40 200 20 70) humayun)
        .125) "mode.midi" :play 'nil)

;; inflect
(events
 (splay 
  (flatten 
   (mode-inflect 
    (randsteps 40 200 20 70) 
    ionian
    '(dn 11 10)
    '(ln 2 (3 2 3))
    '(dn 5 6)))
  .125) "mode.midi" :play 'nil)

;; beta distribution, shushtar mode
(events
 (splay 
  (norests
   (remove-rpts
    (transp (play-mode
             (loop repeat 200 collect
                   (+ (round (* 40
                                (ran :type :beta :a 2 :b 10))) 40))
             shushtar) -10)))
  .125) "mode.midi" :play 'nil)


;;; canons?

;; finnissy effect
(events
 (let ((mypits (heapvec 100 10 50))
       (durs (strums 20 2 6 4 6)))
   (list
    (splay mypits (ferney '(1) '(4) durs))
    (splay (transp mypits 3) (ferney '(1) '(5) durs))
    (splay (transp mypits 21) (ferney '(1) '(3) durs)))) 
 "out.midi" '(0 2 7) :play 'nil)


;; converging-diverging
(events
 (let ((mypits (heapvec 1000 10 50))
       (durs (strums 2 2 6 4 6))
       (divlist '(2 3 4 5 6 8))
       (sectionlen 20))
   (list
    (splay 
     (heapvec 300 4 50)
     (sum-across-all (tempo-shape divlist sectionlen) durs))
    (splay 
     (heapvec 300 4 60)
     (sum-across-all (tempo-shape (reverse divlist) sectionlen) durs))))
 "out.midi" :play 'nil)



;; "chasing" tempi
(events
 (let ((mypits (heapvec 1000 10 50))
       (durs (strums 2 2 6 4 6))
       (divlist '(2 3 4 5 6 8))
       (sectionlen 20))
   (list
    (splay 
     (heapvec 300 4 50)
     (sum-across-all (tempo-shape divlist sectionlen) durs))
    (splay 
     (heapvec 300 4 60)
     (sum-across-all (tempo-shape (transp divlist 1) sectionlen) durs))))
 "out.midi" :play 'nil)


;; slot stress hierarchy as union of residue classes
;; example: Union of mods 2,3,5 below 
; [3=all 3 mods intersect, ....,0=no mods occur]
; (resclassvec 2 3 5) =
;  '(3 0 1 1 1 1 2 0 1 1 2 0 2 0 1 2 1 0 2 0 2 1 1 0 2 1 1 1 1 0))

(events
 (splay 
  (make-poly (heapvec 800 40 40) (resclassvec 2 3 11)) 
  .125)
 "out.midi" :play 'nil)

(events 
 (splay
  (mel-stress (heapvec 100 10 70) (transpose '(2 0 1 0) 1))
  .125) 
 "out.midi" :play 'nil)

(events
 (splay
  (merge-slots
   (list
    (mel-stress (heapvec 100 10 80) (resclassvec 2 3 5 7))
    (mel-stress (heapvec 100 10 70) (resclassvec 2 3 5 7))
    (mel-stress (heapvec 100 10 30) (resclassvec 2 3 5 7))
    (mel-stress (heapvec 100 10 50) (resclassvec 2 3 5 7))))
  .125)
"out.midi" :play 'nil)

(events
 (list
  (splay (pick-stress 100 
                      (transpose '((0 4 7) (2 9) (1 11)) 60) '(3 1 2 1)) .25) 
  (splay (pick-stress 100 
                      (transpose '((0 4 7) (2 9) (1 11)) 48) '(3 1 2 1)) .25) 
  (splay (pick-stress 100 
                      (transpose '((0 4 7) (2 9) (4 11)) 72) '(3 1 2 1)) .25))
 "out.midi" :play 'nil)

(events
 (let ((myvec (make-poly (heapvec 12 12) '(4 4 4))))
   (list
    (splay (pick-stress 1000 (transpose myvec 60) (resclassvec 2 3 5)) .125) 
    (splay (pick-stress 1000 (transpose myvec 36) (resclassvec 2 3 5)) .125) 
    (splay (pick-stress 1000 (transpose myvec 72) (resclassvec 2 3 5)) .125)))
 "out.midi" :play 'nil)

(events
 (splay
  (poly-stress (heapvec 150 50 30) '(3 1 2 1)) .125) 
 "out.midi" :play 'nil)


;; mt-areas

(events
 (splay
  (make-poly (play-mode (heapvec 300 30 20) stravmode) (randvec 20 2 1))
  (sum-across-all (mt-rhyareas 2 3 (heapvec 20 7 3) 2)
                  (strums 7 2 3 4 7)))
 "out.midi" :play 'nil)

;; contour

(defparameter nt6 (ntn->clists '(-1 0 1) 6))

(events
 (let ((hv (safesort (heapvec 6 60 30))))
   (splay
    (loop for n in (flatten nt6) collect (nth n hv)) 
    (makecyc (transp '(.25 .25 .25 .5) 1 #'*))))
 "out.midi" :play 'nil)

;;;;;;; work on 'phrasing'

; (defun frasetree (pitvec list-of-levels basedur) ; returns midi
; (defun frasetree-pits (pitvec list-of-levels) ; returns pit-slot vector

(events
 (frasetree (heapvec 30 10 60) '((7 2 5) (10 7)) 2)
 "out.midi" :play 'nil)

(events
 (let ((mypits
        (frasetree-pits (heapvec 30 10 60) '((7 2 5) (10 7)))))
   (splay mypits (durweight mypits .25)))
   "out.midi" :play 'nil)


;; example mixed phrase lengths 
(events
 (let* ((phraslens (loop repeat 100 collect (pick 2 3)))
        (sloline (randsteps 60 100))
        (basedur .25)
        (slodurs (transp phraslens basedur #'*))
        (fastfigs (loop for n to (- (length phraslens) 1) collect
                        (transp 
                         (if (eql (nth n phraslens) 2) '(7 2) '(7 5 4))
                         (nth n sloline)))))
   (list
    (splay sloline slodurs)
    (splay (flatten fastfigs) basedur))) "out.midi" :play 'nil)


;;; --->> BUT I want to generalize to all pattern types

;; READLIST -- simple utility
;; reads first 'readlen' members of 'alist'
;; (readlist 3 '(1 2 3 4 5)) = (1 2 3) 
(defmacro readlist (readlen alist 
                            &optional (pattype 'line) (args :name 'whatever))
  `(next (new ,pattype :of ,alist ,args) ,readlen))

(readlist 30 '(1 2 32 4))

(readlist-mac 30 (indices 5) heap)

(funcall list '(2 3 4))

;;; checking out new pattern functionality
(defparameter mychord
  (new chord :of (new heap 
                   :notes '(c5 d ef f g af bf c6)
                   :for (new rewrite 
                          :of (listsub '(1 2 3) '(0 1 2) pleasantsrules)))))
;; !
(events
 (splay
  (next mychord 100)
  .25) "out.midi" :play 'nil)

(defparameter cyc1 (new cycle :of '(0 1 10)))
(defparameter cyc2 (new cycle :of '(1 10 0)))

(defparameter plcyc
(new rewrite
          :of (listsub '(1 2 3) '(0 1 2) pleasantsrules)))

(next plcyc #t)

(defparameter pat1
  (new random :of (list (new cycle :keynums '(a4 b c5 d))
                        (new heap :keynums '(gs4 as cs5 ds)))))

(defparameter pat2
  (new cycle :of  `(100 ,(new line :of (indices 21) :for 4) 2887
                    ,(new random :of '(44 55 66 77) :for 2))))

(next pat2 #t)

; mixing patterns using "make-instance"
(defun play-pats (pats trope reps rate)
  (process with dur = (* rate 2.5)
           repeat reps
           for len = (pick 8 12 16)
           for pat = (make-instance (next pats)
                                    :keynums trope
                                    :for len)
           each k in (next pat #t) as x from 0 by rate
           output (new midi :time (+ (now) x)
                       :keynum k
                       :duration dur)
           wait (* rate len)))

(defparameter pcns
  (new random :of '((heap :weight 2) line cycle
                    (palindrome :elide #t) rotation)))

(events (play-pats pcns '(a4 b c5 d) 12 .1) "test.mid")

(defparameter pat1
  (new cycle :of (list (new cycle :of '(50 51 51 53)
                            :for (new cycle :of '(4 3 2 1 0)))
                       (new cycle :of '(70 71 72 73)
                            :for (new cycle of '(0 1 2 3)))
                       (new cycle of '(100 200) :for 1))))

(next pat1 #t)

(defparameter x (new copier :of (new cycle :of '(a b c) :for 2)
               :for 3))


(defparameter pat1
  (new random :of (list (new cycle :keynums '(a4 b c5 d))
                        (new heap :keynums '(gs4 as cs5 ds)))))

(note (next pat1 10))

(defparameter xpat (new cycle :of (indices 10) :for (new heap of (indices 3 5))))

(loop repeat 10 collect
(next xpat #t))





;; RAND-POINTLENS -- generates a length for each member of list
(defun rand-pointlens (alist minlen maxlen)
  (loop repeat (length alist) 
    collect (between minlen (+ 1 maxlen))))

(rand-pointlens '(1 2 3 4) 3 5)

(let ((mylist '(1 2 3 4)
(randvec 10 4 32)


;; READTHRU to apply any pattern type
(defmacro readthru (len pattlist &optional (ptype 'line))
  `(next (new ,ptype :of ,pattlist) ,len))

(readthru 10 (indices 4) )

;; trying to do READTHRU on list
(defmacro readthru-list (lens pattlist &optional (ptype 'line))
  (let ((patt `(new ,ptype :of ,pattlist
                :for (new cycle of ,lens))))
    (loop repeat (length lens) collect (next patt #t))))

(readthru-list (randvec 5 5 4) (indices 10 50))


;; same figure starts at each dbl point
(defun fig-descent (alist 

;; list continues thru each dbl point
(defun list-descent (

;; pattern returns 'next' at each dbl point
(defun pattern-descent

(defparameter apitlist 
  '(68 67 62 66 65 60 63 69 61 64 60 66 63 67 68 65 61 69 62 64))
; (slowline apitlist '(2 3))
; (68 R 62 R R 60 R 69 R R 60 R 63 R R 65 R 69 R R)
; (slots->durs (slowline apitlist '(2 3)))
; ((68 62 60 69 60 63 65 69) (2 3 2 3 2 3 2 3))

;; for reference:              
;; FRASETREE-PITS
;; general recursive function (builds pitch-slot vector)
;; pitvec = base melody
;; list-of-levels = figures from slow to fastest [list of lists]
;; e.g.  (frasetree-pits (randvec 26 8 20)
;                        (list (transp (randvec 5) 14)
;                              (transp (randvec 4) 23)))
(defun frasetree-pits (pitvec list-of-levels)
  (let ((pitlen (length pitvec))
        (bigdur (apply #'* (mapcar #'length list-of-levels))))
    (merge-slots
     (list (menses pitvec bigdur)
            (loop for x in pitvec
                  when list-of-levels
                  append (phrasetree-pits
                           (transp (car list-of-levels) x)
                           (cdr list-of-levels)))))))

(frasetree-pits (randvec 26 8 20)
                        (list (transp (randvec 5) 14)
                              (transp (randvec 4) 23)))

;; diachro-classes

;; 2/3 is a great value!!
(events
 (let ((shf2d3 (shuffle (diachrom-filt (subsets-len (indices 12) 4) 2/3))))
 (splay (transpose shf2d3 60) .75)) "out.midi" :play 'nil) 

(events
 (let ((pits
        (transpose 
         (placereg 
          (flatten
           (smoothlist 
            (shuffle (diachrom-filt (subsets-len (indices 12) 4) 2/3))))
          following-5) 12)))
   (splay pits (durweight pits .25)))
 "out.midi" :play 'nil)

(smoothlist (shuffle (diachrom-filt (subsets-len (indices 12) 4) 2/3)))



(placereg '(4 5 9 11) '(4 1 2 5))


(events
 (let ((shf2d3 (shuffle (diachrom-filt 
                         (proper-subsets (indices 12)) 
                         2/3))))
   (splay (transpose shf2d3 60) .75)) "out.midi" :play 'nil) 

;; 2/4 is also good value!
(events
 (let ((shf2d3 (shuffle 
                (diachrom-filt (subsets-len (indices 12) 4) 2/4))))
   (splay (transpose shf2d3 60) .75)) "out.midi" :play 'nil) 

;; 2/2 has some chromatic "bite"
(events
 (let ((shf2d3 
        (matchreg-chds (cons '(36 60 80)
                             (shuffle 
                              (diachrom-filt 
                               (subsets-len (indices 12) (indices 3 3)) 2/2))))))
   (splay (transpose shf2d3 12) .75)) "out.midi" :play 'nil)



;; match-slots
;; place a subvector where it matches within larger vector



;;;;other



(events
 (splay
  (loop for chd in 
        (loop for x from 100 to 200 by 5 collect
              (mapcar #'round
                      (fm-spectrum x 1.4 4 :spectrum ':keynum)))
        append
        (next 
         (new chord 
           :of (new heap
                 :notes chd
                 :for (new rewrite 
                        :of (listsub '(1 2 3) '(0 1 2) pleasantsrules))))
         100))
  .25) "out.midi" :play 'nil)

(fm-spectrum 400 1.4 4 :spectrum ':keynum)

(defparameter fmspvec
  (loop for chd in 
        (loop for x from 100 to 200 by 5 collect
              (mapcar #'round
                      (fm-spectrum x 1.4 4 :spectrum ':keynum)))
        append
        (next 
         (new chord 
           :of (new heap
                 :notes chd
                 :for (new rewrite 
                        :of (listsub '(1 2 3) '(0 1 2) pleasantsrules))))
         100)))

(events
 (splay
  (conjunct-fragments (topline (keynum fmspvec))) .25) "out.midi" :play 'nil)


(parse-by-reg (topline (keynum fmspvec)))

(defun choral (alist)
  (let ((pbr (parse-by-reg alist)))
    (loop for p in pbr collect (slots->durs p))))

(choral (topline (keynum fmspvec)))

(events
 (loop for x in (choral (topline (keynum fmspvec))) collect
       (splay (first x) (transp (second x) .25 #'*)))
 "out.midi" :play 'nil)



;; PARSE-BY-REG -- splitting melody into slotlist by register
;; arbitrary range divisions (octsize)
;; (parse-by-reg '(60 2 30 31 61 62)) 
; = ((R 2 R R R R) (R R 30 31 R R) (60 R R R 61 62))
(defun parse-by-reg (melody &optional (octsize 12) (base 0))
  (let* ((mel-octs 
          (mapcar (lambda (x) (floor (/ (- x base) octsize))) melody))
         (allocts (remove-duplicates (safesort mel-octs))))
    (loop for x in allocts collect
          (loop for y to (- (length mel-octs) 1) collect
                (if (eql (nth y mel-octs) x) (nth y melody) 'r)))))

(parse-by-reg
(make-poly 
(randvec 80 50) '(2 3)))

;; new utility for redefined 'parse-by-reg'

;; TAKE-REG -- determines [octave] register of pitch num or list
;; added December 2005
(defun take-reg (input &optional (octsize 12) (base 0))
  (typecase input 
    (symbol 'r)
    (number (floor (/ (- input base) octsize)))
    (list (mapcar (lambda (x) (take-reg x octsize base))
                  input))))


;; REDEFINE 'PARSE-BY-REG' TO ACCEPT CHORDS & RESTS: 

(defun pbr (melody &optional (octsize 12) (base 0))
  (let* ((mel-octs (take-reg melody))
         (allocts (remove-duplicates (safesort (flatten mel-octs)))))
    (loop for x in allocts collect
          (loop for y to (- (length mel-octs) 1) collect
                (let ((thismo (nth y mel-octs)))
                  (flet ((putreg (s) (if (eql thismo s) (nth y melody) 'r)))
                    (typecase thismo
                      (symbol 'r)
                      (number (putreg thismo))
                      (list (loop for t in thismo collect (putreg t))))))))))


(pbr '(10 20 30 40))


                (if (numberp (nth y mel-octs))
                    (if (eql (nth y mel-octs) x) (nth y melody 'r))
                           
                (if (eql (nth y mel-octs) x) (nth y melody) 'r)))))

(symbolp 'r)

(pbr (make-poly (randvec 80 50) '(2 3)))

(defun whatfunc (anum)
  (let ((thisval 10))
    (flet ((thisf (x) (* x x)))
      (+ thisval (thisf anum)))))

(whatfunc 7)

;;; testing recursive rewrite
;; outputs midi
(defun rwrecursion (alist tlevels rwrules)
  (let ((rwlen (length (car (last (first rwrules))))))

;; test with 's4freerules'

(defun rwrecursion-midi (tone melody transpvec basedur)
  (let ((len (length melody))
        (levels (length transpvec))
        (rwlen (length (car (last (first rwrules))))))
    (process for i in melody
             for k = (transpose tone i)
             ;; play current tone in melody
             output (new midi :time (now) :duration dur 
                         :amplitude .5 :keynum k
                         :channel 0)
             when transpvec
             ;; sprout melody on tone at next level
             sprout (rwrecursion (transpose k 12)
                                melody
                                (- levels 1)
                                (/ dur len) 
                                amp)
             wait dur)))

(events (sierpinski 'a0 '(0 7 5) 4 3 .5) "sier.midi" :play 'nil)

(defun sierpinski (tone melody levels dur amp)
  (let ((len (length melody)))
    (process for i in melody
             for k = (transpose tone i)
             ;; play current tone in melody
             output (new midi :time (now) :duration dur 
                         :amplitude amp :keynum k
                         :channel 0)
             when (> levels 1)
             ;; sprout melody on tone at next level
             sprout (sierpinski (transpose k 12)
                                melody
                                (- levels 1)
                                (/ dur len) 
                                amp)
             wait dur)))

(loop for x in (randvec 10 4) collect
      (rwgen sfree4rules (list x) 2))

(defparameter sf4 (new rewrite of :sfree4rules))

(menses '(1) 4)

;; sfree3rules is non-uniform [size=3]

(defparameter rw-sf3
  (new rewrite 
    :of sfree3rules
    :initially '(1)))

(loop for x in '(0 10 20 30)

(defun rwrecurs (mel levels rwrules)
  (list (transpose mel (nth 0 levels))
        (transpose (loop for x in mel
                         collect (rwgen rwrules x 1))
                   (nth 1 levels))))

(rwrecurs (indices 4) '(60 70) sfree3rules)
          


(cdr '(1))

;; CURVES
;; time/pitch equations etc.

(defun mtb (mfact subdv)
  (process
    for dur = (/ 1 subdv)
    until (> (now) 20.0)
    output (multievent 'midi :keynum
                       :keynum (+ (expt (now) mfact) 30)
                       :time (now)
                       :duration dur)
    wait dur))

(events 
 (list (mtb 1.1 4) (mtb 1.3 5) (mtb 1.4 7))
 "mtb.midi" :play 'nil)


(defun mtb (mfact subdv)
  (process
    for dur = (/ 1 subdv)
    until (> (now) 20.0)
    output (multievent 'midi :keynum
                       :keynum (+ (* 20 (sin (* (now) mfact))) 60)
                       :time (now)
                       :duration dur)
    wait dur))

(events 
 (list (mtb 1.1 4) (mtb 1.3 5) (mtb 1.4 7))
 "mtb.midi" :play 'nil)


(defun mtb (mfact subdv)
  (process
    for dur in (transp (ferney '(1) (strums 10 2 5 3 6) '(1 1 1 2))
                      (/ 1 subdv) 
                      #'*)
    until (> (now) 20.0)
    output (multievent 'midi :keynum
                       :keynum (+ (* 20 (sin (* (now) mfact))) 60)
                       :time (now)
                       :duration dur)
    wait dur))

(events 
 (list (mtb 1.1 4) 
;(mtb 1.3 5)
 (mtb 1.4 7))
 "mtb.midi" :play 'nil)


(defun mtb (mfact subdv)
  (process
    for dur = (/ 1 subdv)
    until (> (now) 20.0)
    output (multievent 'midi :keynum
                       :keynum (+ (* 20 (sin (* (now) mfact))) 60)
                       :time (now)
                       :duration dur)
    wait dur))

(events 
 (list 
(mtb 1.1 4) 
;(mtb 1.3 5) 
;(mtb 1.4 7)
)
 "mtb.midi" :play 'nil)


(events
 (list
  (splay (durfunc 20 .125 '(round (+ 50 (* 20 (sin (/ x 6)))))) .125)
  (splay (durfunc 20 .25 '(round (+ 53 (* 20 (sin (+ .3 (/ x 6.1))))))) .25))
 "out.midi" :play 'nil)

(sqrt 1.1)

(events
 (list
  (splay (durfunc 30 1/3 '(round (+ 30 (* 8 (sqrt x))))) 1/3)
  (splay (durfunc 30 1/2 '(round (+ 30 (* 8 (sqrt (* 1.1 x)))))) 1/2)
  (splay (durfunc 30 1/6 '(round (+ 30 (* 8 (sqrt (* 1.16 x)))))) 1/6)
)
 "out.midi" :play 'nil)


(events
 (list
  (splay 
   (durfunc 20 '(.5 .75) '(round (+ 50 (* 20 (sin x))))) (makecyc '(.5 .75)))
  (splay
   (durfunc 20 1/3 '(round (+ 50 (* 17 (sin x))))) 1/3)
  )
 "out.midi" :play 'nil)

(exp 4.0)

(timefunc (indices 10 30) '(sin x))

(durfunc 4 .5 '(* 20 (sin x)))


;; plotter

(defparameter myplot
(plotter :zoom .5
         (loop for x from 0 to 1 by .2
               for y = (random 1.0)
               collect x collect y)))



(next
(new range :from 10 :pickto 30)
50)

(scale-order '(8 2 11 5 11) :down)

;; spears stuff

(load "spears/partials8hz-as.lisp")

(defparameter pars8slots
  (spear->slots partials8hz-as))

(defparameter pars8mels
  (spearmels partials8hz-as))

(defparameter qpatt (new random of '(1/6 1/4 1/3)))


;;; !!!
(events
 (loop for mel in pars8mels collect
       (let ((qd (quantdurs (first mel) 
                            (sum-across '(1/5) (second mel))
                            (next qpatt))))
         (splay (first qd) (second qd))))
 "out.midi" :play 'nil)


(events
 (playspears partials8hz-as .125)
 "out.midi" :play 'nil)

(spear->slots partials8hz-as)


(load "spears/cowboys40hz-as.lisp")

(spear->slots cowboys40hz-as)

(loop for x in 

(events (playspears cowboys40hz-as .25) "out.midi" :play 'nil)

(spear->slots cowboys40hz-as)


(events
 (let ((qd
        (quantdurs
         (randvec 200 40 44)
         (loop repeat 100 collect
               (ran :type :uniform :below .5))
         '(1/4 1/3))))
   (splay (first qd) (second qd)))
 "out.midi" :play 'nil)

;;;; 

;; drums: channel 9
;        35 /usr/share/timidity/instruments/kick1
;        36 /usr/share/timidity/instruments/kick2
;        37 /usr/share/timidity/instruments/stickrim strip=tail
;        38 /usr/share/timidity/instruments/snare1
;        39 /usr/share/timidity/instruments/claps
;        40 /usr/share/timidity/instruments/snare2 note=38
;        41 /usr/share/timidity/instruments/tomlo2
;        42 /usr/share/timidity/instruments/hihatcl
;        43 /usr/share/timidity/instruments/tomlo1
;        44 /usr/share/timidity/instruments/hihatpd
;        45 /usr/share/timidity/instruments/tommid2
;        46 /usr/share/timidity/instruments/hihatop
;        47 /usr/share/timidity/instruments/tommid1
;        48 /usr/share/timidity/instruments/tomhi2
;        49 /usr/share/timidity/instruments/cymcrsh1
;        50 /usr/share/timidity/instruments/tomhi1
;        51 /usr/share/timidity/instruments/cymride1
;        52 /usr/share/timidity/instruments/cymchina
;        53 /usr/share/timidity/instruments/cymbell

;; !!
(events 
 (let ((bdurs (ferney '(1) '(2 3))))
   (list
    (ryte 47 (extractdurs '((.5) 1.5) bdurs))
    (ryte '(51 51 51 53) 
          (resultant bdurs
                     (list (ferney '(2) '(2 3)) 1)))
    (ryte (listsub '(35 R 38) '(1 2 3) (strums 4 2 3 2 5))
          (erasedurs '(1) bdurs))
    ))
 "out.midi" :play 'nil)


(avoidurs (ferney '(1) '(4)) (ferney '(1) '(2)))

(events
 (ryte 47
       (resultant (ferney '(1) '(2 3 3))
                  (ferney '(1/2) '(3 2))))
 "out.midi" :play 'nil)

(events
 (splay
  (make-poly
   (play-mode (randvec 100 40 20) stravmode)
   (resclassvec 4 5 7))
  .125)
 "out.midi" :play 'nil)

(make-poly (indices 40) '(2 1 0 3))

;;;; durs->texture

(events
 (let* ((bdurs (ferney '(1) '(2 3)))
       (res
        (resultant
         bdurs
         (extractdurs '((.5) 1.5) bdurs)
         (resultant bdurs
                    (list (ferney '(2) '(2 3)) 1))
         (erasedurs '(1) bdurs))))
   (splay (make-poly (heapvec 200 50 30) (randvec 10 3 1))
          res))
 "out.midi" :play 'nil)

;; some problems here
(defun durs->texture (&rest durlist)
  (let* ((res (resultant durlist))
         (resatx (melint->line 0 res)))
    resatx))


;;; self-stretto

(events
 (splay
  (transp 
   (self-stretto (randrests (heapvec 20 10) 26) 5 5 2)
   50)
  .25)
 "out.midi" :play 'nil)


;; more selfsim
(events
 (selfsim cyclops2p5x9 '(0 2 3) '(2 5 9) '(68 60 76) .25)
 "out.midi" :play 'nil)

(take-poly
 (selfsimvec cyclops2p5x9 '(0 2 3) '(2 5 9) '(68 60 76)))

;;
(defun rp-related (seta setb)
  (


(intersection '(1 (2 3) 4) '(0 1 (2 3)))

;; scanons

(scanons 4 7 3)

(events
 (splay
  (stack-can (transp '(0 3 6 5) 50) 7 3)
  1.25) "out.midi" :play 'nil)

(events
 (splay
  (loop for scan in 
        '((0 4 7 11) (0 3 6 10) (0 3 7 10) (0 4 7 10) 
          (0 3 6 9) (0 3 7 5) (0 4 7 6)
          (0 -1 2 6) (0 3 6 5) (0 4 2 6) (0 -2 2 5) 
          (0 -1 2 5) (0 3 2 5) (0 -2 2 0)
          (0 -1 2 1))
        append (stack-can (transp '(0 3 6 5) (+ 50 (random 12))) 7 3))
  .75) "out.midi" :play 'nil)


(events
 (splay
   (make-poly
    (flatten
    (norests
     (remove-rpts
      (transp
       (expand 
        (make-poly (randvec 12 12) '(2 3 4 3)) 'flat)
       30))))
    '(1 1 2))
  .125) "out.midi" :play 'nil)

(tievec '(1 2 2 4))

collect (transp 30 x))

(scanons 4 7 3)


(transp-to 1 '(4 5 6))

(loop for 
(only-primes scanons-4-7-3)


;; rsm stuff


(defparameter myq (rsm.queue::list->queue (indices 5)))
(defparameter myq2 (rsm.queue::list->queue (heapvec 4 4 10)))

(rsm.queue::get-last myq2)

(defparameter nuq (rsm.queue::append-queue myq myq2))

nuq
myq 

(rsm.queue::empty-p myq)

(rsm.queue::empty-p myq)

(rsm.queue::enqueue 10 myq)

(rsm.queue::dequeue myq)

(rsm.queue::queue->list myq)

myq


;;; PACKAGE FUN

;; CL-VARIATES

; (binomial n p) = # heads when coin flipped n times with probability p
(loop repeat 20 collect
      (cl-variates::binomial 100 .2))

; (geometric p) = # flips until it comes up heads
(loop repeat 40 collect (cl-variates::geometric .5))

;; BPM

;; pattern matching -- all sublists starting with 1 are preserved; the rest manipulated
(loop for x in 
      (make-poly (heapvec 200 2 1) (randvec 19 3 2))
      collect
      (bpm::match x
	((1 . _t) -> x)
	(_w -> (heapvec 3 20))))

;; CL-STATISTICS

;; median value
(let ((myseq (randvec 50 50)))
  (list myseq 
	(statistics::median myseq)))

(let ((myseq (randvec 50 20)))
  (list myseq 
	(float (statistics::variance myseq))))


;; click trax

(events
 (list
  (splay (copylist (list 24) 120) 1.0)
  (splay (copylist (list 25) 2400) .5)
  (splay (copylist (list 26) 480) .25)
  (splay (copylist (list 27) 960) .125)

  (splay (copylist (list 28) 360) 1/3)
  (splay (copylist (list 29) 720) 1/6)
  (splay (copylist (list 30) 1440) 1/12)

  (splay (copylist (list 32) 1200) 1/10)
  (splay (copylist (list 34) 1400) 1/14))
 "out.mid" :play 'nil)


;; fixing scores etc.

(events
 (let ((invec
	(numidi-in "sqtfix/m1sfva.mid")))
   (tplay 
    (list (first invec)
	  (transp (second invec) 2.0 #'*)
	  (transp (third invec) 2.0 #'*))))
 "sqtfix/m1sfvaz.mid" :play 'nil)


(length
(tightrows (heapvec 12) 'grand 7)

(grandrows (heapvec 12))


;;; spacegrp w/ register?

; (defun spacegrp->motive (a-mxlist pitlist durlist &optional (mxtype 'list))


(load "spacegrp.lisp")

(events
 (splay

	    (spaceregchain
	     (chooser (heapvec 10 6) mqmat)
	     '(4 r 5 r 4))

  .25)
 "out.mid" :play 'nil)



(placereg (indices 12) '(3 4 r (3 4)))


(spacereg mz8 '(r (3 4) 3 r 5))


(let* ((myrow (heapvec 12))
       (inv (reverse myrow))
       (pos1 (indices 6))
       (pos2
	(list-complement (indices 12) pos1)))
  (mapcar #'safesort
	  (list
	   (nths pos1 inv)
	   (nths pos2 inv)
	   (nths pos1 myrow)
	   (nths pos2 myrow))))



(let* ((pos1 '(0 4 5 6 9 11))
       (pos2 (list-complement (indices 12) pos1))
       (row1 (heapvec 12))
       (row2 (invert-order row1 7)))
  (list row1 row2
	(mapcar #'safesort
		(list
		 (nths pos1 row1)
		 (nths pos2 row1)
		 (nths pos1 row2)
		 (nths pos2 row2)))))


(invert-order (indices 12 32) 7)


; FROM BOB COYNE

(in-package :cl-user)

(defparameter *question-file* "/tmp/numlist")

#|
;; example contents of question file:
questionTemplate: FIND STATEMENTS BY (Evan Bayh) on (American elections)
questionParaphrase: What did Evan Bayh say about American elections?
location: null
time: null
maximumAnswerLength: 598
documentId: ENG_STEXT-2
questionId: 1058
templateId: 29
topic argument: American elections
|#

(defun read-ascii-list (&optional (file *question-file*)))

;; example of reading an ascii file
(defun read-question (&optional (file *question-file*))
  (with-open-file (instream file :direction :input)
		  (let (input vals)
		    (loop for i = (read-line instream nil :done)
			  do
			  (cond ((eq i :done)
				 (return nil))
				(t
				 (push i input))))
		    ;; Note: input file must have key/value pairs in this order. TODO: fix that.
		    (loop for i in '("questionTemplate:" "questionParaphrase:" "location:" "time:"
				     "maximumAnswerLength:" "documentId:" "questionId:" "templateId:"
				     "topic argument:")
			  for field-id in '(:question-template :question-paraphrase :location
							       :time :max-answer-length
							       :document-id :question-id :template-id :topic-argument)
			  for string in (reverse input)
			  for pos = (when field-id (search i string))
			  do (progn
			       (when (and field-id (eq pos 0))
				 (push (list field-id (subseq string (1+ (length i)) (length string)))
				       vals))))
		    vals)))

(read-question)

;; example of writing an ascii file
(defun write-question (&optional (file *question-file*))
  (with-open-file (stream file :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
		  (loop for i in '("questionTemplate:" "questionParaphrase:" "location:" "time:"
				   "maximumAnswerLength:" "documentId:" "questionId:" "templateId:"
				   "topic argument:")
			for value = (random 100)
			do 
			(format stream "~a ~a~%" i value))))

;; example of executing a command line program
;; e.g. (test-ls "/tmp/" "/tmp/foo")
(defun test-ls (dir output-file) 
  (ext:run-program "/bin/ls" (list "-l" dir)
		  :if-output-exists :supersede
		  :output output-file))

;

(use-system :metatilities-base)
(use-system :cl-containers)
(use-system :cl-graph)

(in-package :cm)

(cl-graph:get-transitive-closure (indices 10))

(defparameter myvar 5)

(alexandria:if-let (oddp myvar) (* 4 myvar) (* -1 myvar))

(alexandria:whichever 5 10 22)

(alexandria:appendf  (indices 10) (indices 3 20))

(alexandria:map-product #'+ '(0 4 5) '(3 6 7) '(8 10 11) '(2 1 9))

;; alexandria:clamp
(mapcar (lambda (x) (alexandria:clamp x 50 88))
(heapvec 100 100))

(use-system :series)

(series:scan '(a )

;; PERLE FUNCTIONS

(defparameter up1s (indices 12))
(defparameter down1s (rotate-list (reverse (indices 12)) 11))
(defparameter up7s (mod12 (transp up1s 7 #'*)))
(defparameter down7s (mod12 (transp down1s 7 #'*)))

(defparameter perlerow '(0 7 5 2 10 9 3 4 8 11 1 6))


(defun row-neighbors (arow)

(let ((arow (heapvec 12)))
  (mapcar (lambda (x) 
	    (mapcar (lambda (y) (nth y arow)) ))
	    (append (list 1) 
		    (transpose-matx
		     (list
		      (indices 10)
		      (indices 10 2)))
		    (list 10))))


  (interlock
   (rotate-list up1s 4)
   (rotate-list down7s 3)
   1 1)

;;; more selfsim work

(autosim-primitive 3 8)

; AUTOSIM-TEST -- takes melody; returns ratio(s)
; -> construct 'autosim' vectors & see if they are autosim 



(all-partitions 7)
(all-partitions 7 'notriv)

(list-partitions (heapvec 6))
(list-partitions (heapvec 6) 'notriv)


;; carter-ish

(events
 (let* ((bh (pickl besthex))
	(stax
	 (mapcar (lambda (x) (car (stack-by bh x)))
		 (indices 6 6))))
   (splay
    (transp
     (flatten
      (mapcar (lambda (x) (chooser (heapvec 30 6) x)) (shuffle stax)))
     45)
    1/6))
 "out.mid" :play 'nil)

(events
 (let* ((sseq (subsequences (randhexrow) 6))
	(stax
	 (mapcar (lambda (x) (car (stack-by x 5))) sseq)))
   (splay
    (transp
     (flatten
      (mapcar (lambda (x) (chooser (heapvec 30 6) x)) (shuffle stax)))
     45)
    1/6))
 "out.mid" :play 'nil)



;; nice serial!
(events
 (let* ((inrow (flatten (map 'list #'placereg (make-poly (randhexrow) 4) '(4 5 6))))
	(staxint 
	 (shuffle (shuffle-all
		   (loop for x in (indices 6 1) collect (extract-intervals inrow x))))))
   (splay
    (make-poly
     (flatten
      (smoothlist
       (shuffle-all
	(flatter (mapcar (lambda (x) (chooser (heapvec 20 (length x)) x)) staxint)))))
     (poissonvec .2 300 1))
    (transp (poissonvec .2 300 1) 1/6 #'*)))
 "out.mid" :play 'nil)

;; deploying rows in mode-fields
(events
 (splay
  (append
   (place-by-mod12 (flatten (tightrows (randhexrow))) 
		   (range-in-mode 48 84 stravmode))
   (place-by-mod12 (flatten (tightrows (randhexrow))) 
		   (range-in-mode 48 84 (transp-mode stravmode 1))))
  .125)
 "out.mid" :play 'nil)

(events
 (play-sd
  (make-ties
   (flatten
    (map 'list
	 (lambda (mel mods) (place-by-mod12 mel (range-in-mode 48 84 (transp-mode stravmode mods))))
	 (make-poly
	  (next (makecyc (richain (randhexrow))) 480)
	  40)
	 (m5 (indices 12)))))
  .125)
 "out.mid" :play 'nil)

(events
 (splay
  (loop repeat 10 append
	(let ((rng (range-in-mode 48 84 (make-ttmode 3))))
	  (chooser (heapvec 40 (length rng)) rng))) 
  .125 #'*)
 "out.mid" :play 'nil)

(defparameter arows 
    (rows-by-seglen (allrows (randhexrow))))

(chop-list (next (makecyc (flatten arows)) 200) 3)

(events
 (splay
  (loop for rngrot in 
	(mapcar #'flatten
		(mapcar (lambda (x) (placereg x '(4 5 6)))
			(allrots (make-poly (randhexrow) 4))))
	append
	(place-by-mod12 (heapvec 120) rngrot))
  .125 #'*)
 "out.mid" :play 'nil)



(chop-list (indices 22) 4)

(filter (lambda (x) (and (= (sum x) 12) (= 1 (apply #'gcd x))))
	(subsets-len (indices 11 1) 3))

(apply #'gcd '(4 8 10))

(subsets-len (repeater (indices 6 1) 3) 3)

;; SETS-LEN -- all subsets including repeats
(sets-len


(defparameter c5-branch
    (generic-branch #'tonnetz-func  
		    (mod12 (expand (list (m7 (indices 12)) '(0 4 7))))))


(defparameter smoothpits (shuffle-all (smoothlist
				 (transp (mapcar #'stack-up (shuffle-all (flatter c5-branch))) 60))))

;; lullaby
(events 
 (let* ((fpits (flatten smoothpits))
	(opits (ornadurs smoothpits .5)))
   (splay fpits opits))
 "out.mid" :play 'nil)


;; 

(defparameter dkrow1 '(5 0 9 7 11 10 3 6 8 1 4 2))

(events
 (let ((pits
	(gather-pits #'consn-p
		     (placereg
		      (norpt
		       (flatten
			(norpt
v			 (shuffle-all
			  (flatter 
			   (mapcar (lambda (x) (transpose-matx (list x (hex-combi-partner x))))
				   (repeater (rows-by-seglen (tightrows dkrow1)) 2))))
			 #'list-eql)))
		      (randvec 31 3 4)))))
   (splay pits (transp (take-poly pits) 1/6 #'*)))
 "out.mid" :play 'nil)

(flatten (mapcar #'reverse (make-poly dkrow1 2)))

;; Andrew Mead in PNM 26:2

(defun row-transp (arow idx)
  (mod12 (transp arow idx)))

(defun row-invp (arow idx)
  (mod12
   (mapcar (lambda (x) (- idx x)) arow)))



(defun rowpits->order
  (mapcar (lambda (x) (position x arow)) (indices 12)))


(defun roworder->pits

(defun row-transo

(defun row-invo


; pforms
; dsym

; COMPLEMENTUNION

(defun complementunion (setx sety)


(let* ((setx '(0 1 6))
       (sety '(0 4 8)))
  (mapcar #'flatten
	  (filter (lambda (x) (not (intersection (first x) (second x))))
		  (match2lists (pforms setx) (pforms sety)))))


(not (intersection '(1 2 3) '(4 2 6)))


(events
 (splay
  (heapvec 100 24 60)
  (transp (transp (poissonvec .2 100) 1) 1/6 #'*))
 "out.mid" :play 'nil)


(events
 (let ((pits
	(flatten (transp (self-expand (heapvec 5 12) 3) 60))))
   (splay pits
	  (leapdur pits 1/6)))
 "out.mid" :play 'nil)


(transp
(flatter
(generic-branch #'tonnetz-func (shuffle (alltransp '(0 2 5)0 ))))
60)

(events
 (splay
  (loop for n from 1 to 5 by .3 collect
	(round-all (expwarp '(60 64 67) n)))
  .5)
 "out.mid" :play 'nil)

(events
 (let ((hexrow '(1 3 6 8 10))
       (regcyc (makecyc '(6 3))))
   (splay
    (loop for n in (m5 (indices 12)) append
	  (consmatch  
	   (menses (placereg 
		    (transp (chooser (heapvec 7 5) hexrow) n) 
		    (next regcyc)) 7)
	   (play-mode (heapvec 30 12 30) (transp-mode ionian n))))
    0.125))
 "out.mid" :play 'nil)

(events
 (let ((pits
	(make-poly
	 (placereg
	  (loop for n in 
		(m7 (indices 12))
		append
		(play-mode (heapvec 50 12 30) (transp-mode ionian n)))
	  middleweight-5)
	 (resclassvec 1 4 7))))
   (splay pits (transp (take-poly pits) .2 #'*)))
 "out.mid" :play 'nil)

(load "tiles.lisp")

(defparameter m5chds
    (mod12
     (loop for n in (m5 (indices 12)) collect
	   (transp '(0 3 5 7) n))))

(defparameter rgrthing 
    (generic-branch #'rgr-alldim1 m5chds))

(events
 (mapcar 
  (lambda (x) (play-sd (make-ties x) 0.25))
  (chds->lines 
   (mapcar (lambda (x) (placereg x (indices 4 3)))
	   (closest-mod-list (mod12 (flatter rgrthing))))))
 "out.mid" :play 'nil)

(defparameter blah (heapvec 12))

(heapvec 12)

(defparameter thisrgr
(generic-branch #'rgr-alldim1 (make-poly (heapvec 12) 3)))


(play-mode

(events (selfsim cyclops2p4x7 (shuffle (transp '(60 62 65) 12)) '(4 9 11 15) '(0 -7 -14 -21) .25)
        "out.mid" :play 'nil)

(events (selfsim cyclops3x8 (shuffle '(60 62 63 65 67)) '(3 9 11) '(0 -7 -14) .25)
        "out.mid" :play 'nil)

(events (selfsim cyclops4p7x9 (shuffle '(60 62 63 65 67)) '(4 10 13) '(0 -7 -14) .25)
        "out.mid" :play 'nil)

(events (selfsim cyclops6x7 (shuffle (transp '(60 62 64 65) 12)) '(6 8 13 15) '(0 -7 -14 -21) .25)
        "out.mid" :play 'nil)

(events
 (let ((fto
	(fromto-stepper (copything 4 7) (heapvec 7 7 1))))
   (splay (heapvec 300 36 50)
	  (transp (flatten (append fto (reverse fto))) .125 #'*)))
 "out.mid" :play 'nil)

(events
 (let ((pits
	(make-poly
	 (placereg
	  (norpt
	   (flatten
	    (shuffle (allrows (randhexrow)))))
	  (heapvec 77 4 3))
	 (transp (rwgen pleasantsrules '(1) 3) 1))))
   (splay pits (ferney '(1)
		       (transp (rwgen mtrules '(1) 5) 3)
		       (take-poly pits))))
 "out.mid" :play 'nil)

(events
 (let ((pits
	(play-mode
	 (transp
	  (flatten (self-expand (randvec 7 5) 5))
	  30) 
	 stravmode)))
   (splay pits (transp (leapdur pits) .125 #'*)))
 "out.mid" :play 'nil)

(events
 (let ((m6 (make-poly (randhexrow) 6)))
   (splay
    (make-poly 
     (placereg
      (flatten (fromto-stepper (first m6) (second m6)))
      middleweight-5)
     (resclassvec 3 5 7))
    (transp (chooser (heapvec 204 20) (ranvec-gaussian 20)) 0.25 #'*)))
 "out.mid" :play 'nil)

(shade-into 30 10 20)

;;  CARTER etc

(defparameter drow (randhexrow))

(defparameter chex3 '(5 6 8 9 10 1))


chex3

;; 99 all-int rows: interval list!
;(defparameter carter1-ints '(2 7 4 3 1 6 11 9 8 5 10))

(events
 (splay
  (loop for n in (shuffle (indices 5 1)) append
	(shuffle (extract-intervals (transp (stack-up row-34) 24) n)))
  (ferney '(7) (randvec 50 3 3)))
 "out.mid" :play 'nil)

(events
 (splay
  (chooser (mod12 (drunkvec 5 100))
	   (transp (stack-up row-34) 24))
  0.125)
 "out.mid" :play 'nil)




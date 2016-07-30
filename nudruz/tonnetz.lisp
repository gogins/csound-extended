;; TONNETZ.LISP
;; building parsimonious tonnetz for use with Common Music
;; see Cohn (JMT 41:1) etc.

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

; (load "nudruz.lisp")

;; SOME BASIC UTILITIES
;; May 2005: "inverse-idx" moved to nudruz.lisp 
;; April 2007: "SIS" moved to nudruz.lisp

;; TZWORTHY -- tests whether tz can apply to a chord at all
(defun tzworthy (chd)
  (eql 3 
       (length (remove-duplicates (sis (prime-form chd))))))

;; the six basic transformations

;; PM -- first transformation type
;; [note: pitches are expected & returned in the 'correct order']
;; (pm '(0 3 7)) = (7 4 0)
(defun pm (pform &optional (modlen 12))
  (let ((psis (sis pform modlen)))
    (if (= (inverse-idx pform) 1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (third pform)
               (+ (second pform) 
                  (- (second psis) (first psis)))
               (first pform))) 
      nil)))

;; IPM -- inverse of first type
;; [note: pitches are expected & returned in the 'correct order']
;; (ipm '(7 4 0)) = (0 3 7)
(defun ipm (iform &optional (modlen 12))
  (let ((isis (sis iform modlen)))
    (if (= (inverse-idx iform) -1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (third iform)
               (- (second iform)
                  (- (second isis) (first isis)))
               (first iform))) 
      nil)))

;; LM -- second transformation type
;; [note: pitches are expected & returned in the 'correct order']
;; (lm '(0 3 7)) = (3 0 8)
(defun lm (pform &optional (modlen 12))
  (let ((psis (sis pform modlen)))
    (if (= (inverse-idx pform) 1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (second pform)
               (first pform)
               (+ (third pform)
                  (- (third psis) (second psis))))) 
      nil)))

;; ILM -- inverse of second type
;; [note: pitches are expected & returned in the 'correct order']
;; (ilm '(7 4 0)) = (4 7 11)
(defun ilm (iform &optional (modlen 12))
  (let ((isis (sis iform modlen)))
    (if (= (inverse-idx iform) -1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (second iform)
               (first iform)
               (- (third iform)
                  (- (third isis) (second isis))))) 
      nil)))

;; RM -- third transformation type
;; [note: pitches are expected & returned in the 'correct order']
;; (rm '(0 3 7)) = (10 7 3)
(defun rm (pform &optional (modlen 12))
  (let ((psis (sis pform modlen)))
    (if (= (inverse-idx pform) 1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (- (first pform)
                  (- (third psis) (first psis)))
               (third pform)
               (second pform)))
      nil)))

;; IRM -- inversion of third type
;; [note: pitches are expected & returned in the 'correct order']
;; (irm '(7 4 0)) = (9 0 4)
(defun irm (iform &optional (modlen 12))
  (let ((isis (sis iform modlen)))
    (if (= (inverse-idx iform) -1)
      (mapcar (lambda (x) (mod x modlen))
              (list
               (+ (first iform)
                  (- (third isis) (first isis)))
               (third iform)
               (second iform)))
      nil)))

;; working with transformations

;; CORRECT-ORDER ... must have correct order of pitches
;; for both prime & inverse when plugging into tzrandchain
;; (correct-order '(0 4 7)) = (7 4 0)
(defun correct-order (a-list &optional (modlen 12))
  (let ((psis (sis a-list))
        (aperms (permutations a-list)))
    (if (= 1 (inverse-idx a-list))
      (find-if 
       (lambda (x) 
         (and (= (first psis) (mod (- (second x) (first x)) modlen))
              (= (second psis) (mod (- (third x) (second x)) modlen))))
       aperms)
      (reverse 
       (find-if 
        (lambda (x) 
          (and (= (second psis) (mod (- (second x) (first x)) modlen))
               (= (first psis) (mod (- (third x) (second x)) modlen))))
        aperms)))))


;; CHOOSE-CHAIN-START -- utility to select starting transform in a chain
;; (choose-chain-start '(7 4 0)) = 4
(defun choose-chain-start (a-chd)
   (if (= (inverse-idx a-chd) 1)
      (random 3)
     (+ 3 (random 3))))

;; TZRANDLIST -- makes list of transforms from 
;; (bipartite) transformation graph;
;;  chooses correct starting transformation
;; (tzrandlist '(0 3 7) 5) = (LM IRM PM ILM PM)
(defun tzrandlist (a-chd len)
  (let* ((tzgraph 
          (new graph :of
               `((pm :id 1 :to ,(new weighting of '(5 6)))
                 (lm :id 2 :to ,(new weighting of '(4 6)))
                 (rm :id 3 :to ,(new weighting of '(4 5)))
                 (ipm :id 4 :to ,(new weighting of '(2 3)))
                 (ilm :id 5 :to ,(new weighting of '(1 3)))
                 (irm :id 6 :to ,(new weighting of '(1 2))))
               :starting-node-index (choose-chain-start a-chd))))
    (next tzgraph len)))

;; GEN-TZ apply transform to/from any vector
;; (gen-tz 'lm '(10 19 26)) = (10 19 27)
(defun gen-tz (transform a-chd &optional (modlen 12))
  (let ((mm (modmult a-chd 1 modlen)))
    (matchreg 
     (closest-mod mm (funcall transform (correct-order mm)) modlen)
     a-chd)))

;;; TZCHAIN -- make a list of trichords from starting chord 
;;; and a tzlist (generalized)
;; (tzchain '(0 3 7) '(pm ilm)) = ((0 3 7) (0 4 7) (11 4 7))
(defun tzchain (a-chd tzlist &optional (modlen 12))
  (cons a-chd
	(when tzlist
	  (tzchain 
           (gen-tz (first tzlist) a-chd modlen) (cdr tzlist)))))

;; TZRANDCHAIN -- main function
;; applies tzrandlist to a chord
;; takes chord and length (total # trichords) -- returns all trichords
;; (tzrandchain '(0 3 7) 4) =  ((0 3 7) (0 4 7) (0 4 9) (0 5 9))
(defun tzrandchain (a-chd len)
  (tzchain a-chd (tzrandlist a-chd (- len 1))))

;;; 
;;; demonstration follows ...
;;;

(defun tzplay ()
  (cm::process for thischord in (tzrandchain '(3 26 48) 20)
           for dur = (pick 3 4 5)
           output (multievent 'midi :keynum
                              :keynum (transp thischord 40)
                              :time (now)
                              :duration dur)
           wait dur))

; store them in a midi file
(cm::events (tzplay) "tzplay.midi")


;; below added June 2006
;; tonnetz metric work -- distances & paths btwn trichords!!

;; Q-STAR -- see Cohn in JMT 41:1 (p. 19)
(defun q-star (num &optional (modlen 12))
  (/ modlen (gcd num modlen)))

;; TZDIMS -- takes chord; returns dimensions of its tonnetz
(defun tzdims (chd &optional (modlen 12))
  (let* ((tsis (sis chd))
         (xdim (/ modlen (first tsis)))
         (ydim (/ modlen (second tsis))))
  (list xdim ydim)))

;; TZMIN-SUM -- utility for tzpath-vectors
(defun tzmin-sum (setof3)
  (+ (abs (first setof3))
     (abs (second setof3))
     (abs (third setof3))))

;; TZPATH-VECTORS -- returns sorted set of (Nx My P(x+y)) factors
;; for any given transposition 'tlen'
(defun tzpath-vectors (x y tlen &optional (modlen 12))
  (sort (screamer-user::tzpathfactors x y tlen modlen)
        (lambda (x y)
          (< (tzmin-sum x) (tzmin-sum y)))))

;; BEST-TZPATH-VECTOR -- finds best set of (Nx My P(x+y)) factors
;; for any given transposition 'tlen'
;; [picked randomly from equally best factors]
(defun best-tzpath-vector (x y tlen &optional (modlen 12))
  (let* ((tzpaths (screamer-user::tzpathfactors x y tlen modlen))
         (minsum (loop for tzp in tzpaths minimize (tzmin-sum tzp))))
    (pickl (no-nils
            (loop for x in tzpaths collect
                  (if (= (tzmin-sum x) minsum) x))))))

;; TZPATH-VECTOR-LEN -- finds set of (Nx My P(x+y)) factors of specified len
;; for any given transposition 'tlen'
;; [picked randomly from equally best factors]
(defun tzpath-vector-len (x y tlen vlen &optional (modlen 12))
  (let* ((tzpaths (screamer-user::tzpathfactors x y tlen modlen)))
    (pickl (no-nils
            (loop for x in tzpaths collect
                  (if (= (tzmin-sum x) vlen) x))))))

;; CT-CANDIDATES
;; returns all tz-primes bordering a given inverse
;; OR returns all inverses bordering a given prime
(defun ct-candidates (invec &optional (modlen 12))
  (let* ((pf (prime-form invec))
         (invidx (inverse-idx invec))
         (allvec                         ;; all possible matches
          (remove-duplicates
           (loop for x to (- modlen 1) collect
                 (mod12 (transp 
                         (if (eql 1 invidx) (pm (correct-order invec)) pf)
                         x)))
           :test #'list-eql)))
    (mapcar #'correct-order
            (no-nils
             (loop for p in allvec collect
                   (if (eql 2 (length (intersection p invec)))
                       p))))))

;; CLOSEST-TO-INV -- returns best factor-list toward 
;; closest tz-prime to inverse in p-to-i moves
;; and closest inverse to tz-prime in i-to-p moves
(defun closest-to-inv (startvec endvec &optional (modlen 12))
  (let* ((startsis (sis startvec))
         (x (first startsis))
         (y (second startsis))
         (ctcands (ct-candidates endvec modlen))
         (tlevels (mod12 
                   (mapcar (lambda (x) (- (car x) 
                                          (car (correct-order startvec))))
                           ctcands)))
         (bests (mapcar (lambda (n) (best-tzpath-vector x y n)) tlevels))
         (pathlens (mapcar #'tzmin-sum bests))
         (quickest (apply #'min pathlens)))
    (pickl
     (no-nils
      (loop for b in bests collect 
            (if (eql (tzmin-sum b) quickest) b))))))

;; FIND-TZ-FLIP -- given two adjacent tztriads, 
;; find transformation type from first to second
(defun find-tz-flip (startvec endvec)
  (let* ((sinvidx (inverse-idx startvec))
         (svec (correct-order startvec))
         (evec (correct-order endvec)))
        (case (position (car (set-difference svec evec)) svec)
          (1 (if (eql sinvidx 1) 'pm 'ipm))
          (0 (if (eql sinvidx 1) 'rm 'irm))
          (2 (if (eql sinvidx 1) 'lm 'ilm)))))

;; TZFACTORS->MOVES -- takes factor 3-list, returns moves (p-to-p or i-to-i)
(defun tzfactors->moves (startvec factlist)
  (let* ((sinvidx (inverse-idx startvec))
         (n (first factlist))
         (m (second factlist))
         (p (third factlist))
         (n-moves 
          (loop repeat (abs n) collect
                (case sinvidx
                  (1 (if (plusp n) (list 'rm 'ipm) (list 'pm 'irm)))
                  (-1 (if (plusp n) (list 'ipm 'rm) (list 'irm 'pm))))))
         (m-moves 
          (loop repeat (abs m) collect
                (case sinvidx
                  (1 (if (plusp m) (list 'pm 'ilm) (list 'lm 'ipm)))
                  (-1 (if (plusp m) (list 'ilm 'pm) (list 'ipm 'lm))))))
         (p-moves 
          (loop repeat (abs p) collect
                (case sinvidx
                  (1 (if (plusp p) (list 'rm 'ilm) (list 'lm 'irm)))
                  (-1 (if (plusp p) (list 'ilm 'rm) (list 'irm 'lm)))))))
    (flatten
     (shuffle
      (no-nils
       (append n-moves m-moves p-moves))))))

;; BEST-TZ-PATH -- shortest intermediate tz-chords btwn 2 chords
;; includes startvec, but not endvec
(defun best-tz-path (startvec endvec)
  (let* ((ssis (sis startvec))
         (s12 (correct-order (mod12 startvec)))
         (e12 (correct-order (mod12 endvec))))
    (if (or (member e12 (ct-candidates s12) :test #'list-eql)
            (list-eql startvec endvec))
        (list startvec)
        (butlast
         (tendreg
          (append
           (if (eql (inverse-idx startvec) (inverse-idx endvec))
               (butlast (tzchain startvec 
                                 (tzfactors->moves 
                                  startvec 
                                  (best-tzpath-vector 
                                   (first ssis)
                                   (second ssis)
                                   (mod12 (- (car e12) (car s12)))))))
               (tzchain startvec
                        (tzfactors->moves
                         startvec (closest-to-inv startvec endvec))))
           (list endvec)))))))
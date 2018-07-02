(in-package :cm)

;; TILING.LISP -- creating tiling canons
;; see Tangian (PNM 41:2) etc.

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

(load "tiles.lisp")

;; PLACE-TILES -- to fill in a tiling
;; can use 'r in pattern-list
;; [based in part on 'scf']

(defun place-tiles (pattern-list tiling-vector)
  (let ((oldies nil))
    ;; first extract the unique ordered elements from orig. list
    (loop for i in tiling-vector
	  do (pushnew i oldies))
    (setq oldies (reverse oldies))
    ;; if wrong number of new elements, signal an error
    (when (not (= (length oldies) (length pattern-list)))
      (error "Length mismatch ~a with ~a" pattern-list oldies))
    (loop for old in oldies
	  for new in pattern-list
	  do (setq tiling-vector (substitute new old tiling-vector)))
    (loop for x in tiling-vector collect (next x))))

;; PLACE-CANTILES -- to fill in a tiling canon
;; ...can use 'r for transp-level
;; based on place-tiles
(defun place-cantiles (melody transp-levels tiling-vector)
  (let ((tranpatts (loop for x in transp-levels collect 
                         (if (numberp x) 
                           (new cycle of
                                (transpose melody x)) 'r ))))
    (when (not (= (length tiling-vector) 
		  (* (length melody) (length transp-levels))))
      (error "Length mismatch ~a with ~a" pattern-list oldies))
    (place-tiles tranpatts tiling-vector)))

;; PLACEFRAG-CANTILES - placing melodies (frags) into tiling vector
(defun placefrag-cantiles (tiling-vector frags)
  (let* ((tvector-adj (transp tiling-vector -1))
	 (fragcycs (loop for frag in frags collect (makecyc frag))))
    (loop for tva in tvector-adj collect (next (nth tva fragcycs)))))

;; SELF-TILE -- use tilist for pitches & slots, placed randomly
(defun self-tile (tilist)
  (let* ((pits 
	  (shuffle
	   (shuffle-all tilist)))
	 (tlen (length (flatten tilist))))
    (flatten
     (merge-slots
      (map 'list (lambda (x y) (place-slots x y tlen))
	   (flatten tilist)
	   (flatten pits))))))


;; TILES014 -- quick mod12 tiling vector
;; 014-pitsets placed randomly in 014-rhythms 
(defun tiles014 ()
  (let* ((tilz014
	  (loop for n in '(0 6) append (list (transp '(0 3 4) n) (transp '(0 1 4) (+ n 1)))))
	 (pits 
	  (shuffle
	   (shuffle-all tilz014)))
	 (tlen 12))
    (flatten
     (merge-slots
      (map 'list (lambda (x y) (place-slots x y tlen))
	   (flatten tilz014)
	   (flatten pits))))))

;; TILES015 -- quick mod12 tiling vector
;; 015-pitsets placed randomly in 015-rhythms 
(defun tiles015 ()
  (let* ((tilz015
	  '((0 1 5) (2 3 7) (4 8 9) (6 10 11)))
	 (pits 
	  (shuffle
	   (shuffle-all tilz015)))
	 (tlen 12))
    (flatten
     (merge-slots
      (map 'list (lambda (x y) (place-slots x y tlen))
	   (flatten tilz015)
	   (flatten pits))))))

;; TILES016 -- quick mod12 tiling vector
;; 016-pitsets placed randomly in 016-rhythms 
(defun tiles016 ()
  (let* ((tilz016
	  '((0 1 6) (2 7 8) (3 4 9) (5 10 11)))
	 (pits 
	  (shuffle
	   (shuffle-all tilz016)))
	 (tlen 12))
    (flatten
     (merge-slots
      (map 'list (lambda (x y) (place-slots x y tlen))
	   (flatten tilz016)
	   (flatten pits))))))

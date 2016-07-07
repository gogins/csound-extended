;; CYCLOPS.LISP
;; melodic possibilities for self-similar canons
;; --> use these with the 'scf' function in nudruz.lisp
;; to substite pitches for character placeholders

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

;; Note: these are easy to find using the gap package (www.gap-system.org)
;; gap> RequirePackage("guava");
;; gap> CyclotomicCosets(2,3); Length(last);
;; >  [ [ 0 ], [ 1, 2 ] ]
;; > 2
;; this becomes a 'cyclops' (a b b)


;; GAP2CYCLOPS converting exported 'gap' to cyclops
(defun gap2cyclops (idxs)
  (let* ((alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
	 (nbrnotes (length idxs))
	 (melsize (length (flatten idxs))))
    (chooser
     (flatten
      (merge-slots
       (loop for n to (- nbrnotes 1) collect
	     (merge-slots
	      (mapcar (lambda (x) (place-slots x n  melsize))
		      (nth n idxs))))))
     (subseq alphabet 0 nbrnotes))))

;; note: cyclops always works for n mod len
;; e.g., 3x4 is equiv. to 7x4, 11x4, etc.
;; ... also works for roots of n

;; note: canon total length = [melody-length]*[lcm of all durations]

(define cyclops2x3 '(a b b))
; 2 members

(define cyclops3p7x4 '(a b c b))
; 3 members

(define cyclops2p3x5 '(a b b b b))
; 2 members

(define cyclops4x5 '(a b c c b))
; 3 members

(define cyclops5x6 '(a b c d c b))
; 4 members

(define cyclops2p4x7 '(a b b c b c c))
; 3 members

(define cyclops3p5x7 '(a b b b b b b))
; 2 members

(define cyclops6x7 '(a b c d d c b))
; 4 members

(define cyclops3x8 '(a b c b d e c e))
; 5 members

(define cyclops5x8 '(a b c d e b f d))
; 6 members

(define cyclops7x8 '(a b c d e d c b))
; 5 members

(define cyclops2p5x9 '(a b b c b b c b b))
; 3 members

(define cyclops4p7x9 '(a b c d b c e b c))
; 5 members

(define cyclops8x9 '(A B C D E E D C B))
; 5 members

(define cyclops3p7x10 '(a b c b c d c b c b))
; 4 members

(define cyclops9x10 '(A B C D E F E D C B))
; 6 members

(define cyclops2678x11 '(A B B B B B B B B B B))
; 2 members

(define cyclops3459x11 '(a b c b b b c c c b c))
; 3 members

(define cyclops10x11 '(A B C D E F F E D C B))
; 10 members

(define cyclops5x12 '(a b c d e b f g e h c g))
; 8 members

(define cyclops7x12 '(a b c d e f g b h d i f))
; 9 members

(define cyclops11x12 '(A B C D E F G F E D C B))
; 7 members

(define cyclops26711x13 '(A B B B B B B B B B B B B))
; 2 members

(define cyclops3x13 '(a b c b d c c e e b d e d))
; 5 members

(define cyclops4p10x13 '(a b c b b c c c c b b c b))
; 3 members

(define cyclops5p8x13 '(a b c c d b d d b d c c b))
; 4 members

(define cyclops9x13 '(a b c b d c c e e b d e d))
; 5 members

(define cyclops12x13 '(A B C D E F G G F E D C B))
; 7 members

(define cyclops3p5x14 '(a b c b c b c d c b c b c b))
; 4 members

(define cyclops9p11x14 '(A B C D C D E F C B E B E D))
; 6 members

(define cyclops13x14 '(A B C D E F G H G F E D C B))
; 8 members

(define cyclops2p8x15 '(a b b c b d c e b c d e c e e))
; 5 members

(define cyclops4x15 '(A B C D B E F G C F H I D G I))
; 9 members

(define cyclops7p13x15 '(A B C D B E D B C D F C D B C))
; 6 members

(define cyclops11x15 '(A B C D E F G C H I F B J H E))
; 10 members

(define cyclops14x15 '(A B C D E F G H H G F E D C B))
; 8 members

(define cyclops3x16 '(a b c b d e c e f b f b d e f e))
; 6 members

(define cyclops5p13x16 '(a b c d e b f d g b c d h b f d))
; 8 members

(define cyclops7x16 '(A B C D E D F B G H F I E I C H))
; 9 members

(define cyclops9x16 '(A B C D E F G H I B J D K F L H))
; 12 members

(define cyclops11x16 '(A B C B D E C E F B G B D E G E))
; 7 members

(define cyclops15x16 '(A B C D E F G H I H G F E D C B))
; 9 members

(define cyclops289p15x17 '(A B B C B C C C B B C C C B C B B))
; 3 members

(define cyclops4p13x17 '(A B C D B D E E C C E E D B D C B))
; 5 members

(define cyclops16x17 '(A B C D E F G H I I H G F E D C B))
; 9 members

(define cyclops5x18 '(a b c d c b e b c f c b e b c d c b))
; 6 members

(define cyclops7p13x18
    (gap2cyclops
     '( ( 0 ) ( 1 7 13 ) ( 2 14 8 ) ( 3 ) ( 4 10 16 ) ( 5 17 11 )
       ( 6 ) ( 9 ) ( 12 ) ( 15 ) )))

(define cyclops11x18
    (gap2cyclops
     '( ( 0 ) ( 1 11 13 17 7 5 ) ( 2 4 8 16 14 10 ) ( 3 15 )
       ( 6 12 ) ( 9 ) )))

(define cyclops13x18
    (gap2cyclops
     '( ( 0 ) ( 1 13 7 ) ( 2 8 14 ) ( 3 ) ( 4 16 10 ) ( 5 11 17 )
       ( 6 ) ( 9 ) ( 12 ) ( 15 ) )))

(define cyclops17x18
    (gap2cyclops
     '( ( 0 ) ( 1 17 ) ( 2 16 ) ( 3 15 ) ( 4 14 ) ( 5 13 ) ( 6 12 )
       ( 7 11 ) ( 8 10 ) ( 9 ) )))

(define cyclops2p3x19 ; 2,3,10,13,14,15
    (gap2cyclops
     '( ( 0 ) ( 1 2 4 8 16 13 7 14 9 18 17 15 11 3 6 12 5 10 ) )))

(define cyclops4x19 ; 4,5,6,9,16,17
    (gap2cyclops
     '( ( 0 ) ( 1 4 16 7 9 17 11 6 5 )
       ( 2 8 13 14 18 15 3 12 10 ) )))

(define cyclops7p11x19
    (gap2cyclops
     '( ( 0 ) ( 1 7 11 ) ( 2 14 3 ) ( 4 9 6 ) ( 5 16 17 )
       ( 8 18 12 ) ( 10 13 15 ) )))

(define cyclops8p12x19
    (gap2cyclops
     '( ( 0 ) ( 1 8 7 18 11 12 ) ( 2 16 14 17 3 5 )
       ( 4 13 9 15 6 10 ) )))

(define cyclops18x19
    (gap2cyclops
     '( ( 0 ) ( 1 18 ) ( 2 17 ) ( 3 16 ) ( 4 15 ) ( 5 14 ) ( 6 13 )
       ( 7 12 ) ( 8 11 ) ( 9 10 ) )))

(define cyclops3p7x20
    (gap2cyclops
     '( ( 0 ) ( 1 3 9 7 ) ( 2 6 18 14 ) ( 4 12 16 8 ) ( 5 15 )
       ( 10 ) ( 11 13 19 17 ) )))

(define cyclops11x20
    (gap2cyclops
     '( ( 0 ) ( 1 11 ) ( 2 ) ( 3 13 ) ( 4 ) ( 5 15 ) ( 6 ) ( 7 17 )
       ( 8 ) ( 9 19 ) ( 10 ) ( 12 ) ( 14 ) ( 16 ) ( 18 ) )))

(define cyclops13p17x20
    (gap2cyclops
     '( ( 0 ) ( 1 13 9 17 ) ( 2 6 18 14 ) ( 3 19 7 11 )
       ( 4 12 16 8 ) ( 5 ) ( 10 ) ( 15 ) )))

(define cyclops19x20
    (gap2cyclops
     '( ( 0 ) ( 1 19 ) ( 2 18 ) ( 3 17 ) ( 4 16 ) ( 5 15 ) ( 6 14 )
       ( 7 13 ) ( 8 12 ) ( 9 11 ) ( 10 ) )))

(define cyclops2x21 '(a b b c b d c e b f d b c d e f b d f d d))
; 6 members

(define cyclops4p16x21
    (gap2cyclops 
     '((0) (1 4 16) ( 2 8 11 ) ( 3 12 6 ) ( 5 20 17 ) ( 7 )
       ( 9 15 18 ) ( 10 19 13 ) ( 14 ) )))

(define cyclops5p17x21
    (gap2cyclops
     '( ( 0 ) ( 1 5 4 20 16 17 ) ( 2 10 8 19 11 13 )
       ( 3 15 12 18 6 9 ) ( 7 14 ) )))

(define cyclops8x21
    (gap2cyclops
     '( ( 0 ) ( 1 8 ) ( 2 16 ) ( 3 ) ( 4 11 ) ( 5 19 ) ( 6 ) ( 7 14 )
       ( 9 ) ( 10 17 ) ( 12 ) ( 13 20 ) ( 15 ) ( 18 ) )))

(define cyclops10p19x21
    (gap2cyclops
     '( ( 0 ) ( 1 10 16 13 4 19 ) ( 2 20 11 5 8 17 )
       ( 3 9 6 18 12 15 ) ( 7 ) ( 14 ) )))

(define cyclops11x21
    (gap2cyclops
     '( ( 0 ) ( 1 11 16 8 4 2 ) ( 3 12 6 ) ( 5 13 17 19 20 10 )
       ( 7 14 ) ( 9 15 18 ) )))

(define cyclops13x21
    (gap2cyclops
     '( ( 0 ) ( 1 13 ) ( 2 5 ) ( 3 18 ) ( 4 10 ) ( 6 15 ) ( 7 )
       ( 8 20 ) ( 9 12 ) ( 11 17 ) ( 14 ) ( 16 19 ) )))

(define cyclops3p5x22 '(a b c b d b c e c b c f d e d b d e c e d e))
; 6 members

(define cyclops3x26 '(a b c d e c f g b d f d h i e i j c e g f i j g j))
; 10 members

(define cyclops5x26 '(A B C D E B E F G F C D G H G D C F G F E B E D C B))
; 8 members

(define cyclops7p11p15x26 '(A B C B C B C B C B C B C D C B C B C B C B C B C B))
; 4 members

(define cyclops9x26 '(A B C B D E C F G B D F D H I E I J C E G F I J G J))
; 10 members

(define cyclops13x27 '(A B C D B C E B C F B C D B C E B C G B C D B C E B C))
; 7 members

(define all-cyclops
    (list
     cyclops2x3 
     cyclops3p7x4
     cyclops2p3x5 
     cyclops4x5 
     cyclops5x6 
     cyclops2p4x7
     cyclops3p5x7
     cyclops6x7 
     cyclops3x8 
     cyclops5x8 
     cyclops7x8 
     cyclops2p5x9 
     cyclops4p7x9 
     cyclops8x9 
     cyclops3p7x10 
     cyclops9x10 
     cyclops2678x11 
     cyclops3459x11 
     cyclops10x11 
     cyclops5x12 
     cyclops7x12 
     cyclops11x12 
     cyclops26711x13 
     cyclops3x13 
     cyclops4p10x13 
     cyclops5p8x13 
     cyclops9x13 
     cyclops12x13 
     cyclops3p5x14 
     cyclops9p11x14 
     cyclops13x14 
     cyclops2p8x15 
     cyclops4x15 
     cyclops7p13x15 
     cyclops11x15 
     cyclops14x15 
     cyclops3x16 
     cyclops5p13x16 
     cyclops7x16 
     cyclops9x16 
     cyclops11x16 
     cyclops15x16 
     cyclops289p15x17 
     cyclops4p13x17 
     cyclops16x17 
     cyclops5x18 
     cyclops7p13x18
     cyclops11x18
     cyclops13x18
     cyclops17x18
     cyclops2p3x19 
     cyclops4x19 
     cyclops7p11x19
     cyclops8p12x19
     cyclops18x19
     cyclops3p7x20
     cyclops11x20
     cyclops13p17x20
     cyclops19x20
     cyclops2x21 
     cyclops4p16x21
     cyclops5p17x21
     cyclops8x21
     cyclops10p19x21
     cyclops11x21
     cyclops13x21
     cyclops3p5x22 
     cyclops3x26 
     cyclops5x26 
     cyclops7p11p15x26 
     cyclops9x26 
     cyclops13x27))

;; PROPER-CYCLOPS -- all non-trivial cyclops expressed as integer indices
;; *cdr only*!
(define proper-cyclops
    (mapcar (lambda (x) (scf (indices 26) (cdr x)))
	    (filter (lambda (x) (> (length (remove-duplicates x)) 2)) all-cyclops))) 

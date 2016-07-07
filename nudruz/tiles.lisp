;; TILES.LISP
;; some tilings found using 'tilings.gap' script
;; these are input to 'place-tiles' and 'place-cantiles'

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

;gap> tilings([1,1,0,0,1],1,5,400);
;[ 1, 1, 2, 1, 1 ]  Length 15
(define tilevec15 '(1 1 2 2 1 3 2 3 4 4 5 5 4 3 5))

; gap> tilings([1,0,1,1,0,0,1],1,6,1400);
; [ 5, 1, 2, 2, 2, 4, 4, 1, 2, 2, 1 ]  Length 44
(define tilevec44 '(1 2 3 2 2 4 3 2 3 4 1 4 5 6 3 1 5 4 5 7 8 
6 8 8 5 6 8 7 9 10 1 7 9 10 9 10 11 6 11 11 9 10 11 7))

; gap> tilings([1,0,0,1],1,7,400);

; [ 1, 1, 2, 2, 1, 1 ]  Length 12
(define tilevec4a '(1 2 3 1 2 4 5 6 3 5 6 4))

; [ 1, 1, 3, 1, 1, 1 ]  Length 12
(define tilevec4b '(1 2 3 1 2 4 5 6 4 5 6 3))

; [ 2, 1, 1, 2, 1, 1 ]  Length 12
(define tilevec4c '(1 2 3 4 2 3 1 5 6 4 5 6))

; [ 2, 1, 2, 2, 2, 1 ]  Length 12
(define tilevec4d '(1 2 3 4 2 5 1 6 3 4 6 5))

; [ 3, 1, 1, 1, 1, 1 ]  Length 12
(define tilevec4e '(1 2 3 4 2 3 4 5 6 1 5 6))

; [ 3, 1, 2, 1, 2, 1 ]  Length 12
(define tilevec4f '(1 2 3 4 2 5 4 6 3 1 6 5))


; gap> tilings((1101)15400);
;( 5 2 4 4 1 2 )  Length 18
(define tilevec5a '( 1 2 3 2 4 1 3 2 4 5 5 6 5 6 3 1 4 6 ))

;( 5 3 5 3 1 1 )  Length 18
(define tilevec5b '( 1 2 3 4 2 1 4 3 5 5 2 5 4 6 6 1 6 3 ))

;( 1 2 4 4 4 1 2 )  Length 21
(define tilevec5c '( 1 1 2 1 2 3 4 5 2 3 4 5 6 6 7 6 7 3 4 5 7 ))

;( 2 3 5 4 4 1 2 )  Length 21
(define tilevec5d '( 1 2 1 3 2 4 1 5 3 4 2 5 6 6 7 6 7 4 3 5 7 ))

;( 1 4 1 1 2 2 1 )  Length 21
(define tilevec5e '( 1 1 2 1 3 3 2 3 4 4 5 4 5 6 2 6 5 7 7 6 7 ))

;( 1 4 4 2 1 2 1 )  Length 21
(define tilevec5f '( 1 1 2 1 3 4 2 4 3 5 5 4 5 6 2 6 3 7 7 6 7 ))

;( 3 1 1 3 1 2 1 )  Length 21
(define tilevec5g '( 1 2 2 1 2 3 3 4 3 1 4 5 5 6 5 6 4 7 7 6 7 ))

;( 2 2 5 5 5 1 2 2 )  Length 24
(define tilevec5h '( 1 2 1 2 3 4 1 2 5 3 4 6 6 5 6 7 8 7 8 3 4 7 8 5 ))

;( 3 1 2 2 3 1 2 1 )  Length 24
(define tilevec5i '( 1 2 2 1 2 3 4 3 4 1 5 3 4 5 6 6 7 6 7 5 8 8 7 8 ))

;( 3 3 5 1 1 2 2 1 )  Length 24
(define tilevec5j '( 1 2 3 1 2 4 4 3 4 1 2 5 5 6 5 6 7 3 7 6 8 8 7 8 ))

;( 5 1 3 1 4 3 2 2 )  Length 24
(define tilevec5k '( 1 2 2 3 2 1 3 4 4 5 4 6 3 5 6 1 7 8 7 8 6 5 7 8 ))

;( 5 1 5 5 2 2 3 1 )  Length 24
(define tilevec5m '( 1 2 2 3 2 1 4 5 3 5 6 4 6 5 7 1 6 7 3 8 8 4 8 7 ))

;( 2 3 2 5 5 5 1 2 2 )  Length 27
(define tilevec5n 
      '( 1 2 1 3 2 3 1 4 5 3 2 6 4 5 7 7 6 7 8 9 8 9 4 5 8 9 6 ))

;( 4 2 3 3 2 3 1 2 1 )  Length 27
(define tilevec5p 
      '( 1 2 3 2 1 3 4 2 5 4 5 3 1 6 5 4 6 7 7 8 7 8 6 9 9 8 9 ))

;( 4 2 3 4 5 5 2 3 2 )  Length 27
(define tilevec5q 
      '( 1 2 3 2 1 3 4 2 5 6 4 3 1 5 6 7 8 7 4 8 9 7 9 5 6 8 9 ))

;( 4 5 1 1 5 4 4 1 2 )  Length 27
(define tilevec5r 
      '( 1 2 3 3 1 3 2 4 4 5 4 6 1 7 5 6 2 7 8 8 9 8 9 6 5 7 9 ))

;( 4 5 1 4 2 4 3 3 1 )  Length 27
(define tilevec5s
      '( 1 2 3 3 1 3 2 4 5 6 5 4 1 6 5 7 2 8 7 4 8 6 9 9 7 9 8 ))

;( 5 1 5 1 1 4 3 2 2 )  Length 27
(define tilevec5t
      '( 1 2 2 3 2 1 4 4 3 4 5 5 6 5 7 1 6 7 3 8 9 8 9 7 6 8 9 ))

;( 5 3 4 5 4 4 4 3 1 )  Length 27
(define tilevec5u 
      '( 1 2 3 4 2 1 3 5 4 6 2 5 7 6 3 1 7 8 4 5 8 6 9 9 7 9 8 ))

;gap> tilings((1011)15400);
;( 5 1 5 3 1 3 )  Length 18
(define tilevec6a
      '( 1 2 3 2 2 4 5 6 5 5 1 4 3 6 4 1 6 3 ))

;( 2 4 5 4 1 2 )  Length 18
(define tilevec6b
      '( 1 2 3 4 1 5 1 5 5 2 6 4 3 2 6 4 6 3 ))

;( 1 2 2 4 1 1 1 )  Length 21
(define tilevec6c
      '( 1 2 1 1 3 2 4 2 3 5 3 5 5 6 4 6 6 7 4 7 7 ))

;( 2 4 4 4 1 2 1 )  Length 21
(define tilevec6d
      '( 1 2 3 4 1 5 1 5 5 2 3 4 6 2 3 4 6 7 6 7 7 ))

;( 2 4 5 4 1 3 2 )  Length 21
(define tilevec6e
      '( 1 2 3 4 1 5 1 5 5 2 6 4 3 2 7 4 6 3 7 6 7 ))

;( 1 2 3 1 3 1 1 )  Length 21
(define tilevec6f
      '( 1 2 1 1 3 2 4 2 4 4 3 5 6 3 6 6 7 5 7 7 5 ))

;( 1 2 4 4 1 2 1 )  Length 21
(define tilevec6g
      '( 1 2 1 1 3 2 4 2 5 6 5 5 3 6 4 6 3 7 4 7 7 ))

;( 1 2 2 3 4 1 1 1 )  Length 24
(define tilevec6h
      '( 1 2 1 1 3 2 4 2 3 5 3 6 4 6 6 4 7 5 7 7 8 5 8 8 ))

;( 1 2 2 5 1 3 3 1 )  Length 24
(define tilevec6i
      '( 1 2 1 1 3 2 4 2 3 5 3 5 5 6 7 8 4 8 8 6 7 4 6 7 ))

;( 1 2 3 1 2 2 3 1 )  Length 24
(define tilevec6j
      '( 1 2 1 1 3 2 4 2 4 4 3 5 6 3 7 5 6 5 6 8 7 8 8 7 ))

;( 2 2 4 3 5 3 1 1 )  Length 24
(define tilevec6k
      '( 1 2 3 4 1 2 1 2 5 4 3 6 4 7 3 7 7 6 5 8 6 8 8 5 ))

;( 3 1 3 4 4 2 3 1 )  Length 24
(define tilevec6m
      '( 1 2 3 2 2 4 1 5 3 1 6 3 7 4 6 5 6 4 7 5 8 7 8 8 ))

;( 3 1 5 5 2 5 2 1 )  Length 24
(define tilevec6n
      '( 1 2 3 2 2 4 1 5 6 1 7 5 3 5 7 4 7 3 6 8 4 8 8 6 ))

;( 2 2 3 5 4 3 1 1 )  Length 24
(define tilevec6p
      '( 1 2 3 4 1 2 1 2 3 5 6 3 7 4 7 7 6 5 4 6 8 5 8 8 ))

;( 5 2 2 5 5 1 2 2 )  Length 24
(define tilevec6q
      '( 1 2 3 4 5 2 3 2 3 6 1 6 6 4 5 1 7 8 4 5 7 8 7 8 ))

;( 5 5 2 2 5 4 3 1 )  Length 24
(define tilevec6r
      '( 1 2 3 4 5 6 3 4 3 4 1 2 7 6 5 1 2 6 7 5 8 7 8 8 ))

;( 4 5 1 2 4 5 3 3 3 )  Length 27
(define tilevec6s
      '( 1 2 3 4 3 3 5 4 1 4 6 2 1 7 5 8 2 9 5 7 6 8 7 9 8 6 9 ))

;( 3 1 3 4 4 5 2 4 1 )  Length 27
(define tilevec6t
      '( 1 2 3 2 2 4 1 5 3 1 6 3 7 4 8 5 7 4 7 5 6 9 8 9 9 6 8 ))

;( 3 1 4 1 3 2 3 3 1 )  Length 27
(define tilevec6u
      '( 1 2 3 2 2 4 1 4 4 1 3 5 6 7 3 8 6 5 6 7 5 8 7 9 8 9 9 ))

;( 4 3 1 4 4 5 4 2 3 )  Length 27
(define tilevec6v
      '( 1 2 3 4 3 3 5 2 1 6 2 4 1 7 5 4 8 9 5 6 8 7 8 9 6 7 9 ))

;( 5 2 2 5 5 1 3 2 2 )  Length 27
(define tilevec6w
      '( 1 2 3 4 5 2 3 2 3 6 1 6 6 4 5 1 7 8 4 5 9 8 7 8 9 7 9 ))

;( 4 4 1 4 4 5 3 3 1 1 )  Length 30
(define tilevec6x
      '( 1 2 3 4 3 3 5 6 1 2 7 4 1 2 5 4 7 6 5 7 8 9 6 9 9 10 8 10 10 8 ))

;( 4 5 1 2 4 5 2 3 2 2 )  Length 30
(define tilevec6y
      '( 1 2 3 4 3 3 5 4 1 4 6 2 1 7 5 8 2 7 5 7 6 8 9 10 8 6 9 10 9 10 ))

;( 5 2 2 3 5 5 5 2 4 3 )  Length 30
(define tilevec6z
      '( 1 2 3 4 5 2 3 2 3 4 1 6 4 7 5 1 8 9 10 5 8 6 8 7 10 9 6 10 7 9 ))

;( 5 1 2 2 3 5 5 3 1 3 )  Length 30
(define tilevec6aa
      '( 1 2 3 2 2 4 3 5 3 4 1 4 6 5 7 1 5 8 9 10 9 9 6 8 7 10 8 6 10 7 ))

;( 5 1 2 3 5 5 2 5 3 2 )  Length 30
(define tilevec6bb
      '( 1 2 3 2 2 4 3 5 3 6 1 4 7 8 4 1 7 5 7 6 9 10 5 8 6 10 9 10 8 9 ))

;( 4 5 1 2 4 5 2 3 3 2 2 )  Length 33
(define tilevec6cc
      '( 1 2 3 4 3 3 5 4 1 4 6 2 1 7 5 8 2 7 5 7 6 8 9 
        10 8 6 11 10 9 10 11 9 11 ))

;( 4 4 1 4 4 5 3 2 2 3 1 )  Length 33
(define tilevec6dd
      '( 1 2 3 4 3 3 5 6 1 2 7 4 1 2 5 4 
        7 6 5 7 8 9 6 10 8 9 8 9 11 10 11 11 10 ))

;gap> tilings((101001)15400);
;( 2 1 3 3 1 1 1 )  Length 21
(define tilevec7 
      '( 1 2 3 2 1 4 2 5 3 5 1 4 5 6 7 6 7 3 6 7 4 ))

;gap> tilings((101001)13400);
;( 2 1 3 3 1 1 1 )  Length 21
(define tilevec8a
      '( 1 2 3 2 1 4 2 5 3 5 1 4 5 6 7 6 7 3 6 7 4 ))

;( 3 2 1 3 1 2 3 3 1 1 1 )  Length 33
(define tilevec8b
      '( 1 2 3 4 3 2 1 3 5 4 5 2 6 5 7 1 6 8 4 9 7 9 6 8 9 
        10 11 10 11 7 10 11 8 ))

;( 2 2 3 2 3 3 1 2 3 3 1 1 1 )  Length 39
(define tilevec8c
      '( 1 2 3 4 1 2 5 4 3 6 1 2 5 4 7 6 7 3 8 7 9 5 8 10 6 
        11 9 11 8 10 11 12 13 12 13 9 12 13 10 ))

;( 1 1 2 2 2 1 1 2 2 2 3 3 3 1 1 )  Length 45
(define tilevec8d
      '( 1 2 1 2 3 1 2 4 3 5 6 4 6 5 3 6 7 4 7 5 8 7 9 10 8 
        11 9 10 12 13 8 11 9 10 12 13 14 15 14 15 11 14 15 12 13))

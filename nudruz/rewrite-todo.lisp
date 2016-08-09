(in-package :cm)

(load "rewrite.lisp")

; thue-morse
(defun morse-thue (len)
  (loop for x from 0 to (- len 1) collect
        (mod (logcount x) 2)))


;; the binary representation of an integer 
;; = (write x :base 2)

(car (multiple-value-list 

(reduce #'+ (write-to-string 9 :base 2))

(bit 9)

(char-int (write-to-string 9 :base 2))




(char-int (digit-char 7))

(morse-thue 10)

(mod 3 2)

(define samplerules
  '((s -> (a b c))
    (a -> (a sma b))
    (a -> (b c))
    (b -> (sma a c))
    (b -> (smb c))
    (c -> c)
    (sma -> sma)
    (smb -> smb)))


(defun derive (rules nonterminals init)
  (if (intersection nonterminals init)
    (derive rules nonterminals (rw-next rules init))
    init))




(define myauto 
  (new rewrite of 
       '(((1 letter) -> 3)
         ((1 digit) -> 2)
         (2 -> 2)
         ((3 letter) -> 3)
         ((3 digit) -> 3)
         (3 -> 3)
         (1 -> 1))
       :initially '(1 letter)))


(derive sample2 '(1 2 3) '(1 letter letter digit letter))

(define abcrw  (new rewrite :of '((a :id a)
                     (b :id b)
                     (c :id c))
       :initially '(a b c)
       :rules '(
                (a -> c)
                (b -> b)
                (c -> c)
                )))

(next abcrw 20)

(define mytrans (new rewrite of ))

(define abrw
  (new rewrite
    :of '((a :id a)
          (b :id b))
    :initially '(a b)
    :rules '(
             (a -> a b a)
             (b -> b b)
             )))

(next abrw 10)

(defun abrw-fun (myfirst mysecond len)
  (let ((intern-rw (new rewrite
                     :of `((,myfirst :id a)
                           (,mysecond :id b))
                     :initially '(a b)
                     :rules '(
                              (a -> a b a)
                              (b -> b b)
                              ))))
    (next intern-rw len)))

(abrw-fun .5 .25 20)

(define abrw2
  (new rewrite
    :of '((a :to (a b a))
          (b :to (b b)))
    :initially '(a b)))

(next abrw2 10)

(define dkauto
  (new rewrite
    :of '((a :id a)
          (b :id b))
    :initially '(a)
    :rules '(
             (a -> a b)
             (b -> b b)
             )))

(define myauto 
  (new rewrite :of '((n1 :id n1)
                     (n2 :id n2)
                     (n3 :id n3)
                     (a :id a)
                     (b :id b))
       :initially '(n1 b)
       :rules '(((n1 a) -> n3)
                ((n1 b) -> n2)
                ((n3 a) -> n3)
                ((n3 b) -> n3))
       :for 100))

(next myauto 10)

(define myauto2
  (new rewrite :of '((bigS :id bigS)
                     (bigA :id bigA)
                     (bigB :id bigB)
                     (a :id a)
                     (b :id b)
                     (c :id c))
       :rules '((bigS -> bigA bigB c)
                (bigA -> a bigB)
                (bigA -> bigB c)
                (bigB -> a bigA c)
                (bigB -> b c))))

(next myauto2 100)



;;; 


(defun rwmangle-gens (smalldur largedur gens)
  (let ((intern-rw 
         (new rewrite 
           :of `((,smalldur :id 1)
                 (,largedur :id 2))
           :initially '(1)
           :rules '((2 -> 1 1 2)
                    ((1 1) -> 2 2)))))
    (loop repeat gens collect
          (loop until (eop? intern-rw) collect (next intern-rw)))))

(rwmangle-gens .5 2 10)



(

(defun mypat (startnum gens) 
  (let* ((intern-rw (new rewrite
                      :of '((+1 :to (+1 -1 +1))
                            (-1 :to (-1 -1 +1)))))
         (intern-range
          (new range 
            :initially 25
            :stepping intern-rw)))
    (loop repeat gens collect
      (loop until (eop? intern-rw) collect (next intern-range)))))

(define steprw (new rewrite :of `(,a ,(+ a )

(define mystep (new range :initially 1 :stepping -1 +3 -3)))

(next mystep 10)

(mypat '(25) 10)

(new range :initially 25
                  :stepping 
(loop until eop? my

;; work with melints!!!

'(1 4 -2 3)


(define newstep 
  (new rewrite :of '((0 :id a)
                     (1 :id b)
                     (-1 :id c))
                   :initially '(a)
                   :rules '((a -> a b)
;                            (b -> b)
;                            (c -> c)
)))

(next newstep :chunk)

(pickl '(4 6 9))

(melint->line 1 '(0 0 1 -1 0 1 -1 1 -1 0 1 -1 1 -1 1 -1 0 1 -1 1))

(melint->line 40 '(2 2 3))


;;;;;;; RANDOM BOOLEAN NETWORKS -- model on 'life2d' 

(rand01array 3 4 .5)
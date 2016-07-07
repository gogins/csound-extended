;;; modifications by Drew Krause

;; genetic-salesman.lisp
;; (change-font-this-window "monaco" 18)

;; Lee Spector, November 1994

#|
Code for a genetic algorithm solution to the
traveling salesman problem, inspired by the
treatment in Dewdney's THE NEW TURING OMNIBUS,
Computer Science Press, W.H. Freeman and Company,
1993, p. 106.

NOTE: The purpose of this code is to demonstrate
genetic algorithms, NOT to make the genetic
solution to the traveling salesman problem as
efficient as possible. Several features of this
code -- e.g. the mapping to city names -- are
real time-wasters if you're looking for
short run-times.

Tours are represented as lists of "removal" 
numbers. For example, if the list of cities
is (a b c d e f) then (2 3 1 3 1 1) represents
the tour (b d a f c e):

                                    TOUR
start w/std sequence: a b c d e f
remove item 2:        a c d e f     b
remove item 3:        a c e f       b d
remove item 1:        c e f         b d a
remove item 3:        c e           b d a f
remove item 1:        e             b d a f c
remove item 1:                      b d a f c e
|#


(defun random-tour (n)
  "Returns a random valid tour of n cities. A tour
is valid as long as the i-th digit never exceeds
n+1-i."
  (let ((tour nil))
    (dotimes (i n)
      (push (1+ (random (- n i))) ; OUR i is 0-based
            tour))
    (reverse tour)))

;; (random-tour 6)

(defun tour->cities (removal-list city-list)
  "Returns a tour, expressed as a list of city names,
given a removal-list representation and a list of
all city names."
  (let ((sym-tour nil) city)
    (dolist (n removal-list)
      (setq city (nth (1- n) city-list))
      (push city sym-tour)
      (setq city-list (remove city city-list)))
    (reverse sym-tour)))

;; (tour->cities '(2 3 1 3 1 1) '(a b c d e f))


;; We'll represent a traveling salesman problem
;; as a object containing a list of city names
;; and an array of city-to-city distances.

(defclass salesman-problem ()
  ((names :accessor names :initarg :names)
   (distances :accessor distances)))

(defmethod initialize-instance :after
           ((s salesman-problem) &rest init-args)
  (declare (ignore init-args))
  (let ((num-cities (length (names s))))
    (setf (distances s)
          (make-array (list num-cities num-cities)
                      :initial-element 1))))

(defmethod distance ((s salesman-problem)
                     city-1 city-2)
  (let ((index-1 (position city-1 (names s)))
        (index-2 (position city-2 (names s))))
    (aref (distances s) index-1 index-2)))

(defmethod set-distance ((s salesman-problem)
                         city-1 city-2 distance)
  (let ((index-1 (position city-1 (names s)))
        (index-2 (position city-2 (names s))))
    (setf (aref (distances s) index-1 index-2)
          distance)))

(defvar *sp*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tour-length (tour sp &optional (debug nil))
  "Returns the total length of tour, which should
be a removal-list, with respect to sp, which should
be a salesman-problem"
  (let ((cities (tour->cities tour (names sp)))
        (total 0))
    (dotimes (n (1- (length cities)))
      (incf total (distance sp
                            (nth n cities)
                            (nth (1+ n) cities))))
    (if debug
      (format t "~%length of tour: ~A is ~A.~%"
              cities total))
    total))

;; (tour-length (random-tour 8) *sp*)
;; (tour-length (random-tour 8) *sp* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ga-salesman (sp pop-size gens)
  "Tries to solve the given salesman problem
by running a genetic algorithm with population
size pop-size for gens generations. Returns three
values: the symbolic form of the best tour found,
the total distance of that tour, and the final
population."
  (let ((pop nil)
        (best-fitness 100000) (best-tour nil))
    ;; make the initial population
    (dotimes (n pop-size)
      (push (random-tour (length (names sp))) pop))
    ;; for each generation
    (dotimes (g gens)
      (format t "~%Generation #~A.~%" g)
      (setq pop (sort-by-fitness pop sp))
      (report-on-pop pop sp)
      (when (< (tour-length (first pop) sp)
               best-fitness) ;; save best so far
        (setq best-tour (first pop)
              best-fitness (tour-length best-tour sp)))
      (setq pop (next-generation pop)))
    ;; return the best
    (values (tour->cities best-tour (names sp))
            best-fitness
            (mapcar #'(lambda (tr)
                        (tour->cities tr (names sp)))
                    pop))))

(defun sort-by-fitness (pop sp)
  "Returns a copy of pop, which should be a list
of removal-list tour representations, sorted so that
the shortest tour -- relative to problem sp -- is
first, etc."
  (sort (copy-list pop)
        #'(lambda (t1 t2)
            (< (tour-length t1 sp)
               (tour-length t2 sp)))))

(defun report-on-pop (pop sp)
  "Reports on the best tour on pop, which should be
sorted by fitness, and on the average fitness."
  (format t "Best of generation tour: ~A~%"
          (tour->cities (first pop) (names sp)))
  (format t "Total length of this tour: ~A~%"
          (tour-length (first pop) sp))
  (format t "Average length of tours in population: ~A~%"
          (average (mapcar #'(lambda (tr)
                               (tour-length tr sp))
                           pop)))
  (format t "-----------------------------------"))

(defun average (numbers)
  (float (/ (apply #'+ numbers) (length numbers))))

(defun next-generation (pop)
  "Applies genetic operations to produce a new
population from pop, which should be sorted by
fitness. Uses only reproduction -- from the 50%
best -- and crossover -- from the 50% best. Note
that many variations in detail are possible."
  (let ((new-pop nil)
        (half (floor (length pop) 2)))
    ;; reproduction from the top 50% (only a few)
    (do () ((> (length new-pop) (/ half 4)))
      (push (nth (random half) pop) new-pop))
    ;; crossover from the top 50%
    (do () ((>= (length new-pop) (length pop)))
      (push (crossover (nth (random half) pop)
                       (nth (random half) pop))
            new-pop))
    new-pop))

(defun crossover (list1 list2)
  "Returns a random child of the two given lists, the
initial segment will come from list1 and the final
segment will come from list2."
  (let ((cross-pt (random (1+ (length list1)))))
    (append (butlast list1
                     (- (length list1) cross-pt))
            (nthcdr cross-pt list2))))

;; (crossover '(1 2 3 4 5 6) '(a b c d e f))

;; a bigger traveling salesman problem instance

(setq *sp*
      (make-instance 'salesman-problem
        :names '(a b c d e f g h i j k l m n o p
                 q r s t u v w x y z)))

(dotimes (c1 26)
  (dotimes (c2 26)
    (set-distance *sp*
                  (nth c1 (names *sp*))
                  (nth c2 (names *sp*))
                  (random 100))))

(distance *sp* 'a 'z)

;; (ga-salesman *sp* 10 2)
;; (ga-salesman *sp* 20 2)
;; (ga-salesman *sp* 20 10)
;; (ga-salesman *sp* 20 20)

(define mysp
      (make-instance 'salesman-problem
        :names '(a b c d e f g h i j k l m n o p
                 q r s t u v w x y z)))

(dotimes (c1 26)
  (dotimes (c2 26)
    (set-distance mysp
                  (nth c1 (names mysp))
                  (nth c2 (names mysp))
                  (random 100))))

(distance mysp 'a 'z)

(ga-salesman mysp 10 2)
      
                            

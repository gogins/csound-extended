(in-package :cm)

;; Functions imported from depencency projects not in QuickLisp

;;--------------------------------------------------
;; FROM rsm.mod

(defun %get-powers (k n)
  "Get the list of the factor <k> that appears in <n>."
  (loop 
     :with nn = n
     :with facts = nil
     :while (= (mod nn k) 0) :do
     (setf nn (/ nn k))
     (push k facts)
     :finally (return facts)))

(defun %get-powers-of-2-3 (n)
  "Get the list of the primes 2 and 3 that occur in <n>."
  (let ((2-facts (%get-powers 2 n))
        (3-facts (%get-powers 3 n)))
    (nconc 2-facts 3-facts)))

(defun factors (n &key (no-dups t))
  "Computes and returns a list of the primes factors of <n>. If <no-dups> is
true, then no multiple entries of a factor are returned.
Example: (rsm.mod:factors 100)
         (2 5)
Example: (rsm.mod:factors 100 :no-dups nil)
         (2 2 5 5)"
  (let ((2-3-facts (%get-powers-of-2-3 n)))
    (let ((other-facts
           (loop 
              :with nn = (/ n (apply #'cl:* 2-3-facts))
              :with m = (isqrt nn)
              :with k = 5
              :with factors = nil
              :with skip fixnum = 2
              :while (<= k m) :do
              (if (= (mod nn k) 0)
                  (progn
                    (setf nn 
                          (do ((n1 nn (/ n1 k)))
                              ((> (mod n1 k) 0) n1)
                            (push k factors)))
                    (setf m (isqrt nn)))
                  (progn
                    (incf k skip)
                    (if (= skip 2)
                        (setf skip 4)
                        (setf skip 2))))
              :finally (return (nreverse 
                                (if (> nn 1)
                                    (cons nn factors)
                                    factors))))))
      (if no-dups
          (delete-duplicates
           (nconc 2-3-facts other-facts))
          (nconc 2-3-facts other-facts)))))

;;--------------------------------------------------
;; FROM cllib/simple

(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))


;;--------------------------------------------------
;; FROM cllib/math.lisp

(defun subsets (set)
  "return a list of all subsets of the given set (represented as a list)"
  (let ((first (first set)) (rest (rest set)))
    (if rest
        (let ((others (subsets rest)))
          (nconc others
                 (mapcar (lambda (subset)
                           (cons first subset))
                         others)))
        (list nil (list first)))))

(defun make-vector-indexed (len)
  "Return a simple vector #(0 1 ... (1-len))."
  (let ((vv (make-array len)))
    (dotimes (ii len vv)
      (setf (aref vv ii) ii))))

(defun vector-shuffle (vec)
  "Generate a random permutation of the vector in place.
If the argument is a number, return a new random vector of this length.
Uses the Fisher/Yates algorithm, see
 Knuth, TAOCP vol 2 Algorithm 3.4.2P, p.145
 R.A. Fisher & F. Yates, Statistical Tables, London 1938, Example 12
 R. Durstenfeld, CACM 7 (1964), 420.
This is more or less the same as
  (permutation vec (random (! (length vec))))
except that the factorial is likely to be far too large for `random'."
  (etypecase vec
    (vector (loop :for ii :downfrom (1- (length vec)) :to 1
                  :for jj = (random (1+ ii))
                  :unless (= jj ii)
                  :do (rotatef (aref vec ii) (aref vec jj)))
            vec)
    (number (vector-shuffle (make-vector-indexed vec)))))

(defun permutation
    (vec nth &optional (len (1- (length vec))) (fact (alexandria:factorial len)))
  "Generate the NTH permutation of the vector VEC in place.
The algorithm is similar to the standard Fisher/Yates one, but instead
of random numbers [a_n-1,...,a_1] it represents a number in [0;n!] as
   x = a_n-1*(n-1)! + ... + a_1
The original vector is returned when NTH = (1- (! (length vec)))."
  (loop :for ff = fact :then (/ ff ii)
        :for ii :downfrom len :to 1 :with jj
        :do (setf (values jj nth) (floor nth ff))
        :unless (= jj ii)
        :do (rotatef (aref vec ii) (aref vec jj)))
  vec)

(defmacro with-permutations-shuffle ((var vec &optional ret-form) &body body)
  "Gererate the successive shufflings of vector VEC using `permutation'.
VEC is not modified, VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The original vector is the last one returned."
  (alexandria:with-gensyms (vv len len1 fact ii jj tot)
   `(let* ((,vv ,vec) (,len (length ,vv))  (,len1 (1- ,len)) (,fact (alexandria:factorial ,len1))
            (,var (copy-seq ,vv)) (,tot (* ,len ,fact)))
      (dotimes (,ii ,tot ,(or ret-form tot))
        (dotimes (,jj ,len) (setf (aref ,var ,jj) (aref ,vv ,jj)))
        (permutation ,var ,ii ,len1 ,fact)
        ,@body))))

(defun check-permutations-end (name found length)
  (let ((fact (alexandria:factorial length)))
    (unless (= found fact)
      (error "~s: generated ~:d permutation~:p, not ~d!=~:d, as expected"
             name found length fact))))

(defmacro with-permutations-swap ((var vec &optional ret-form) &body body)
  "Bind VAR to each permutation of vector VEC in turn, then execute the BODY.
Thus, BODY is evaluated (! (length vec)) times.
VEC is not modified; VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated by transposing adjacent elements,
according to the CACM algorithm 115 [H.F.Trotter, Comm ACM 5 (Aug 1962) 434].
The original vector is the last one returned."
  (alexandria:with-gensyms (nn pp dd kk qq ii done top)
    `(let* ((,var (copy-seq ,vec)) (,ii 0) (,nn (length ,var))
            (,top (- ,nn 2)) (,kk 0) (,qq 0) (,done nil)
            (,pp (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element -1))
            (,dd (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element 1)))
      (declare (type (array fixnum (*)) ,pp ,dd) (fixnum ,kk ,qq))
      (loop
       (tagbody
          (setq ,kk 0 ,top (- ,nn 2))
        :index
          (setf ,qq (+ (aref ,pp ,top) (aref ,dd ,top))
                (aref ,pp ,top) ,qq)
          (when (= ,qq (1+ ,top))
            (setf (aref ,dd ,top) -1)
            (go :loop))
          (when (/= -1 ,qq) (go :swap))
          (setf (aref ,dd ,top) 1)
          (incf ,kk)
        :loop
          (when (> ,top 0)
            (decf ,top)
            (go :index))
          (setq ,qq 0 ,done t)
        :swap
          (incf ,qq ,kk) (rotatef (aref ,var ,qq) (aref ,var (1+ ,qq))))
       ;;(format t "~4d * ~s [k ~d] [n ~d] [p ~s] [d ~s] [q ~d] [done ~s]~%"
       ;; ,ii ,var ,kk ,top ,pp ,dd ,qq ,done)
       (incf ,ii)
       ,@body
       (when ,done
         (check-permutations-end 'with-permutations-swap ,ii ,nn)
         (return ,(or ret-form ii)))))))

(defmacro with-permutations-lex ((var len &optional ret-form) &body body)
  "Bind VAR to each permutation of vector [0:LEN-1] in turn,
then execute the BODY - i.e, BODY is evaluated (! len) times.
VAR storage is allocated only once, not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated in the lexicographic order,
according to the CACM algorithm 202 [M.K.Shen, Comm ACL 6 (Sept 1963) 517]."
  (alexandria:with-gensyms (ll nn ww ii)
    `(let* ((,ll ,len) (,var (make-vector-indexed ,ll)) (,nn 0))
      (declare (fixnum ,ll ,nn))
      (loop
       (unless (zerop ,nn)
         (let ((,ww (1- ,ll)))
           (declare (fixnum ,ww))
           (do () ((or (zerop ,ww) (< (aref ,var (1- ,ww)) (aref ,var ,ww))))
             (decf ,ww))
           (when (zerop ,ww)
             (check-permutations-end 'with-permutations-lex ,nn ,ll)
             (return ,(or ret-form nn)))
           (let ((,ii (position (aref ,var (1- ,ww)) ,var
                                :from-end t :test #'<)))
             (rotatef (aref ,var (1- ,ww)) (aref ,var ,ii)))
           (dotimes (,ii (ash (- ,ll ,ww) -1))
             (rotatef (aref ,var (- ,ll ,ii 1)) (aref ,var (+ ,ww ,ii))))))
       (incf ,nn)
       ,@body))))

(defun permutations-list (vec &key (method :lex))
  "Return the list of all the permutations of the vector VEC.
The order in which the permutations are listed is either
 lexicographic (when :METHOD is :LEX, which is the default),
  in which case `with-permutations-lex' is used;
 shuffling (when :METHOD is :SHUFFLE)
  in which case `with-permutations-shuffle' is used;
 transposing adjacent elements (when :METHOD is :SWAP),
  in which case `with-permutations-swap' is used.
:SWAP is more than twice as fast as :LEX
 and more that 10 times as fast as :SHUFFLE"
  (declare (ignorable method))
  (with-collect (coll)
    (ecase method
      (:lex (with-permutations-lex (vv (length vec))
              (let ((tv (copy-seq vec)))
                (dotimes (ii (length vec))
                  (setf (aref tv ii) (aref vec (aref vv ii))))
                (coll tv))))
      (:shuffle (with-permutations-shuffle (vv vec) (coll (copy-seq vv))))
      (:swap (with-permutations-swap (vv vec) (coll (copy-seq vv)))))))
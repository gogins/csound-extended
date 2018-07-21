;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; ORCHESTRA EDIT TOOLS
;;; By Karim Haddad 1998 Hyperion Experimental Studio
;;; Special thanks to Carlos Agon, Gerard Assayag, Mikhail Malt & Laurent Pottier
;;;===================================================


(in-package :om)



;=====================================
; header & variable declaration
;=====================================
(defvar *lastcsdfile* ())

(om::defmethod! header  ((sr number) 
                         (kr number)
                         (ksmps number)
                         (nchnls number))
  :icon 162
  :indoc '("sample rate" "control rate" "sr/kr ratio" "number of channels")
  :initvals '(44100 441 100 2)
  :doc "Outputs a Csound orchestra file header in the format format: 
       sr = 44100
       kr = 441
       ksmps = 1
       nchnls = 1"
 
  (format nil "sr= ~3D ~%kr= ~3D ~%ksmps= ~3D ~%nchnls= ~1D" sr kr ksmps nchnls))


;---------------------------Value converters--------------------------------------

(om::defmethod! convert-val ((mode symbol)(value t))
  :icon 130
  :indoc '( "mode" "value" )
  :initvals '('ampdb 'p4)
  :menuins (list '(0 (("ampdb" 'ampdb) 
                     ("dbamp" 'dbamp)
                     ("ftlen" 'ftlen)
                     ("int" 'int)
                     ("frac" 'frac)
                     ("i" 'i)
                     ("abs" 'abs)
                     ("exp" 'exp)
                     ("log" 'log)
                     ("sqrt" 'sqrt)
                     ("sin" 'sin)
                     ("cos" 'cos)
                     ("abs" 'abs))))
  :doc "Csound value converters."
  (let ((neu-val (if (listp value) (first value) value)))
  (format nil "~D(~D) " mode neu-val)))


(om::defmethod! convert-pitch ((mode symbol)(value t))
  :icon 130
  :indoc '("mode" "value" )
  :initvals '('octpch 'p4)
  :menuins (list '(0 (("octpch" 'octpch) 
                ("pchoct" 'pchoct)
                ("cpsch" 'cpsch)
                ("cpspch" 'cpspch)
                ("octcps" 'octcps)
                ("cpsoct" 'cpsoct))))
  :doc "Csound pitch converters"  
  (let ((neu-val (if (listp value) (first value) value)))
  (format nil "~D(~D) " mode neu-val)))


;----------------------VARIABLE DECLARATION---------------------------------------

(om::defmethod! assign-val  ((name t) 
                       (mode symbol)
                       (lst? t))
  :icon 156
  :indoc '("variable" "symbol" "value" )
  :initvals '( 'idur '= 'p3)
  :menuins (list '(1 (("=" '=) 
                      ("init" 'init))))
  :doc "Csound variable declaration"
  
  (let* ((name1 
          (if (listp name)
            (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                       (if (listp x) (list (first (om::flat x)) ",")
                                                           (list x ","))) name)))))
              (do ((n 0 (1+ n))
                   (lst '() (if (not (stringp (nth n input)))
                              (push (nth n input) lst)
                              (push (nth n input) lst))))
                  ((= n (length input)) (reverse lst)))) name))
         (state1  
          (om::x-append (if (listp name1)
                          (first name1) name1) "  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) (list lst?))))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (om::x-append (if (listp name)
                                 (rest name) nil)
                               (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                                                             x nil)) (list lst?))))))))
    (om::remove-dup output 'equal 1)))


;================================================================================
;==============================SIGNAL GENERATORS=================================
;================================================================================

(om::defmethod! line-seg ((name t) (mode symbol) (ia t) (idur1 t) (ib t) &rest more)
  :icon 143
  :indoc '("name" "opcode" "init value" "duration" "end value" "optional arguments")
  :initvals '( 'k1 'line 0 'p3 1 nil)
  :menuins (list '(1 (("line" 'line) 
                      ("expon" 'expon) 
                      ("linseg" 'linseg)
                      ("expseg" 'expseg))))
  :doc "Csound opcodes to generates a line or envelope.

Generates a curve between two or more specified points.

<mode> values:
- line: trace a straight line between specified points
- expon: trace an exponential curve between specified points. 
- lineseg: trace a series of line segments  between specified points. 
- expseg: trace a series of exponential segments between specified points. 

<ia> is the initial value of the segment (or first segment)
<ib> is the end value of the segment (or first segment)
<idur1> is the duration of the segment (or first segment)

For multiple segments (lineseg and expseg) use <more> to specify additional values and segment durations.


LINE-SEG is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf more (append (list ia idur1 ib) more))
  (let* ((state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) more)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) 
                         (om::flat-once 
                          (reverse 
                           (remove nil (mapcar #'(lambda (x) (if (listp x) x nil)) more)))))))
    (om::remove-dup output 'equal 1)))



(om::defmethod! oscil  ((name t) (mode symbol) (xamp t) (xcps t) (ifn t)
                        &rest lst?)
  :icon 142
  :indoc '("name" "opcode" "amplitude" "frequency (Hz)" "table number" "optional")
  :initvals '( 'asig 'oscil 10000 440 1 nil)
  :menuins (list '(1 (("oscil" 'oscil) 
                      ("oscili" 'oscili) 
                      ("foscil" 'foscil)
                      ("foscili" 'foscili)
                      ("loscil" 'loscil))))
  :doc "Csound oscillator opcodes.

<mode> values:
- oscil: simple oscillator (the table is incrementally sampled modulo the table <ifn> length and the value obtained is multiplied by <xamp>).
- oscili: simple oscillator with linear interpolation.
- foscil: frequency modulated oscillator
- foscili: frequency modulated oscillator with linear interpolation
- loscil: read sampled sound from a table

<xamp> is the ampliude of the output signal
<xcps> the base frequency of the oscillator
<ifn> is the oscillator table number
Depending on the mode and oscillator opcode, use <lst?> optional inputs to specify additional arguments.

OSCIL is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list xamp xcps ifn) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


(om::defmethod! phasor ((name t) 
                        (xcps t)
                        &rest lst?)
  :icon 164
  :indoc '("name"  "frequency" "optional")
  :initvals '( 'k1  1 nil)
  :doc "Csound PHASOR opcode.

Produces a normalized moving phase value.

PHASOR is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list xcps) lst?))
  (let* (
         (state1  
          (om::x-append name"  phasor  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


(om::defmethod! tablei ((name t) 
                        (mode symbol)
                        (indx t) 
                        (ifn t)                  
                        &rest lst?)
  :icon 144
  :indoc '("name" "opcode" "indx" "ifn" "optional")
  :initvals '( 'k1 'tablei nil 1 nil)
  :menuins (list '(1 (("table" 'table) 
                      ("tablei" 'tablei) 
                      ("phasor" 'phasor)
                      ("oscil1" 'oscil1)
                      ("oscil1i" 'oscil1i))))
  :doc "Csound table access opcodes.

TABLEI is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list indx ifn) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode "  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))



(om::defmethod! rand ((name t) 
                      (mode symbol)
                      (xamp t)
                      &rest lst?)
  :icon 159
  :indoc '("name" "opcode" "amplitude" "optional")
  :initvals '( 'k1 'rand 1 nil)
  :menuins (list '(1 (("rand" 'rand) 
                      ("randh" 'randh) 
                      ("randi" 'randi))))
  :doc "Csound random generator access opcodes.

RAND is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list xamp) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


;================================================================================
;==============================SIGNAL MODIFIERS==================================
;================================================================================

(om::defmethod! linen  ((name t) 
                        (mode symbol)
                        (xamp t)
                        (irise t)
                        (idur t)
                        (idec t)
                        &rest lst?)
  :icon 155
  :indoc '("name" "opcode" "xamp" "irise" "idur" "idec" "optional" )
  :initvals '('k1 'linen 10000 0.5 'p3 0.5 nil)
  :menuins (list '(1 (("linen" 'linen) 
                      ("linenr" 'linenr) 
                      ("envlpx" 'envlpx)
                )))
  :doc "Csound signal modification opcodes.

Used to apply a straight line rise and decay patterns to an input amplitude signal.

LINEN is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
   (setf lst? (append (list xamp irise idur idec) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


(om::defmethod! reverb  ((name t) 
                         (mode symbol)
                         (asig t)
                         (krvt t)
                         &rest lst?)
  :icon 157
  :indoc '("name" "opcode" "asig" "krvt" "optional")
  :initvals '( 'arevb 'reverb 'asig 5 nil)
  :menuins (list '(1 (("reverb" 'reverb) 
                      ("reverb2" 'reverb2) 
                      ("comb" 'comb)
                      ("alpass" 'alpass))))
  :doc "Csound reverberation opcodes.

REVERB is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
   (setf lst? (append (list asig krvt) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


;-------------------------------------filters-----------------------------------------------------------------------

(om::defmethod! porta  ((name t) 
                       (mode symbol)
                       
                       &rest lst?)
  :icon 160
  :indoc '("name" "opcode" "argument" )
  :initvals '( nil 'porta nil)
  :menuins (list '(1 (("porta" 'porta) 
                      ("tone" 'tone) 
                      ("atone" 'atone)
                      ("reson" 'reson)
                      ("areson" 'areson))))
  :doc "Csound filter opcodes.

PORTA is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  
  (let* ((state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))

(om::defmethod! butterhp  ((name t) 
                            (mode symbol)
                            (asig t)
                            (kfreq t)
                            &rest lst?)
  :icon 160
  :indoc '("name" "opcode" "asig" "kfreq" "optional")
  :initvals '( 'ahpf 'butterhp 'asig 1000 nil)
  :menuins (list '(1 (("butterhp" 'butterhp) 
                      ("butterlp" 'butterlp) 
                      ("butterbp" 'butterbp)
                      ("butterbr" 'butterbr))))
  :doc "Csound Butterworth filter opcodes.

BUTTERHP is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list asig kfreq) lst?))
  (let* (
         (state1  
          (om::x-append name"  " mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))



;================================================================================
;=============================ARITHMETIC OPERATORS================================
;================================================================================


(defmethod plus-bin  ((name1 t) (name2 t) (mode symbol))
  (let* ((strg (cond
                ((eq mode 'norm) "~D+~D")
                ((eq mode 'left) "(~D)+~D")
                ((eq mode 'rigth) "~D+(~D)")))
         (tete (format nil strg (if (listp name1)
                                  (first (om::flat name1))
                                  name1)
                       (if (listp name2)
                         (first (om::flat name2))
                         name2)))
         (liste (om::x-append
                 (remove '() (if (listp name1) name1 nil))
                 (remove '() (if (listp name2) name2 nil))))
         (enlev-dup (om::remove-dup liste 'equal 1)))
    (om::x-append tete enlev-dup)))

(om::defmethod! plus  ((mode symbol) (name1 t) (name2 t) &rest additionnal)
  :icon 148
  :indoc '("addition mode" "name1" "name2" "add")
  :initvals '('norm nil nil nil)
  :menuins (list '(0 (("norm" 'norm) ("left-part" 'left) ("rigth-part" 'rigth))))
  :doc "Csound opcode to add two signals.

Use left or right parenthesis (or none) depending on <mode> ['norm, 'left or 'right]

PLUS is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (let ((rep (plus-bin name1 name2 mode)))
    (loop for elem in additionnal do
          (setf rep (plus-bin rep elem mode)))
    rep))


(defmethod minus-bin  ((name1 t) (name2 t) (mode symbol))
  (let* ((strg (cond
                ((eq mode 'norm) "~D-~D")
                ((eq mode 'left) "(~D)-~D")
                ((eq mode 'rigth) "~D-(~D)")))
         (tete (format nil strg (if (listp name1)
                                  (first (om::flat name1))
                                  name1)
                       (if (listp name2)
                         (first (om::flat name2))
                         name2)))
         (liste (om::x-append
                 (remove '() (if (listp name1) name1 nil))
                 (remove '() (if (listp name2) name2 nil))))
         (enlev-dup (om::remove-dup liste 'equal 1)))
    (om::x-append tete enlev-dup)))

(om::defmethod! minus  ((mode symbol) (name1 t) (name2 t) &rest additionnal)
  :icon 146
  :indoc '("subtraction mode" "name1" "name2" "add")
  :initvals '('norm nil nil nil)
  :menuins (list '(0 (("norm" 'norm) ("left-part" 'left) ("rigth-part" 'rigth))))
  :doc "Csound opcode to minus two signals.

Use left or right parenthesis (or none) depending on <mode> ['norm, 'left or 'right]

MINUS is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (let ((rep (minus-bin name1 name2 mode)))
    (loop for elem in additionnal do
          (setf rep (minus-bin rep elem mode)))
    rep))


(defmethod multiply-bin  ((name1 t) (name2 t) (mode symbol))
  (let* ((strg (cond
                ((eq mode 'norm) "~D*~D")
                ((eq mode 'left) "(~D)*~D")
                ((eq mode 'rigth) "~D*(~D)")))
         (tete (format nil strg (if (listp name1)
                                  (first (om::flat name1))
                                  name1)
                       (if (listp name2)
                         (first (om::flat name2))
                         name2)))
         (liste (om::x-append
                 (remove '() (if (listp name1) name1 nil))
                 (remove '() (if (listp name2) name2 nil))))
         (enlev-dup (om::remove-dup liste 'equal 1)))
    (om::x-append tete enlev-dup)))

(om::defmethod! multiply  ((mode symbol) (name1 t) (name2 t) &rest additionnal)
  :icon 147
  :indoc '("multiplication mode" "name1" "name2" "add")
  :initvals '('norm nil nil nil)
  :menuins (list '(0 (("norm" 'norm) ("left-part" 'left) ("rigth-part" 'rigth))))
  :doc "Csound opcode to multiply two signals.

Use left or right parenthesis (or none) depending on <mode> ['norm, 'left or 'right]

MULTIPLY is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (let ((rep (multiply-bin name1 name2 mode)))
    (loop for elem in additionnal do
          (setf rep (multiply-bin rep elem mode)))
    rep))


(defmethod divide-bin  ((name1 t) (name2 t) (mode symbol))
  (let* ((strg (cond
                ((eq mode 'norm) "~D/~D")
                ((eq mode 'left) "(~D)/~D")
                ((eq mode 'rigth) "~D/(~D)")))
         (tete (format nil strg (if (listp name1)
                                  (first (om::flat name1))
                                  name1)
                       (if (listp name2)
                         (first (om::flat name2))
                         name2)))
         (liste (om::x-append
                 (remove '() (if (listp name1) name1 nil))
                 (remove '() (if (listp name2) name2 nil))))
         (enlev-dup (om::remove-dup liste 'equal 1)))
    (om::x-append tete enlev-dup)))

(om::defmethod! divide  ((mode symbol) (name1 t) (name2 t) &rest additionnal)
  :icon 145
  :indoc '("division mode" "name1" "name2" "add")
  :initvals '('norm nil nil nil)
  :menuins (list '(0 (("Norm" 'norm) ("left-part" 'left) ("rigth-part" 'rigth))))
  :doc "Csound opcode to divide two signals.

Use left or right parenthesis (or none) depending on <mode> ['norm, 'left or 'right]

DIVIDE is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (let ((rep (divide-bin name1 name2 mode)))
    (loop for elem in additionnal do
          (setf rep (divide-bin rep elem mode)))
    rep))

;================================================================================
;==============================ANY STATEMENTS====================================
;================================================================================

;this one is to pass info from a statement to another

(om::defmethod! statement  ((name t) 
                            (opcode t)
                            &rest lst?)
  :icon 161
  :indoc '("name" "opcode" "argument" )
  :initvals '( nil nil nil)
  :doc "A Csound statement.

This box allows to add any possible statement in the Csound instrument.

STATEMENT is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  
  (let* ((state1  
          (om::x-append name"  " opcode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil)) lst?)))))))
    (om::remove-dup output 'equal 1)))


;================================================================================
;==============================output============================================
;================================================================================


(om::defmethod! soundin  ((name t) 
                          (ifilcod t) 
                          &rest lst?)
  :icon 158
  :indoc '("name" "name sound" "optional")
  :initvals '(a1 soundin nil)
  :doc "A Csound statement for sound file input.

SOUNDIN is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list (if (numberp ifilcod) ifilcod
                             (format nil "~s" ifilcod)))
                     lst?))
  (let* ((state1  
          (om::x-append name"  "(format nil "    soundin ")
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                      x nil))lst?)))))))
    (om::remove-dup output 'equal 1)))


(defun ivar-sort (list)
  (let* ((ivar (remove '()
                       (mapcar #'(lambda (x) (cond ((stringp x) nil)
                                                   ((string= (first x) 'i :start1 0 :end1 1) x)
                                                   (nil))) list)))
         (reste (remove '()
                        (mapcar #'(lambda (x) (cond ((stringp x) nil)
                                                    ((string= (first x) 'i :start1 0 :end1 1) nil)
                                                    (x)))
                                list))))
    (om::x-append ivar reste)))


(om::defmethod! out  ((mode symbol) (asig t) &rest lst?)
  :icon 152
  :indoc '("opcode" "asig" "optional")
  :initvals '( 'out 'a1 nil)
  :menuins (list '(0 (("out" 'out) 
                      ("outs" 'outs) 
                      ("outs1" 'outs1)
                      ("outs2" 'outs2))))
  :doc "Csound opcode for instrument output.

Outputs <asig> to 
- a mono output if <mode> = 'out
- a stereo output if <mode> = 'outs
- a stereo channel 1 if <mode> = 'out1
- a stereo channel 2 if <mode> = 'out2

OUT is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
"
  (setf lst? (append (list asig) lst?))
  (let* ((state1  
          (om::x-append "     "mode"  "
                        (let((input (butlast (om::flat (mapcar #'(lambda (x) 
                                                                   (if (listp x) (list (first (om::flat x)) ",")
                                                                       (list x ","))) lst?)))))
                          (do ((n 0 (1+ n))
                               (lst '() (if (not (stringp (nth n input)))
                                          (push (nth n input) lst)
                                          (push (nth n input) lst))))
                              ((= n (length input)) (reverse lst))))))
         (output0 (append (list state1) (om::flat-once (reverse (remove '() (mapcar #'(lambda (x) (if (listp x)
                                                                                                    x nil)) lst?))))))
         (output1 (reverse (om::remove-dup output0 'equal 1))))
    (ivar-sort output1)))


;====================================inst============================================

(om::defmethod! inst  ((inst-num t)
                       (vars t)
                       (out t)
                         &rest lst?)
  :icon 154
  :indoc '("instrument number" "assign-val" "out" "gvar")
  :initvals '(1 nil nil nil)
  :doc "Global Csound instrument declaration.

<inst-num> is the instrument number as referred in the Csound scores.
<vars> is a list of variables as declared with ASSIGN-VAR
<out> is the output of the OUT opcode box, which includes all signal processing.


INST is part of the library of Csound OPCODES boxes used to design Csound instruments.

See also http://www.csounds.com/manual/html/index.html for more info about Csound programming.
" 
 (let*  ((result1 (om::x-append 
                    (list (list(format nil "~% ")))
                    (list (list (format nil "instr ~D" inst-num)))
                    (list (list (format nil "~% ")))
                    vars
                    (list (list (format nil "~% ")))
                    out))
          (rem-strg (mapcar #'(lambda (x) (if (stringp x) nil x)) result1))
          (fin-result (remove '() rem-strg))
    (output1 (if lst? 
              (om::x-append fin-result  (om::flat-once lst?)
                           (list (list (format nil "~% "))) 
                           (list (list "    endin")))
        (om::x-append  
         fin-result
         (list (list (format nil "~% "))) 
         (list (list "    endin")))))
    (output2 (reverse (om::remove-dup (reverse output1) 'equal 1))))
    (remove '() (mapcar #'(lambda (x) (if (listp x) x nil)) output2))))



;;;==================================
;;; WRITE ORC FILE
;;;==================================

(defun flat-max-1-orc (l)
  (while (listp (caar l))
    (setf l  (om::flat-once l))) l)

(defun print-seq-aux-orc (seq dest)
  (cond ((equal 'i seq)
         (format dest "~D" seq))
        ((integerp seq)
         (format dest "~D~a" seq #\tab))
        ((numberp seq)
         (format dest "~4,4f~a"(float seq) #\tab))
        (t 
         (format dest "~D" seq))))

(defun printorcseq (seq dest)
    (if (atom seq)
        (print-seq-aux-orc seq dest)
      (while seq
        (print-seq-aux-orc (pop seq) dest))))

;;;==================================
;;; New box OM2Csound 2.0 (2010)
;;;==================================

(om::defmethod! write-csound-orc (out header instr &rest more-instr)
  :icon 135
  :indoc '("filename" "file header" "csound instrument" "other instruments")
  :initvals '(file "" nil nil)
  :menuins '((0 (("text output" 'no-file) ("file output" 'file))))
  :doc "Writes a csound score in a <file>.
        
        <out> can be a pathname or a file name. If the name only is given, the file will be written in the OM 'temp files' folder.
        If <out> is nil, a dialog will allow to choose a file manually.
        If <out> is 'no-file, the function returns the orchestra as a list of Csound code lines.

        <header> is a file header as returned by the function HEADER. 
        <instr> is an instrument formatted by the OM2Csound ORC editor tools.
        <more> allows to connect other Csound instrumets.
"

  (let ((ins-data (flat-max-1-orc (cons instr more-instr)))
        (filename (handle-new-file-exists 
                   (cond ((equal out 'no-file) nil)
                         ((pathnamep out) out)
                         ((stringp out) (tmpfile out :type "orc"))
                         (t (om-choose-new-file-dialog :directory (or *lastcsdfile* *om-tmpfiles-folder*)
                                                       :prompt "Save csound ORC..." :types '("Csound ORC Files" "*.orc")))))))
    (if  filename
      (progn
        (format t "Writing CSOUND ORCHESTRA : ~s" (namestring filename))
        (with-open-file  (fd filename :direction :output :if-does-not-exist :create)   
          (format fd "~D" header)    
          (format fd "~% ~%")
          (let ((glou nil))
            (loop for ins in ins-data do
                  (loop for item in ins do
                        (setf glou (print-comment item fd glou))
                        (printorcseq item fd))
                  (format fd "~%")))
          )
        (probe-file filename))
      (let ((rep nil)
            (glou nil))
        (push header rep)
        (loop for ins in ins-data do
              (with-open-stream (s (make-string-output-stream))
                  (loop for item in ins do
                        (setf glou (print-comment item s glou))
                        (printorcseq item s))
                  (push (get-output-stream-string s) rep))
                  )
        (reverse rep)
        )
      )))


(om::defmethod! editorc  (header instr &rest lst?)
  :icon 135
  :doc "DEPRECATED: use WRITE-SCOUND-ORC"
  (apply 'write-csound-orc (append (list nil header instr) lst?)))

(om::defmethod! auto-editorc  (header instr-list &optional (name "my-orc"))
  :icon 135
  :doc "DEPRECATED: use WRITE-SCOUND-ORC"
  (apply 'write-csound-orc (append (list name header) instr-list)))


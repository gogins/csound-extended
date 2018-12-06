;;; 
;;; ASDF 3.1 compliant system definition file for David Cope's 
;;; "Computer Models of Musical Creativity" code.
;;;
;;; Michael Gogins
;;;

(require :asdf)    
#+sbcl
(require 'sb-introspect)
;;; Turn off all those zillions of SBCL warnings.
(declaim #+sbcl (sb-ext:muffle-conditions style-warning))
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
#+lispworks
(push "~/.local/share/common-lisp/source/" asdf:*central-registry*)

(asdf::defsystem "cmmc"
  :description "Algorithmic composition code/library/examples from David Cope."
  :long-description "Algorithmic composition code/library/examples from David Cope. This code is fairly old, has not been recently maintained, so don't count on everything working."
  :version "1.0"
  :author "David Cope (http://artsites.ucsc.edu/faculty/cope/), ASDF file by Michael Gogins <michael.gogins@gmail.com>"
  :serial f
  
  ; Files commented out below depend on MCL and I don't want to patch them 
  ; (at least not yet), or are init files not needed here.
  
  :components  
  (
    ; (:file "david-cope-cmmc/apprentice-chapter-11/help")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/IO")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/Init")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/windows")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/midi-file-read")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/QT-music")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/utilities")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-2")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-3")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-6")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-5")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-4")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/data/BACH-1-1")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/apprentice")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/objects")
    ; (:file "david-cope-cmmc/apprentice-chapter-11/groupings")
    ; (:file "david-cope-cmmc/fuzzy-chapter-3/fuzzy")
    ; (:file "david-cope-cmmc/associate-chapter-9/Associate")
    
    (:file "david-cope-cmmc/infer-chapter-6/Infer")
    
    ; (:file "david-cope-cmmc/sonify-chapter-3/play-midi-QT")
    
    (:file "david-cope-cmmc/sonify-chapter-3/Sonify")
    
    (:file "david-cope-cmmc/analogy-chapter-6/Analogy")
    
    (:file "david-cope-cmmc/gradus-chapter-6/Gradus")
    
    (:file "david-cope-cmmc/cosine-chapter-3/Cosine")
    ; (:file "david-cope-cmmc/cosine-chapter-3/play-midi-QT")
    ; (:file "david-cope-cmmc/serendipity-chapter-8/load-midi")
    ; (:file "david-cope-cmmc/serendipity-chapter-8/play-midi-QT")
    ; (:file "david-cope-cmmc/serendipity-chapter-8/init")
    ; (:file "david-cope-cmmc/serendipity-chapter-8/serendipity")
    ; (:file "david-cope-cmmc/speac-chapter-7/speac-analysis")
    ; (:file "david-cope-cmmc/speac-chapter-7/form-window")
    ; (:file "david-cope-cmmc/speac-chapter-7/test-music/chopin-67-4")
    ; (:file "david-cope-cmmc/speac-chapter-7/test-music/chopin-33-3")
    ; (:file "david-cope-cmmc/speac-chapter-7/test-music/chopin-63-2")
    ; (:file "david-cope-cmmc/speac-chapter-7/new-form")
    ; (:file "david-cope-cmmc/speac-chapter-7/top-level")
    ; (:file "david-cope-cmmc/speac-chapter-7/menubar")
    ; (:file "david-cope-cmmc/speac-chapter-7/init")
    ; (:file "david-cope-cmmc/speac-chapter-7/speac")
    ; (:file "david-cope-cmmc/speac-chapter-7/utilities")
    ; (:file "david-cope-cmmc/speac-chapter-7/variables")
    ; (:file "david-cope-cmmc/speac-chapter-7/pattern-match")
    ; (:file "david-cope-cmmc/speac-chapter-7/overview")
    ; (:file "david-cope-cmmc/ca-chapter-3/CA")
    ; (:file "david-cope-cmmc/ca-chapter-3/play-midi-QT")
    ; (:file "david-cope-cmmc/ca-chapter-3/init")
    ; (:file "david-cope-cmmc/network-chapter-3/network")
    ; (:file "david-cope-cmmc/network-chapter-3/play-midi-QT")
    ; (:file "david-cope-cmmc/markov-chapter-3/play-midi-QT")
    
    (:file "david-cope-cmmc/markov-chapter-3/Markov")
    
    ; (:file "david-cope-cmmc/sorcerer-chapter-5/init")
    
    (:file "david-cope-cmmc/improvise-chapter-4/improvise")
    
    ; (:file "david-cope-cmmc/improvise-chapter-4/play-midi-QT")
    ; (:file "david-cope-cmmc/improvise-chapter-4/windows")
    ; (:file "david-cope-cmmc/improvise-chapter-4/midi-file-read")
    ; (:file "david-cope-cmmc/improvise-chapter-4/init")
    
    (:file "david-cope-cmmc/improvise-chapter-4/utilities")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/georgiaiam")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/improv-1")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/improv-2")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/fourbros")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/hbirthday")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/forgray")
    (:file "david-cope-cmmc/improvise-chapter-4/sample-databases/improv")
    
    (:file "david-cope-cmmc/poet-chapter-2/Poet")
    
    ; (:file "david-cope-cmmc/chorale-chapter-4/play-midi-QT")
    ; (:file "david-cope-cmmc/chorale-chapter-4/chorale-mcl")
    ; (:file "david-cope-cmmc/chorale-chapter-4/init")
    
    (:file "david-cope-cmmc/chorale-chapter-4/utilities")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb6")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb5")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb3")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb2")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb12")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb11")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb1")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb13")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb7")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb9")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb10")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb8")
    (:file "david-cope-cmmc/chorale-chapter-4/data/jsb4")
    )
)


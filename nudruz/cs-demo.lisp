
(load "Druzaks/nudruz.lisp")
(load "rhythm.lisp")

;; RPCYC -- makes a long list of keynums
(define rpcyc
  (loop repeat 25
  
    ;; make a list between 5 and 9 random pitches
    for bpits = (randpits (between 5 9))
    ;; state the pitches
    collect (loop for x in bpits collect x)
    ;; then rpt between 1 and 6 times: transpose list to a random pitlevel
    collect (loop repeat (+ 1 (random 5))
		for y in (transp-to (random 12) bpits) collect y)
    ;; then collect one chd-inversion (shuffle oct-size index)
    collect (chd-inversion (shuffle bpits) (random 12) (random 4))))

;; RHYTCYC -- makes long rhythm cycle
(define rhytcyc (new cycle of 
		     ;; merges attacks within list1 by list2
		     (combine-atks
		     ;; combines measures & subdivisions into a cycle
		      (cyc-rhythms '(1) '(8 9))
		      ;; cycle of (min-upb max-upb min-dnb-dur max-dnb-dur)
		      (upbeatcyc 6 9 4 12))))

;; MIDI PLAYBACK
;; define some midi events
(define flcyc
  (let 	((inpits (flatten rpcyc)))
  (process 
    for thispit in inpits
    for thisdur = (next rhytcyc)
        output (new midi :time (now)
                     :keynum (+ 60 thispit)
                     :duration thisdur)
	wait (float thisdur))))


;; store them in a midi file
(events flcyc "myscore.midi")

;; CSOUND
;; setups
(defobject i1 (i)
  (amp
   freq
   (p6 :initform .25)
   (p7 :initform .25))
  (:parameters time dur amp freq p6 p7))

(set-sco-output-hook! #'play-sco-file)

;; now define some csound events

(define csflcyc
  (let 	((inpits (flatten rpcyc)))
  (process 
    for thispit in inpits
    for thisdur = (next rhytcyc)
        output (new i1 :time (now)
                     :freq  (hertz (+ 60 thispit))
                     :dur thisdur
		     :amp (between .5 .7))
	wait (float thisdur))))

;; NEED TO DEFINE SCO-HEADERS
; for dkspace
(define sco-header
	"f 1 0 16384 10 1")

;; now write out to sco and wav
(events csflcyc "csflcyc.sco" 0
	:header sco-header
	:options "-d -m0 -W"
	:orchestra "dkspace.orc"
	:output "csflcyc.wav")


;; NOW LET'S USE P6 (dkspace2.orc)

(define csflcyc
  (let 	((inpits (flatten rpcyc)))
  (process 
    for thispit in inpits
    for thisdur = (next rhytcyc)
        output (new i1 :time (now)
                     :freq  (hertz (+ 60 thispit))
                     :dur thisdur
		     :amp (between .5 .7)
		     :p6 (between .01 .04))
	wait (float thisdur))))

;; LET'S ALSO ADD OVERTONES TO THE SINE
; for dkspace 
(define sco-header
	"f 1 0 16384 10 1 .5")

;; now write out to sco and wav
(events csflcyc "csflcyc2.sco" 0
	:header sco-header
	:options "-d -m0 -W"
	:orchestra "dkspace2.orc"
	:output "csflcyc2.wav")

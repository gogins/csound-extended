(in-package :cm)

;; maps perc names to channel 9 keynums
(defparameter drumset '((bd . 35) (sn . 38) (hhc . 42) (hho . 46)
		  (ride . 51) (cup . 53) (crash . 49)
		  (tom1 . 48) (tom2 . 45) (tom3 . 41)
		  (rim . 37)))

;; works at 1, .8 .75
(defparameter rock-1 '((bd 0 2 2.5)
		 (sn 1 3 3.5 3.75)
		 (hhc 0 .5 1 1.5 2 2.5 3 3.5)))

;; works at 1
(defparameter soulpower '((bd 0 .5 2.5)
		    (sn .25 1 1.75 2.25 3 3.75)
		    (hhc 0 1 1.5 2 3 3.5)
		    (hho .5 2.5)))

(defparameter stubble-1 '((bd .5 2.5 3.25)
		    (sn 1 1.75 3 3.75)
		    (hhc 0 .25 .5 .75 1 1.25 1.5 1.75 2 2.25 2.5 2.75
		     3 3.25 3.5 3.75)))

(defparameter nate-jones '((bd 0 .5 2.5 4.25 4.5 6.5 7.75)
		     (sn 1.75 2.25 3.5 5.75 6.25 7)
		     (hhc 0 1 1.5 2 2.5 3 3.5 4 5 5.5 6 6.5 7 7.5)
		     (hho .5 4.5)))

(defparameter newmark-1 '((bd 0 2.25 3 3.75)
		  (sn .25 2.5 3.25)
		  (hhc 0 .5 1.5 2 2.5 3 3.5)
		  (hho .75)))

(defparameter newmark-2 '((bd 0 .75 2.25 3.75)
		    (sn 1 2.5 3.25)
		    (hhc .5 1 1.5 2 2.5 3 3.5 3.75)
		    (hho 0)))

(defparameter cisco-kid '((bd 0 1.25 1.75 3.25 3.75)
		    (rim 1 3)
		    (hhc 0 .5 .75 1 1.5 1.75 2 2.5 2.75 3 3.5 3.75))) 

(defparameter bop-1 '((hhc 1 3)
		(ride 0 1 1.666 2 3 3.666)
		(sn 0 .66666 1.3333 2 2.6666 3.33333)
		(bd .3333 1 1.666 2.333 3 3.6666)))

(defparameter bop-2 '((hhc 1 3)
		(ride 0 1 1.666 2 3 3.666)
		(tom1 1.333 3.333)
		(sn .3333 .6666 2.3333 2.6666)
		(tom2 3.6666)))

(defparameter bop-3 '((hhc .6666 1.6666 2.33333 3.3333)
		(sn .3333 1 2.3333 3)
		(bd 0 1.33333 2 3.6666)))

(defparameter bop-3a '((hhc .6666 1.6666 2.33333 3.3333)
		(sn .3333 1 2.3333 3)
		(cup .3333 1 2.3333 3)
		(bd 0 1.33333 2 3.6666)))



;; works well at temposcale = .5
(defparameter elvin '((ride 0 1 2 2.6666 3 4 5 5.666)
		(sn 0 .3333 1.6666 2.3333 3 3.3333 4.6666 5.3333)
		(bd 0.6666 1.33333 2 4 5.66666)
		(hhc 1 2.66666 3.666666 4.333333 5)))

;; works well at .5
(defparameter maxroach '((sn 0 3 6)
		(bd 1 2 3.5 5.5 6.5 7.5)
		(hhc .5 2.5 4 5 7)
		(tom1 1.5)
		(tom2 4.5)))

(defparameter blakey-1 '((sn .3333 .6666 1 1.3333 1.6666 2.333 3 3.666)
		   (tom2 0 2 3.3333)
		   (hhc 1 3)))

(defparameter blackwell '((sn 0 1.5)
		    (ride .5 1 2 3 3.5)
		    (tom1 2.5)
		    (bd .5 1 2 3 3.5)))

(defparameter tony '((sn 0 .3333 1.5 2 2.3333 2.6666 3.5)
		 (ride .6666 1 3)
		 (bd .6666 1 3)))

(defparameter blues '((sn 1 3)
		(bd 0 2)
		(hhc 0 .333 .666 1 1.333 1.666 2 2.3333 2.666 3 3.333 3.666)))

(defparameter speed-metal-1 '((hho 0 1 2 3)
		      (sn 1 3)
		      (bd 0 2)))

(defparameter speed-metal-2 '((hho 0 1 2 3)
		      (sn 0 1 2 3)
		      (bd .5 1.5 2.5 3.5)))

;; example of run-time choices
; (defparameter mything (new cycle :of (list 1 2 (pval (pick 4 6)))))


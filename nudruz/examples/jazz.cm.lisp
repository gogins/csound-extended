(require :asdf)
(require :nudruz)
(in-package :cm)

#|
This tutorial presents an implementation of an Automatic Jazz
program. The code is derived from a program originally written by Erik
Flister at CCRMA, Stanford University, as a project for his
undergraduate computer music class. His original program has been
simplified and adapted here to work with General MIDI
instruments. Flister's improviser generates music for a jazz trio
consisting of a piano, acoustic bass and percussion. Our first step
will be to specify the appropriate MIDI program changes to establish
the Jazz Combo instruments on channels 1,2 and 10 of the synthesizer,
or 0, 1 and 9 in zero-based counting.

Channel  0: Acoustic Piano
Channel  1: Acoustic Bass (plucked)
Channel  9: Percussion
|#

(new seq :name 'combo
     :subobjects
     (list (new midi-program-change :time 0 
                :program +acoustic-grand-piano+
                :channel 0)
           (new midi-program-change :time 0 
                :program +acoustic-bass+
                :channel 1)))

#|
Note that we did not have to specify a program change for the
percussion part. This is because MIDI defines channel 9 to be the
"drum track", a specialized channel that maps key numbers to
individual drum sounds. Here are key numbers the percussion part will
use:

Closed hi hat:       42
Electric Snare:     40
Acoustic Bass Drum: 35
Ride Cymbal 1:      51
Ride Cymbal 2:      59

The automatic jazz program uses a conductor process. The conductor
runs for a specified number of measures and sprouts piano, percussion
and bass processes to improvise each measure. Each instrument process
uses data passed to it from the conductor process. This data includes
the jazz scale to improvise with, a transposition level for the jazz
scale, a tempo factor and an overall amplitude level. The amplitude
level is adjusted on a per measure basis by the main conductor
process. The other data are defined as global variables that can be
adjusted and redefined by the composer.
|#

(defparameter jazz-scale ; dorian with decorated octave
  '(0 2 3 5 7 9 10 12 14)) 

(defparameter jazz-changes ; key changes
  '(bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf))

(defparameter jazz-tempo 120)

#|
The Percussion Parts

The percussion parts for the Jazz Combo consist of two ride cymbals, a
high hat, snare and bass drums. We will introduce these parts in their
order of complexity, from simplest to most difficult.

The High Hat

The High Hat percussion process is very simple, it just plays the High
Hat on the second and fourth quarter Q of every measure and rests
R on the first and third beats. Each sound lasts for the duration
one triplet eighth note T8.
|#

(defun jazz-high-hat (tmpo ampl)
  ;; generate a 4/4 measure of high-hat
  (let ((rhy (rhythm 'q tmpo))
        (dur (rhythm 't8 tmpo))
        (amp (amplitude 'mp))
        (pat (new cycle 
               :of (list 'r +closed-hi-hat+
                         'r +closed-hi-hat+ ))))
    (process repeat 4
             output
             (new midi :time (now) 
                  :keynum (next pat)
                  :channel 9 :duration dur
                  :amplitude (* amp ampl))
             wait rhy)))

#|
Listen to eight measures of the High Hat. Since the process generates
only one measure, we collect eight "versions" of the process and
offset each by two seconds, exactly the duration of the combo's 4/4
measure at tempo 120.
|#

(events (loop repeat 8 collect (jazz-high-hat 120 1))
        "jazz.mid" '(0 2 4 6 8 10 12 14) :versioning true)

#|
Jazz Drums 

The jazz-drums process randomly selects between playing the snare, the
bass drum or resting one quarter of the time. One tenth of the time
the process produces a very loud tone.
|#

(defun jazz-drums (tmpo ampl)
  (let ((knums (new weighting 
                 :of `((r :weight .25)
                       , +electric-snare+
                       , +acoustic-bass-drum+)))
        (rhys (new cycle :of '(t4 t8)))
        (amps (new weighting :of '(f (ffff :weight .1)))))
    (process repeat 8
             for k = (next knums)
             for a = (amplitude (next amps))
             for r = (rhythm (next rhys) tmpo)
             output
             (new midi :time (now)
                  :keynum k :channel 9 
                  :duration r 
                  :amplitude (* a ampl))
             wait r)))

#|
Now listen to eight measures of the drum and hi hat together.
|#

(events (loop repeat 8
          collect (list (jazz-high-hat 120 .99)
                        (jazz-drums 120 .99)))                    
        "jazz.mid"
        '(0 2 4 6 8 10 12 14))

#|
Cymbals

The cymbals process performs a constant stream of triplet eighths in
which the ride1 cymbal is played on the beginning or every quarter
note. The second and third triplets of each beat are either rests or a
random choice between ride1, ride2 or a rest.  Here is the beat map
for a measure of the process, where 1 means the ride1 cymbal is
played, - means a rest and x means a random choice between ride1,
ride2 or a rest:

Triplet 8th: 1  2  3    4  5  6    7  8  9   10 11 12
Cymbals:     1  -  x    1  -  1    1  x  x    1  x  1 

The random elements marked x are created by this helper function.
|#

(defun or12r (wt)
  ;; wt is weight of resting relative to playing
  ;; return weighting pattern that slightly prefers playing a ride 1
  ;; pattern over a ride 2 pattern
  (new weighting
    :of (list
         `(,(new weighting
                 ;; play ride cymbal 1 or rest
                 :of `(, +ride-cymbal-1+ (r :weight ,wt) )
                 :for 1)
            :weight 1.5)
         (new weighting 
           ;; play ride cymbal 2 or rest
           :of `(,+ride-cymbal-2+ (r :weight ,wt) )
           :for 1))
    :for 1))

(defun jazz-cymbals (tmpo ampl)
  (let* ((rhy (rhythm 't8 tmpo))
         (amps (new cycle
                 :of '(mf mp fff f mp ffff mf
                       mp fff f mp ffff)))
         (knums
          (new cycle
            :of
            ;; triplet patterns
            (list +ride-cymbal-1+ 'r (or12r 5)
                  +ride-cymbal-1+ 'r  +ride-cymbal-1+ 
                  +ride-cymbal-1+ (or12r 7) (or12r 7)
                  +ride-cymbal-1+ (or12r 3)  +ride-cymbal-1+))))
    (process repeat 12
             for k = (next knums)
             for a = (amplitude (next amps))
             output
             (new midi :time (now) 
                  :keynum k
                  :channel 9 
                  :duration rhy
                  :amplitude (* a ampl ))
             wait rhy)))

#|
Now listen to all three of the percussion parts together.
|#

(events (loop repeat 8 
          collect
          (list (jazz-high-hat 120 1)
                (jazz-drums 120 1)
                (jazz-cymbals 120 1)))
        "jazz.mid"
        '(0 2 4 6 8 10 12 14))

#|
Jazz Piano

The jazz piano improvises jazz chords based on a pattern of root
changes and a scale pattern that is transposed to each root. The piano
randomly choose between playing triplet eighths or straight eights for
a given measure.
|#

(defun jazz-piano (scale on tmpo ampl)
  ;; generate a measure of jazz harmony.
  ;; measure contains either 8 or 12 notes.
  (let* ((reps (odds .65 8 12))
         (rhys (if (= reps 8)
                 (new cycle :of '(t4 t8))
                 (new cycle :of 't8)))
         (amps (if (= reps 8)
                 (new weighting 
                   :of (list (new cycle :of '(mp f)) 
                             (new cycle :of '(mf ff))))
                 (new weighting
                   :of (list (new cycle :of '(mp p f))
                             (new cycle :of '(mf mp ff))))))
         (knms (new weighting
                 :of `((,(new heap :of scale
                              :for (new weighting 
                                     :of '(1 2 3 4))) 
                        :weight ,(new weighting 
                                   :of '(1.15 1.65)))
                       r))))
    (process repeat reps
             for r = (rhythm (next rhys) tmpo)
             for a = (amplitude (next amps))
             for l = (transpose (next knms true) on)
             each k in l
             output (new midi :time (now)
                         :keynum k
                         :duration r
                         :amplitude (* a ampl)
                         :channel 0)
             wait r)))

#|
Each measure of the jazz-piano part will contain either 8 or 12 notes
determined by the odds function, which in this process chooses 8 notes
per measure approximately 65% of the time, otherwise 12 notes. The
rhys variable is set to a pattern of rhythms that depends on the value
of reps. If the piano plays 8 notes in the measure then the rhythmic
pattern for the process will consists of triplet quarter (tq) followed
by a triplet 8ths (te), otherwise the piano will play twelve triplet
8ths. The harmonies are generated by a random pattern that selects
between a rest and a heap of notes created from the scale that was
passed into process. Probability of choosing a note is either 1.16 or
1.65 relative to the rest and each time the heap is selected it will
generate one to four notes.
|#

(events (loop repeat 4
              collect (jazz-piano jazz-scale 'bf3 120 1))
        "jazz.mid"
        '(0 2 4 6))

#|
Acoustic Bass

The acoustic bass part is the most complex in terms of its
implementation. The bass part plays a melodic line built out of tones
from the jazz-scale's tonic seventh chord alternating with color tones
outside the tonic chord. The process first divides the jazz scale's (0
2 3 5 7 9 10 11 12 14) into two sets. The tonic set contains the tonic
seventh pitches 0 2, 4, 6 and 7 and the color set contains the
decoration pitches 1, 3, 5, 7 and 9.  The bass plays a series of 12
triplets per measure, on each triplet only one of the two sets is
possible. On all but the first triplet a rest is also possible.
|#

(defun getset (scale ints)
  ;; return the notes in scale at the positions in ints.
  ;; used to partition scale into tonic and decoration
  (loop for i in ints collect (elt scale i)))

(defun rancyc (data prob)
  ;; create an element for a weighting patter, elements datum
  ;; is a cyclic pattern. element has :weight prob
  (list (new cycle :of data) :weight prob))

(defun jazz-bass (scale on tmpo ampl)
  (let ((rhy (rhythm 'te tmpo))
        (tonics (new weighting 
                  :of (getset scale '(0 2 4 6 7))))
        (colors (new weighting 
                  :of (getset scale '(1 3 5 6 8))))
        (amps (new cycle 
                :of '(mp p ffff fff p fff
                      mp p ffff fff mp fff)))
        (durs (new cycle :of '(tq t8 t8)))
        ;; beat map. t is tonic, c is color, r is rest
        (bmap 
         (new cycle 
           :of
           (list
            ;; 5 possible patterns for triplets 1-4
            (new weighting :for 1
                 :of (list (rancyc '(t r r c) 1.0)
                           (rancyc '(t r r r) .25)
                           (rancyc '(t r t c) .22)
                           (rancyc '(t c t c) .065)
                           (rancyc '(t c t r) .014)))
            ;; 5 possible patterns for 5-7
            (new weighting :for 1
                 :of (list (rancyc '(r r t) 1.0)
                           (rancyc '(r r r) .25)
                           (rancyc '(r c t) .22)
                           (rancyc '(t c t) .038)
                           (rancyc '(t c r) .007)))
            ;; 5 possible patterns for 8-10
            (new weighting :for 1
                 :of (list (rancyc '(r r c) 1.0)
                           (rancyc '(r t c) .415)
                           (rancyc '(r r r) .25)
                           (rancyc '(c t c) .11)
                           (rancyc '(c t r) .018)))
            ;; two possible values for 11
            (new weighting :for 1 
                 :of '((r :weight 1)
                       (t :weight .25)))
            ;; two possible values for 12
            (new weighting :for 1
                 :of '((r :weight 1)
                       (c :weight .25)))))))
    (process repeat 12
             for x = (next bmap)
             for k = (if (equal x 't)
                       (next tonics)
                       (if (equal x 'c)
                         (next colors)
                         x))
             for d = (rhythm (next durs) tmpo)
             for a = (amplitude (next amps))
             output
             (new midi :time (now) 
                  :keynum (transpose k on)
                  :channel 1
                  :duration d
                  :amplitude (* a ampl))
            wait rhy)))

(events (loop repeat 8
              collect (jazz-bass jazz-scale 'bf2 120 1))
        "jazz.mid" 
        '(0 2 4 6 8 10 12 14))


#|
Conductor process

The jazz-combo function is the conductor process. It does not make
sound itself but rather sprouts other processes to make sound, each
sprouted process generates one measure of a particular part.
|#
(defun jazz-combo (measures changes tempo scale)
  (let ((roots (new cycle :of changes))
        (ampl 1))
    (process for meas below measures
             for root = (next roots)
             if (= 0 (mod meas 12)) 
             set ampl = (between .5 1)
             sprout (jazz-piano scale root tempo ampl)
             sprout (jazz-cymbals tempo ampl)
             sprout (jazz-high-hat tempo ampl)
             sprout (jazz-drums tempo ampl)
             sprout (jazz-bass scale (transpose root -12)
                               tempo ampl)
             wait (rhythm 'w tempo))))
#|

(events (list #&combo
              (jazz-combo 48 jazz-changes
                          jazz-tempo jazz-scale))
        "jazz.mid")

(events (list #&combo
              (jazz-combo 48 jazz-changes
                          jazz-tempo jazz-scale))
        "jazz.mid")
|#

; Currently orc-text doesn't do percussion; jazz-combo is for General MIDI.
(let ((csound-seq (new seq :name "csound-seq")))
(events (jazz-combo 48 jazz-changes jazz-tempo jazz-scale)
        "jazz.mid"))



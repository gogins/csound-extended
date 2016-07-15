(in-package :cm)

#|

This tutorial explains how frequency information is represented in CM
and how to work with tunings and modes.  A tuning is a partitioning,
or quantizing, of Hertz frequency space into a series of discrete --
but not necessarily equal -- steps called scale degrees. Every scale
degrees has an ordinal position in the tuning called a keynum (key
number) and may also have one or more note names associated with them.

A mode is a transposable subset of a tuning.

The Standard Chromatic Scale

The system defines the standard equal tempered chromatic scale to be a
tuning based on a low C with 12 equal divisions per octave and with 10
octaves of note notes. The default tuning for the system is kept in
the variable *scale* and is initially set the standard chromatic
scale.
|#

*scale*

#|
Notes, Key numbers and Hertz

Composers commonly work with frequency information in several
different formats:

  Note names, symbolic spellings like Sa, Barang, C sharp, or D double flat 
  Key numbers, or scale degrees, like 0, 60 (Middle C) and so on.
    (The maximum key number for MIDI synths is 127.)
  Hertz values like 440 (Tuning A) or 261.6255 (Middle C)

Common Music provides 3 functions -- note, keynum and hertz -- that
map between these three different frequency formats.
|#

(note 60)

(note '(60 440) :hz true)

(keynum '(as4 bf4))

(keynum 1234.0 :hz true)

(hertz 'c4)

(hertz 69)

#|
Common Music extends the notion of ordinal position (integer key
numbers) to include floating point key numbers kkk.ccc where kkk is
the integer keynum and ccc is interpreted as cents above kkk. So the
value 69.5 means 50 cents above the keynum 69 in the standard
chromatic scale.
|#

(hertz 69)

(hertz 69.5)

(hertz 70)

#|
The cent is a common tuning measurement and is defined to be 1/1200th
of an octave. Since are 1200 cents per octave each semi-tone in the
equal tempered scale contains 100 cents. The cent is an arbitrary unit
but a single cent difference is not perceivable by humans so it is
more adequate for measuring tuning. Humans have a hard time
distinguishing between tones that are less than 5 cents apart.
|#

(cents->scaler 1200)

(scaler->cents (expt 2 1/12))

(* 440 (cents->scaler 1))

#|
Since numbers can be interpreted as either key numbers or hertz, Hertz
values passed to note or keynum must be indicated using the :hz
keyword to distinguish them from the (more common) case of key
numbers.

Note names in the standard chromatic scale are symbols with three
constituent parts:

  A note class name like  C D E F G A B
  An (optional) accidental like N S F SS FF (Natural, Sharp, Flat,
    Double-sharp, Double-flat)
  An octave number -1 to 9, where octave 4 is the Middle C octave
|#

(keynum 'c4)

(keynum 'cn4)

(hertz 'fs2)

(hertz 'gff0)

(keynum 'as6)

#|
Note that a symbol like C is not really not a note, its a pitch class
-- the name of a note without regard to octave. However, the functions
note keynum and hertz all accept LISTS of data to be passed in and
when note lists are specified, octave numbers only have to be provided
when they change value. The default octave is 4, or the Middle C
octave.
|#

(note '(c4 d e f g a b c5 d e fs g a bf c6))

(keynum '(c4 d e f g a b c5 d e fs g a bf c6))

(hertz '(c4 d e f g a b c5 d e fs g a bf c6))


#|
Tunings

You can create your own "custom" tuning systems quite easily. For
example, here is the definition of an equal tempered, quarter-tone
scale:
|#

(new tuning :cents (loop repeat 24 collect 50))

#|
This next example creates a tuning with 13 equal tempered steps per
octave:
|#

(new tuning :name 'bad-luck-scale
     :ratios (loop for i to 13 collect (expt 2 (/ i 13))))

#|
Alternate Tunings and MIDI

Since MIDI was developed by the popular music industry it should not
be surprising to learn that it has a very strong bias towards the
standard equal-tempered chromatic scale. There are basically only two
ways to introduce micro-tonal inflections: re-tune the synth, or use
pitch bends to alter the frequency of a sounding tone. In Common
Music, all MIDI streams have a :channel-tuning init that, if set,
causes pitch bends to be automatically sent when floating point key
numbers are generated. (The name :channel-tuning refers to the fact
that pitch bends in MIDI are channel specific).  If the value of
:channel-tuning is a number it represents the number of micro-tonal
divisions per half step: 1 is semi-tone, 2 is quarter-tone and so
on. See the documentation on Midi streams for more information.

Let's listen to some micro-tonal output by defining and then using a
tuning system based on the (untempered) harmonic series. Our example
uses a 6 tone scale, you can experiment with other segments of the
series to hear what they sound like.
|#

(defparameter harms
  ;; an 6-tone scale whose steps are partials 6-12 in the harmonic
  ;; series
  (new tuning 
    :ratios (loop with lo = 6
                  for h from 6 to 12 collect (/ h lo))))

#|
In order to listen to this scale using MIDI we must convert integer
key numbers in the harms tuning back into floating-point keynums in
the standard chromatic scale, always remembering that MIDI's maximum
keynum value is 127. Lets look at the Middle-c octave to see what
these converted values look like:
|#

(loop for i from 30 to 36 collect (keynum i :from harms))

(keynum 62 :from harms) ; this is the highest key 

#|
Now lets create a function that plays an upward or downward segment of
a scale.
|#

(defun stepper (k1 k2 scale rate dur amp1 amp2)
  (process with i = (if (> k2 k1) 1 -1) 
           and k = k1
           output
           (new midi :time (now)
                :keynum (keynum k :from scale)
                :duration dur
                :amplitude (rescale k k1 k2 amp1 amp2))
           wait rate
           do (incf k i)
           until (= k k2)))

#|
Remember, when you use MIDI and you want to listen to tones that are
not in the standard chromatic scale, you need to set the streams
:channel-tuning init to some appropriate value or you won't hear any
difference.
|#

(events (stepper 20 40 harms .1 .35 .2 .7)
        "scales.sco"
        :channel-tuning 3 :versioning true)

(events (stepper 50 40 harms .1 .35 .2 .7)
        "scales.sco"
        :channel-tuning 3)

#|
We can make a (slightly) more interesting effect by defining another
process that computes random segments to play and then sprouts, or
inserts, these processes inside the already running scheduler.
|#

(defun whirl (end rate dur k1 low high scale )
  (process while (< (now) end)
    for k2 = (between low high)
    unless (= k1 k2)
    sprout (stepper k1 k2 scale rate dur
                    (between .2 .8)
                    (between .2 .8))
    wait (* rate (abs (- k1 k2)))
    set k1 = k2))

(events (whirl 10 .1 .5 20 10 50 harms)
        "scales.sco"
        :channel-tuning 3)

#|
To cancel any current tuning adjustments in your synth you should send
pitch bend values to reset the channel tuning levels back to zero:
|#

(defun clearbends ()
  (process for i below 16
           output (new midi-pitch-bend :time (now)
                       :channel i :bend 0)))



(events (clearbends) "clear.mid" :play true)

#|
To cancel microtuning in an existing file specify false as the
:channel-tuning value:
|#

(io "scales.sco" :channel-tuning false)

#|
TRY:

1. The function ran returns numbers from a specified distribution
   type.  What would be the effect if k2 were set using ran instead of
   between?
     (ran :type :uniform :from 10 :below 20)
     (ran :type :low :from 10 :below 20)
     (ran :type :high :from 10 :below 20)
     (round (ran :type :beta :a .3 :b .3 :from 10 :below 20))
2. Stepper could be made more musically interesting if, instead of
   always incrementing by 1, it "stuttered" once in awhile, by leaping
   back one or two tones before continuing on.
3. Another effect worth trying: allow stepper to select any tone on
   either side the actual tone within some window width.

Working with modes

A mode defines a subset of a tuning, where adjacent key numbers in the
mode map to non-adjacent key numbers in the mode's tuning. Lets see
how this works by first defining a mode.
|#

(defparameter maj (new mode :name 'major :steps '(2 2 1 2 2 2 1)))

;mkg #&major

#|
The :steps list defines the distance (in half steps) between adjacent
key numbers for one octave of the mode. Since the mode did not specify
a tuning it defaults to the standard chromatic scale. The step values
of the major scale add up to a 12 semi-tone octave, but modes other
than mod 12 are also possible.

Note that the standard chromatic scale as 12 notes per octave while
the major scale is mod 7. The keynum function lets you convert between
the different "coordinate systems" of the tuning and mode.
|#

(keynum 'c4 :to maj)

(keynum 35 :from maj)

#|
The keynum function can also determine if a given key number is in the
mode or to filter a key number to through a mode to force the closest
mode member.
|#

(keynum 61 :in? maj)

(keynum 61 :through maj)

#|
Modes are transposable subsets of a tuning. To shift a modes to a new
tonic using the transpose function
|#

(note (keynum 35 :from maj))

(transpose maj 'd)

(note (keynum 35 :from maj))

(transpose maj 'af)

(note (keynum 35 :from maj))

#|
Lets define a process that plays random triads from the major mode.
|#

(defun ranchords (len pos mode amp)
  (let ((rhy (new heap :of '(.5 .25 .124))))
    (process for i below  len 
             for r = (next rhy)
             for j = (keynum pos :to mode)
             then (drunk j 2 :low 0 :high 56)
             each k in (keynum (list j (+ j 2) (+ j 4))
                               :from mode)
             output (new midi :time (now) :keynum k 
                         :amplitude amp)
             wait r)))
                         
(events (ranchords 30 'c4 maj .3) "scales.mid")
          
#|
TRY:

1. Define a minor mode and pass it to ranchord.
2. Use whirl to play modes.
3. Change whirl to accept a list of pitch classes and then randomly
   change keys every once in a while.

Here are some basic mod 12 modes to try out (Drew Krause).
|#

(defparameter pentatonic (new mode :steps '(2 2 3 2 3))) ; 5 members
(defparameter pelog (new mode :steps '(1 2 4 1 4))) ; 5 members
(defparameter wholetone (new mode :steps '(2 2 2 2 2 2))) ; 6 members
(defparameter mlt5 (new mode :steps '(1 4 1 1 4 1))) ; 6 members
(defparameter symmetric6 (new mode :steps '(1 3 1 3 1 3))) ; 6 members
(defparameter ionian (new mode :steps '(2 2 1 2 2 2 1))) ; 7 members
(defparameter octatonic (new mode :steps '(1 2 1 2 1 2 1 2))) ; 8 members
(defparameter mlt4 (new mode :steps '(1 1 3 1 1 1 3 1))) ; 8 members
(defparameter mlt6 (new mode :steps '(2 2 1 1 2 2 1 1))) ; 8 members
(defparameter mlt3 (new mode :steps '(2 1 1 2 1 1 2 1 1))) ; 9 members
(defparameter mlt7 (new mode :steps '(1 1 1 2 1 1 1 1 2 1))) ; 10 members

#|
Some cooler (not mod 12) modes, also by Drew:
|#

(defparameter stravmode (new mode :steps '(2 1 2)))
(defparameter hyperlydian (new mode :steps '(2 2 2 1)))
(defparameter hyperphrygian (new mode :steps '(1 2 2)))




; S L I P P E R Y   C H I C K E N   C F F I   I N T E R F A C E   T O   
; C S O U N D
;
; Copyright (C) 2016 Michael Gogins
;
; This file belongs to Csound.
;
; This software is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.
;
; This software is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public
; License along with this software; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
; This file is handwritten and should be maintained by keeping it up to date
; with regard to include/csound.h. This file is not intended to be complete
; and essentially defines a Steel Bank Common Lisp interface to a subset of
; the most useful functions in csound.h. At the present time, only pointers,
; strings, and other primitive types are used in this interface.
(in-package :sc)

;;; ****m* csound-play
;;; DESCRIPTION
;;; Render the specified slippery-chicken using Csound. Note that for events 
;;; that contain chords, the event's amplitude slot
;;; will be used for all pitches unless each individual pitch object in the
;;; chord has its amplitude slot set to a number (in which case the event's
;;; amplitude slot will be ignored.)  Csound output options must be set in the 
;;; csd text.
;;; 
;;; ARGUMENTS
;;; - A slippery-chicken object.
;;; - A "heredoc" string in the format of a Csound Structured Data file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; ; - :voices. NIL or a list of player IDs indicating which of the players'
;;;   parts are to be included in the resulting MIDI file. If NIL, all players'
;;;   parts will be included. Default = NIL.
;;; - :players. A synonym for voices. This is the preferred keyword as it aligns
;;;   better with other classes, but :voices is retained for older code
;;;   compatibility. 
;;; - :start-section. An integer that is the number of the first section for
;;;   which the MIDI file is to be generated. Default = 1.
;;; - :num-sections. An integer that is the number of sections to produce MIDI
;;;   data for in the MIDI file. If NIL, all sections will be written. 
;;;   Default = NIL.
;;; - :from-sequence. An integer that is the number of the sequence within the
;;;   specified section from which to start generating MIDI data. NB: This
;;;   argument can only be used when the num-sections = 1. Default = 1.
;;; - :num-sequences. An integer that is the number of sequences for which MIDI
;;;   data is to be generated in the resulting MIDI file, including the
;;;   sequence specified in from-sequence. If NIL, all sequences will be
;;;   written. NB: This argument can only be used when the num-sections = 1.
;;;   Default = NIL.
;;; - :update-amplitudes. T or NIL to indicate whether events should have their
;;;   amplitude slots updated to ensure that any dynamic marks that have been
;;;   added (and therefore amplitude slots updated as a side-effect) reflect
;;;   the subsequent events' amplitudes also. This will override any amplitude
;;;   slots that have been changed other than by (setf (amplitude ...)) or
;;;   (add-mark ... 'some-dynamic). Also, if T, then hairpins (cresc, dim) will
;;;   result in increasing/decreasing amplitudes over their extent (but this
;;;   will only work if the amplitude/dynamic of the hairpins' starting and
;;;   ending events are correct, and in that case intervening events'
;;;   amplitudes will be overwritten no matter how they've been set; also this
;;;   won't handle cases where there's a hairpin end and a new hairpin
;;;   beginning on the same event ). Default  = T
;;; - :force-velocity. Either: an integer between 0 and 127 (inclusive) that is
;;;   the MIDI velocity value which will be given to all notes in the resulting
;;;   MIDI file, or a function which takes an event object argument and
;;;   returns a velocity from it (e.g. randomising the existing amplitude
;;;   slightly). Default = NIL.
;;; 
;;; RETURN VALUE
;;; Returns the Common Music sequence object containing the music.
;;; 
;;; EXAMPLE
#|
;;; An example with some typical values for the keyword arguments. ;
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                     (hn (french-horn :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
        :set-map '((1 (1 1 1 1 1 1 1))
                   (2 (1 1 1 1 1 1 1))
                   (3 (1 1 1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((4 4) h (q) e (s) s))
                                :pitch-seq-palette ((1 2 3))))
                            (2 ((((4 4) (q) e (s) s h))
                                :pitch-seq-palette ((1 2 3))))
                            (3 ((((4 4) e (s) s h (q)))
                                :pitch-seq-palette ((2 3 3))))
                            (4 ((((4 4) (s) s h (q) e))
                                :pitch-seq-palette ((3 1 2)))))
        :rthm-seq-map '((1 ((cl (1 2 1 2 1 2 1))
                            (hn (1 2 1 2 1 2 1))
                            (vc (1 2 1 2 1 2 1))))
                        (2 ((cl (3 4 3 4 3 4 3))
                            (hn (3 4 3 4 3 4 3))
                            (vc (3 4 3 4 3 4 3))))
                        (3 ((cl (1 2 1 2 1 2 1))
                            (hn (1 2 1 2 1 2 1))
                            (vc (1 2 1 2 1 2 1))))))))
  (midi-play mini 
             :midi-file "/tmp/md-test.mid"
             :voices '(cl vc)
             :start-section 2))

;;; An example that passes a (lambda) function to :force-velocity. Usually, by
;;; default, event amplitudes between 0.0 and 1.0 will map onto MIDI velocities
;;; of 0 to 127. Here we map them to velocities of 0 to 100 instead.
(midi-play +jitterbug+ :force-velocity
           #'(lambda (event)
               (floor (* (amplitude event) 100))))

|#
;;; SYNOPSIS
#+cm-2
(defmethod csound-play ((sc slippery-chicken) csd
                      &key 
                        ;; no subsection refs: use from-sequence instead
                        (start-section 1) 
                        ;; these voices are used to get the actual sequence
                        ;; orders i.e. each voice will be appended to <section>
                        ;; when calling get-data.
                        ;; if nil then all voices.
                        (voices nil)
                        ;; MDE Thu Oct 18 17:07:57 2018 -- players is a more
                        ;; appropriate keyword than voices but keep the latter
                        ;; for historical code and make the two equivalent
                        (players nil)
                        (csound-seq (cm::new cm::seq :name "csound-seq"))
                        (from-sequence 1)
                        (num-sequences nil)
                        ;; if nil we'll write all the sections
                        (num-sections nil)
                        ;; MDE Mon Jun 13 12:30:55 2016
                        (update-amplitudes t)
                        ;; MDE Tue Jun  4 19:06:11 2013
                        (auto-open (get-sc-config 'midi-play-auto-open))
                        ;; if this is a 7-bit number we'll use this for all
                        ;; notes  
                        (force-velocity nil))
;;; ****
  (when update-amplitudes ; MDE Mon Jun 13 12:32:30 2016 
    (update-amplitudes sc)
    (handle-hairpins sc))
  (when (and players voices)
    (error "slippery-chicken::midi-play: please use either voices or players, ~
            not both."))
  (when players (setq voices players))
  (setf voices
        (cond ((and voices (listp voices)) voices)
              ((and voices (atom voices)) (list voices))
              ((not voices) (get-players (ensemble sc)))
              (t (error "slippery-chicken::midi-play: voices = ~a!?" voices))))
  ;; MDE Fri May 11 15:13:18 2012 -- if there's only one section....
  (when (and (not num-sections)
             (= start-section 1)
             (= 1 (get-num-sections sc)))
    (setf num-sections 1))
  ;; MDE Fri May 11 13:02:06 2012 -- 
  ;; MDE Mon May 14 18:46:01 BST 2012 -- we no longer have this keyword
  #|
  (when (> time-scaler 1.0)
  (error "slippery-chicken::midi-play: scaling durations by more than 1.0 ~
             would ~%interfere with MIDI note-on/note-off combinations."))
  |#
  (unless (integer>0 from-sequence)
    (error "slippery-chicken::midi-play: ~
            from-sequence must be an integer >= 1."))
  ;; MDE Fri May 11 11:42:48 2012 -- 
  (when (and num-sequences 
             (or (not num-sections)     ; MDE Fri May 11 11:56:13 2012 -- 
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::midi-play: num-sequences keyword should only ~
            be used ~%when num-sections = 1."))
  ;; MDE Fri May 11 11:58:17 2012
  (when (and from-sequence (/= 1 from-sequence)
             (or (not num-sections)
                 (and num-sections (> num-sections 1))))
    (error "slippery-chicken::midi-play: from-sequence keyword should only ~
            be used ~%when num-sections = 1."))
  (when (and num-sections (= 1 num-sections) (not num-sequences))
    (let ((ns (num-seqs sc start-section)))
      (unless ns 
        (error "slippery-chicken::midi-play: can't get number of sequences ~
                for section ~a." start-section))
      (setf num-sequences (- ns (1- from-sequence)))))
  (let* ((voices-events (get-events-start-time-duration 
                         sc start-section voices 
                         :time-scaler 1.0
                         :from-sequence from-sequence
                         :num-sequences num-sequences
                         :num-sections num-sections
                         :get-time-sig-changes t
                         :ignore-rests nil
                         :include-rests t))
         ;; MDE Mon May  7 10:41:07 2012 -- for pieces with subsections
         (secobj (get-section sc start-section))
         (nth-seq-ref (full-ref (get-first-section secobj)))
         ;; MDE Thu Oct 17 19:10:27 2013 -- the following doesn't work if there
         ;; are sub-sub-sections!  
          #|
  (if (has-subsections secobj)
         (full-ref (print (data (first (data secobj)))))
         start-section))
         |#
         ;; do all the program changes for the beginning irrespective of
         ;; whether the player changes instrument or not. subsequent program
         ;; changes are handled in the event class.
         (midi-setup 
          (loop 
             for voice in voices
             for player = (get-player sc voice)
             for current-ins = (id (get-current-instrument-for-player
                                    start-section voice from-sequence sc))
             for ins = (get-data current-ins (instrument-palette sc))
             collect
             (list (midi-channel player) (midi-program ins))
             when (microtonal-chords-p player)
             collect
             (list (microtones-midi-channel player)
                   (midi-program ins)))))
    (cm::process-voices voices-events csound-seq (get-tempo sc 1) midi-setup
                        (- (start-time-qtrs
                            (get-nth-sequenz (piece sc) nth-seq-ref
                                             (first voices) 
                                             (1- from-sequence))))
                        force-velocity)
    (cm::render-with-csd csound-seq csd)
    csound-seq))




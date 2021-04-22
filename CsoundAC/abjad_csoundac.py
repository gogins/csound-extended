'''
Author: Michael Gogins.

This file demonstrates how to translate an abjad score to a CsoundAC Score.
'''
import abjad
import CsoundAC

pitches = [[3,11,2,1],[7,1,0,3]]
score = abjad.demo.ligeti.make_desordre_score(pitches)
lilypond = abjad.demo.ligeti.make_desordre_lilypond_file(score)
lilypond.show()
'''
Author: Michael Gogins.

This file demonstrates how to translate a music21 Score to a CsoundAC Score.
'''
from music21 import *
import CsoundAC

agnusI = corpus.parse('palestrina/agnus_I_01', forceSource=True)
print(agnusI)
agnusI.show('lily.pdf')
agnusI.show('text')
'''
Author: Michael Gogins.

This file implements the translation of music21 Scores to CsoundAC Scores.
'''
from music21 import *
import CsoundAC

if __name__ == '__main__':
    
    # Unit test.

    agnusI = corpus.parse('palestrina/agnus_I_01', forceSource=True)
    agnusI.show('lily.pdf')
    agnusI.show('text')
    # Flattens the score and filters out all but notes.
    notes = agnusI.flat.notes
    print(notes)
    print(help(note.Note))
    # By default, times and durations are in quarter notes.
    for note_ in notes:
        temp = vars(note_)
        for item in temp:
            print(item, ':', temp[item])        
        for site_ in note_.sites:
            print(site_)
        print(note_.pitch, note_.offset, note_.duration.quarterLength)

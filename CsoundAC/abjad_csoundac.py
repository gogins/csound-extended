'''
Author: Michael Gogins.

This file demonstrates how to translate an abjad Score to a CsoundAC Score.

There's a lot to like about abjad, but it mostly seems like the pet of a few 
programmer/composers or composer/programmers, and to defer to Lilypond for all 
the grunt work.
'''
import abjad
import CsoundAC

rh_voice = abjad.Voice(name="RH_Voice")
rh_staff = abjad.Staff([rh_voice], name="RH_Staff")
lh_voice_1 = abjad.Voice(name="LH_Voice_1")
lh_voice_2 = abjad.Voice(name="LH_Voice_2")
lh_staff = abjad.Staff(name="LH_Staff", simultaneous=True)
lh_staff.extend([lh_voice_1, lh_voice_2])
piano_staff = abjad.StaffGroup(lilypond_type="PianoStaff", name="Piano_Staff")
piano_staff.extend([rh_staff, lh_staff])
score = abjad.Score([piano_staff], name="Score")

rh = r"""
    a'8 \pp [ ( g' f' e' ] d'4 )
    g'8 \mp [ ( f' e' d' ] c'8 \< d'16 e' f'8 e' \! d'2 \> ~ d' \! )
"""
lh_1 = r"s1 * 2/4 s1 * 3/4 s1 * 2/4 \voiceOne b2 ~ b"
lh_2 = r"""
    b4 d'8 \pp [ ( c' b a ] af4 )
    c'8 \mp [ ( bf a g ] fs g16 a16 \voiceTwo b4 a g2 )
"""

rh_voice.extend(rh)
mark = abjad.MetronomeMark((1, 4), 60)
abjad.attach(mark, rh_voice[0])

lh_voice_1.extend(lh_1)
lh_voice_2.extend(lh_2)

time_signature = abjad.TimeSignature((2, 4))
note = abjad.select(rh_voice).note(0)
abjad.attach(time_signature, note)
time_signature = abjad.TimeSignature((3, 4))
note = abjad.select(rh_voice).note(4)
abjad.attach(time_signature, note)
time_signature = abjad.TimeSignature((2, 4))
note = abjad.select(rh_voice).note(9)
abjad.attach(time_signature, note)

clef = abjad.Clef("bass")
note = abjad.select(lh_voice_2).note(0)
abjad.attach(clef, note)
bar_line = abjad.BarLine("|.")
note = abjad.select(lh_voice_2).note(-1)
abjad.attach(bar_line, note)

note = abjad.select(rh_voice).note(-2)
abjad.override(note).hairpin.to_barline = False

# Doesn't do a damned thing.

abjad.persist.as_midi(score, "fragment")

# Does produce a MIDI file, squirreled away in ~/.abjad/output/{tempfilename}.mid.
# This can be configured with arguments to the Player creator. The Player seems to 
# use Lilypond exclusively for output.

abjad.play(score)

# Not much help, actually.

print(help(score))
print(dir(abjad.score.Note))
print(dir(abjad.score.Leaf))
print(help(abjad.io.Player))

# Now we're getting somewhere. Timeline seems to flatten the containers.
# Times and durations are rational numbers relative to the beat implied by 
# the time signature. Pitches are Lilypond pitches, i.e. c is c3, c' is c5, 
# c, is c2. Changes of tempo would warp this, not sure how that works for 
# MIDI, maybe that's why MIDI dropped out of abjad. MIDI remains in Lilypond, 
# but there is no hook into the MIDI file creation from Python.

for leaf in abjad.iterate(score).timeline():
    print(type(leaf), leaf, leaf._start_offset, leaf._get_duration(), leaf._multiplier)
    
abjad.show(score)

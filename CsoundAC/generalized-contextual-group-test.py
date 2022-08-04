
'''
Tests for the K transformation of the Generalized Contextual Group, see 
https://math.ucr.edu/home/baez/week234.html.
'''
import CsoundAC

print(__doc__)

chord = CsoundAC.chordForName("C7")
#print(chord.information())
chord = chord.K()
#print(chord.information())

'''
For instance, let X = Y = ⟨2,11,4,9⟩ = ⟨D,B,E,A⟩. Then K(Y ) = I2+11(Y )
= ⟨I1(D), I1(B), I1(E), I1(A)⟩
= ⟨B,D,A,E⟩,
because the inversion which swaps D and B is I1 = I2+11. Notice that ⟨B, D, A, E⟩ is that inverted form of ⟨D, B, E, A⟩ which has the first two entries swapped.
'''

def I(chord, center):
    result = chord.clone()
    for voice in range(chord.voices()):
        pitch = chord.getPitch(voice)
        pitch_inverted = i(pitch, center)
        result.setPitch(voice, pitch_inverted)
    return result

def i(pitch, center):
    inverse = center - pitch
    return inverse
    
def K(chord):
    result = chord.clone()
    pitch0 = chord.getPitch(0)
    pitch1 = chord.getPitch(1)
    center = pitch0 + pitch1
    for voice in range(chord.voices()):
        pitch = chord.getPitch(voice)
        #pitch_inverted = center - pitch;
        pitch_inverted = i(pitch, center)
        result.setPitch(voice, pitch_inverted);
        "Chord::K: %3d center %9.4f p %9.4f p_i %9.4f p_i_pc: %9.4f\n".format(voice, center, pitch, pitch_inverted)
    return result
           
chord = CsoundAC.chordForName("C7")
chord_k = K(chord)
print("chord: {} K => \n       {}\n".format(chord.epcs().toString(),chord_k.epcs().toString()))
chord = CsoundAC.Chord((11,2,9,4))
chord_k = K(chord)
print("chord: {} K => \n       {}\n".format(chord.epcs().toString(),chord_k.epcs().toString()))

chord = CsoundAC.chordForName("C7")
for center in range(12):
    chord_i = I(chord, center)
    print("chord: {} I{} => \n       {}".format(chord.epcs().toString(), center, chord_i.epcs().toString()))
    print("      ", chord.I(center).epcs().toString())
    
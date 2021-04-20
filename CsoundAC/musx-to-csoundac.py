###############################################################################
"""
Ring modulation generates pairwise sum and difference tones of two input 
spectra. This little etude utilizes both the input and output spectra 
simultaneously to create a funky two part composition.

To run this script cd to the parent directory of musx_demos/ and do:
```bash
python3 -m musx_demos.rm
```
"""


import random
from musx.spectral import rmspectrum
from musx.generators import choose, jumble, cycle
from musx.scheduler import Scheduler
from musx.scales import keynum, hertz, scale
from musx.ran import pick
from musx.tools import setmidiplayer, playfile
from musx.rhythm import intempo
from musx.midi import MidiNote, MidiSeq, MidiFile
from musx.midi.gm import AcousticGrandPiano, Xylophone, Flute, FretlessBass, SteelDrums,\
     Clarinet, Marimba, AcousticBass


def accompaniment(q, reps, dur, set1, set2):
    """
    Creates the accompanyment part from the ring modulation input specta.

    Parameters
    ----------
    q : Sheduler
        The scheduling queue
    reps : int
        The number of sections to compose.
    dur : int
        The surface rhythm
    set1 : list
        Ring modulation input 1.
    set2 : list
        Ring modulation input 2.
    """
    # Create a cycle of the two inputs
    pat = cycle([set1, set2])
    for _ in range(reps*2):
        # Get the next set.
        keys = next(pat)
        # Iterate the keys, play as a chord.
        for k in keys:
            # Create a midi note at the current time.
            m = MidiNote(time=q.now, dur=dur, key=k, amp=.3, chan=0)
            # Add it to our output seq.
            q.out.addevent(m)
        # Wait till the next chord.
        yield dur


def melody(q, reps, dur, set3):
    """
    Creates the melodic part out of the ring modulated output specta.
 
    Parameters
    ----------
    q : Sheduler
        The scheduling queue.
    reps : int
        The number of sections to compose.
    dur : int
        The surface rhythm
    keys : list
        List of keynums from ring modulation output.
    """
    # Create a cycle of the two inputs
    pat = cycle(set3)
    for _ in range(2 * reps):
        m = MidiNote(time=q.now, dur=dur/2, key=next(pat), amp=.7, chan=1)
        q.out.addevent(m)
        # Wait till the next note
        yield dur


def rmfunky(q, reps, dur, keys):
    """
    Main composer chooses input spectra , creates a ring modulated
    output spectrum and plays them using two parts.

    Parameters
    ----------
    q : Sheduler
        The scheduling queue
    reps : int
        The number of sections to compose.
    dur : int
        The surface rhythm
    keys : list
        List of keynums for ring modulation input.
    """
    num = choose([1,2,3])
    # scramble the cycle of fourths
    pat = jumble(keys)
    for _ in range(reps):
        # input1 is 1, 2 or 3 notes from cycle of 4ths
        keys1 = [next(pat) for _ in range(next(num))]
        # input2 is same
        keys2 = [next(pat) for _ in range(next(num))]
        # ring modulate the inputs
        spect = rmspectrum([hertz(k) for k in keys1], [hertz(k) for k in keys2])
        # convert to keynums
        keys3 = spect.keynums(quant=1, unique=True, minkey=21, maxkey=108)
        # sprout composers to play inputs and output
        playn = pick(3,4,5)
        q.compose(accompaniment(q, playn, dur, keys1, keys2))
        q.compose(melody(q, playn, dur, keys3))
        # do it again after composers finish
        yield (dur * playn * 2)


if __name__ == '__main__':
    # It's good practice to add any metadata such as tempo, midi instrument
    # assignments, micro tuning, etc. to track 0 in your midi file.
    t0 = MidiSeq.metaseq(ins={0: Marimba, 1: Clarinet})
    # Track 1 will hold the composition.
    t1 = MidiSeq()
    # Create a scheduler and give it t1 as its output object.
    q = Scheduler(t1)
    # Musical material is the cycle of fourths.
    keys = scale(40, 12, 5)
    # Surface rhythm
    rhy = intempo(.25, 74)
    # Start our composer in the scheduler, this creates the composition.
    q.compose(rmfunky(q, 24, rhy, keys))
    # Write a midi file with our track data.
    f = MidiFile("rm.mid", [t0, t1]).write()
    for track in f:
        print(track.print())
        print(dir(track[0]))
        print(track[0].toextern())
        print(track[0].tostring())
    # To automatially play demos use setmidiplayer() to assign a shell
    # command that will play midi files on your computer. Example:
    setmidiplayer("timidity")
    print(f"Wrote '{f.pathname}'.")
    playfile(f.pathname)





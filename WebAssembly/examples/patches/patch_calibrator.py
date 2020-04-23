import ctcsound
import random
import sys

orc = '''
sr = 48000
ksmps = 128
nchnls = 2
nchnls_i = 1
0dbfs = 1

// These must be initialized here to be in scope for both 
// the note and the audio patches.

gi_Fluidsynth fluidEngine 0, 0
gi_FluidSteinway fluidLoad "Steinway_C.sf2", gi_Fluidsynth, 1
fluidProgramSelect gi_Fluidsynth, 0, gi_FluidSteinway, 0, 1

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 6/amd64/Pianoteq\ 6.so", 0
vstinfo gi_Pianoteq 

alwayson "PianoOutFluidsynth"
alwayson "PianoOutPianoteq"

#includestr "$PATCH_FILENAME"

// Comment out if you don't have the Steinway_C.sf2 SoundFont.
#include "PianoOutFluidsynth.inc"

// Comment out if you don't have the Pianoteq and vst4cs.
#include "PianoOutPianoteq.inc"

#include "MasterOutput.inc"

iampdbfs init 1
prints "Default amplitude at 0 dBFS: %9.4f\\n", iampdbfs
idbafs init dbamp(iampdbfs)
prints "dbA at 0 dBFS:               %9.4f\\n", idbafs
iheadroom init 6
prints "Headroom (dB):               %9.4f\\n", iheadroom
idbaheadroom init idbafs - iheadroom
prints "dbA at headroom:             %9.4f\\n", idbaheadroom
iampheadroom init ampdb(idbaheadroom)
prints "Amplitude at headroom:       %9.4f\\n", iampheadroom
prints "Balance so the 'overall amps' at the end of performance is -6 dBFS.\\n"
prints "nchnls:                      %9.4f\\n", nchnls

connect "$PATCH_NAME", "outleft", "MasterOutput", "inleft"
connect "$PATCH_NAME", "outright", "MasterOutput", "inright"
connect "PianoOutFluidsynth", "outleft", "MasterOutput", "inleft"
connect "PianoOutFluidsynth", "outright", "MasterOutput", "inright"
connect "PianoOutPianoteq", "outleft", "MasterOutput", "inleft"
connect "PianoOutPianoteq", "outright", "MasterOutput", "inright"

alwayson "MasterOutput"
'''

def generate_score():
    score = 'f 0 60\n'
    time_= 1.0
    # Generate a chord.
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 36, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 48, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 55, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 59, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 64, 70)
    time_ += 7
    # Generate a random pattern.
    for note_i in xrange(50):
        duration = random.choice([0.125, 0.33334, 0.25, 0.66667, 1])
        time_ += duration / 2;
        key = random.choice(range(24, 108, 1))
        velocity = random.choice([80, 80-6, 80-12, 80-18]) / 3
        score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, duration, key, 60)
    time_ += 2
    # Generate notes to calibrate loudness.
    for duration in [0.125, .25, 2]:
        for key in range(24,108,3):
            time_ = time_ + duration * 1.5
            velocity = random.choice([80, 80-6, 80-12, 80-18])
            score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, duration, key, velocity)
    time_ += 2
    # Test score-driven note-on, note-off.
    score += 'i 1.01 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 36, 70)
    time_ += 1
    score += 'i 1.02 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 48, 70)
    time_ += 1
    score += 'i 1.03 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 55, 70)
    time_ += 1
    score += 'i 1.04 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 59, 70)
    time_ += 1
    score += 'i 1.05 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 64, 70)

    time_ += 5
    score += 'i -1.01 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 1, 36, 70)
    time_ += 1
    score += 'i -1.02 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 1, 48, 70)
    time_ += 1
    score += 'i -1.03 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 1, 55, 70)
    time_ += 1
    score += 'i -1.04 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 1, 59, 70)
    time_ += 1
    score += 'i -1.05 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 1, 64, 70)
    return score

print(sys.argv)
patch_filename = sys.argv[1]
patch_name = sys.argv[2]
output = sys.argv[3]
message_level = 1 + 2 + 32 + 128
csound = ctcsound.Csound()
csound.message("Patch file: {} Patch name: {} Output: {}\n".format(patch_filename, patch_name, output))
csound.setOption("-d")
csound.setOption("--nchnls=2")
csound.setOption(output)
csound.setOption("-m%d" % message_level)
csound.setOption("-+msg_color=0")
csound.setOption("--simple-sorted-score")
csound.setOption("--omacro:PATCH_FILENAME={}".format(patch_filename))
csound.setOption("--omacro:PATCH_NAME={}".format(patch_name))
csound.compileOrc(orc)
csound.readScore(generate_score())
csound.start()
csound.perform()


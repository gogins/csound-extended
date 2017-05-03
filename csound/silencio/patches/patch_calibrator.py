import ctcsound
import random
#output = "test.wav"
output = "dac"

orc = '''
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1

#include "ToneWheelOrgan.inc"
#include "HeavyMetal.inc"
#include "FilteredSines.inc"
#include "ZakianFlute.inc"
#include "Harpsichord.inc"
#include "DelayedPlucked.inc"
#include "FMModerate2.inc"
#include "FM_Clang.inc"
#include "PianoNotePianoteq.inc"
#include "Blower.inc"
#include "Droner.inc"
;#include "PianoNoteFluidsynth.inc"
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

connect "Blower", "outleft", "MasterOutput", "inleft"
connect "Blower", "outright", "MasterOutput", "inright"
connect "Bower", "outleft", "MasterOutput", "inleft"
connect "Bower", "outright", "MasterOutput", "inright"
connect "Buzzer", "outleft", "MasterOutput", "inleft"
connect "Buzzer", "outright", "MasterOutput", "inright"
connect "DelayedPlucked", "outleft", "MasterOutput", "inleft"
connect "DelayedPlucked", "outright", "MasterOutput", "inright"
connect "Droner", "outleft", "MasterOutput", "inleft"
connect "Droner", "outright", "MasterOutput", "inright"
connect "FM_Clang", "outleft", "MasterOutput", "inleft"
connect "FM_Clang", "outright", "MasterOutput", "inright"
connect "FilteredSines", "outleft", "MasterOutput", "inleft"
connect "FilteredSines", "outright", "MasterOutput", "inright"
connect "FMModerate2", "outleft", "MasterOutput", "inleft"
connect "FMModerate2", "outright", "MasterOutput", "inright"
connect "Harpsichord", "outleft", "MasterOutput", "inleft"
connect "Harpsichord", "outright", "MasterOutput", "inright"
connect "HeavyMetal", "outright", "MasterOutput", "inright"
connect "HeavyMetal", "outleft", "MasterOutput", "inleft"
connect "Phaser", "outleft", "MasterOutput", "inleft"
connect "Phaser", "outright", "MasterOutput", "inright"
connect "PianoOut", "outleft", "MasterOutput", "inleft"
connect "PianoOut", "outright", "MasterOutput", "inright"
connect "Shiner", "outleft", "MasterOutput", "inleft"
connect "Shiner", "outright", "MasterOutput", "inright"
connect "Sweeper", "outleft", "MasterOutput", "inleft"
connect "Sweeper", "outright", "MasterOutput", "inright"
connect "ToneWheelOrgan", "outleft", "MasterOutput", "inleft"
connect "ToneWheelOrgan", "outright", "MasterOutput", "inright"
connect "YiString", "outleft", "MasterOutput", "inleft"
connect "YiString", "outright", "MasterOutput", "inright"
connect "ZakianFlute", "outleft", "MasterOutput", "inleft"
connect "ZakianFlute", "outright", "MasterOutput", "inright"

;#include "PianoOutFluidsynth.inc"
#include "PianoOutPianoteq.inc"

alwayson "PianoOut"
alwayson "MasterOutput"

'''

def generate_score():
    score = ''
    time_= 1.0
    for duration in [0.125, .25, 2]:
        for key in xrange(24,108,3):
            time_ = time_ + duration * 1.5
            velocity = random.choice([80, 80-6, 80-12, 80-18])
            score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0.5\n' % (time_, duration, key, velocity)
    return score

message_level = 1 + 2 + 32 + 128
csound = ctcsound.Csound()
csound.setOption("-d")
csound.setOption("-o%s" % output)
csound.setOption("-m%d" % message_level)
csound.compileOrc(orc)
csound.readScore(generate_score())
csound.start()
csound.perform()


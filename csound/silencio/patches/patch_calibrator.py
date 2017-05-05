import ctcsound
import random
#output = "test.wav"
output = "dac"

prologue = '''
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_pan = p6
i_depth = p7
i_height = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 70
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_XXX_level)
'''

epilog = '''
iattack = .002
isustain = p3
irelease = 0.1
p3 = iattack + isustain + irelease
a_declick linsegr 0, iattack, 1, isustain, 1, irelease, 0
aleft, aright pan2 asignal * i_amplitude * a_declick * k_gain, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "XXX            i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
'''

orc = '''
sr = 48000
ksmps = 15
nchnls = 2
0dbfs = 1

#include "STKBowed.inc"

#include "Blower.inc"
#include "DelayedPlucked.inc"
#include "Droner.inc"
#include "FilteredSines.inc"
#include "FM_Clang.inc"
#include "FMBell.inc"
#include "FMModerate.inc"
#include "FMModerate2.inc"
#include "FMModulatedChorus.inc"
#include "Guitar.inc"
#include "Guitar2.inc"
#include "Harpsichord.inc"
#include "HeavyMetal.inc"
#include "LivingstonGuitar.inc"
#include "Melody.inc"
#include "Plucked.inc"
#include "Rhodes.inc"
#include "STKBeeThree.inc"
#include "STKBowed.inc"
#include "STKPlucked.inc"
#include "ToneWheelOrgan.inc"
#include "TubularBell.inc"
#include "ZakianFlute.inc"

#include "PianoNotePianoteq.inc"
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
connect "FilteredSines", "outleft", "MasterOutput", "inleft"
connect "FilteredSines", "outright", "MasterOutput", "inright"
connect "FM_Clang", "outleft", "MasterOutput", "inleft"
connect "FM_Clang", "outright", "MasterOutput", "inright"
connect "FMBell", "outleft", "MasterOutput", "inleft"
connect "FMBell", "outright", "MasterOutput", "inright"
connect "FMModerate", "outleft", "MasterOutput", "inleft"
connect "FMModerate", "outright", "MasterOutput", "inright"
connect "FMModerate2", "outleft", "MasterOutput", "inleft"
connect "FMModerate2", "outright", "MasterOutput", "inright"
connect "FMModulatedChorus", "outleft", "MasterOutput", "inleft"
connect "FMModulatedChorus", "outright", "MasterOutput", "inright"
connect "Guitar", "outleft", "MasterOutput", "inleft"
connect "Guitar", "outright", "MasterOutput", "inright"
connect "Guitar2", "outleft", "MasterOutput", "inleft"
connect "Guitar2", "outright", "MasterOutput", "inright"
connect "Harpsichord", "outleft", "MasterOutput", "inleft"
connect "Harpsichord", "outright", "MasterOutput", "inright"
connect "HeavyMetal", "outright", "MasterOutput", "inright"
connect "HeavyMetal", "outleft", "MasterOutput", "inleft"
connect "LivingstonGuitar", "outleft", "MasterOutput", "inleft"
connect "LivingstonGuitar", "outright", "MasterOutput", "inright"
connect "Melody", "outleft", "MasterOutput", "inleft"
connect "Melody", "outright", "MasterOutput", "inright"
connect "Phaser", "outleft", "MasterOutput", "inleft"
connect "Phaser", "outright", "MasterOutput", "inright"
connect "PianoOut", "outleft", "MasterOutput", "inleft"
connect "PianoOut", "outright", "MasterOutput", "inright"
connect "Plucked", "outleft", "MasterOutput", "inleft"
connect "Plucked", "outright", "MasterOutput", "inright"
connect "Rhodes", "outleft", "MasterOutput", "inleft"
connect "Rhodes", "outright", "MasterOutput", "inright"
connect "Shiner", "outleft", "MasterOutput", "inleft"
connect "Shiner", "outright", "MasterOutput", "inright"
connect "STKBeeThree", "outleft", "MasterOutput", "inleft"
connect "STKBeeThree", "outright", "MasterOutput", "inright"
connect "STKBowed", "outleft", "MasterOutput", "inleft"
connect "STKBowed", "outright", "MasterOutput", "inright"
connect "STKPlucked", "outleft", "MasterOutput", "inleft"
connect "STKPlucked", "outright", "MasterOutput", "inright"
connect "Sweeper", "outleft", "MasterOutput", "inleft"
connect "Sweeper", "outright", "MasterOutput", "inright"
connect "ToneWheelOrgan", "outleft", "MasterOutput", "inleft"
connect "ToneWheelOrgan", "outright", "MasterOutput", "inright"
connect "TubularBell", "outleft", "MasterOutput", "inleft"
connect "TubularBell", "outright", "MasterOutput", "inright"
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


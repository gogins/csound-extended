import ctcsound
import random
import sys

orc = '''
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1

#includestr "$PATCH_FILENAME"
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

alwayson "MasterOutput"
'''

def generate_score():
    score = ''
    time_= 1.0
    for duration in [0.125, .25, 2]:
        for key in range(24,108,3):
            time_ = time_ + duration * 1.5
            velocity = random.choice([80, 80-6, 80-12, 80-18])
            score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, duration, key, velocity)
    return score

print(sys.argv)
patch_filename = sys.argv[1]
patch_name = sys.argv[2]
message_level = 1 + 2 + 32 + 128
csound = ctcsound.Csound()
csound.message("Patch file: {} Patch name: {}\n".format(patch_filename, patch_name))
csound.setOption("-d")
csound.setOption("--nchnls=2")
output = "/tmp/{}.wav".format(patch_filename)
csound.setOption("-o%s" % output)
csound.setOption("-m%d" % message_level)
csound.setOption("-+msg_color=0")
csound.setOption("--omacro:PATCH_FILENAME={}".format(patch_filename))
csound.setOption("--omacro:PATCH_NAME={}".format(patch_name))
csound.compileOrc(orc)
csound.readScore(generate_score())
csound.start()
csound.perform()


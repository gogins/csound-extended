import ctcsound
import random
import sys

output = "dac"

patch_name = sys.argv[1]
print("patch_name: ", patch_name)

prologue = '''
gi_INSTR_level init 0
gi_INSTR_attack init 0.002
gi_INSTR_release init 0.01
gk_INSTR_midi_dynamic_range init 127
instr INSTR
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
xtratim gi_INSTR_attack + gi_INSTR_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_INSTR_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 58
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_INSTR_level)
'''

epilog = '''
a_declicking linsegr 0, gi_INSTR_attack, 1, i_sustain, 1, gi_INSTR_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "INSTR          i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
;printks "INSTR          i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
'''

orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 1

giPianoteq init 0

#include "QQQ.inc"

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
prints "nchnls:                      %9.4f\\n", nchnls

connect "QQQ", "outleft", "MasterOutput", "inleft"
connect "QQQ", "outright", "MasterOutput", "inright"

alwayson "MasterOutput"

'''.replace("QQQ", patch_name)

def generate_score():
    score = 'f 0 60\n'
    time_= 1.0
    for duration in [1]:
        for key in xrange(24,108,12):
            time_ = time_ + duration * 1.5
            velocity = random.choice([80, 80-6, 80-12, 80-18])
            score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, duration, key, velocity)
    time_ += 2
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 36, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 48, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 55, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 59, 70)
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, 5, 64, 70)
    time_ += 7
    for note_i in xrange(50):
        duration = random.choice([0.125, 0.33334, 0.25, 0.66667, 1])
        time_ += duration / 2;
        key = random.choice(range(24, 108, 1))
        velocity = random.choice([80, 80-6, 80-12, 80-18]) / 3
        score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, duration, key, 60)
    time_ += 2
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 36, 70)
    #time_ += 1
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 48, 70)
    #time_ += 1
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 55, 70)
    #time_ += 1
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 59, 70)
    #time_ += 1
    score += 'i 1 %9.4f %9.4f %9.4f %9.4f 0 0.5\n' % (time_, -1, 64, 70)
    return score

message_level = 1 + 2 + 32 + 128
csound = ctcsound.Csound()
csound.setOption("-d")
csound.setOption("--nchnls=2")
csound.setOption("-o%s" % output)
csound.setOption("-m%d" % message_level)
csound.compileOrc(orc)
csound.readScore(generate_score())
csound.start()
csound.perform()


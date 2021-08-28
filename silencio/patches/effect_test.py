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
gk_FMModerate2_level chnexport "gk_FMModerate2_level", 3
gi_FMModerate2_carrier chnexport "gi_FMModerate2_carrier", 3
gi_FMModerate2_modulator chnexport "gi_FMModerate2_modulator", 3
gi_FMModerate2_fmamplitude chnexport "gi_FMModerate2_fmamplitude", 3
gi_FMModerate2_index chnexport "gi_FMModerate2_index", 3

gk_FMModerate2_level init 0
gi_FMModerate2_carrier init 1
gi_FMModerate2_modulator init 4
gi_FMModerate2_fmamplitude init 9
gi_FMModerate2_index init 2

gi_FMModerate2_cosine ftgen 0, 0, 65537, 11, 1

instr FMModerate2
; Author: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 85
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
i_frequency = cpsmidinn(i_midi_key)
k_gain = ampdb(gk_FMModerate2_level)
iattack = 0.002
isustain = p3
idecay = 1.5
irelease = 0.05
icarrier = gi_FMModerate2_carrier
imodulator = gi_FMModerate2_modulator
ifmamplitude = gi_FMModerate2_fmamplitude
index = gi_FMModerate2_index
ifrequencyb = i_frequency * 1.003
icarrierb = icarrier * 1.004
aindenv transegr 0.0, iattack, -8.0, 1.0, idecay, -8.0, 0.025, isustain, 0.0, 0.025, irelease, 7.0, 0.0
aindex = aindenv * index * ifmamplitude
aouta foscili 1.0, i_frequency, icarrier, imodulator, index, gi_FMModerate2_cosine
aoutb foscili 1.0, ifrequencyb, icarrierb, imodulator, index, gi_FMModerate2_cosine; Plus amplitude correction.
a_signal = (aouta + aoutb) * aindenv
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gi_Mverb2020 vstinit "/home/mkg/.local/lib/Mverb2020.so", 1

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

connect "FMModerate2", "outleft", "$PATCH_NAME", "inleft"
connect "FMModerate2", "outright", "$PATCH_NAME", "inright"
connect "$PATCH_NAME", "outleft", "MasterOutput", "inleft"
connect "$PATCH_NAME", "outright", "MasterOutput", "inright"

alwayson "$PATCH_NAME"
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
    for note_i in range(50):
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


'''
Author: Michael Gogins
'''

import CsoundAC
import os
import random
# Using the same random seed for each performance makes the performance 
# deterministic, not random.
random.seed(221)
import signal
import string
import sys
import traceback

print('Set "rendering" to:     "soundfile" or "audio".')
print
rendering = "audio"

model = CsoundAC.MusicModel()
score = model.getScore()

script_filename = sys.argv[0]
print('Full Python script:     %s' % script_filename)
title, exte = os.path.splitext(os.path.basename(script_filename))
model.setTitle(title)
model.setArtist("Michael Gogins")
model.setAuthor("Michael Gogins")
model.setYear("2020")
model.generateAllNames()
soundfile_name = model.getOutputSoundfileFilepath()
print('Soundfile name:         %s' % soundfile_name)
dac_name = 'dac:plughw:1,0'
print('Audio output name:      %s' % dac_name)
print

print('Rendering option:       %s' % rendering)
rendering_commands = {
    'soundfile':    'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -RWZdfo %s' % (soundfile_name),
    'audio':        'csound -d -r 48000 -k 128 -m195 -+msg_color=0 -o%s'       % (dac_name),
}
csound_command = rendering_commands[rendering]
print('Csound command line:    %s' % csound_command)
print

import musx_csoundac

from musx.paint import brush, spray
from musx.midi import MidiNote, MidiSeq, MidiFile
from musx.scheduler import Scheduler
from musx.generators import cycle, choose
from musx.tools import playfile, setmidiplayer
from musx.midi.gm import Vibraphone

# Scale1 is a 7-tone microtonal scale that evokes the Pelog scale.
scale1 = [0, 2.2, 3.8, 6.6, 7.1, 9.3, 10, 12, 14.4, 15.8, 18.6, 19.1]
# Scale2 is a 5-tone pentatonic scale that evokes the Slendro scale.
scale2 = [0, 2, 3.7, 6.9, 9.1]
scale3 = [[0, 6.9], [2, 6.9], [3.7, 9.1], [9.1, 14], [3.7, 12]]
melody = [[36, 43.1, 46], [42.6, 50.2], [38.2, 48, 51.8], [39.8, 46, 54.6],
          [54.6, 62.2, 66.6], [57.3, 66.6, 69.3], [58, 62.2, 67.1], 
          [36, 45.3, 48], [60, 66.6, 69.3], [46, 55.1, 60], 
          [42.6, 50.2, 57.3], [46, 55.1, 60], [57, 66.6, 69.3], 
          [57.3, 66.6, 69.3], [54.6, 62.2, 66.6]]
# Amplitude scaler to adapt to different sound fonts. this is for fluidsynth
def A(a): return ([s * 1.35 for s in a] if type(a) is list else a*1.5)
# Microtonal divisions of the semitone. 2 is quartertone (50 cents) etc.
tuning=7
# It's good practice to add any metadata such as tempo, midi instrument
# assignments, micro tuning, etc. to track 0 in your midi file.
t0 = MidiSeq.metaseq(ins={i: Vibraphone for i in range(tuning)}, tuning=tuning)
# Track 1 holds the composition.
t1 = MidiSeq()
# Create a scheduler and give it t1 as its output object.
q = Scheduler(t1)
 # The sections of the piece
s1=spray(q, key=48, dur=3, rhy=[1, .5], amp=A([.3, .35, .4]),
            band=scale1, end=40, tuning=tuning)
s2=spray(q, key=48, dur=3, rhy=[1, .5], amp=A([.4, .45, .5]), 
            band=scale3, end=25, tuning=tuning)
s3=spray(q, key=72, dur=3, rhy=[.75, .25, .5], amp=A([.4, .3, .35]),
            band=[3.8, 7.1, [9.3, 14.4]], end=35, tuning=tuning)
s4=spray(q, key=72, dur=3, rhy=[.75, .25, .5], amp=A([.6, .5, .55]),
            band=[9.3, 12, 14.2, 18.6, [26.2, 30.6]], end=30, tuning=tuning)
s5=spray(q, key=84,  dur=3, rhy=[.75, .25, .5], amp=A([.6, .5, .55]),
            band=[3.8, 7.1, 9.3, [7.1, 12]],  end=15, tuning=tuning)
s6=spray(q, key=24,  dur=5, rhy=[1, 1, .5, 2, 2], amp=A(.5),
            band=scale2, end=55, tuning=tuning)

s7=brush(q, key=[86.2, 93.3, 87.8, 91.1], dur=4, rhy=[.25, .25, .5], 
            amp=A(.3), end=50, tuning=tuning)
s8=brush(q, key=[86.2, [93.3, 98.8], 87.8, 91.1], dur=4,
            rhy=[.25, .25, .25, .25, .5], amp=A(.25), end=10, tuning=tuning)
s9=brush(q, key=[81.3, 74.4, 78.6, 72], dur=2, rhy=[.5, .25, .25],
            amp=A(.25), end=50, tuning=tuning)
s10=brush(q, key=melody, dur=8,
             rhy=[2, 1, 1, .5, .5, 3, 1.5, 1.5, 4, 2, 1, 1, .5, .5, 3, 1.5, 1.5, 1.5, .5, 4],
             amp=A([.3, .4, .35, .35]), end=40, tuning=tuning)

s11=spray(q, key=72, dur=2, rhy=1/3, amp=A(.18),
             band=[[0, 14.4], [3.8, 12], [15.8, 7.1], [2.2, 9.3], [0, 10],
                   [9.3, 2.2], [7.1, 14.4], [0, 9.3], [3.8, 12]],
             end=36, tuning=tuning)
s12=spray(q, key=60, dur=2, rhy=.5, amp=A(.25), band=scale2, end=41, tuning=tuning)
s13=spray(q, key=48, dur=4, rhy=[1, 1, 2, 1, 1, 1, 1, 1, .5, .5, 2, 1, .5, .5, 1, 1, 2, 2, .5, .5, 1, 4],
             amp=A(.35), band=scale3, end=32, tuning=tuning)
s14=brush(q, key=[[36, 42.6, 43.1, 48, 51.8, 57.3, 63.8, 86.4], [12, 24, 31.1, 36, 42.6]],
             len=2, dur=8, rhy=[4, 8], amp=A(.25), tuning=tuning)
# Start our composers in the scheduler, this creates the composition.
q.compose([[0, s1], [40, s2], [10, s3], [40, s4], [50, s5], [20, s6],
           [65, s7], [80, s8], [73, s9], [79, s10],
           [121, s11], [121, s12], [129, s13], [162, s14] ] )
# Write a midi file with our track data.
f = MidiFile("gamelan.mid", [t0, t1]).write()
# To automatially play demos use setmidiplayer() to assign a shell
# command that will play midi files on your computer. Example:
#   setmidiplayer("fluidsynth -iq -g1 /usr/local/sf/MuseScore_General.sf2")
print(f"Wrote '{f.pathname}'.")
#playfile(f.pathname)
csoundac_score_node = CsoundAC.ScoreNode()
csoundac_score = csoundac_score_node.getScore()
musx_csoundac.to_csoundac_score(f, csoundac_score)

print("Generated:")
print(csoundac_score.getCsoundScore())

orc = '''
sr = 48000
ksmps = 128
nchnls = 2
0dbfs = 100

; Ensure the same random stream for each rendering.
; rand, randh, randi, rnd(x) and birnd(x) are not affected by seed.

seed 38493

gi_Pianoteq vstinit "/home/mkg/Pianoteq\ 7/x86-64bit/Pianoteq\ 7.so", 0

alwayson "PianoOutPianoteq"
alwayson "ReverbSC"
alwayson "MasterOutput"

connect "ChebyshevMelody", "outleft", "ReverbSC", "inleft"
connect "ChebyshevMelody", "outright", "ReverbSC", "inright"
connect "FMWaterBell", "outleft", "ReverbSC", "inleft"
connect "FMWaterBell", "outright", "ReverbSC", "inright"
connect "Harpsichord", "outleft", "ReverbSC", "inleft"
connect "Harpsichord", "outright", "ReverbSC", "inright"
connect "Rhodes", "outleft", "ReverbSC", "inleft"
connect "Rhodes", "outright", "ReverbSC", "inright"
connect "ZakianFlute", "outleft", "ReverbSC", "inleft"
connect "ZakianFlute", "outright", "ReverbSC", "inright"

connect "PianoOutPianoteq", "outleft", "ReverbSC", "inleft"
connect "PianoOutPianoteq", "outright", "ReverbSC", "inright"
connect "ReverbSC", "outleft", "MasterOutput", "inleft"
connect "ReverbSC", "outright", "MasterOutput", "inright"

gk_PianoNotePianoteq_midi_dynamic_range init 127
instr PianoNotePianoteq
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_PianoNotePianoteq_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_homogeneity = p11
instances active p1
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_pitch_correction = 44100 / sr
; prints "Pitch factor:   %9.4f\\n", i_pitch_correction
vstnote gi_Pianoteq, 0, i_midi_key, i_midi_velocity, i_duration
endin

gk_ZakianFlute_midi_dynamic_range init 80
gk_ZakianFlute_level init 0
gk_ZakianFlute_pan init .5
gi_ZakianFLute_seed init .5
gi_ZakianFLute_f2  ftgen 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
gi_ZakianFlute_f26 ftgen 0, 0, 65537, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
gi_ZakianFlute_f27 ftgen 0, 0, 65537, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
gi_ZakianFlute_f28 ftgen 0, 0, 65537, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
gi_ZakianFlute_f29 ftgen 0, 0, 65537, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
gi_ZakianFlute_f30 ftgen 0, 0, 65537, -10, 1229, 16, 34, 57, 32
gi_ZakianFlute_f31 ftgen 0, 0, 65537, -10, 163, 31, 1, 50, 31
gi_ZakianFlute_f32 ftgen 0, 0, 65537, -10, 4128, 883, 354, 79, 59, 23
gi_ZakianFlute_f33 ftgen 0, 0, 65537, -10, 1924, 930, 251, 50, 25, 14
gi_ZakianFlute_f34 ftgen 0, 0, 65537, -10, 94, 6, 22, 8
gi_ZakianFlute_f35 ftgen 0, 0, 65537, -10, 2661, 87, 33, 18
gi_ZakianFlute_f36 ftgen 0, 0, 65537, -10, 174, 12
gi_ZakianFlute_f37 ftgen 0, 0, 65537, -10, 314, 13
gi_ZakianFlute_wtsin ftgen 0, 0, 65537, 10, 1

instr ZakianFlute
; Author: Lee Zakian
; Adapted by: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65.2
i_normalization = ampdb(-i_overall_amps) / 2
i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ZakianFlute_level)
iattack = .002
isustain = p3
irelease = .3
xtratim iattack + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
ip3 = (p3 < 3.0 ? p3 : 3.0)
; parameters
; p4 overall amplitude scaling factor
ip4 init i_amplitude
; p5 pitch in Hertz (normal pitch range: C4-C7)
ip5 init iHz
; p6 percent vibrato depth, recommended values in range [-1., +1.]
ip6 init 0.5
; 0.0 -> no vibrato
; +1. -> 1% vibrato depth, where vibrato rate increases slightly
; -1. -> 1% vibrato depth, where vibrato rate decreases slightly
; p7 attack time in seconds
; recommended value: .12 for slurred notes, .06 for tongued notes
; (.03 for short notes)
ip7 init .08
; p8 decay time in seconds
; recommended value: .1 (.05 for short notes)
ip8 init .08
; p9 overall brightness / filter cutoff factor
; 1 -> least bright / minimum filter cutoff frequency (40 Hz)
; 9 -> brightest / maximum filter cutoff frequency (10,240Hz)
ip9 init 5
; initial variables
iampscale = ip4 ; overall amplitude scaling factor
ifreq = ip5 ; pitch in Hertz
ivibdepth = abs(ip6*ifreq/100.0) ; vibrato depth relative to fundamental frequency
iattack = ip7 * (1.1 - .2*gi_ZakianFLute_seed) ; attack time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947) ; reset gi_ZakianFLute_seed
idecay = ip8 * (1.1 - .2*gi_ZakianFLute_seed) ; decay time with up to +-10% random deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifiltcut tablei ip9, gi_ZakianFLute_f2 ; lowpass filter cutoff frequency
iattack = (iattack < 6/kr ? 6/kr : iattack) ; minimal attack length
idecay = (idecay < 6/kr ? 6/kr : idecay) ; minimal decay length
isustain = p3 - iattack - idecay
p3 = (isustain < 5/kr ? iattack+idecay+5/kr : p3) ; minimal sustain length
isustain = (isustain < 5/kr ? 5/kr : isustain)
iatt = iattack/6
isus = isustain/4
idec = idecay/6
iphase = gi_ZakianFLute_seed ; use same phase for all wavetables
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
; vibrato block
; kvibdepth linseg .1, .8*p3, 1, .2*p3, .7
kvibdepth linseg .1, .8*ip3, 1, isustain, 1, .2*ip3, .7
kvibdepth = kvibdepth* ivibdepth ; vibrato depth
kvibdepthr randi .1*kvibdepth, 5, gi_ZakianFLute_seed ; up to 10% vibrato depth variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibdepth = kvibdepth + kvibdepthr
ivibr1 = gi_ZakianFLute_seed ; vibrato rate
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ivibr2 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
if ip6 < 0 goto vibrato1
kvibrate linseg 2.5+ivibr1, p3, 4.5+ivibr2 ; if p6 positive vibrato gets faster
 goto vibrato2
vibrato1:
ivibr3 = gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate linseg 3.5+ivibr1, .1, 4.5+ivibr2, p3-.1, 2.5+ivibr3 ; if p6 negative vibrato gets slower
vibrato2:
kvibrater randi .1*kvibrate, 5, gi_ZakianFLute_seed ; up to 10% vibrato rate variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kvibrate = kvibrate + kvibrater
kvib oscili kvibdepth, kvibrate, gi_ZakianFlute_wtsin
ifdev1 = -.03 * gi_ZakianFLute_seed ; frequency deviation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev2 = .003 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev3 = -.0015 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
ifdev4 = .012 * gi_ZakianFLute_seed
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kfreqr linseg ifdev1, iattack, ifdev2, isustain, ifdev3, idecay, ifdev4
kfreq = kHz * (1 + kfreqr) + kvib
if ifreq < 427.28 goto range1 ; (cpspch(8.08) + cpspch(8.09))/2
if ifreq < 608.22 goto range2 ; (cpspch(9.02) + cpspch(9.03))/2
if ifreq < 1013.7 goto range3 ; (cpspch(9.11) + cpspch(10.00))/2
goto range4
; wavetable amplitude envelopes
range1: ; for low range tones
kamp1 linseg 0, iatt, 0.002, iatt, 0.045, iatt, 0.146, iatt, \
0.272, iatt, 0.072, iatt, 0.043, isus, 0.230, isus, 0.000, isus, \
0.118, isus, 0.923, idec, 1.191, idec, 0.794, idec, 0.418, idec, \
0.172, idec, 0.053, idec, 0
kamp2 linseg 0, iatt, 0.009, iatt, 0.022, iatt, -0.049, iatt, \
-0.120, iatt, 0.297, iatt, 1.890, isus, 1.543, isus, 0.000, isus, \
0.546, isus, 0.690, idec, -0.318, idec, -0.326, idec, -0.116, idec, \
-0.035, idec, -0.020, idec, 0
kamp3 linseg 0, iatt, 0.005, iatt, -0.026, iatt, 0.023, iatt, \
0.133, iatt, 0.060, iatt, -1.245, isus, -0.760, isus, 1.000, isus, \
0.360, isus, -0.526, idec, 0.165, idec, 0.184, idec, 0.060, idec, \
0.010, idec, 0.013, idec, 0
iwt1 = gi_ZakianFlute_f26 ; wavetable numbers
iwt2 = gi_ZakianFlute_f27
iwt3 = gi_ZakianFlute_f28
inorm = 3949
goto end
range2: ; for low mid-range tones
kamp1 linseg 0, iatt, 0.000, iatt, -0.005, iatt, 0.000, iatt, \
0.030, iatt, 0.198, iatt, 0.664, isus, 1.451, isus, 1.782, isus, \
1.316, isus, 0.817, idec, 0.284, idec, 0.171, idec, 0.082, idec, \
0.037, idec, 0.012, idec, 0
kamp2 linseg 0, iatt, 0.000, iatt, 0.320, iatt, 0.882, iatt, \
1.863, iatt, 4.175, iatt, 4.355, isus, -5.329, isus, -8.303, isus, \
-1.480, isus, -0.472, idec, 1.819, idec, -0.135, idec, -0.082, idec, \
-0.170, idec, -0.065, idec, 0
kamp3 linseg 0, iatt, 1.000, iatt, 0.520, iatt, -0.303, iatt, \
0.059, iatt, -4.103, iatt, -6.784, isus, 7.006, isus, 11, isus, \
12.495, isus, -0.562, idec, -4.946, idec, -0.587, idec, 0.440, idec, \
0.174, idec, -0.027, idec, 0
iwt1 = gi_ZakianFlute_f29
iwt2 = gi_ZakianFlute_f30
iwt3 = gi_ZakianFlute_f31
inorm = 27668.2
goto end
range3: ; for high mid-range tones
kamp1 linseg 0, iatt, 0.005, iatt, 0.000, iatt, -0.082, iatt, \
0.36, iatt, 0.581, iatt, 0.416, isus, 1.073, isus, 0.000, isus, \
0.356, isus, .86, idec, 0.532, idec, 0.162, idec, 0.076, idec, 0.064, \
idec, 0.031, idec, 0
kamp2 linseg 0, iatt, -0.005, iatt, 0.000, iatt, 0.205, iatt, \
-0.284, iatt, -0.208, iatt, 0.326, isus, -0.401, isus, 1.540, isus, \
0.589, isus, -0.486, idec, -0.016, idec, 0.141, idec, 0.105, idec, \
-0.003, idec, -0.023, idec, 0
kamp3 linseg 0, iatt, 0.722, iatt, 1.500, iatt, 3.697, iatt, \
0.080, iatt, -2.327, iatt, -0.684, isus, -2.638, isus, 0.000, isus, \
1.347, isus, 0.485, idec, -0.419, idec, -.700, idec, -0.278, idec, \
0.167, idec, -0.059, idec, 0
iwt1 = gi_ZakianFlute_f32
iwt2 = gi_ZakianFlute_f33
iwt3 = gi_ZakianFlute_f34
inorm = 3775
goto end
range4: ; for high range tones
kamp1 linseg 0, iatt, 0.000, iatt, 0.000, iatt, 0.211, iatt, \
0.526, iatt, 0.989, iatt, 1.216, isus, 1.727, isus, 1.881, isus, \
1.462, isus, 1.28, idec, 0.75, idec, 0.34, idec, 0.154, idec, 0.122, \
idec, 0.028, idec, 0
kamp2 linseg 0, iatt, 0.500, iatt, 0.000, iatt, 0.181, iatt, \
0.859, iatt, -0.205, iatt, -0.430, isus, -0.725, isus, -0.544, isus, \
-0.436, isus, -0.109, idec, -0.03, idec, -0.022, idec, -0.046, idec, \
-0.071, idec, -0.019, idec, 0
kamp3 linseg 0, iatt, 0.000, iatt, 1.000, iatt, 0.426, iatt, \
0.222, iatt, 0.175, iatt, -0.153, isus, 0.355, isus, 0.175, isus, \
0.16, isus, -0.246, idec, -0.045, idec, -0.072, idec, 0.057, idec, \
-0.024, idec, 0.002, idec, 0
iwt1 = gi_ZakianFlute_f35
iwt2 = gi_ZakianFlute_f36
iwt3 = gi_ZakianFlute_f37
inorm = 4909.05
goto end
end:
kampr1 randi .02*kamp1, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp1 = kamp1 + kampr1
kampr2 randi .02*kamp2, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp2 = kamp2 + kampr2
kampr3 randi .02*kamp3, 10, gi_ZakianFLute_seed ; up to 2% wavetable amplitude variation
gi_ZakianFLute_seed = frac(gi_ZakianFLute_seed*105.947)
kamp3 = kamp3 + kampr3
awt1 poscil kamp1, kfreq, iwt1, iphase ; wavetable lookup
awt2 poscil kamp2, kfreq, iwt2, iphase
awt3 poscil kamp3, kfreq, iwt3, iphase
asig = awt1 + awt2 + awt3
asig = asig*(iampscale/inorm)
kcut linseg 0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0 ; lowpass filter for brightness control
afilt tone asig, kcut
a_signal balance afilt, asig
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_signal *= .7
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

//////////////////////////////////////////////
// Original by Steven Yi.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
gk_FMWaterBell_level init 0
gi_FMWaterBell_attack init 0.002
gi_FMWaterBell_release init 0.01
gi_FMWaterBell_sustain init 20
gi_FMWaterBell_sustain_level init .1
gk_FMWaterBell_index init .5
gk_FMWaterBell_crossfade init .5
gk_FMWaterBell_vibrato_depth init 0.05
gk_FMWaterBell_vibrato_rate init 6
gk_FMWaterBell_midi_dynamic_range init 127
gi_FMWaterBell_cosine ftgen 0, 0, 65537, 11, 1
instr FMWaterBell
i_instrument = p1
i_time = p2
i_duration = p3
; One of the envelopes in this instrument should be releasing, and use this:
i_sustain = 1000
;xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
xtratim gi_FMWaterBell_attack + gi_FMWaterBell_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_FMWaterBell_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 81
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization * 1.6
k_gain = ampdb(gk_FMWaterBell_level)
a_signal fmbell	1, i_frequency, gk_FMWaterBell_index, gk_FMWaterBell_crossfade, gk_FMWaterBell_vibrato_depth, gk_FMWaterBell_vibrato_rate, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine, gi_FMWaterBell_cosine ;, gi_FMWaterBell_sustain
;a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 0
a_envelope linsegr 0, gi_FMWaterBell_attack, 1, i_sustain, 1, gi_FMWaterBell_release, 0
; ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
; a_envelope transegr 0, gi_FMWaterBell_attack, 12, 1, i_sustain, 12, gi_FMWaterBell_sustain_level, gi_FMWaterBell_release, 12, 0
a_signal = a_signal * i_amplitude * a_envelope * k_gain
;_signal = a_signal * i_amplitude * k_gain
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
outleta "outleft", a_out_left
outleta "outright", a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
; printks "FMWaterBell    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Harpsichord_midi_dynamic_range init 127
gk_Harpsichord_level init 0
gk_Harpsichord_pick init .075
gk_Harpsichord_reflection init .5
gk_Harpsichord_pluck init .75
gi_Harpsichord_harptable ftgen 0, 0, 65537, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Harpsichord_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 110
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Harpsichord_level)
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 40.0, -25.0, 0.0
apluck pluck i_amplitude, kHz, iHz, 0, 1
aharp poscil aenvelope, kHz, gi_Harpsichord_harptable
aharp2 balance apluck, aharp
a_signal	= (apluck + aharp2)
i_attack = .0005
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain
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
;printks "Harpsichord      %9.4f   %9.4f\\n", 0.5, a_out_left, a_out_right
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_ChebyshevMelody_level init 0
gi_ChebyshevMelody_attack init 0.003
gi_ChebyshevMelody_release init 0.01
gk_ChebyshevMelody_midi_dynamic_range init 127
gk_ChebyshevMelody_level init 0
gi_ChebyshevMelody_chebyshev ftgen 0, 0, 65537, -7, -1, 150, 0.1, 110, 0, 252, 0
gi_ChebyshevMelody_sine ftgen 0, 0, 65537, 10, 1
gi_ChebyshevMelody_cook3 ftgen 0, 0, 65537, 10, 1, .4, 0.2, 0.1, 0.1, .05
instr ChebyshevMelody
///////////////////////////////////////////////////////
// Original by Jon Nelson.
// Adapted by Michael Gogins.
///////////////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_ChebyshevMelody_attack + gi_ChebyshevMelody_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_ChebyshevMelody_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 72
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ChebyshevMelody_level)

iattack = .01
isustain = p3
irelease = .01
p3 = iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
iamplitude = ampdb(i_midi_velocity) * 7.
ip6 = gi_ChebyshevMelody_chebyshev
kHz = k(iHz)
idB = i_midi_velocity
i1 = iHz
k100 randi 1,0.05
ak101 poscil 1, 5 + k100, gi_ChebyshevMelody_sine
ak102 linseg 0, .5, 1, p3, 1
k100 = i1 + (ak101 * ak102)
; Envelope for driving oscillator.
ip3 init 3.0
; k1 linenr 0.5, ip3 * .3, ip3 * 2, 0.01
ak1 linseg 0, ip3 * .3, .5, ip3 * 2, 0.01, isustain, 0.01, irelease, 0
; k2 line 1, p3, .5
ak2 linseg 1.0, ip3, .5, isustain, .5, irelease, 0
ak1 = ak2 * ak1
; Amplitude envelope.
ak10 expseg 0.0001, iattack, 1.0, isustain, 0.8, irelease, .0001
ak10 = (ak10 - .0001)
; Power to partials.
k20 linseg 1.485, iattack, 1.5, (i_sustain + irelease), 1.485
; a1-3 are for cheby with p6=1-4
a1 poscil ak1, k100 - .25, gi_ChebyshevMelody_cook3
; Tables a1 to fn13, others normalize,
a2 tablei a1, ip6, 1, .5
a3 balance a2, a1
; Try other waveforms as well.
a4 foscili 1, k100 + .04, 1, 2.000, k20, gi_ChebyshevMelody_sine
a5 poscil 1, k100, gi_ChebyshevMelody_sine
a6 = ((a3 * .1) + (a4 * .1) + (a5 * .8)) * ak10
a7 comb a6, .5, 1 / i1
a8 = (a6 * .9) + (a7 * .1)
asignal balance a8, a1
a_declicking linsegr 0, gi_ChebyshevMelody_attack, 1, i_sustain, 1, gi_ChebyshevMelody_release, 0
a_signal = asignal * i_amplitude * a_declicking * k_gain
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
printks "ChebyshevMelody i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\\n", .1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_Rhodes_level init 0
gk_Rhodes_midi_dynamic_range init 127
gi_Rhodes_sine ftgen 0, 0, 65537, 10, 1
gi_Rhodes_cosine ftgen 0, 0, 65537, 11, 1
gi_Rhodes_blank ftgen 0, 0, 65537, 10, 0 ; Blank wavetable for some Cook FM opcodes.
instr Rhodes
; Authors: Perry Cook, John ffitch, Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Rhodes_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_overall_amps = 82
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Rhodes_level)
iindex = 4
icrossfade = 3
ivibedepth = 0.2
iviberate = 6
ifn1 = gi_Rhodes_sine
ifn2 = gi_Rhodes_cosine
ifn3 = gi_Rhodes_sine
ifn4 = gi_Rhodes_blank
ivibefn = gi_Rhodes_sine
a_signal fmrhode i_amplitude, i_frequency, iindex, icrossfade, ivibedepth, iviberate, ifn1, ifn2, ifn3, ifn4, ivibefn
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * a_declicking * k_gain
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

gk_PianoOutPianoteq_level init 0
gi_PianoOutPianoteq_print init 1
gk_PianoOutPianoteq_front_to_back init 0
gk_PianoOutPianoteq_left_to_right init 1/7
gk_PianoOutPianoteq_bottom_to_top init 0
instr PianoOutPianoteq
; Should be "D4 Daily Practice".
vstprogset gi_Pianoteq, 0
; Sustain off.
vstparamset gi_Pianoteq, 0, 0
; Reverb off.
vstparamset gi_Pianoteq, 72, 0
k_gain = ampdb(gk_PianoOutPianoteq_level)
i_overall_amps = 89
i_normalization = ampdb(-i_overall_amps) * 2
i_amplitude = ampdb(80) * i_normalization
if gi_PianoOutPianoteq_print == 1 then
  vstinfo gi_Pianoteq
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
ainleft init 0
ainright init 0
aoutleft, aoutright vstaudio gi_Pianoteq, ainleft, ainright
a_signal = aoutleft + aoutright
a_signal *= k_gain
a_signal *= i_amplitude
a_out_left, a_out_right pan2 a_signal, gk_PianoOutPianoteq_left_to_right
#ifdef USE_SPATIALIZATION
a_signal = a_out_left + a_out_right
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, gk_PianoOutPianoteq_front_to_back, gk_PianoOutPianoteq_left_to_right, gk_PianoOutPianoteq_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
; printks "PianoOutPt     L %9.4f R %9.4f l %9.4f\\n", 0.5, a_out_left, a_out_right, gk_Piano_level
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Reverb_feedback init 0.875
gk_Reverb_wet init 0.5
gi_Reverb_delay_modulation init 0.0075
gk_Reverb_frequency_cutoff init 15000
instr ReverbSC
gk_Reverb_dry = 1.0 - gk_Reverb_wet
aleftin init 0
arightin init 0
aleftout init 0
arightout init 0
aleftin inleta "inleft"
arightin inleta "inright"
aleftout, arightout reverbsc aleftin, arightin, gk_Reverb_feedback, gk_Reverb_frequency_cutoff, sr, gi_Reverb_delay_modulation
aleftoutmix = aleftin * gk_Reverb_dry + aleftout * gk_Reverb_wet
arightoutmix = arightin * gk_Reverb_dry + arightout * gk_Reverb_wet
outleta "outleft", aleftoutmix
outleta "outright", arightoutmix
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_MasterOutput_level init 0
gS_MasterOutput_filename init "check.wav"
instr MasterOutput
aleft inleta "inleft"
aright inleta "inright"
k_gain = ampdb(gk_MasterOutput_level)
printks2 "Master gain: %f\\n", k_gain
iamp init 1
aleft butterlp aleft, 18000
aright butterlp aright, 18000
outs aleft * k_gain, aright * k_gain
; We want something that will play on my phone.
i_amplitude_adjustment = 1; ampdbfs(-3) / 32767
i_filename_length strlen gS_MasterOutput_filename
if i_filename_length > 0 goto filename_exists
goto filename_endif
filename_exists:
prints sprintf("Output filename: %s\\n", gS_MasterOutput_filename)
fout gS_MasterOutput_filename, 18, aleft * i_amplitude_adjustment, aright * i_amplitude_adjustment
filename_endif:
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

; It is important for levels to be evenly balanced _on average_! This 
; enables instruments to come forward and recede according to their 
; response to MIDI velocity.

gk_PianoOutPianoteq_level init  -8 
gk_ZakianFlute_level init       25
gk_FMWaterBell_level init       24;16
gk_ChebyshevMelody_level init   28;19
gk_Harpsichord_level init       27
gk_Rhodes_level init            31

gk_Reverb_wet init 0.25
gk_Reverb_feedback init 0.85
gi_Reverb_delay_modulation init 0.0875
gk_Reverb_frequency_cutoff init 14000
'''
rescale = CsoundAC.Rescale()
rescale.addChild(csoundac_score_node)
rescale.setRescale(CsoundAC.Event.INSTRUMENT, bool(1), bool(1), 1, 3.99)
rescale.setRescale(CsoundAC.Event.VELOCITY, bool(1), bool(1), 50, 20)
model.addChild(rescale)
model.setCsoundOrchestra(orc)
model.setCsoundCommand(csound_command)
model.generate()
# Fix ending. Instruments would drop out, sustain till the end. Some notes 
# course will decay first.
score = model.getScore()
score_duration = score.getDuration() + 4.
sounding = set()
score.save(model.getMidifileFilepath())
model.performMaster()
if rendering == 'soundfile':
    model.translateMaster()


instr Reverb1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Authors: Sean Costello and J. Lato
; Adapted by Michael Gogins
; p4 = gain of reverb. Adjust empirically
; for desired reverb time. .6 gives
; a good small "live" room sound, .8
; a small hall, .9 a large hall,
; .99 an enormous stone cavern.
; p5 = amount of random pitch modulation
; for the delay lines. 1 is the "normal"
; amount, but this may be too high for
; held pitches such as piano tones.
; Adjust to taste.
; p6 = cutoff frequency of lowpass filters
; in feedback loops of delay lines,
; in Hz. Lower cutoff frequencies results
; in a sound with more high-frequency
; damping.
; p7 = wet fraction (0 all dry, 1 all wet).
; 8 delay line FDN reverb, with feedback matrix based upon 
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III, 
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music 
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
idry = 1 - p7
iwet = p7
igain = p4 
ipitchmod = p5 
itone = p6 
asignal inleta "in"
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
idel1 = (2473.000/sr)
idel2 = (2767.000/sr)
idel3 = (3217.000/sr)
idel4 = (3557.000/sr)
idel5 = (3907.000/sr)
idel6 = (4127.000/sr)
idel7 = (2143.000/sr)
idel8 = (1933.000/sr)
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
k1 randi .001, 3.1, .06
k2 randi .0011, 3.5, .9
k3 randi .0017, 1.11, .7
k4 randi .0006, 3.973, .3
k5 randi .001, 2.341, .63
k6 randi .0011, 1.897, .7
k7 randi .0017, 0.891, .9
k8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for 
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following 
; equation, and replace the .25 by 2/N, where N is the 
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltapi idel1 + k1 * ipitchmod
delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + k2 * ipitchmod
delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + k3 * ipitchmod
delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + k4 * ipitchmod
delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + k5 * ipitchmod
delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + k6 * ipitchmod
delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + k7 * ipitchmod
delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + k8 * ipitchmod
delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * igain, itone
afilt2 tone adel2 * igain, itone
afilt3 tone adel3 * igain, itone
afilt4 tone adel4 * igain, itone
afilt5 tone adel5 * igain, itone
afilt6 tone adel6 * igain, itone
afilt7 tone adel7 * igain, itone
afilt8 tone adel8 * igain, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel 
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
asignal = aout1 + aout2
; To the master output.
adry = asignal * idry
awet = asignal * iwet
outleta "out", adry + awet
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

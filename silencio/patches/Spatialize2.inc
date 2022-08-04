gShelp init {{
S P A T I A L I Z E

Copyright (c) 2014-2022 by Michael Gogins.
This software is licensed under the terms of the
GNU Lesser Public License.

Many thanks to Jan Jacob Hofmann for his advanced Csound
spatialization system, which I have adapted here, and adapted to the 
bformenc1/bformdec2 opcodes.

This software is a system of Ambisonic and/or HRTF-based
audio spatialization for Csound. I have simplified Hofmann's
excellent code by avoiding all Csound macros, and instead
using user-defined opcodes along with the signal flow graph
opcodes that send and receive signals using audio-rate array
variables.

If a single source fans out to one or more sinks, the sinks are opcodes.
If multiple sources fan in to a single sink, the sources are instr outlets
and the sink is an instr inlet.

The available speaker rigs are:

 1  Stereo - L(90), R(-90); this is an M+S style stereo decode.
    0th order, 4 channels.
 2  Quad - FL(45), BL(135), BR(-135), FR(-45). 
    1st order, 4 channels.
 3  5.0 - L(30), R(-30), C(0), BL(110), BR(-110). Note that many people do not 
    actually use the angles above for their speaker arrays and a good decode 
    for DVD etc can be achieved using the 
    2nd or 3rd order -- 9 or 16 channels.
 4  Octagon - FFL(22.5), FLL(67.5), BLL(112.5), BBL(157.5), BBR(-157.5), 
    BRR(-112.5), FRR(-67.5), FFR(-22.5). This is a first-, second- or 
    third-order decode, depending on the number of input channels.
    1st, 2nd, or 3rd order -- 4, 9, or 16 channels.
 5  Cube - FLD(45,-35.26), FLU(45,35.26), BLD(135,-35.26), BLU(135,35.26), 
    BRD(-135,-35.26), BRU(-135,35.26), FRD(-45,-35.26), FRU(-45,35.26). 
    1st order -- 4 channels.
 6  Hexagon - FL(30), L(90) BL(150), BR(-150), R(-90), FR(-30). 
    1st or 2nd order -- 4 or 9 channels.
21  2D binaural configuration. This first decodes to a octagon configuration and 
    then applies HRTF filters.
31  3D binaural configuration. This first decodes to a dodecahedron 
    configuration and then applies HRTF filters.

Please note the coordinate conventions. The listener is always the origin.
In this system, spatial locations are passed to the spatialize opcode in
Cartesian coordinates. These are the same as those used in physics:

-- x runs from ahead to behind.
-- y runs from left to right. This is the same as stereo pan.
-- z runs from below to above.

Spherical coordinates are more or less standard for Ambisonics, and are
similar to but not identical with those used in physics. The only differences
are that Ambisonics uses elevation but physics uses inclination, and
Ambisonics uses degrees but physics uses radians.

-- r is distance.
-- azimuth or theta is anti-clockwise angle from 0 in front of the listener.
-- elevation or phi is angle above or below the x-y plane. (Note that
   inclination, also called polar angle, would be angle away from the
   zenith pole.)

USAGE

In the orchestra header:

gi_BformatDecoder_SpeakerRig init 5

alwayson SpatialReverb
alwayson Bformat3Decoder

connect "SpatializingInstrument", "out", "SpatialReverb", "in"
connect "SpatializingInstrument", "outbformat", "Bformat3Decoder", "inbformat"
connect "SpatialReverb", "outbformat", "Bformat3Decoder", "inbformat"

instr SpatializingInstrument
absignal[] init 16
asignal STKBlowBotl cpsmidinn(p4), 1, 4, kv1, 11, 10, 1, 50, 128
absignal, asend Spatializer asignal, 87, 15, 15
; Send a signal with only distance cues to the global spatial reverb instr.
outleta "out", asend
; Send the spatialized signal to the global Ambisonc decoder instr.
outletv "outbformat", absignal
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

TODO

-- Upgrade demonstration piece with jspline spatial trajectories
   and algorithmically generated percussive notes.

-- Plug in and test early reflections code.

-- Test with rigs 2 through 5.

-- See if babo can be adapted for spatial reverb.

-- Upgrade panning with arbitrary measured speaker positions.

-- Try spat3d for global reverb and early reflections (not doing,
   as not periphonic).

}}

opcode kprintlevels, 0, Sa[]
Slabel, absignal[] xin
krms0 rms absignal[0]
krms1 rms absignal[1]
krms2 rms absignal[2]
krms3 rms absignal[3]
printks "%s %9.4f %9.4f %9.4f %9.4f\n", 1., Slabel, krms0, krms1, krms2, krms3
endop

;prints gShelp

; Global parameters that can be re-defined in the orchestra header.

gk_Spatialize_Verbose init 0
gi_BformatDecoder_SpeakerRig chnexport "gi_BformatDecoder_SpeakerRig", 3 ; 0
gi_BformatDecoder_SpeakerRig init 2
gk_Spatialize_SpeakerRigRadius chnexport "gk_Spatialize_SpeakerRigRadius", 3 ; 0
gk_Spatialize_SpeakerRigRadius init 5.0

/**
 * Returns the highest Ambisonic order available using bformenc1 for the 
 * global speaker rig.
 */
opcode channels_for_rig, i, 0
i_channels init -1
if     gi_BformatDecoder_SpeakerRig == 1 then
  i_channels = 4
elseif gi_BformatDecoder_SpeakerRig == 2 then
  i_channels = 4
elseif gi_BformatDecoder_SpeakerRig == 3 then
  i_channels = 16
elseif gi_BformatDecoder_SpeakerRig == 4 then
  i_channels = 16
elseif gi_BformatDecoder_SpeakerRig == 5 then
  i_channels = 4
elseif gi_BformatDecoder_SpeakerRig == 6 then
  i_channels = 9
elseif gi_BformatDecoder_SpeakerRig == 21 then
  i_channels = 16
elseif gi_BformatDecoder_SpeakerRig == 31 then
  i_channels = 16
endif
xout i_channels
endop

opcode ambi_zero, 0, a[]
a_vector[] xin
k_index init 0
k_bound lenarray a_vector
while k_index < k_bound do
  a_vector[k_index] = 0
  k_index = k_index + 1
od
endop

opcode ambi_assign, 0, a[]a
a_vector[], a_scalar xin
k_index init 0
k_bound lenarray a_vector
while k_index < k_bound do
  a_vector[k_index] = a_scalar
  k_index = k_index + 1
od
endop

opcode ambi_plus_add, 0, a[]a[]
a_vector[], a_addend[] xin
k_index init 0
k_bound lenarray a_vector
while k_index < k_bound do
  a_vector[k_index] = a_vector[k_index] + a_addend[k_index]
  k_index = k_index + 1
od
endop

opcode CartesianToPolar, kkk, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
; Normalize the coordinates to unit vectors from the origin.
kx = kx / kdistance
ky = ky / kdistance
kz = kz / kdistance
; Limit distance to prevent too close a sound becoming too loud.
kdistance = (kdistance < 0.3 ? 0.3 : kdistance)
; Calculate the elevation.
kelevation = cosinv(sqrt(1 - kz^2))
; If z is negative, make elevation negative also.
kelevation = (kz < 0 ? -kelevation : kelevation)
; Calculate the angle.
kangle = sininv(ky / cos(kelevation))
; Distinguish between positive x and negative x.
kangle = (kx >= 0 ? kangle : 3.14159265 - kangle)
; Distinguish between positive and negative y and x.
kangle = (ky <= 0 && kx >= 0 ? 6.28318531 + kangle : kangle)
xout kangle, kelevation, kdistance
endop

opcode CartesianToPolarDegrees, kkk, kkk
kx, ky, kz xin
; Calculate the distance from the origin (the listener).
kdistance = sqrt(kx^2 + ky^2 + kz^2)
; Normalize the coordinates to unit vectors from the origin.
kx = kx / kdistance
ky = ky / kdistance
kz = kz / kdistance
; Limit distance to prevent too close a sound becoming too loud.
kdistance = (kdistance < 0.3 ? 0.3 : kdistance)
; Calculate the elevation.
kelevation = cosinv(sqrt(1 - kz^2))
; If z is negative, make elevation negative also.
kelevation = (kz < 0 ? -kelevation : kelevation)
; Calculate the angle.
kangle = sininv(ky / cos(kelevation))
; Distinguish between positive x and negative x.
kangle = (kx >= 0 ? kangle : 3.14159265 - kangle)
; Distinguish between positive and negative y and x.
kangle = (ky <= 0 && kx >= 0 ? 6.28318531 + kangle : kangle)
kangle = kangle * 57.295779513
kelevation = kelevation * 57.295779513
xout kangle, kelevation, kdistance
endop

; Zero distance blows this up.
gk_DopplerByDistance_Mach chnexport "gk_DopplerByDistance_Mach", 3 ; 0
gk_DopplerByDistance_Mach init 340.29
opcode DopplerByDistance, a, ak
asignal, kdistance xin
; ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]
adopplered doppler asignal, kdistance * gk_Spatialize_SpeakerRigRadius, 0, i(gk_DopplerByDistance_Mach), 6
xout adopplered
endop

; Zero distance blows this up.
opcode LowpassByDistance, a, ak
asignal, kdistance xin
afiltered butterlp asignal, 22000 * sqrt(1.0 / kdistance) + .000001
abalanced balance afiltered, asignal
xout abalanced
endop

opcode ReverbSC, a, akkkk
asignal, kReverbFeedback, kDelayModulation, kReverbWet, itone xin
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

; 8 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 8 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
; Coded by Sean Costello, October 1999
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
ak1 randi .001, 3.1, .06
ak2 randi .0011, 3.5, .9
ak3 randi .0017, 1.11, .7
ak4 randi .0006, 3.973, .3
ak5 randi .001, 2.341, .63
ak6 randi .0011, 1.897, .7
ak7 randi .0017, 0.891, .9
ak8 randi .0006, 3.221, .44
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .25 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 + afilt7 + afilt8)
adum1 delayr 1
adel1 deltapi idel1 + ak1 * kDelayModulation
 delayw asignal + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + ak2 * kDelayModulation
 delayw asignal + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + ak3 * kDelayModulation
 delayw asignal + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + ak4 * kDelayModulation
 delayw asignal + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + ak5 * kDelayModulation
 delayw asignal + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + ak6 * kDelayModulation
 delayw asignal + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + ak7 * kDelayModulation
 delayw asignal + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + ak8 * kDelayModulation
 delayw asignal + apj - afilt8
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * kReverbFeedback, itone
afilt2 tone adel2 * kReverbFeedback, itone
afilt3 tone adel3 * kReverbFeedback, itone
afilt4 tone adel4 * kReverbFeedback, itone
afilt5 tone adel5 * kReverbFeedback, itone
afilt6 tone adel6 * kReverbFeedback, itone
afilt7 tone adel7 * kReverbFeedback, itone
afilt8 tone adel8 * kReverbFeedback, itone
; The outputs of the delay lines are summed
; and sent to the stereo outputs. This could
; easily be modified for a 4 or 8-channel
; sound system.
aout1 = (afilt1 + afilt3 + afilt5 + afilt7)
aout2 = (afilt2 + afilt4 + afilt6 + afilt8)
aoutput = aout1 + aout2
aoutwet = aoutput * kReverbWet
aoutdry = aoutput * (1 - kReverbWet)
aoutput = aoutwet + aoutdry
xout aoutput
endop

gk_LocalReverbByDistance_Wet chnexport "gk_LocalReverbByDistance_Wet", 3 ; 0
gk_LocalReverbByDistance_Wet init 0.2
; This is a fraction of the speaker rig radius.
gk_LocalReverbByDistance_FrontWall chnexport "gk_LocalReverbByDistance_FrontWall", 3 ; 0
gk_LocalReverbByDistance_FrontWall init 0.9
gk_LocalReverbByDistance_ReverbDecay chnexport "gk_LocalReverbByDistance_ReverbDecay", 3 ; 0
gk_LocalReverbByDistance_ReverbDecay init 0.6
gk_LocalReverbByDistance_CutoffHz chnexport "gk_LocalReverbByDistance_CutoffHz", 3 ; 0
gk_LocalReverbByDistance_CutoffHz init sr
gk_LocalReverbByDistance_RandomDelayModulation chnexport "gk_LocalReverbByDistance_RandomDelayModulation", 3 ; 0
gk_LocalReverbByDistance_RandomDelayModulation init 1.0

opcode LocalReverbByDistance, aa, ak
; Re-implemented from Dodge and Jerse.
asignal, kdistanceMeters xin
kdry = 1.0 - gk_LocalReverbByDistance_Wet
knormalizedDistance = kdistanceMeters / gk_Spatialize_SpeakerRigRadius + .00001
klocalAttenuation = 1.0 / knormalizedDistance
kglobalAttenuation = 1.0 / sqrt(knormalizedDistance)
alocalReverbSend = asignal * klocalAttenuation
aglobalReverbSend = asignal * kglobalAttenuation
iechoMeters = 2.0 * i(gk_Spatialize_SpeakerRigRadius) * i(gk_LocalReverbByDistance_FrontWall)
isecondsPerMeter = 1.0 / i(gk_DopplerByDistance_Mach)
imillisecondsPerMeter = isecondsPerMeter * 1000.0
iglobalReverbSendDelayMilliseconds = i(gk_Spatialize_SpeakerRigRadius) * i(gk_LocalReverbByDistance_FrontWall) * imillisecondsPerMeter
; print iglobalReverbSendDelayMilliseconds
aglobalReverbSendDelayed vdelay aglobalReverbSend, iglobalReverbSendDelayMilliseconds, 1
alocalReverbLeft, alocalReverbRight reverbsc	alocalReverbSend, alocalReverbSend, gk_LocalReverbByDistance_ReverbDecay, gk_LocalReverbByDistance_CutoffHz, sr, i(gk_LocalReverbByDistance_RandomDelayModulation), 1
alocalReverbWet = ((alocalReverbLeft + alocalReverbRight) / 2.0) * gk_LocalReverbByDistance_Wet
alocalReverbDry = kdry * asignal
alocalReverberatedSignal = alocalReverbDry + alocalReverbWet
if gk_Spatialize_Verbose == 1 then
printks "LocalReverbByDistance asignal: %9.4f klocalAttentuaton: %9.4f iglobalReverbSendDelayMilliseconds: %9.4f kglobalAttenuation: %9.4f alocalReverberatedSignal: %9.4f aglobalReverbSend: %9.4f\n", 0.5, asignal, klocalAttenuation, iglobalReverbSendDelayMilliseconds, kglobalAttenuation, alocalReverberatedSignal, aglobalReverbSendDelayed
endif
xout alocalReverberatedSignal, aglobalReverbSendDelayed
endop

; Switch on/off diffuse reflections [0=off, 1=on] and adjust gain of reflection (e.g.: 1= normal gain, 2= twice as loud, 0.5 = half as loud).
gk_EarlyReflection_diffref chnexport "gk_EarlyReflection_diffref", 3 ; , 0
gk_EarlyReflection_diffref init 0 * 0
; Switch on/off specular reflections [0=off, 1=on] and adjust gain of reflection (e.g.: 1= normal gain, 2= twice as loud, 0.5 = half as loud).
gk_EarlyReflection_specref chnexport "gk_EarlyReflection_specref", 3 ; , 0
gk_EarlyReflection_specref init 0 * 0
; Position of FrontWall at the x-axis [units] for calculation of early reflections.
gk_EarlyReflection_FrontWall chnexport "gk_EarlyReflection_FrontWall", 3 ; , 0
gk_EarlyReflection_FrontWall init 1.6
; Position of SideWall at the y-axis [units] for calculation of early reflections.
gk_EarlyReflection_SideWall chnexport "gk_EarlyReflection_SideWall", 3 ; , 0
gk_EarlyReflection_SideWall init 1.3
; Height of the Ceiling measured from the zero-point of the coordinate-system [units] for calculation of early reflections.
gk_EarlyReflection_Ceiling chnexport "gk_EarlyReflection_Ceiling", 3 ; , 0
gk_EarlyReflection_Ceiling init 1.1
; Damping factor of the reflecting surface (gain of reverb).
gk_EarlyReflection_damp chnexport "gk_EarlyReflection_damp", 3 ; , 0
gk_EarlyReflection_damp init 0.1
; Absorbtion characteristics of the wall in respect to high frequencies: frequencies above ihighdamp get filtered out.
gk_EarlyReflection_highdamp chnexport "gk_EarlyReflection_highdamp", 3 ; , 0
gk_EarlyReflection_highdamp init 15000
; Low frequency boost for sources near a wall.
gk_EarlyReflection_lfboost chnexport "gk_EarlyReflection_lfboost", 3 ; , 0
gk_EarlyReflection_lfboost init 1
opcode DiffuseEarlyReflect, a[], akkki
asignal, kx, ky, kz, iwhichwall xin

if iwhichwall == 1 goto iwhichwall_1
 goto iwhichwall_1_else
 iwhichwall_1:
 krefx = gk_EarlyReflection_FrontWall
 goto iwhichwall_1_endif
iwhichwall_1_else:
 krefx = kx
iwhichwall_1_endif:

if iwhichwall == 2 goto iwhichwall_2
 goto iwhichwall_2_else
 iwhichwall_2:
 krefx = -gk_EarlyReflection_FrontWall
 goto iwhichwall_2_endif
iwhichwall_2_else:
 krefx = kx
iwhichwall_2_endif:

if iwhichwall == 3 goto iwhichwall_3
 goto iwhichwall_3_else
 iwhichwall_3:
 kfefy = gk_EarlyReflection_SideWall
 goto iwhichwall_3_endif
iwhichwall_3_else:
 krefy = ky
iwhichwall_3_endif:

if iwhichwall == 4 goto iwhichwall_4
 goto iwhichwall_4_else
 iwhichwall_4:
 krefy = -gk_EarlyReflection_SideWall
 goto iwhichwall_4_endif
iwhichwall_4_else:
 krefy = ky
iwhichwall_4_endif:

if iwhichwall == 5 goto iwhichwall_5
goto iwhichwall_5_else
iwhichwall_5:
 krefz = gk_EarlyReflection_Ceiling
 goto iwhichwall_5_endif
iwhichwall_5_else:
 krefz = kz
iwhichwall_5_endif:

if iwhichwall == 6 goto iwhichwall_6
goto iwhichwall_6_else
iwhichwall_6:
 krefz = -gk_EarlyReflection_Ceiling
 goto iwhichwall_6_endif
iwhichwall_6_else:
 krefz = kz
iwhichwall_6_endif:

kA, kE, kD CartesianToPolar kx, ky, kz
kAref, kEref, kDref CartesianToPolar krefx, krefy, krefz
kDdiff = sqrt((kx - krefx)^2 + (ky-krefy)^2 + (kz - krefz)^2)
kDist = kDdiff + kDref
; Convert to meters.
kDmtr = kDist * gk_Spatialize_SpeakerRigRadius
; Delay time in milliseconds.
kdel = kDmtr / 0.345
; Reflection behind the source
kAdist = abs(kA - kAref)
kEdist = abs(kE - kEref)
; Reduce the angle kAdist to values between 0 and 2 pi.
kAdist = (kAdist > 6.2831 ? kAdist - 6.2831 : kAdist)
kmute = kAdist + kEdist
; Mute the reflection if it gets behind the source.
kmute = (kmute < 1.5707 ? sin(kmute) : 1)
; Attenuate the early reflection by distance.
k_gain = 1 / kDist * gk_EarlyReflection_damp * kmute
; Cut high frequencies by distance.
kDdiff = ( kDdiff < 0.05 ? 0.05 : kDdiff)
klfboost = (sqrt(1 / kDdiff ) + kD) * gk_EarlyReflection_lfboost
afilt tone asignal, (sqrt(1 / kDist)) * sr
; Boost low frequencies near the wall.
afilt pareq afilt, 174, klfboost, 0.7071, 1
afilt balance afilt, asignal
adel interp kdel
asigref vdelay3 afilt, adel, 200 ; milliseconds
asigref = asigref * k_gain
; Convert radians to degrees.
kAref = kAref * 57.2958
kEref = kEref * 57.2958
i_channels channels_for_rig
absignal[] init i_channels
absignal bformenc1 asigref, kAref, kEref
xout absignal
endop

opcode DiffuseEarlyReflections, a[], akkk
asignalin, kx, ky, kz xin
; Filter high frequencies to represent absorbtion by the wall.
asignal tone asignalin, gk_EarlyReflection_highdamp
i_channels channels_for_rig
absignal[] init i_channels
abreflectedSignal[] init i_channels
; Bounce the signal off each of the walls.
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 1
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 2
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 3
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 4
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 5
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal DiffuseEarlyReflect asignal, kx, ky, kz, 6
ambi_plus_add absignal, abreflectedSignal
xout absignal
endop

opcode SpecularEarlyReflection, a[], akkkik
asignal, kx, ky, kz, iwhichwall, kmydiff xin
ksum = abs(iwhichwall) + abs(kmydiff)
krefx = (kx / ksum) * abs(iwhichwall)
krefy = (ky / ksum) * abs(iwhichwall)
krefz = (kz / ksum) * abs(iwhichwall)
krefx = (iwhichwall = 1 ? gk_EarlyReflection_FrontWall : (kx / ksum) * abs(iwhichwall))
krefx = (iwhichwall = 2 ? -gk_EarlyReflection_FrontWall : (kx / ksum) * abs(iwhichwall))
krefy = (ky / ksum) * abs(iwhichwall)
krefz = (kz / ksum) * abs(iwhichwall)

if iwhichwall == 1 goto iwhichwall_1
 goto iwhichwall_1_else
 iwhichwall_1:
 krefx = gk_EarlyReflection_FrontWall
 goto iwhichwall_1_endif
iwhichwall_1_else:
 krefx = kx
iwhichwall_1_endif:

if iwhichwall == 2 goto iwhichwall_2
 goto iwhichwall_2_else
 iwhichwall_2:
 krefx = -gk_EarlyReflection_FrontWall
 goto iwhichwall_2_endif
iwhichwall_2_else:
 krefx = kx
iwhichwall_2_endif:

if iwhichwall == 3 goto iwhichwall_3
 goto iwhichwall_3_else
 iwhichwall_3:
 kfefy = gk_EarlyReflection_SideWall
 goto iwhichwall_3_endif
iwhichwall_3_else:
 krefy = ky
iwhichwall_3_endif:

if iwhichwall == 4 goto iwhichwall_4
 goto iwhichwall_4_else
 iwhichwall_4:
 krefy = -gk_EarlyReflection_SideWall
 goto iwhichwall_4_endif
iwhichwall_4_else:
 krefy = ky
iwhichwall_4_endif:

if iwhichwall == 5 goto iwhichwall_5
goto iwhichwall_5_else
iwhichwall_5:
 krefz = gk_EarlyReflection_Ceiling
 goto iwhichwall_5_endif
iwhichwall_5_else:
 krefz = kz
iwhichwall_5_endif:

if iwhichwall == 6 goto iwhichwall_6
goto iwhichwall_6_else
iwhichwall_6:
 krefz = -gk_EarlyReflection_Ceiling
 goto iwhichwall_6_endif
iwhichwall_6_else:
 krefz = kz
iwhichwall_6_endif:
; Prevent zero to prevent division by zero error (NaN) later.
krefx = (krefx < 0.001 && krefx > -0.001 ? 0.001 : krefx)
krefy = (krefy < 0.001 && krefy > -0.001 ? 0.001 : krefy)
krefz = (krefz < 0.001 && krefz > -0.001 ? 0.001 : krefz)
kA, kE, kD CartesianToPolar kx, ky, kz
kAref, kEref, kDref CartesianToPolar krefx, krefy, krefz
kDdiff = sqrt((kx - krefx)^2 + (ky-krefy)^2 + (kz - krefz)^2)
kDist = kDdiff + kDref
; Convert to meters.
kDmtr = kDist * gk_Spatialize_SpeakerRigRadius
; Delay time in milliseconds.
kdel = kDmtr / 0.345
; Reflection behind the source
kAdist = abs(kA - kAref)
kEdist = abs(kE - kEref)
; Reduce the angle kAdist to values between 0 and 2 pi.
kAdist = (kAdist > 6.2831 ? kAdist - 6.2831 : kAdist)
kmute = kAdist + kEdist
; Mute the reflection if it gets behind the source.
kmute = (kmute < 1.5707 ? sin(kmute) : 1)
; Attenuate the early reflection by distance.
k_gain = 1 / kDist * gk_EarlyReflection_damp * kmute
; Cut high frequencies by distance.
kDdiff = ( kDdiff < 0.05 ? 0.05 : kDdiff)
klfboost = (sqrt(1 / kDdiff ) + kD) * gk_EarlyReflection_lfboost
afilt tone asignal, (sqrt(1 / kDist)) * sr
; Boost low frequencies near the wall.
afilt pareq afilt, 174, klfboost, 0.7071, 1
afilt balance afilt, asignal
adel interp kdel
asigref vdelay3 afilt, adel, 200
asigref = asigref * k_gain
; Convert radians to degrees.
kAref = kAref * 57.2958
kEref = kEref * 57.2958
i_channels channels_for_rig
absignal[] init i_channels
absignal bformenc1 asigref, kAref, kEref
;;kprintlevels "SpecularEarlyReflect output", absignal
xout absignal
endop

opcode SpecularEarlyReflections, a[], akkk
asignal, kx, ky, kz xin
kiwall1 = gk_EarlyReflection_FrontWall
kiwall2 = -gk_EarlyReflection_FrontWall
kiwall3 = gk_EarlyReflection_SideWall
kiwall4 = -gk_EarlyReflection_SideWall
kiwall5 = gk_EarlyReflection_Ceiling
kiwall6 = -gk_EarlyReflection_Ceiling
kdiff1 = kiwall1 - kx ; distance of reflection to the FrontWall
kdiff2 = kiwall2 - kx ; distance of reflection to the backwall
kdiff3 = kiwall3 - ky ; distance of reflection to the left SideWall
kdiff4 = kiwall4 - ky ; distance of reflection to the right SideWall
kdiff5 = kiwall5 - kz ; distance of reflection to the Ceiling
kdiff6 = kiwall6 - kz ; distance of reflection to the floor
; Filter high frequencies to represent absorption by the walls.
afilteredSignal tone asignal, gk_EarlyReflection_highdamp
i_channels channels_for_rig
absignal[] init i_channels
abreflectedSignal[] init i_channels
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 1, kdiff1
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 2, kdiff2
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 3, kdiff3
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 4, kdiff4
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 5, kdiff5
ambi_plus_add absignal, abreflectedSignal
abreflectedSignal SpecularEarlyReflection afilteredSignal, kx, ky, kz, 6, kdiff6
ambi_plus_add absignal, abreflectedSignal
;;kprintlevels "SpecularEarlyReflections output", absignal
xout absignal
endop

opcode Spatialize, a[]a, akkk
asignal, kx, ky, kz xin
kdistance init 0
kazimuth, kelevation, kdistance CartesianToPolarDegrees kx, ky, kz
adistanceCuedSignal init 0
asend init 0
adoppleredSignal DopplerByDistance asignal, kdistance
afilteredSignal LowpassByDistance adoppleredSignal, kdistance

aglobalReverbSend init 0
i_channels channels_for_rig
abspatializedSignal[] init i_channels
if     gi_BformatDecoder_SpeakerRig == 0 then
abspatializedSignal bformenc1 afilteredSignal, kazimuth, kelevation
asend = afilteredSignal
elseif gi_BformatDecoder_SpeakerRig == 1 then
;; adistanceCuedSignal, aglobalReverbSend LocalReverbByDistance afilteredSignal, kdistance
abspatializedSignal bformenc1 adistanceCuedSignal, kazimuth, kelevation
asend = aglobalReverbSend
elseif gi_BformatDecoder_SpeakerRig >  1 then   
adistanceCuedSignal, aglobalReverbSend LocalReverbByDistance afilteredSignal, kdistance
abspatializedSignal bformenc1 adistanceCuedSignal, kazimuth, kelevation
i_channels channels_for_rig
abdiffuseEarlyReflectionsSignal[] init i_channels
abdiffuseEarlyReflectionsSignal DiffuseEarlyReflections adistanceCuedSignal, kx, ky, kz
ambi_plus_add abspatializedSignal, abdiffuseEarlyReflectionsSignal
abspecularEarlyReflectionsSignal[] init i_channels
abspecularEarlyReflectionsSignal SpecularEarlyReflections afilteredSignal, kx, ky, kz
ambi_plus_add abspatializedSignal, abspecularEarlyReflectionsSignal
;;kprintlevels "Spatialize output", abspatializedSignal
asend = aglobalReverbSend
endif

xout abspatializedSignal, asend
endop


gk_SpatialReverb_ReverbDecay chnexport "gk_SpatialReverb_ReverbDecay", 3
gk_SpatialReverb_ReverbDecay init 0.6
gk_SpatialReverb_CutoffHz chnexport "gk_SpatialReverb_CutoffHz", 3
gk_SpatialReverb_CutoffHz init sr
gk_SpatialReverb_RandomDelayModulation chnexport "gk_SpatialReverb_RandomDelayModulation", 3
gk_SpatialReverb_RandomDelayModulation init 1.0

instr SpatialReverb2
; 12 delay line FDN reverb, with feedback matrix based upon
; physical modeling scattering junction of 12 lossless waveguides
; of equal characteristic impedance. Based on Julius O. Smith III,
; "A New Approach to Digital Reverberation using Closed Waveguide
; Networks," Proceedings of the International Computer Music
; Conference 1985, p. 47-53 (also available as a seperate
; publication from CCRMA), as well as some more recent papers by
; Smith and others.
;
; Coded by Sean Costello, October 1999/ modified by Jan Jacob Hofmann August 2005
; Note: arevsig is the global input to the reverb.

; Set ksmps to 1 because feedback lines are used.
setksmps 1
arevsig inleta "in"
itone init i(gk_SpatialReverb_CutoffHz)
afilt1 init 0
afilt2 init 0
afilt3 init 0
afilt4 init 0
afilt5 init 0
afilt6 init 0
afilt7 init 0
afilt8 init 0
afilt9 init 0
afilt10 init 0
afilt11 init 0
afilt12 init 0
; Delay times chosen to be prime numbers.
; Works with sr=44100/ sr=48000 ONLY. If you wish to
; use a different delay time, find some new
; prime numbers that will give roughly the
; same delay times for the new sampling rate.
; Or adjust to taste.
if sr == 41000 goto sr_41000
goto sr_48000
sr_41000:
idel1 = (1237.000/sr)
idel2 = (1381.000/sr)
idel3 = (1609.000/sr)
idel4 = (1777.000/sr)
idel5 = (1951.000/sr)
idel6 = (2063.000/sr)
idel7 = (1069.000/sr)
idel8 = ( 967.000/sr)
idel9 = (1889.000/sr)
idel10 = (1693.000/sr)
idel11 = (1453.000/sr)
idel12 = (1291.000/sr)
goto sr_endif
sr_48000:
if sr == 48000 goto sr_48000_1
goto sr_endif
sr_48000_1:
idel1 = (1361.000/sr)
idel2 = (1499.000/sr)
idel3 = (1753.000/sr)
idel4 = (1933.000/sr)
idel5 = (2129.000/sr)
idel6 = (2243.000/sr)
idel7 = (1163.000/sr)
idel8 = (1051.000/sr)
idel9 = (2053.000/sr)
idel10 = (1693.000/sr)
idel11 = (1847.000/sr)
idel12 = (1409.000/sr)
sr_endif:
; k1-k8 are used to add random pitch modulation to the
; delay lines. Helps eliminate metallic overtones
; in the reverb sound.
k1 randi .001, 3.1, .06
k2 randi .0011, 3.5, .9
k3 randi .0017, 1.11, .7
k4 randi .0006, 3.973, .3
k5 randi .001, 2.341, .63
k6 randi .0011, 1.897, .17
k7 randi .0017, 0.891, .19
k8 randi .0006, 3.221, .44
k9 randi .001, 1.891, .88
k10 randi .0011, 2.317, .95
k11 randi .0017, 1.091, .36
k12 randi .0006, 2.821, .41
; apj is used to calculate "resultant junction pressure" for
; the scattering junction of e.g. 8 lossless waveguides
; of equal characteristic impedance. If you wish to
; add more delay lines, simply add them to the following
; equation, and replace the .25 by 2/N, where N is the
; number of delay lines.
apj = .16666666667 * (afilt1 + afilt2 + afilt3 + afilt4 + afilt5 + afilt6 +afilt7 + afilt8 + afilt9 + afilt10 +afilt11 + afilt12)
adum1 delayr 1
adel1 deltapi idel1 + k1 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt1
adum2 delayr 1
adel2 deltapi idel2 + k2 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt2
adum3 delayr 1
adel3 deltapi idel3 + k3 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt3
adum4 delayr 1
adel4 deltapi idel4 + k4 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt4
adum5 delayr 1
adel5 deltapi idel5 + k5 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt5
adum6 delayr 1
adel6 deltapi idel6 + k6 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt6
adum7 delayr 1
adel7 deltapi idel7 + k7 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt7
adum8 delayr 1
adel8 deltapi idel8 + k8 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt8
adum9 delayr 1
adel9 deltapi idel9 + k9 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt9
adum10 delayr 1
adel10 deltapi idel10 + k10 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt10
adum11 delayr 1
adel11 deltapi idel11 + k11 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt11
adum12 delayr 1
adel12 deltapi idel12 + k12 * gk_SpatialReverb_RandomDelayModulation
 delayw arevsig + apj - afilt12
; 1st order lowpass filters in feedback
; loops of delay lines.
afilt1 tone adel1 * gk_SpatialReverb_ReverbDecay, itone
afilt2 tone adel2 * gk_SpatialReverb_ReverbDecay, itone
afilt3 tone adel3 * gk_SpatialReverb_ReverbDecay, itone
afilt4 tone adel4 * gk_SpatialReverb_ReverbDecay, itone
afilt5 tone adel5 * gk_SpatialReverb_ReverbDecay, itone
afilt6 tone adel6 * gk_SpatialReverb_ReverbDecay, itone
afilt7 tone adel7 * gk_SpatialReverb_ReverbDecay, itone
afilt8 tone adel8 * gk_SpatialReverb_ReverbDecay, itone
afilt9 tone adel9 * gk_SpatialReverb_ReverbDecay, itone
afilt10 tone adel10 * gk_SpatialReverb_ReverbDecay, itone
afilt11 tone adel11 * gk_SpatialReverb_ReverbDecay, itone
afilt12 tone adel12 * gk_SpatialReverb_ReverbDecay, itone
; iA1-iA12 are the angles pointing to the 12 corners of an icosahedron.
; iE1-iE12 are the elevations pointing to the 12 corners of an icosahedron.
iA1 = 0 ; angle reverberant point 1
iA2 = 0.6238 ; angle reverberant point 2
iA3 = 1.2566 ; angle reverberant point 3
iA4 = 1.8849 ; angle reverberant point 4
iA5 = 2.5132 ; angle reverberant point 5
iA6 = 3.1415 ; angle reverberant point 6
iA7 = 3.7699 ; angle reverberant point 7
iA8 = 4.3982 ; angle reverberant point 8
iA9 = 5.0265 ; angle reverberant point 9
iA10 = 5.6548 ; angle reverberant point 10
iA11 = 0 ; angle reverberant point 11
iA12 = 0 ; angle reverberant point 12
iE1 = 0.463646 ; height reverberant point 1 ( 26.565?)
iE2 = -0.463646 ; height reverberant point 2 ( -26.565?)
iE3 = 0.463646 ; height reverberant point 3 ( 26.565?)
iE4 = -0.463646 ; height reverberant point 4 ( -26.565?)
iE5 = 0.463646 ; height reverberant point 5 ( 26.565?)
iE6 = -0.463646 ; height reverberant point 6 ( -26.565?)
iE7 = 0.463646 ; height reverberant point 7 ( 26.565?)
iE8 = -0.463646 ; height reverberant point 8 ( -26.565?)
iE9 = 0.463646 ; height reverberant point 9 ( 26.565?)
iE10 = -0.463646 ; height reverberant point 10 ( -26.565?)
iE11 = 1.570796 ; height reverberant point 11 ( 90?)
iE12 = -1.570796 ; height reverberant point 12 ( -90?)
i_channels channels_for_rig
aboutput[] init i_channels
abreverberated[] init i_channels
aboutput       bformenc1 afilt1, iA1, iE1
abreverberated bformenc1 afilt2, iA2, iE2
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt3, iA3, iE3
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt4, iA4, iE4
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt5, iA5, iE5
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt6, iA6, iE6
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt7, iA7, iE7
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt8, iA8, iE8
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
abreverberated bformenc1 afilt9, iA9, iE9
aboutput[ 0] = aboutput[ 0] + abreverberated[ 0]
aboutput[ 1] = aboutput[ 1] + abreverberated[ 1]
aboutput[ 2] = aboutput[ 2] + abreverberated[ 2]
aboutput[ 3] = aboutput[ 3] + abreverberated[ 3]
aboutput[ 4] = aboutput[ 4] + abreverberated[ 4]
aboutput[ 5] = aboutput[ 5] + abreverberated[ 5]
aboutput[ 6] = aboutput[ 6] + abreverberated[ 6]
aboutput[ 7] = aboutput[ 7] + abreverberated[ 7]
aboutput[ 8] = aboutput[ 8] + abreverberated[ 8]
aboutput[ 9] = aboutput[ 9] + abreverberated[ 9]
aboutput[10] = aboutput[10] + abreverberated[10]
aboutput[11] = aboutput[11] + abreverberated[11]
aboutput[12] = aboutput[12] + abreverberated[12]
aboutput[13] = aboutput[13] + abreverberated[13]
aboutput[14] = aboutput[14] + abreverberated[14]
aboutput[15] = aboutput[15] + abreverberated[15]
outletv "outbformat", aboutput
if gk_Spatialize_Verbose != 0 goto gk_Spatialize_Verbose_not_0
goto gk_Spatialize_Verbose_0
gk_Spatialize_Verbose_not_0:
kelapsed timeinsts
gk_Spatialize_Verbose_0:
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_SpatialReverb_Gain init 0.2
instr SpatialReverb
ainput inleta "in"
asignal = ainput / 32.0 * gk_SpatialReverb_Gain
;kcutoff = 22000 / gk_SpatialReverb_CutoffHz
kcutoff = gk_SpatialReverb_CutoffHz
aleft1, aright1 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt1 = aleft1 + aright1
aleft2, aright2 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt2 = aleft2 + aright2
aleft3, aright3 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt3 = aleft3 + aright3
aleft4, aright4 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt4 = aleft4 + aright4
aleft5, aright5 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt5 = aleft5 + aright5
aleft6, aright6 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt6 = aleft6 + aright6
aleft7, aright7 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt7 = aleft7 + aright7
aleft8, aright8 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt8 = aleft8 + aright8
aleft9, aright9 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt9 = aleft9 + aright9
aleft10, aright10 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt10 = aleft10 + aright10
aleft11, aright11 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt11 = aleft11 + aright11
aleft12, aright12 reverbsc asignal, asignal, gk_SpatialReverb_ReverbDecay, kcutoff, sr
afilt12 = aleft12 + aright12
afiltc1 tone afilt1, gk_SpatialReverb_CutoffHz
afiltc2 tone afilt2, gk_SpatialReverb_CutoffHz
afiltc3 tone afilt3, gk_SpatialReverb_CutoffHz
afiltc4 tone afilt4, gk_SpatialReverb_CutoffHz
afiltc5 tone afilt5, gk_SpatialReverb_CutoffHz
afiltc6 tone afilt6, gk_SpatialReverb_CutoffHz
afiltc7 tone afilt7, gk_SpatialReverb_CutoffHz
afiltc8 tone afilt8, gk_SpatialReverb_CutoffHz
afiltc9 tone afilt9, gk_SpatialReverb_CutoffHz
afiltc10 tone afilt10, gk_SpatialReverb_CutoffHz
afiltc11 tone afilt11, gk_SpatialReverb_CutoffHz
afiltc12 tone afilt12, gk_SpatialReverb_CutoffHz
; iA1-iA12 are the angles pointing to the 12 corners of an icosahedron.
; iE1-iE12 are the elevations pointing to the 12 corners of an icosahedron.
iA1 = 0 ; angle reverberant point 1
iA2 = 0.6238 ; angle reverberant point 2
iA3 = 1.2566 ; angle reverberant point 3
iA4 = 1.8849 ; angle reverberant point 4
iA5 = 2.5132 ; angle reverberant point 5
iA6 = 3.1415 ; angle reverberant point 6
iA7 = 3.7699 ; angle reverberant point 7
iA8 = 4.3982 ; angle reverberant point 8
iA9 = 5.0265 ; angle reverberant point 9
iA10 = 5.6548 ; angle reverberant point 10
iA11 = 0 ; angle reverberant point 11
iA12 = 0 ; angle reverberant point 12
iE1 = 0.463646 ; height reverberant point 1 ( 26.565?)
iE2 = -0.463646 ; height reverberant point 2 ( -26.565?)
iE3 = 0.463646 ; height reverberant point 3 ( 26.565?)
iE4 = -0.463646 ; height reverberant point 4 ( -26.565?)
iE5 = 0.463646 ; height reverberant point 5 ( 26.565?)
iE6 = -0.463646 ; height reverberant point 6 ( -26.565?)
iE7 = 0.463646 ; height reverberant point 7 ( 26.565?)
iE8 = -0.463646 ; height reverberant point 8 ( -26.565?)
iE9 = 0.463646 ; height reverberant point 9 ( 26.565?)
iE10 = -0.463646 ; height reverberant point 10 ( -26.565?)
iE11 = 1.570796 ; height reverberant point 11 ( 90?)
iE12 = -1.570796 ; height reverberant point 12 ( -90?)
i_channels channels_for_rig
aboutput[] init i_channels
abreverberated1[] init i_channels
abreverberated2[] init i_channels
abreverberated3[] init i_channels
abreverberated4[] init i_channels
abreverberated5[] init i_channels
abreverberated6[] init i_channels
abreverberated7[] init i_channels
abreverberated8[] init i_channels
abreverberated9[] init i_channels
abreverberated10[] init i_channels
abreverberated11[] init i_channels
abreverberated12[] init i_channels
ambi_zero aboutput
abreverberated1 bformenc1 afiltc1, iA1, iE1
ambi_plus_add aboutput, abreverberated1
abreverberated2 bformenc1 afiltc2, iA2, iE2
ambi_plus_add aboutput, abreverberated2
abreverberated2 bformenc1 afiltc3, iA3, iE3
ambi_plus_add aboutput, abreverberated3
abreverberated4 bformenc1 afiltc4, iA4, iE4
ambi_plus_add aboutput, abreverberated4
abreverberated5 bformenc1 afiltc5, iA5, iE5
ambi_plus_add aboutput, abreverberated5
abreverberated6 bformenc1 afiltc6, iA6, iE6
ambi_plus_add aboutput, abreverberated6
abreverberated7 bformenc1 afiltc7, iA7, iE7
ambi_plus_add aboutput, abreverberated7
abreverberated8 bformenc1 afiltc8, iA8, iE8
ambi_plus_add aboutput, abreverberated8
abreverberated9 bformenc1 afiltc9, iA9, iE9
ambi_plus_add aboutput, abreverberated9
abreverberated10 bformenc1 afiltc10, iA10, iE10
ambi_plus_add aboutput, abreverberated10
abreverberated11 bformenc1 afiltc11, iA11, iE11
ambi_plus_add aboutput, abreverberated11
abreverberated12 bformenc1 afiltc12, iA12, iE12
ambi_plus_add aboutput, abreverberated12
kprintlevels "SpatialReverb output", aboutput
outletv "outbformat", aboutput
if gk_Spatialize_Verbose != 0 goto gk_Spatialize_Verbose_not_0
goto gk_Spatialize_Verbose_0
gk_Spatialize_Verbose_not_0:
aoutput sum aboutput[0], aboutput[1], aboutput[2], aboutput[3], aboutput[4], aboutput[5], aboutput[6], aboutput[7], aboutput[8], aboutput[9], aboutput[10], aboutput[11], aboutput[12], aboutput[13], aboutput[14], aboutput[15]
kelapsed timeinsts
gk_Spatialize_Verbose_0:
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

gk_Bformat2DecodeBinaural_krelcoef init 1
opcode Bformat2DecodeBinaural, a[], a[]i
absignal[], isr xin
krelcoef = gk_Bformat2DecodeBinaural_krelcoef
aw = absignal[ 0]
ax = absignal[ 1]
ay = absignal[ 2]
az = absignal[ 3]
ar = absignal[ 4]
as = absignal[ 5]
at = absignal[ 6]
au = absignal[ 7]
av = absignal[ 8]
; Decode 2nd order ambisonic as if it is a 12 channel dodecahedron loudspeaker rig.
; This configuration produces an 'in-phase' response and each speaker will have only a single maximum given a moving virtual sound source.
; w x y z r s t u v
; Speaker 1 <0.0000,0.0000,1.0000> centre top
achnl_co_1 sum aw * 0.1179, az * 0.1316, ar * 0.0491
; Speaker 2 <0.0000,0.0000,-1.0000> centre bottom
achnl_co_2 sum aw * 0.1179, az * -0.1316, ar * 0.0491
; Speaker 3 <0.7236,0.5257,0.4472>
achnl_co_3 sum aw * 0.1179, ax * 0.0952, ay * 0.0692, az * 0.0589, ar * -0.0098, as * 0.0238, at * 0.0173, au * 0.0091, av * 0.0280
; Speaker 4 <-0.7236,-0.5257,-0.4472>
achnl_co_4 sum aw * 0.1179, ax * -0.0952, ay * -0.0692, az * -0.0589, ar * -0.0098, as * 0.0238, at * 0.0173, au * 0.0091, av * 0.0280
; Speaker 5 <0.7236,-0.5257,0.4472>
achnl_co_5 sum aw * 0.1179, ax * 0.0952, ay * -0.0692, az * 0.0589, ar * -0.0098, as * 0.0238, at * -0.0173, au * 0.0091, av * -0.0280
; Speaker 6 <-0.7236,0.5257,-0.4472>
achnl_co_6 sum aw * 0.1179, ax * -0.0952, ay * 0.0692, az * -0.0589, ar * -0.0098, as * 0.0238, at * -0.0173, au * 0.0091, av * -0.0280
; Speaker 7 <-0.2764,0.8507,0.4472>
achnl_co_7 sum aw * 0.1179, ax * -0.0364, ay * 0.1119, az * 0.0589, ar * -0.0098, as * -0.0091, at * 0.0280, au * -0.0238, av * -0.0173
; Speaker 8 <0.2764,-0.8507,-0.4472>
achnl_co_8 sum aw * 0.1179, ax * 0.0364, ay * -0.1119, az * -0.0589, ar * -0.0098, as * -0.0091, at * 0.0280, au * -0.0238, av * -0.0173
; Speaker 9 <-0.2764,-0.8507,0.4472>
achnl_co_9 sum aw * 0.1179, ax * -0.0364, ay * -0.1119, az * 0.0589, ar * -0.0098, as * -0.0091, at * -0.0280, au * -0.0238, av * 0.0173
; Speaker 10 <0.2764,0.8507,-0.4472>
achnl_co_10 sum aw * 0.1179, ax * 0.0364, ay * 0.1119, az * -0.0589, ar * -0.0098, as * -0.0091, at * -0.0280, au * -0.0238, av * 0.0173
; Speaker 11 <-0.8944,0.0000,0.4472>
achnl_co_11 sum aw * 0.1179, ax * -0.1177, az * 0.0589, ar * -0.0098, as * -0.0295, au * 0.0295
; Speaker 12 <0.8944,0.0000,-0.4472>
achnl_co_12 sum aw * 0.1179, ax * 0.1177, az * -0.0589, ar * -0.0098, as * -0.0295, au * 0.0295
; Rig Decode Matrix to Reproduce Spherical Harmonics (Second Order)
; This rig configuration produces a strict idealised response that satisfies the Ambisonic matching equations. Generally this type of configuration produces a relatively small stable listening area.
; w x y z r s t u v
; Speaker 1 <0.0000,0.0000,1.0000> centre top
achnl_sh_1 sum aw * 0.1179, az * 0.2500, ar * 0.4167
; Speaker 2 <0.0000,0.0000,-1.0000> centre bottom
achnl_sh_2 sum aw * 0.1179, az * -0.2500, ar * 0.4167
; Speaker 3 <0.7236,0.5257,0.4472>
achnl_sh_3 sum aw * 0.1179, ax * 0.1809, ay * 0.1314, az * 0.1118, ar * -0.0833, as * 0.2023, at * 0.1469, au * 0.0773, av * 0.2378
; Speaker 4 <-0.7236,-0.5257,-0.4472>
achnl_sh_4 sum aw * 0.1179, ax * -0.1809, ay * -0.1314, az * -0.1118, ar * -0.0833, as * 0.2023, at * 0.1469, au * 0.0773, av * 0.2378
; Speaker 5 <0.7236,-0.5257,0.4472>
achnl_sh_5 sum aw * 0.1179, ax * 0.1809, ay * -0.1314, az * 0.1118, ar * -0.0833, as * 0.2023, at * -0.1469, au * 0.0773, av * -0.2378
; Speaker 6 <-0.7236,0.5257,-0.4472>
achnl_sh_6 sum aw * 0.1179, ax * -0.1809, ay * 0.1314, az * -0.1118, ar * -0.0833, as * 0.2023, at * -0.1469, au * 0.0773, av * -0.2378
; Speaker 7 <-0.2764,0.8507,0.4472>
achnl_sh_7 sum aw * 0.1179, ax * -0.0691, ay * 0.2127, az * 0.1118, ar * -0.0833, as * -0.0773, at * 0.2378, au * -0.2023, av * -0.1469
; Speaker 8 <0.2764,-0.8507,-0.4472>
achnl_sh_8 sum aw * 0.1179, ax * 0.0691, ay * -0.2127, az * -0.1118, ar * -0.0833, as * -0.0773, at * 0.2378, au * -0.2023, av * -0.1469
; Speaker 9 <-0.2764,-0.8507,0.4472>
achnl_sh_9 sum aw * 0.1179, ax * -0.0691, ay * -0.2127, az * 0.1118, ar * -0.0833, as * -0.0773, at * -0.2378, au * -0.2023, av * 0.1469
; Speaker 10 <0.2764,0.8507,-0.4472>
achnl_sh_10 sum aw * 0.1179, ax * 0.0691, ay * 0.2127, az * -0.1118, ar * -0.0833, as * -0.0773, at * -0.2378, au * -0.2023, av * 0.1469
; Speaker 11 <-0.8944,0.0000,0.4472>
achnl_sh_11 sum aw * 0.1179, ax * -0.2236, az * 0.1118, ar * -0.0833, as * -0.2500, au * 0.2500
; Speaker 12 <0.8944,0.0000,-0.4472>
achnl_sh_12 sum aw * 0.1179, ax * 0.2236, az * -0.1118, ar * -0.0833, as * -0.2500, au * 0.2500
achnl_1 = achnl_co_1 * krelcoef + achnl_sh_1 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_2 = achnl_co_2 * krelcoef + achnl_sh_2 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_3 = achnl_co_3 * krelcoef + achnl_sh_3 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_4 = achnl_co_4 * krelcoef + achnl_sh_4 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_5 = achnl_co_5 * krelcoef + achnl_sh_5 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_6 = achnl_co_6 * krelcoef + achnl_sh_6 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_7 = achnl_co_7 * krelcoef + achnl_sh_7 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_8 = achnl_co_8 * krelcoef + achnl_sh_8 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_9 = achnl_co_9 * krelcoef + achnl_sh_9 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_10 = achnl_co_10 * krelcoef + achnl_sh_10 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_11 = achnl_co_11 * krelcoef + achnl_sh_11 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
achnl_12 = achnl_co_12 * krelcoef + achnl_sh_12 * (1 - krelcoef) ; ratio between controlled opposites and spherical harmonic coefficients
if isr == 44100 goto isr_44100
goto isr_48000
isr_44100:
aleft_1, aright1 hrtfstat achnl_1, 1, 89.99, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_2, aright2 hrtfstat achnl_2, -1, -89.99, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-44100-left.dat", "hrtf-44100-right.dat", 8.8, 44100
goto isr_endif
isr_48000:
if isr == 48000 goto isr_48000_1
goto isr_96000
isr_48000_1:
aleft_1, aright_1 hrtfstat achnl_1, 1, 89.99, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_2, aright_2 hrtfstat achnl_2, -1, -89.99, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-48000-left.dat", "hrtf-48000-right.dat", 8.8, 48000
isr_96000:
if isr == 96000 goto isr_96000_1
goto isr_endif
isr_96000_1:
aleft_1, aright_1 hrtfstat achnl_1, 1, 89.99, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_2, aright_2 hrtfstat achnl_2, -1, -89.99, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_3, aright_3 hrtfstat achnl_3, -36, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_4, aright_4 hrtfstat achnl_4, 144, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_5, aright_5 hrtfstat achnl_5, 36, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_6, aright_6 hrtfstat achnl_6, -144, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_7, aright_7 hrtfstat achnl_7, -108, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_8, aright_8 hrtfstat achnl_8, 72, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_9, aright_9 hrtfstat achnl_9, 108, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_10, aright_10 hrtfstat achnl_10, -72, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_11, aright_11 hrtfstat achnl_11, 180, 26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
aleft_12, aright_12 hrtfstat achnl_12, 0, -26.565, "hrtf-96000-left.dat", "hrtf-96000-right.dat", 8.8, 96000
;else
;printks "Sampling rate = %f is not handled by Bformat2DecodeBinaural.", 1, sr
;exitnow
;endif
;endif
;endif
isr_endif:
prints "Loaded HRTF data.\n"
; Inter-aural time delay (seconds)
aleft_1 delay aleft_1, 0.00028986
aleft_2 delay aleft_2, 0.00028986
aleft_3 delay aleft_3, 0.00020974
aleft_4 delay aleft_4, 0.00036997
aleft_5 delay aleft_5, 0.00036997
aleft_6 delay aleft_6, 0.00020974
aleft_7 delay aleft_7, 0.00008011
aleft_8 delay aleft_8, 0.00049960
aleft_9 delay aleft_9, 0.00049960
aleft_10 delay aleft_10, 0.00008011
aleft_11 delay aleft_11, 0.00028986
aleft_12 delay aleft_12, 0.00028986
aright_1 delay aright_1, 0.00028986
aright_2 delay aright_2, 0.00028986
aright_3 delay aright_3, 0.00036997
aright_4 delay aright_4, 0.00020974
aright_5 delay aright_5, 0.00020974
aright_6 delay aright_6, 0.00036997
aright_7 delay aright_7, 0.000499597
aright_8 delay aright_8, 0.00008011
aright_9 delay aright_9, 0.00008011
aright_10 delay aright_10, 0.000499597
aright_11 delay aright_11, 0.00028986
aright_12 delay aright_12, 0.00028986
asignal[] init 2
aleft sum aright_1, aright_2, aright_3, aright_4, aright_5, aright_6, aright_7, aright_8, aright_9, aright_10, aright_11, aright_12
aright sum aleft_1, aleft_2, aleft_3, aleft_4, aleft_5, aleft_6, aleft_7, aleft_8, aleft_9, aleft_10, aleft_11, aleft_12
asignal[0] dcblock aleft
asignal[1] dcblock aright
xout asignal
endop

gk_BformatDecoder_MasterLevel init 0.0
instr BformatDecoder
irig init i(gi_BformatDecoder_SpeakerRig)
kmasterlevel = pow(10, gk_BformatDecoder_MasterLevel / 20)
asignal[] init nchnls
i_channels channels_for_rig
absignal[] init i_channels
absignal[] inletv "inbformat"
if irig == 0 goto irig_0
goto irig_1
irig_0:
 asignal bformdec1 1, absignal
 goto irig_endif
irig_1:
if irig == 1 goto irig_1_1
goto irig_2
irig_1_1:
 asignal Bformat2DecodeBinaural, absignal, sr
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
goto irig_endif
irig_2:
if irig == 2 goto irig_2_1
goto irig_3
irig_2_1:
 asignal bformdec1 2, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
 goto irig_endif
irig_3:
if irig == 3 goto irig_3_1
goto irig_else
irig_3_1:
 asignal bformdec1 3, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
 asignal[4] = asignal[4] * kmasterlevel
 goto irig_endif
irig_else:
 asignal bformdec1 irig, absignal
 asignal[0] = asignal[0] * kmasterlevel
 asignal[1] = asignal[1] * kmasterlevel
 asignal[2] = asignal[2] * kmasterlevel
 asignal[3] = asignal[3] * kmasterlevel
 asignal[4] = asignal[4] * kmasterlevel
 asignal[5] = asignal[5] * kmasterlevel
 asignal[6] = asignal[6] * kmasterlevel
 asignal[7] = asignal[7] * kmasterlevel
irig_endif:
out asignal
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

/*
0. Dual decoder (velocity and energy decoders using dual-band splitting).
1. Velocity decoder.
2. Energy decoder.
*/
gi_BformatDecoder2_Decoder chnexport "gi_BformatDecoder2_Decoder", 3 ; 0
gi_BformatDecoder2_Decoder init 0
/*
Radius of speaker rig in meters.
*/
gi_BformatDecoder2_Distance chnexport "gi_BformatDecoder2_Distance", 3 ; 0
gi_BformatDecoder2_Distance init 1
/*
Only effective for dual decoder.
*/
gi_BformatDecoder2_SplitHz chnexport "gi_BformatDecoder2_SplitHz", 3 ; 0
gi_BformatDecoder2_SplitHz init 400
/*
0. Energy
1. RMS
2. Amplitude
*/
gi_BformatDecoder2_DecoderMix chnexport "gi_BformatDecoder2_DecoderMix", 3 ; 0
gi_BformatDecoder2_DecoderMix init 0
gS_BformatDecoder2_LeftFile init "hrtf-96000-left.dat"
gS_BformatDecoder2_RightFile init "hrtf-96000-right.dat"
gk_BformatDecoder2_MasterLevel chnexport "gk_BformatDecoder2_MasterLevel", 3 ; 0
gk_BformatDecoder2_MasterLevel init 0
instr BformatDecoder2
kmasterlevel = pow(10, gk_BformatDecoder2_MasterLevel / 20)
printks2 "gk_BformatDecoder2_MasterLevel", gk_BformatDecoder2_MasterLevel
asignal[] init nchnls
aoutsignal[] init nchnls
i_channels channels_for_rig
absignal[] init i_channels
absignal[] inletv "inbformat"
// aout[] bformdec2 isetup, abform[], [idecoder, idistance, ifreq, imix, ifilel, ifiler]
asignal bformdec2 gi_BformatDecoder_SpeakerRig, absignal, gi_BformatDecoder2_Decoder, gi_BformatDecoder2_Distance, gi_BformatDecoder2_SplitHz, gi_BformatDecoder2_DecoderMix, gS_BformatDecoder2_LeftFile, gS_BformatDecoder2_RightFile
aoutsignal = asignal * kmasterlevel
out aoutsignal
;if gk_Spatialize_Verbose == 0 then
printks "%-24s in  rig %3d %9.4f %9.4f %9.4f %9.4f #%3d\n", 2., nstrstr(p1), gi_BformatDecoder_SpeakerRig, k(absignal[0]), k(absignal[1]), k(absignal[2]), k(absignal[3]), active(p1)
printks "%-24s out rig %3d %9.4f %9.4f %9.4f %9.4f #%3d\n", 2., "         ", gi_BformatDecoder_SpeakerRig, k(asignal[0]), k(asignal[1]), k(asignal[2]), k(asignal[3]), active(p1)
;lllendif
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin


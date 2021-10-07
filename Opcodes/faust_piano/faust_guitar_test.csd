<CsoundSynthesizer>

<CsOptions>
-m165 -otemp.wav -m195 -Fbachfug.mid --midi-key=4 --midi-velocity=5 --terminate-on-midi
</CsOptions>

<CsInstruments>
sr = 48000
ksmps = 100
nchnls = 2
0dbfs = 1

gk_FaustGuitar_level chnexport "gk_FaustGuitar_level", 3 ;  0
gk_FaustGuitar_midi_dynamic_range chnexport "gk_FaustGuitar_midi_dynamic_range", 3 ;  20
gk_FaustGuitar_bend chnexport "gk_FaustGuitar_bend", 3 ;  20
gk_FaustGuitar_gain chnexport "gk_FaustGuitar_gain", 3 ;  20
gk_FaustGuitar_sustain chnexport "gk_FaustGuitar_sustain", 3 ;  20
gk_FaustGuitar_shape chnexport "gk_FaustGuitar_shape", 3 ;  20
gk_FaustGuitar_scale chnexport "gk_FaustGuitar_scale", 3 ;  20
gk_FaustGuitar_tapBody chnexport "gk_FaustGuitar_tapBody", 3 ;  20
gk_FaustGuitar_pluckPosition chnexport "gk_FaustGuitar_pluckPosition", 3 ;  20
gk_FaustGuitar_outGain chnexport "gk_FaustGuitar_outGain", 3 ;  20

gk_FaustGuitar_level init -10
gk_FaustGuitar_midi_dynamic_range init 60
gk_FaustGuitar_bend init 0
gk_FaustGuitar_gain init .5
gk_FaustGuitar_sustain init 1
gk_FaustGuitar_shape init .57
gk_FaustGuitar_scale init .517
gk_FaustGuitar_tapBody init 0
gk_FaustGuitar_pluckPosition init .09
gk_FaustGuitar_outGain init 1


gk_FaustModularBody_level init -10


instr FaustGuitar
//////////////////////////////////////////////
//  Instrument definition patch FaustGuitar.
//  Author: Michael Gogins
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_FaustGuitar_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_frequency = cpsmidinn(i_midi_key)
//  Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 53.5 + 12
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FaustGuitar_level)
i_attack = .005
i_sustain = p3
i_release = .1
xtratim i_attack + i_release

ip01_freq init i_frequency
kp02_bend init i(gk_FaustGuitar_bend)
ip03_gain init i(gk_FaustGuitar_gain)
ip04_sustain init i(gk_FaustGuitar_sustain)
ip05_shape init i(gk_FaustGuitar_shape)
ip06_scale init i(gk_FaustGuitar_scale)
ip07_tapBody init i(gk_FaustGuitar_tapBody)
kp08_pluckPosition init i(gk_FaustGuitar_pluckPosition)
ip09_outGain init i(gk_FaustGuitar_outGain)
ip10_gate init 1

;print ip01_freq
;print i(kp02_bend)
;print ip03_gain
;print ip04_sustain
;print ip05_shape
;print ip06_scale
;print ip07_tapBody
;print i(kp08_pluckPosition)
;print ip09_outGain
;print ip10_gate

a_left, a_right modularInterpInstrMIDI ip01_freq, kp02_bend, ip03_gain, ip04_sustain, ip05_shape, ip06_scale, ip07_tapBody, kp08_pluckPosition, ip09_outGain, ip10_gate
a_signal = a_left + a_right
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
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
endin

;#include "FaustModularBody.inc"

#include "MasterOutput.inc"

connect "FaustModularBody",  "outleft",  "MasterOutput",        "inleft"
connect "FaustModularBody",  "outright", "MasterOutput",        "inright"
connect "FaustGuitar",  "outleft",  "MasterOutput",        "inleft"
connect "FaustGuitar",  "outright", "MasterOutput",        "inright"

alwayson "MasterOutput.inc"

instr Clocker
if p2 == 0 then
gi_started date
print gi_started
endif
gk_current  date
gk_elapsed = gk_current - gi_started
printks "elapsed time: %9.4f\n", .1, gk_elapsed
endin

alwayson "Clocker"

</CsInstruments>
<CsScore>
</CsScore>
</CsoundSynthesizer>

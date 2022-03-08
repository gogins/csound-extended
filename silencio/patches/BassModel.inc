gk_BassModel_level chnexport "gk_BassModel_level", 3
gi_BassModel_attack chnexport "gi_BassModel_attack", 3
gi_BassModel_release chnexport "gi_BassModel_release", 3
gk_BassModel_midi_dynamic_range chnexport "gk_BassModel_midi_dynamic_range", 3

gk_BassModel_level init 0
gi_BassModel_attack init 0.003
gi_BassModel_release init 0.01
gk_BassModel_midi_dynamic_range init 20

gi_BassModel_sine ftgen 0, 0, 65537, 10, 1

instr BassModel
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_time = p2
// If p3 is greater than 0, the performance is score-driven. To prevent 
// a click at release time, we set the sustain segment to p3, and 
// extend p3 so that the release will complete before the instance is 
// deleted.
if p3 > 0 then
 i_sustain = p3
endif
// If p3 is less than 0, the performance is MIDI-driven or otherwise 
// real-time. To enable the envelope to work, we set the sustain segment to a 
// large value, and count on the release segment of the envelope to be 
// triggered by the note off, and thus to complete without a click. However, 
// if the d opcode is used to turn the instance off, there will be a click 
// instead of a smooth release.
if p3 < 0 then
 i_sustain = 1000
endif
i_midi_key = p4
while i_midi_key > 100 do
  i_midi_key -= 12
od
i_midi_dynamic_range = i(gk_BassModel_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 77
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_BassModel_level)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; p1 p2 p3 p4 p5 p6
; Start Dur Amp Pitch PluckDur
; i2 128 4 1400 6.00 0.25
; i2 + 2 1200 6.01 0.25
; i2 . 4 1000 6.05 0.5
; i2 . 2 500 6.04 1
; i2 . 4 1000 6.03 0.5
; i2 . 16 1000 6.00 0.5
iHz = i_frequency
ifqc = iHz
ip4 = i_amplitude
ip6 = 0.5
ipluck = 1 / ifqc * ip6
kcount init 0
adline init 0
ablock2 init 0
ablock3 init 0
afiltr init 0
afeedbk init 0
koutenv linseg 0, .01, 1, p3 - .11 , 1, .1 , 0 ; Output envelope
kfltenv linseg 0, 1.5, 1, 1.5, 0 
; This envelope loads the string with a triangle wave.
kenvstr linseg 0, ipluck / 4, -ip4 / 2, ipluck / 2, ip4 / 2, ipluck / 4, 0, p3 - ipluck, 0
aenvstr = kenvstr
ainput tone aenvstr, 200
; DC Blocker
ablock2 = afeedbk - ablock3 + .99 * ablock2
ablock3 = afeedbk
ablock = ablock2
; Delay line with filtered feedback
adline delay ablock + ainput, 1 / ifqc - 15 / sr
afiltr tone adline, 400
; Resonance of the body 
abody1 reson afiltr, 110, 40
abody1 = abody1 / 5000
abody2 reson afiltr, 70, 20
abody2 = abody2 / 50000
afeedbk = afiltr
aout = afeedbk
a_signal = 50 * koutenv * (aout + kfltenv * (abody1 + abody2))
a_declicking linsegr 0, gi_BassModel_attack, 1, i_sustain, 1, gi_BassModel_release, 0
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
prints "%-24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "BassModel     i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gi_StringPad_sine ftgen 0, 0, 65537, 10, 1
opcode StringPad_Chorus, aa, aaii
a1, a2, idlyml, ifrequency xin
k1 oscili idlyml/ifrequency, 1, gi_StringPad_sine
ar1l vdelay3 a1, idlyml/5+k1, 900 ;delayed sound 1
ar1r vdelay3 a2, idlyml/5+k1, 900 ;delayed sound 1
k2 oscili idlyml/ifrequency, .995, gi_StringPad_sine
ar2l vdelay3 a1, idlyml/5+k2, 700 ;delayed sound 2
ar2r vdelay3 a2, idlyml/5+k2, 700 ;delayed sound 2
k3 oscili idlyml/ifrequency, 1.05, gi_StringPad_sine
ar3l vdelay3 a1, idlyml/5+k3, 700 ;delayed sound 3
ar3r vdelay3 a2, idlyml/5+k3, 700 ;delayed sound 3
k4 oscili idlyml/ifrequency, 1, gi_StringPad_sine
ar4l vdelay3 a1, idlyml/5+k4, 900 ;delayed sound 4
ar4r vdelay3 a2, idlyml/5+k4, 900 ;delayed sound 4
aoutl = (a1+ar1l+ar2l+ar3l+ar4l)*.5
aoutr = (a2+ar1r+ar2r+ar3r+ar4r)*.5
xout aoutl, aoutr
endop

gk_StringPad_level init 0
gi_StringPad_attack init 0.003
gi_StringPad_release init 0.01
// TODO: Randomize this?
gi_StringPad_position init .76
gi_StringPad_chorus_delay_milliseconds init 10
gi_StringPad_chorus_frequency init 30
gk_StringPad_midi_dynamic_range init 127
gi_StringPad_pad ftgen 0, 0, 65537, 10, 1, .5, .33, .25,  .0, .1,  .1, .1
gi_StringPad_sine ftgen 0, 0, 65537, 10, 1
instr StringPad
//////////////////////////////////////////////
// StringPad originally by Anthony Kozar.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_StringPad_attack + gi_StringPad_release
i_midi_key = p4
i_midi_dynamic_range = i(gk_StringPad_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
// Spatial location is specified in Ambisonic coordinates.
k_space_front_to_back = p6
// AKA stereo pan.
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
// Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 174
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_StringPad_level)

; String-pad borrowed from the piece "Dorian Gray",
; http://akozar.spymac.net/music/ Modified to fit my needs
ihz = i_frequency
idb = i_midi_velocity
ipos = gi_StringPad_position
iamp = ampdb(idb)
; Slow attack and release
kctrl linseg 0, p3/4, iamp, p3/2, 0 
; Slight chorus effect
afund oscili kctrl, ihz, gi_StringPad_pad ; audio oscillator
acel1 oscili kctrl, ihz - .1, gi_StringPad_pad ; audio oscillator - flat
acel2 oscili kctrl, ihz + .1, gi_StringPad_pad ; audio oscillator - sharp
a_signal = afund + acel1 + acel2
; Cut-off high frequencies depending on midi-velocity
; (larger velocity implies more brighter sound)
a_signal butterlp a_signal, (p5-60)*40+900
a_declicking linsegr 0, gi_StringPad_attack, 1, i_sustain, 1, gi_StringPad_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain
#ifdef USE_SPATIALIZATION
a_spatial_reverb_send init 0
a_bsignal[] init 16
a_bsignal, a_spatial_reverb_send Spatialize a_signal, k_space_front_to_back, k_space_left_to_right, k_space_bottom_to_top
outletv "outbformat", a_bsignal
outleta "out", a_spatial_reverb_send
#else
a_out_left, a_out_right pan2 a_signal, k_space_left_to_right
a_out_left, a_out_right StringPad_Chorus a_out_left, a_out_right, 10, 20
outleta "outleft", a_out_left
outleta "outright", a_out_right
#endif
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
;printks "StringPad i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

gk_FMModulatedChorus_level init 0
gi_FMModulatedChorus_attack init 0.003
gi_FMModulatedChorus_release init 0.01
gk_FMModulatedChorus_midi_dynamic_range init 127
gi_FMModulatedChorus_ln ftgen 0, 0, 65536, -12, 20.0 ; Unscaled ln(I(x)) from 0 to 20.0.
gi_FMModulatedChorus_cosine ftgen 0, 0, 65536, 11, 1 ; Cosine wave. Get that noise down on the most widely used table!
gi_FMModulatedChorus_sine ftgen 0, 0, 65536, 10, 1
instr FMModulatedChorus
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
i_instrument = p1
i_time = p2
i_sustain = p3
xtratim gi_FMModulatedChorus_attack + gi_FMModulatedChorus_release
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_FMModulatedChorus_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 85.2
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_FMModulatedChorus_level)

i_modr_start = 0.3
i_modr_end = 2.2
; shift it.
ishift = 4.0 / 12000
; convert parameter 5 to oct.
ioct = i_midi_key
akadsr linsegr 0, gi_FMModulatedChorus_attack, 1, i_sustain, 1, gi_FMModulatedChorus_release, 0
akmodi linseg 0, gi_FMModulatedChorus_attack, 5, i_sustain, 2, gi_FMModulatedChorus_release, 0
; r moves from i_modr_start to i_modr_end in p3 secs.
akmodr linseg i_modr_start, i_sustain, i_modr_end
a1 = akmodi * (akmodr - 1 / akmodr) / 2
; a1*2 is argument normalized from 0-1.
a1ndx = abs(a1 * 2 / 20)
a2 = akmodi * (akmodr + 1 / akmodr) / 2
a3 tablei a1ndx, gi_FMModulatedChorus_ln, 1
ao1 oscili a1, i_frequency, gi_FMModulatedChorus_cosine
a4 = exp(-0.5 * a3 + ao1)
; Cosine
ao2 oscili a2 * i_frequency, i_frequency, gi_FMModulatedChorus_cosine
; Final output left
aoutl oscili 1 * akadsr * a4, ao2 + cpsmidinn(ioct + ishift), gi_FMModulatedChorus_sine
; Final output right
aoutr oscili 1 * akadsr * a4, ao2 + cpsmidinn(ioct - ishift), gi_FMModulatedChorus_sine
asignal = aoutl + aoutr
a_signal = asignal * i_amplitude

a_declicking linsegr 0, gi_FMModulatedChorus_attack, 1, i_sustain, 1, gi_FMModulatedChorus_release, 0
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
;printks "FMModulatedChorus i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

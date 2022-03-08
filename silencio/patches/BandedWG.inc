gk_BandedWG_level chnexport "gk_BandedWG_level", 3
gi_BandedWG_attack chnexport "gi_BandedWG_attack", 3
gi_BandedWG_release chnexport "gi_BandedWG_release", 3
gk_BandedWG_midi_dynamic_range chnexport "gk_BandedWG_midi_dynamic_range", 3

gk_BandedWG_level init 0
gi_BandedWG_attack init 0.01
gi_BandedWG_release init 0.05
gk_BandedWG_midi_dynamic_range init 20

instr BandedWG
//////////////////////////////////////////////
// Original by Hans Mikelson.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
xtratim gi_BandedWG_attack + gi_BandedWG_release
a_declicking linsegr 0, gi_BandedWG_attack, 1, p3, 1, gi_BandedWG_release, 0

i_instrument = p1
i_time = p2
i_sustain = p3
i_duration = p3
i_midi_key = p4
while i_midi_key < 44 do
  i_midi_key = i_midi_key + 12
od
while i_midi_key > 90 do
  i_midi_key = i_midi_key - 12
od
i_midi_dynamic_range = i(gk_BandedWG_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.6 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 41
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_BandedWG_level)
a_signal STKBandedWG i_frequency, 1.
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
;printks "BandedWG      i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d l%9.4f r%9.4f\n", 1, p1, p2, p3, p4, p5, p7, active(p1), dbamp(rms(a_out_left)), dbamp(rms(a_out_right))
endin

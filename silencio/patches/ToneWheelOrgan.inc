gk_ToneWheelOrgan_level init 0
instr ToneWheelOrgan
; Author: Hans Mikelson
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 112
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ToneWheelOrgan_level)
gk_Harpsichord_pan = .5
iattack = .002
isustain = p3
irelease = .3
xtratim iattack + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
iphase = 0.25
ikey = p4 ;12 * int(p4 - 6) + 100 * (p4 - 6)
kfqc = kHz
isine ftgenonce 0, 0, 65537, 10, 1
; The lower tone wheels have increased odd harmonic content.
; Rotor Tables
itw1 ftgenonce 0, 0, 65537, 10, 1, 0.02, .01
itw2 ftgenonce 0, 0, 65537, 10, 1, 0, 0.2, 0, 0.1, 0, .05, 0, .02
; Rotating Speaker Filter Envelopes
itw3 ftgenonce 0, 0, 65537, 7, 0, 110, 0, 18, 1, 18, 0, 110, 0
itw4 ftgenonce 0, 0, 65537, 7, 0, 80, 0.2, 16, 1, 64, 1, 16, 0.2, 80, 0
; Distortion Tables
itw5 ftgenonce 0, 0, 65537, 8, -.8, 336, -.78, 800, -.7, 5920, .7, 800, .78, 336, .8
itw6 ftgenonce 0, 0, 65537, 8 -.8, 336, -.76, 3000, -.7, 1520, .7, 3000, .76, 336, .8
; Table for Reed Physical Model
iwheel1 = ((ikey - 12) > 12 ? itw1 : itw2)
iwheel2 = ((ikey + 7) > 12 ? itw1 : itw2)
iwheel3 = (ikey > 12 ? itw1 : itw2)
iwheel4 = isine
; Start Dur Amp Pitch SubFund Sub3rd Fund 2nd 3rd 4th 5th 6th 8th
;i1 0 6 200 8.04 8 8 8 8 3 2 1 0 4
asubfund poscil3 8, 0.5 * kfqc, iwheel1, iphase / (ikey - 12)
asub3rd poscil3 8, 1.4983 * kfqc, iwheel2, iphase / (ikey + 7)
afund poscil3 8, kfqc, iwheel3, iphase / ikey
a2nd poscil3 8, 2 * kfqc, iwheel4, iphase / (ikey + 12)
a3rd poscil3 3, 2.9966 * kfqc, iwheel4, iphase / (ikey + 19)
a4th poscil3 2, 4 * kfqc, iwheel4, iphase / (ikey + 24)
a5th poscil3 1, 5.0397 * kfqc, iwheel4, iphase / (ikey + 28)
a6th poscil3 0, 5.9932 * kfqc, iwheel4, iphase / (ikey + 31)
a8th poscil3 4, 8 * kfqc, iwheel4, iphase / (ikey + 36)
a_signal = asubfund + asub3rd + afund + a2nd + a3rd + a4th + a5th + a6th + a8th
i_attack = .002
i_sustain = p3
i_release = 0.01
xtratim i_attack + i_sustain + i_release
a_declicking linsegr 0, i_attack, 1, i_sustain, 1, i_release, 0
a_signal = a_signal * i_amplitude * a_declicking * k_gain * 1.46
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


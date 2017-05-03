gk_ToneWheelOrgan_level init 0
instr ToneWheelOrgan
; Author: Hans Mikelson
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
i_pan = p6
i_depth = p7
i_height = p8
i_phase = p9
i_overall_amps = 112
i_normalization = ampdb(-i_overall_amps) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ToneWheelOrgan_level)
gk_Harpsichord_pan = .5
iattack = .002
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
iphase = 0.25
ikey = p4 ;12 * int(p4 - 6) + 100 * (p4 - 6)
kfqc = kHz
isine ftgenonce 0, 0, 65536, 10, 1
; The lower tone wheels have increased odd harmonic content.
; Rotor Tables
itw1 ftgenonce 0, 0, 65536, 10, 1, 0.02, .01
itw2 ftgenonce 0, 0, 65536, 10, 1, 0, 0.2, 0, 0.1, 0, .05, 0, .02
; Rotating Speaker Filter Envelopes
itw3 ftgenonce 0, 0, 65536, 7, 0, 110, 0, 18, 1, 18, 0, 110, 0
itw4 ftgenonce 0, 0, 65536, 7, 0, 80, 0.2, 16, 1, 64, 1, 16, 0.2, 80, 0
; Distortion Tables
itw5 ftgenonce 0, 0, 65536, 8, -.8, 336, -.78, 800, -.7, 5920, .7, 800, .78, 336, .8
itw6 ftgenonce 0, 0, 65536, 8 -.8, 336, -.76, 3000, -.7, 1520, .7, 3000, .76, 336, .8
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
asignal = asubfund + asub3rd + afund + a2nd + a3rd + a4th + a5th + a6th + a8th
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
asignal = asignal * adeclick * i_amplitude * k_gain
aleft, aright pan2 asignal, i_pan
outleta "outleft", aleft
outleta "outright", aright
prints "ToneWheelOrgan i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p6, active(p1)
endin


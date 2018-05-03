gk_Bower_midi_dynamic_range init 127
gk_Bower_attack init .0125
gk_Bower_release init .0125
gk_Bower_level init 0.5
gk_Bower_pressure init 0.25
instr Bower
if p3 == -1 then
  p3 = 1000000
endif
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_dynamic_range = i(gk_Bower_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_frequency = cpsmidinn(i_midi_key)
; Adjust the following value until "overall amps" at the end of performance is about -6 dB.
i_level_correction = 74.6
i_normalization = ampdb(-i_level_correction) / 2
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_Bower_level)
iattack = i(gk_Bower_attack)
idecay = i(gk_Bower_release)
isustain = p3 - i(gk_Bower_release)
p3 = iattack + isustain + idecay
iamp = i_amplitude
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(i_midi_key)
kamp = kenvelope
kfreq = ihertz
kpres = 0.25
krat rspline 0.006,0.988,1,4
kvibf = 4.5
kvibamp = 0
iminfreq = 30
isine ftgenonce 0,0,65536,10,1
aSig wgbow kamp,kfreq,gk_Bower_pressure,krat,kvibf,kvibamp,isine,iminfreq
aleft, aright pan2 aSig / 7, k_space_left_to_right
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
outleta "outleft", aleft * k_gain
outleta "outright", aright * k_gain
prints "Bower          i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin

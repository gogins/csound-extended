gk_ZakianFlute_midi_dynamic_range init 20
gk_ZakianFlute_level init 0
gk_ZakianFlute_pan init .5
gi_ZakianFLute_seed init .5
instr ZakianFlute
; Author: Lee Zakian
; Adapted by: Michael Gogins
i_instrument = p1
i_time = p2
i_duration = p3
i_midi_key = p4
i_midi_velocity = p5
k_space_front_to_back = p6
k_space_left_to_right = p7
k_space_bottom_to_top = p8
i_phase = p9
i_overall_amps = 65
i_normalization = ampdb(-i_overall_amps) / 2
i_midi_dynamic_range = i(gk_ZakianFlute_midi_dynamic_range)
i_midi_velocity = p5 * i_midi_dynamic_range / 127 + (63.5 - i_midi_dynamic_range / 2)
i_amplitude = ampdb(i_midi_velocity) * i_normalization
k_gain = ampdb(gk_ZakianFlute_level)
iattack = .002
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
iHz = cpsmidinn(i_midi_key)
kHz = k(iHz)
aenvelope transeg 1.0, 20.0, -10.0, 0.05
ip3 = (p3 < 3.0 ? p3 : 3.0)
if2 ftgenonce 0, 0, 16, -2, 40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240
if26 ftgenonce 0, 0, 65536, -10, 2000, 489, 74, 219, 125, 9, 33, 5, 5
if27 ftgenonce 0, 0, 65536, -10, 2729, 1926, 346, 662, 537, 110, 61, 29, 7
if28 ftgenonce 0, 0, 65536, -10, 2558, 2012, 390, 361, 534, 139, 53, 22, 10, 13, 10
if29 ftgenonce 0, 0, 65536, -10, 12318, 8844, 1841, 1636, 256, 150, 60, 46, 11
if30 ftgenonce 0, 0, 65536, -10, 1229, 16, 34, 57, 32
if31 ftgenonce 0, 0, 65536, -10, 163, 31, 1, 50, 31
if32 ftgenonce 0, 0, 65536, -10, 4128, 883, 354, 79, 59, 23
if33 ftgenonce 0, 0, 65536, -10, 1924, 930, 251, 50, 25, 14
if34 ftgenonce 0, 0, 65536, -10, 94, 6, 22, 8
if35 ftgenonce 0, 0, 65536, -10, 2661, 87, 33, 18
if36 ftgenonce 0, 0, 65536, -10, 174, 12
if37 ftgenonce 0, 0, 65536, -10, 314, 13
iwtsin ftgenonce 0, 0, 65536, 10, 1
prints "ZakianFlute    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin

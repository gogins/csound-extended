gk_Buzzer_attack init .0125
gk_Buzzer_release init .0125
gk_Buzzer_Harmonics init 15
gk_Buzzer_level init .5
instr Buzzer
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 4
iattack = i(gk_Buzzer_attack)
idecay = i(gk_Buzzer_release)
isustain = p3 - i(gk_Buzzer_attack)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
;asignal gbuzz kenvelope, ihertz, 3, gk_FirstHarmonic, gk_DistortFactor, gisine
isine ftgenonce 0, 0, 65536, 10, 1
gk_Harmonics = gk_Buzzer_Harmonics
asignal buzz kenvelope, ihertz, gk_Harmonics, isine
asignal = asignal * 3
;asignal vco2 kenvelope, ihertz, 12
;asignal poscil3 kenvelope, ihertz, giharmonics
;asignal distort asignal, gk_DistortFactor * .4, giwaveshaping
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Buzzer_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Buzzer         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin


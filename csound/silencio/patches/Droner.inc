gk_Droner_partial1 init .5
gk_Droner_partial2 init .05
gk_Droner_partial3 init .1
gk_Droner_partial4 init .2
gk_Droner_partial5 init .1
gk_Droner_partial6 init 0
gk_Droner_partial7 init 0
gk_Droner_partial8 init 0
gk_Droner_partial9 init 0
gk_Droner_partial10 init 0
gk_Droner_level init 0
gi_Droner_waveform init 0
instr Droner
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
ipan = p6
idepth = p7
iheight = p8
iphase = p9
k1 = gk_Droner_partial1
k2 = gk_Droner_partial2
k3 = gk_Droner_partial3
k4 = gk_Droner_partial4
k5 = gk_Droner_partial5
k6 = gk_Droner_partial6
k7 = gk_Droner_partial7
k8 = gk_Droner_partial8
k9 = gk_Droner_partial9
k10 = gk_Droner_partial10
iwaveform = gi_Droner_waveform
i_overall_amps = -19.76
i_normalization = ampdb(-i_overall_amps) / 2
iamp = ampdb(ivelocity) * i_normalization
iattack = .5
idecay = .5
isustain = p3
p3 = iattack + isustain + idecay
aenvelope transeg 0.0, iattack / 2.0, 1.5, 1 / 2.0, iattack / 2.0, -1.5, 1, isustain, 0.0, 1, idecay / 2.0, 1.5, 1 / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0, 0, 65536, 10, 1, 0, .02
if iwaveform == 0 then
asignal poscil3 1, ihertz, isine
endif
if iwaveform == 1 then
asignal vco2 1, ihertz, 8 ; integrated saw
endif
if iwaveform == 2 then
asignal vco2 1, ihertz, 12 ; triangle
endif
asignal chebyshevpoly asignal, 0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10
asignal = asignal * aenvelope * iamp
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
k_gain = ampdb(gk_Droner_level)
outleta "outleft", aleft * k_gain
outleta "outright", aright * k_gain
;;;;;;;;;;;;;;;;;;;;i
prints "Droner        i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p6
endin

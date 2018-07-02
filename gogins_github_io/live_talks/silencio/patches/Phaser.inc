gk_Phaser_ratio1 init 1
gk_Phaser_ratio2 init 1/3
gk_Phaser_index1 init 1
gk_Phaser_index2 init 0.0125
gk_Phaser_level init 0.5
instr Phaser
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity) * 8
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
isine ftgenonce 0,0,65536,10,1
khertz = ihertz
ifunction1 = isine
ifunction2 = isine
a1,a2 crosspm gk_Phaser_ratio1, gk_Phaser_ratio2, gk_Phaser_index1, gk_Phaser_index2, khertz, ifunction1, ifunction2
aleft, aright pan2 a1+a2, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft * kenvelope
aright = adamping * aright * kenvelope
kgain = ampdb(gk_Phaser_level)
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Phaser         i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\\n", p1, p2, p3, p4, p5, p7, active(p1)
endin

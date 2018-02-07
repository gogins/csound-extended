gk_Sweeper_britel init 0
gk_Sweeper_briteh init 2.9
gk_Sweeper_britels init .2 / 3
gk_Sweeper_britehs init 2.5 / 2
gk_Sweeper_level init 0.5
instr Sweeper
//////////////////////////////////////////////
// Original by Iain McCurdy.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
insno = p1
istart = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p6
ipan = p7
iamp = ampdb(ivelocity)
gisine ftgenonce 0, 0, 65536, 10, 1
gioctfn ftgenonce 0, 0, 65536, -19, 1, 0.5, 270, 0.5
iattack = i(gk_overlap)
idecay = i(gk_overlap)
isustain = p3 - i(gk_overlap)
p3 = iattack + isustain + idecay
kenvelope transeg 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
icps = ihertz
kamp expseg 0.001,0.02,0.2,p3-0.01,0.001
ktonemoddep jspline 0.01,0.05,0.2
ktonemodrte jspline 6,0.1,0.2
ktone poscil3 ktonemoddep, ktonemodrte, gisine
kbrite rspline gk_Sweeper_britel, gk_Sweeper_briteh, gk_Sweeper_britels, gk_Sweeper_britehs
ibasfreq init icps
ioctcnt init 3
iphs init 0
a1 hsboscil kenvelope, ktone, kbrite, ibasfreq, gisine, gioctfn, ioctcnt, iphs
amod poscil3 0.25, ibasfreq*(1/3), gisine
arm = a1*amod
kmix expseg 0.001, 0.01, rnd(1), rnd(3)+0.3, 0.001
kmix=.25
a1 ntrpol a1, arm, kmix
;a1 pareq a1/10, 400, 15, .707
;a1 tone a1, 500
kpanrte jspline 5, 0.05, 0.1
kpandep jspline 0.9, 0.2, 0.4
kpan poscil3 kpandep, kpanrte, gisine
a1,a2 pan2 a1, kpan
a1 delay a1, rnd(0.1)
a2 delay a2, rnd(0.11)
kenv linsegr 1, 1, 0
kenv = kenvelope
aleft = a1*kenv*.02
aright = a2*kenv*.02
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
aleft = adamping * aleft
aright = adamping * aright
kgain = ampdb(gk_Sweeper_level) / 3
outleta "outleft", aleft * kgain
outleta "outright", aright * kgain
prints "Sweeper       i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", p1, p2, p3, p4, p5, p7, active(p1)
endin


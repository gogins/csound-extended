gk_Piano_level init .25
instr PianoOut
kgain = ampdb(gk_Piano_level)
giFluidsynth fluidEngine 0, 0
giFluidSteinway fluidLoad "Piano Steinway Grand Model C (21,738KB).sf2", giFluidsynth, 1
fluidProgramSelect giFluidsynth, 0, giFluidSteinway, 0, 1
i_instrument = p1
i_time = p2
i_duration = p3
i_midikey = p4
i_midivelocity = p5
aoutleft, aoutright fluidOut giFluidsynth
aoutleft = aoutleft * 3000000
aoutright = aoutright * 3000000
;printks "fluidOut: %9.4f %9.4f\n", 0.5, aoutleft, aoutright
outleta "outleft", aoutleft * kgain
outleta "outright", aoutright * kgain
;;;;;;;;;;;;;;;;;;;;i
prints "PianoOutFl    i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\\n", p1, p2, p3, p4, p5, p7
endin

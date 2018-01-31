<CsoundSynthesizer>
<CsOptions>
-odac -iadc 
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 100
nchnls = 2 
nchnls_i = 1
0dbfs  = 1

instr 1
prints "Sing or play into the microphone..."
ainput in	

imaximum_delay_milliseconds 1000
amodulation_hz
amodulation_depth
amodulator   squinewave 
amodulator = amodulator + .5
aoutput vdelay3 ainput, amodulator, imaximum_delay_milliseconds
     outs  asig, asig

printks "ain: %9.4f aout: %9.4f\n", .5, ainput, aoutput

outs aoutput, aoutput
endin
</CsInstruments>
<CsScore>
i 1 0 200
</CsScore>
</CsoundSynthesizer>
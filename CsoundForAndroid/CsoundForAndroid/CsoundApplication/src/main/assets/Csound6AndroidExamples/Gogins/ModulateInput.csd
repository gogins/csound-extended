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
ainput                      in	
imaximum_delay_milliseconds init        4000
amodulation_hz              init        35
amodulation_depth           init        0.125
aclip                       init        0.5
askew                       init        0.5
asyncin                     init        0
kdelay                      chnget      "slider1"
                            printks2    "kdelay:            %9.4f\n", kdelay
kmodulation_hz              chnget      "slider2"
                            printks2    "kmodulation_hz:    %9.4f\n", kmodulation_hz
kmodulation_depth           chnget      "slider2"
                            printks2    "kmodulation_depth: %9.4f\n", kmodulation_depth
kclip                       chnget      "slider3"
                            printks2    "kclip:             %9.4f\n", kclip
kskew                       chnget      "slider4"
                            printks2    "kskew:             %9.4f\n", kskew
akmodulation_hz             =           a(kmodulation_hz)
akclip                      =           a(kclip)
akskew                      =           a(kskew)
amodulator                  squinewave  akmodulation_hz, akclip, akskew, 0
amodulator                  =           amodulator * kmodulation_depth + .5
adelay                      =           kdelay * amodulator
aoutput                     vdelay3     ainput, adelay, imaximum_delay_milliseconds
                            outs        aoutput, aoutput
                            printks     "ain: %9.4f aout: %9.4f\n", .5, ainput, aoutput
                            outs        aoutput, aoutput
                            endin
</CsInstruments>
<CsScore>
i 1 0 200
</CsScore>
</CsoundSynthesizer>
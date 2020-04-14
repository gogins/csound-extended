<CsoundSynthesizer>
<CsOptions>
-m195 -odac:plughw:1,0 -iadc:plughw:1,0  -f -+rtaudio=alsa 
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 128
nchnls = 2 
nchnls_i = 1
0dbfs  = 1
                            chnset      400, "kdelay"
                            chnset      3, "kmodulation_hz"
                            chnset      .125, "kmodulation_depth"
                            chnset      .5, "kclip"
                            chnset      .5, "kskew"

                            instr 1
                            prints "Sing or play into the microphone...\n"
ainput                      inch        1	
imaximum_delay_milliseconds init        4000
amodulation_hz              init        35
amodulation_depth           init        0.125
aclip                       init        0.5
askew                       init        0.5
;asyncin                     init        0
                            
kdelay                      chnget      "kdelay"
                            printks2    "kdelay:            %9.4f\n", kdelay
kmodulation_hz              chnget      "kmodulation_hz"
                            printks2    "kmodulation_hz:    %9.4f\n", kmodulation_hz
kmodulation_depth           chnget      "kmodulation_depth"
                            printks2    "kmodulation_depth: %9.4f\n", kmodulation_depth
kclip                       chnget      "kclip"
                            printks2    "kclip:             %9.4f\n", kclip
kskew                       chnget      "kskew"
                            printks2    "kskew:             %9.4f\n", kskew
akmodulation_hz             =           a(kmodulation_hz)
akclip                      =           a(kclip)
akskew                      =           a(kskew)
amodulator                  squinewave  akmodulation_hz, akclip, akskew, 0
amodulator                  =           amodulator * kmodulation_depth + .5
adelay                      =           kdelay * amodulator
aoutput                     vdelay3     ainput, adelay, imaximum_delay_milliseconds
                            outs        aoutput, aoutput
                            printks     "ain: %9.4f aout: %9.4f\n", .5, dbamp(rms(ainput)), dbamp(rms(aoutput))
                            outs        ainput, aoutput
                            endin
</CsInstruments>
<CsScore>
i 1 0 200
</CsScore>
</CsoundSynthesizer></textarea>

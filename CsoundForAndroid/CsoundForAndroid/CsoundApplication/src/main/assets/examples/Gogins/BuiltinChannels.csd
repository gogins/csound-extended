<CsoundSynthesizer>
<CsOptions>
-odac -iadc 
</CsOptions>
<CsInstruments>

sr = 48000
ksmps = 128
nchnls = 2 
nchnls_i = 1
0dbfs  = 1

                    instr 1
                    prints      "Printing all builtin channel names and values as values change...\n"
gk_slider1          chnget      "slider1"
                    printks2    "slider1:           %9.4f\n", gk_slider1
gk_slider2          chnget      "slider2"
                    printks2    "slider2:           %9.4f\n", gk_slider2
gk_slider3          chnget      "slider3"
                    printks2    "slider3:           %9.4f\n", gk_slider3
gk_slider4          chnget      "slider4"
                    printks2    "slider4:           %9.4f\n", gk_slider4
gk_slider5          chnget      "slider5"
                    printks2    "slider5:           %9.4f\n", gk_slider5
gk_slider6          chnget      "slider6"
                    printks2    "slider6:           %9.4f\n", gk_slider6
gk_slider7          chnget      "slider7"
                    printks2    "slider7:           %9.4f\n", gk_slider7
gk_slider8          chnget      "slider8"
                    printks2    "slider8:           %9.4f\n", gk_slider8
gk_slider9          chnget      "slider9"
                    printks2    "slider9:           %9.4f\n", gk_slider9
gk_butt1            chnget      "butt1"
                    printks2    "butt1:             %9.4f\n", gk_butt1
gk_butt2            chnget      "butt2"
                    printks2    "butt2:             %9.4f\n", gk_butt2
gk_butt3            chnget      "butt3"
                    printks2    "butt3:             %9.4f\n", gk_butt3
gk_butt4            chnget      "butt4"
                    printks2    "butt4:             %9.4f\n", gk_butt4
gk_butt5            chnget      "butt5"
                    printks2    "butt5:             %9.4f\n", gk_butt5
gk_trackpadx        chnget      "trackpad.x"
                    printks2    "trackpad.x:        %9.4f\n", gk_trackpadx
gk_trackpady        chnget      "trackpad.y"
                    printks2    "trackpad.y:        %9.4f\n", gk_trackpady
gk_accelerometerX   chnget      "accelerometerX"
                    printks2    "accelerometerX:    %9.4f\n", gk_accelerometerX
gk_accelerometerY   chnget      "accelerometerY"
                    printks2    "accelerometerY:    %9.4f\n", gk_accelerometerY
gk_accelerometerZ   chnget      "accelerometerZ"
                    printks2    "accelerometerZ:    %9.4f\n", gk_accelerometerZ
                    endin
                    
</CsInstruments>
<CsScore>
i 1 0 360
</CsScore>
</CsoundSynthesizer>
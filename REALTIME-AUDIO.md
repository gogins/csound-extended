# How to Configure Ubuntu for Optimal Audio

On my NUC and perhaps on other Linux computers, configuring the system for efficient, dropout-free, low-latency audio can be tricky. It is desirable to disable PulseAudio, and then to make Csound use ALSA with the _native_ audio frame rate and sample size.

## Disable PulseAudio

Edit /etc/pulse/client conf and reconfigure autospawn (turn it off):
```
; autospawn = yes
autospawn = no
```

Then kill the pulseaudio daemon: `killall -9 pulseaudio`.

## Reconfigure ALSA

The default ALSA card is 0, but it will probably need to be some other number, probably 1 (e.g. for USB audio). To change the default card for a single user, create and/or edit ~/.asoundrc to reassign the default card and other parameters:

```
pcm.!default {
        type hw
        card 1
}

ctl.!default {
        type hw           
        card 1
}
```

Then determine the default native ALSA audio format. On my Intel NUC this is 44100 Hz 24 bits little-endian. Then as there appear to be issues with Csound's default ALSA driver, Csound must use only the _PortAudio_ driver and only the _native_ ALSA audio format, e.g. on my NUC: `-+rtaudio=PortAudio -r 44100 -k 100 -fodac`.

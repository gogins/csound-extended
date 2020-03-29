# How to Configure Ubuntu for Optimal Real-Time Audio

On my NUC and perhaps on other Linux computers, configuring the system for efficient, dropout-free, low-latency real-time audio can be tricky. It is desirable to disable PulseAudio, and then to make Csound use ALSA with the _native_ audio frame rate and sample size, which depends on the operating system audio driver and the audio interface device.

## Disable PulseAudio

Edit `/etc/pulse/client.conf` and reconfigure `autospawn` (turn it off):
```
; autospawn = yes
autospawn = no
```

Then kill the pulseaudio daemon: `killall -9 pulseaudio`.

## Reconfigure ALSA

The default ALSA card is 0, but it will probably need to be some other number, probably 1 (e.g. for USB audio). To change the default card for a single user, create and/or edit `~/.asoundrc` to reassign the default card:

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

## Rendering to Soundfile

You can and should use a higher-precision format for rendering to soundfiles. All playback software of which I am aware can handle any format and play it back properly.

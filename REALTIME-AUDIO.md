# How to Configure Linux for Optimal Real-Time Audio

On my NUC and perhaps on other Linux computers, configuring the system for efficient, dropout-free, low-latency real-time audio can be tricky. It may be desirable to disable PulseAudio, and then to make Csound and all other audio software on the computer use low-level ALSA alone.

## Disable PulseAudio

Edit `/etc/pulse/client.conf` and reconfigure `autospawn` (turn it off):
```
; autospawn = yes
autospawn = no
```

Then kill the pulseaudio daemon: `killall -9 pulseaudio`.

## Reconfigure ALSA

Audio configuration for Ubuntu is not well documented, especially not for musicians or programmers. However, I have learned some things.

It is indeed possible to permanently turn off PulseAudio and use only plain ALSA for all audio interfacing. This provides audio with less overhead and fewer glitches, but only for one application at a time. This is the best choice for me and probably for most serious musicians.

The basic thing in ALSA is a "card," and their names should be determined thus:
```
mkg@xenakis:~/csound-extended/WebAssembly/examples$ cat /proc/asound/cards
 0 [PCH            ]: HDA-Intel - HDA Intel PCH
                      HDA Intel PCH at 0xdf240000 irq 138
 1 [UA25EX         ]: USB-Audio - UA-25EX
                      EDIROL UA-25EX at usb-0000:00:14.0-4, full speed
```
Cards in turn have devices, which can be discovered thus ([card number - device number]):
```
mkg@xenakis:~/csound-extended/WebAssembly/examples$ cat /proc/asound/devices
  1:        : sequencer
  2: [ 1]   : control
  3: [ 1- 0]: digital audio playback
  4: [ 1- 0]: digital audio capture
  5: [ 1- 0]: raw midi
  6: [ 0]   : control
  7: [ 0- 3]: digital audio playback
  8: [ 0- 7]: digital audio playback
  9: [ 0- 8]: digital audio playback
 10: [ 0- 9]: digital audio playback
 11: [ 0-10]: digital audio playback
 12: [ 0- 2]: hardware dependent
 33:        : timer
```
More information can be obtained thus, which lists all available cards and their devices:
```
mkg@xenakis:~/csound-extended/WebAssembly/examples$ aplay -L
null
    Discard all samples (playback) or generate zero samples (capture)
pulse
    PulseAudio Sound Server
default
hdmi:CARD=PCH,DEV=0
    HDA Intel PCH, HDMI 0
    HDMI Audio Output
... und so weiter.
```
The difference between `hw` and `plughw` is important; `hw` identifies a device that can use only its native sample rate and format, `plughw` identifies the same device, but with the addition of a plugin that automatically handles sample rate and format conversions. For our purposes `plughw` should always be used. The audio format conversions in ALSA behave as though they are considerably more efficient than those in PulseAudio.

For most pieces, the Csound options should be:
```
-d -f -m195 -+rtaudio=alsa -odac:plughw:1,0 [ -iadc:plughw:1,0 ]
```
In the browser, there is only 1 input channel, outside the browser there are probably 2 input channels.

It is vital to understand that with plain ALSA, only one audio stream can be active at a time. Hence, in the browser, only one tab can use audio at a time. Other audio tabs should be closed before running a WebAssembly piece.

The Chromium browser is invoked by the shell script `/usr/bin/chromium-browser`. Normally, Chromium uses PulseAudio. The script has comments saying that the browser audio configuration can be overridden to use plain ALSA in an initialization file or in an environment variable. Indeed, I got ALSA to work in Chromium by putting the following into my `~/.chromium-browser.init` file:
```
export CHROMIUM_FLAGS="${CHROMIUM_FLAGS} --alsa-input-device=plughw:1,0 --alsa-output-device=plughw:1,0"
```

## Rendering to Soundfile

You can and should use a higher-precision format for rendering to soundfiles. All playback software of which I am aware can handle any format and play it back properly.

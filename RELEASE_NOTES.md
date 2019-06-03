# Release Notes for csound-extended and Related Projects
Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## 3 June 2013

I am pleased to announce new releases of various extensions to Csound written or
maintained by me for Linux and Android. These include:

### csound-extended package 1.3.2

https://github.com/gogins/csound-extended

The csound-extended Debian package includes the CsoundAC algorithmic composition
library for C++, the nudruz algorithmic composition library for Common Lisp,
csound.node for running Csound in NW.js, the LuaJIT opcodes for Csound, and the
cmask opcodes for Csound. Changes in this release include

1. Changed from the v8 API to the Node Addon API for better maintainability of
csound.node.

2. New KMeansMCRM node in CsoundAC that uses k-means clustering to translate IFS
attractors to scores.

3. Improved ImageToScore2 node in CsoundAC that enables some image processing
before translation to a score.

4. Consistency of dimensions of notes across languages in csound-extended,

5. The addition of the LuaJIT opcodes, which have been removed from the core
Csound repository.

6. Better Doxygen documentation.

7. Instructions for building csound-extended with the current version of Csound.

8. Updated playable (run examples in Web browsers) version of the Csound
Reference Manual at
https://gogins.github.io/csound-extended/html/indexframes.html.

9. Added CsoundProducer header-file-only class to integrate post-processing and
tagging of pieces.

10. Embeddable Common Lisp now embedded in CsoundProducer, enabling Common Lisp
code e.g. from nudruz to run in CsoundAC and csound.node.

11. Improved Csound patch library.

12. Improved spatialization system in Csound patch library.

13. Removal of CsoundHtml5 due to instability and overlapping functionality with
both CsoundQt and csound.node.

### Csound for Android app release 19

https://play.google.com/store/apps/details?id=com.csounds.Csound6&hl=en_US

The Csound for Android app includes most of Csound, including a number of plugin
opcodes. Changes in this release include:

1. Built-in text editor based on CodeMirror.

2. Improved tabbed user interface; it is not necessary to change settings to
work with HTML, builtin widgets, or plain Csound.

3. Corrected bug for "Save" which now works.

4. Oboe audio stream now stopped from a separate thread to prevent crashing.

5. Built-in browser for the Csound Reference Manual.

5. Restored user-configurable audio driver.

6. Improved audio stability and performance.

7. Improved "Scrims" example piece.

### csound-aeolus package 2.1.1

https://github.com/gogins/csound-aeolus

The csound-aeolus package provides Fons Adriaensen's wonderful Aeolus pipe organ
emulation as a Csound opcode.

There are no major changes in this release.

### csound-vst package 1.0.2

https://t.umblr.com/redirect?z=https%3A%2F%2Fwww.dropbox.com%2Fs%2Flns8ull4bbifjp7%2Fcsound-vst-1.0.0-Linux.deb%3Fdl%3D0&t=YWUzN2M1ZjA1MGJkYmJhM2U5YmFhMjUzOTNkYmI1ZjM4MjA5MzQ3NixUbU4zdWpESg%3D%3D&p=&m=0

The csound-vst package includes both Csound as a VST 2 plugin, and opcodes for
hosting VST plugins in Csound.

There are no major changes in this release.

**For more information, please consult the repository commit logs.**

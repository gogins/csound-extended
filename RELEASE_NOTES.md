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

2. Improved ImageToScore2 node in CsoundAC.

3. Consistency of dimensions of notes across languages in csound-extended,

3. The addition of the LuaJIT opcodes, which have been removed from the core
Csound repository.

4. Better Doxygen documentation.

5. Instructions for building csound-extended with the current version of Csound.

7. Updated playable version of the Csound Reference Manual at
https://gogins.github.io/csound-extended/html/indexframes.html.

8. Added CsoundProducer header-file-only class to integrate post-processing of
pieces.

9. Removal of CsoundHtml5 due to instability and overlapping functionality with
both CsoundQt and csound.node.

### Csound for Android app release 19

https://play.google.com/store/apps/details?id=com.csounds.Csound6&hl=en_US

The Csound for Android app includes most of Csound, including a number of plugin
opcodes. Changes in this release include:

1. Built-in text editor based on CodeMirror.

2. Improved tabbed user interface; it is not necessary to change settings to
work with HTML, builtin widgets, or plain Csound.

3. Corrected bug for "Save".

4. Built-in browser for the Csound Reference Manual.

5. Restored user-configurable audio driver.

6. Improved audio stability and performance.

7. Improved "Scrims" example piece.

### csound-aeolus package 2.1.1

https://github.com/gogins/csound-aeolus

The csound-aeolus package provides Fons Adrianssen's wonderful Aeolus pipe organ
emulation as a Csound opcode.

There are no major changes in this release.

### csound-vst package 1.0.2

The csound-vst package includes both Csound as a VST 2 plugin, and opcodes for
hosting VST plugins in Csound.

There are no major changes in this release.

**For more information, please consult the repository commit logs.**

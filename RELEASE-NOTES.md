# Release Notes for csound-extended and Related Projects
Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## 22 May 2021

I have refactored my GitHub projects, repositories, build systems, and 
packages. 

All Csound examples and tests for all my repositories have now also been moved 
into their own repository at https://github.com/gogins/csound-examples.

Basically, csound-extended itself now targets only C++ and Python. Other 
targets have been moved out into their own repositories.

See this GitHub issue for details: 
https://github.com/gogins/csound-extended/issues/158

## 3 February 2021

### [csound-extended package 2.0.0](https://github.com/gogins/csound-extended/releases)

The chord space facility in CsoundAC has been very useful to me over the years, but recently I took at look at the "chord space group" facility and discovered many bugs in the underlying chord symmetry code (based on the theory of Callender, Quinn, and Tymoczko). 

I have now discovered and, I hope, corrected all major incorrect assumptions and algorithms. I have thoroughly reviewed the predicates and transformations for octave equivalence (O), permutational equivalence (P), transpositional equivalence (T), and inversional equivalence (I). I have rewritten the predicates and transformations for the I, OPT, and OPTI equivalence relations to take into account the cyclical regions of OPT and OPTI fundamental domains. In doing this I have derived the fundamental domains from first principles and implemented the inversion flats in each OPT sector of the cyclical region using linear algebra. I have subjected all of this work has to what I hope are comprehensive unit tests. Please log an issue here for any bugs you find.

An interactive online demonstration of OP, OPT, and OPTI space for trichords, implemented using the CsoundAC Chord class and the WebAssembly build of Csound, can be found [here](https://gogins.github.io/csound-examples/trichord_space.html).

The previous ChordSpaceGroup class has been replaced by a new PITV class that uses prime form (P), inversion (I), transposition (T), and octavewise revoicing (V) as subgroups. Prime form equivalence is used in place of the previous OPTI equivalence because prime form abstracts from the cyclical region with its revoicings and is thus orthogonal to octavewise revoicing.

The HarmonyIfs class has been rewritten to use the PITV class.

The JavaScript modules ChordSpace.js and Silencio.js are now deprecated. The mathematical corrections mentioned above have not been made in ChordSpace.js. Please use the WebAssembly build of CsoundAC for new compositions instead of Silencio.js and ChordSpace.js. The same API is available in Python and C++.

The csound-extended build of Csound for WebAssembly has been updated to be more flexible with respect to the browser's audio configuration. Csound and the browser must use ksmps = 128 and sr = 48000, but otherwise the number of input and output channels need not be the same between Csound and the browser. This should mean that any Csound piece running in the browser will render to the default audio output and have access to the default audio input.

All of my examples for Csound, csound-extended, csound-extended-wasm, Csound for Android the app, and my other contributions have been consolidated into my [csound-examples](https://github.com/gogins/csound-examples) repository.

The examples for the nudruz system of algorithmic composition written in Common Lisp, originally created by Drew Krause, remain in this repository and have been slightly updated to make sure they still work.

The Python interface for CsoundAC now specifically targets Python 3.9 in order to be compatible with Rick Taube's musx Python system for algorithmic composition. Musx provides most of the functionality of Common Music in Python. It should now be possible to use CsoundAC (via its Python interface) with musx.

## 21 May 2020

### [csound-extended package 1.4.0](https://github.com/gogins/csound-extended/releases)

The Python interface for CsoundAC now specifically targets Python 3 rather than Python 2. This has been tested with Python versions 3.6.9 and 3.7.0.

Csound performances can now be stopped and restarted in csound.node, and Csound message printing now works for successive performances.

Some of the examples using csound.node and WebAssembly have been improved.

The `csound_loader.js` script for loading Csound in the same manner across the csound.node, Android, and WebAssembly platforms has been much improved and is much more reliable.

The ChordSpace module in the CsoundAC library now supports fairly extensive operations on scales and their chords, in the spirit of Roman numeral analysis and "functional harmony," including:

 1.  Returning a chord of any size and any whole interval spacing in scale steps for any scale degree (Roman numeral) of any scale.
 2.  For a given scale and a given chord, return the scale degree of that chord in that scale, if it belongs to the scale.
 3.  Transposing a chord in a given scale by any whole number of scale steps; may be used to implement typical root progressions.
 4.  Transposing a given scale to a new tonic by any number of semitones, which can be fractional.
 5.  Transposing a given scale to a new tonic on any scale degree.
 6.  For a given chord in a given scale, return any other scales to which that chord also belongs; can be used to implement common-chord modulations.
 7.  For a given chord in a given scale, return it in the form of a secondary dominant or other secondary function, if that is possible; may be used to implement secondary dominants and other secondary functions.
 8.  For a given chord in a given scale and a secondary function, return the relative tonicization of that chord, that is, the scale in which that chord has that secondary function.
 9.  Create a new scale with a name and any number of pitches, which may be in any system of temperament and may have any interval structure; all of the above operations will work with such scales.

The CsoundAC VoiceleadingNode class now implements conforming notes produced by children of this node to specific instances of the Chord class, by timed segment. This makes composing algorithmically using chords, neo-Riemannian transformations of chords, and automatic voice-leading much easier and more flexible.

Out of order and incorrect comments in the VoiceleadingNode class have been fixed, leading to corrected Doxygen documentation for this class.

The ["live" version of the Csound Reference Manual](https://gogins.github.io/csound-extended/html/indexframes.html), using the WebAssembly build of Csound to play most of the examples from the Web browser, has been updated with the latest sources for the manual and my latest WebAssembly build of Csound.

A page of advice on obtaining efficient audio performance on Linux has been added.

### [csound-android](https://github.com/gogins/csound-android/releases)

A debuggable version of the Csound for Android app is now available on the GitHub release page. The app has also been updated on the [Google Play Store](https://play.google.com/store/apps/details?id=com.csounds.Csound6&hl=en)

The build system for the Csound for Android app has been considerably simplified and made easier to run. Old and redundant libraries and code have been removed.

A bug preventing writing to the filesystem on some devices has been fixed. Thanks to Karin Daum for helpful advice with this. The Csound for Android app on the Google Play Store has been updated with this fix.

Permissions handling has been improved in accordance with Android guidelines. However, there is still a crash on opening the file chooser on the HTC Life U11.

### [csound-vst](https://t.umblr.com/redirect?z=https%3A%2F%2Fwww.dropbox.com%2Fs%2Ffx7uhzj5gqzm31g%2Fcsound-vst-1.1.1-Linux.tar.gz%3Fdl%3D0&t=Y2NmM2JiYjA3NmYwY2FmYjViNWQ3MmM0NDgyMGJjN2ZkNWQyYmQ1OSxUbU4zdWpESg%3D%3D&p=&m=0)

This download contains Linux x86-64 binaries for the vst4cs opcodes and the CsoundVST plugin that embeds all of Csound in a VST2 plugin. The download has been rebuilt with the current beta release of Csound.

### [csound-aeolus](https://github.com/gogins/csound-aeolus/releases)

This package implements Fons Adriaensen's marvelous Aeolus software emulation of pipe organs as a Csound opcode. The package has been rebuilt with the current beta release of Csound.
    
### [michael.gogins.studio](https://github.com/gogins/michael.gogins.studio)
    
I am now hosting certain pieces of mine in my personal GitHub repository for live performance in Web browsers. The first such piece is "Scrims v2" which updates Scrims, premiered at the New York City Electroacoustic Music Festival in 2016, for the current WebAssembly build of Csound as well as improved Csound instruments and more reliable performance. This is a piece of interactive visual music. Other pieces will follow. 

I have updated my "live talk" on algorithmic composition, originally given at the National University of Quilmes in Buenos Aires in 2018, to use the current version of my build of Csound for WebAssembly/

I have added a new "live talk" on my implementations of JavaScript interfaces for Csound in csound.node, WebAssembly, and Android.

Please enter any bug reports on the Issues pages of the respective repositories, or just email me if that is easier. I'm also open to suggestions.

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

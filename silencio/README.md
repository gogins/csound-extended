# csound-silencio

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

__**PLEASE NOTE: The algorithmic composition code here is now deprecated in favor of the WebAssembly build/JavaScript interface to CsoundAC, also maintained in this repository. The JavaScript ChordSpace code contains many errors that have been fixed in the CsoundAC Chord class. Please use CsoundAC for new compositions.**__

Generative music, algorithmic composition, score generation, call it what you will. Silencio is the first system for algorithmic composition that is designed to run on smartphones and tablets as well as personal computers. 

Silencio features advanced score generators based on recurrent iterated function systems and parametric Lindenmayer systems, and includes code for chord transformations and voice-leading inspired by the work of [Dmitri Tymoczko](http://dmitri.mycpanel.princeton.edu/) and other mathematical music theorists. I have been performing works composed with Silencio at conferences and festivals for several years.

Please note, some full-scale examples for Silencio may be found here.

The original version of Silencio was written in the Lua programming language. This is no longer supported. The current version of Silencio is written in portable JavaScript because that makes more capabilities (audio, MIDI, animated 3-dimensional graphics, video, proper mathematics typesetting, symbolic mathematics, <it>etc</it>., <it>etc</it>.) available to Csound than any other programming environment.

Only the JavaScript version is currently under development. If and when [WebAssembly](http://webassembly.org/) becomes an accepted standard implemented in major Web browsers, I will port Silencio to WebAssembly, probably by updating my [CsoundAC C++ code base](https://github.com/csound/csound/tree/develop/frontends/CsoundAC) which is the inspiration for Silencio. 

Silencio is designed to be used with [Csound](http://csound.github.io/) as part of a computer music "playpen" for rapid, iterative composition and development without functional limitations:

2. CsoundQt front end for Csound.

3. Csound for Android.

4. csound.node for Windows and Linux.

5. Csound for WebAssembly.

Currently, the environments I recommend for musicians are csound.node running in [NW.js](http://nwjs.io/) on the desktop, and [Csound for Android](https://play.google.com/store/apps/details?id=com.csounds.Csound6&hl=en) for mobile devices.

Please log any bug reports or requests for enhancements at 
https://github.com/gogins/csound-extended/issues.

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit log.

## License

This code is licensed under the terms of the GNU Library General Public License, version 2.1.

## Installation

To use the Csound instrument definition library that is found in `silencio/patches`, include this directory in Csound's `INCDIR` environment variable.

Otherwise, see the main README.md for csound-extended.

## Building

Please see the main README.md for csound-extended.


## News

### 29 October 2016

Please note, development of Silencio has moved to the csound/silencio subdirectory of this repository. The original repository at https://github.com/gogins/silencio remains in place with a notice to come here for the current version of the code.

### 12 October 2016

I have added a WebGL-based 3-dimensional, zoomable piano roll score display to Silencio.js. And I have re-organized this repository to make it easier to understand and use.

Tarmo Johannes and I are working to update [CsoundQt](https://github.com/CsoundQt/CsoundQt) to use the Qt SDK's QtWebEngine for HTML rendering and JavaScript in place of the Chromium Embedded Framework, which will simplify the code and bring CsoundQt with HTML5 to OSX and Linux as well as Windows.

### 2 August 2016

I am now hosting some examples of pieces and code that use Silencio at https://github.com/gogins/gogins.github.io.

### 25 August 2015

I have now ported all of Silencio, including all of ChordSpace except the chord space group, from Lua to JavaScript. As time permits and projects demand, I will probably port selected parts of other people's algorithmic composition code to JavaScript. Tendency masks are one candidate, Xenakis sieves are another. This is to support my project of integrating HTML and JavaScript with Csound for a complete, self-contained "playpen" for computer music composers, especially for algorithmic composition.



# csound-extended-wasm

Edward Costello, Steven Yi, Henri Manson, Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

THis directory builds, packages, and tests csound-extended for WebAssembly.
This build replaces `CsoundObj.js` from the core Csound repository with a 
WebAssembly build of Csound, compiled using the Emscripten LLVM toolchain.
This build uses the new WebAudio AudioWorklet for superior performance with 
fewer audio issues.

* A number of C++ plugin opcodes (here, statically linked).

* A new JavaScript interface to Csound that follows, as exactly as possible,
  the interface defined by the Csound class in `csound.hpp` and also
  implemented in CsoundOboe in `csound_oboe.hpp` for the Csound for Android
  app, and in `csound.node`.

* Additional Csound API methods exposed to JavaScript.

Please log any bug reports or requests for enhancements at
https://github.com/gogins/csound-extended/issues.

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit log.

## Installation

Download the latest version of `csound-extended-wasm-version.zip` from the 
[release page here](https://github.com/gogins/csound-extended/releases). Unzip 
it and it is ready to use.

## Usage

The examples herein will run from the [GitHub pages of the csound-extended
repository](https://gogins.github.io/csound-extended/). To run the examples
locally, either clone and build this repository, or download the
`csound-extended-wasm-version.zip` archive and unpack it.

Examine `minimal.html` for a bare-bones example of using Csound for 
WebAssembly. Examine `player.html` for an example that uses the 
`csound_loader.js` helper script. These examples have comments that 
discuss all required points of usage.

Then, run the self-contained Web server, `httpd.py`, in the WebAssembly
directory, and navigate in your Web browser to localhost at the port reported
at server startup.

The `cmask.html` example will run with `csound.node` in NW.js 30.2 or later,
and will run with `CsoundAudioNode.js` in Chrome 66 or later. 

The `csound_loader.js` script is provided as a convenient method of
detecting and using available implementations of Csound that run in HTML5
environments, including, in decreasing order of preference:

1. Android (using the CsoundAndroid library).
2. CsoundQt (using the built-in JavaScript wrapper).
3. NW.js (using `csound.node`).
4. Current Web browsers (using `CsoundAudioNode.js`).

## Building

See the main README.md of csound-extended for build instructions.

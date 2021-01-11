# csound-extended-wasm

Edward Costello, Steven Yi, Henri Manson, Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

THis directory builds and packages csound-extended for WebAssembly.
This build replaces `CsoundObj.js` from the core Csound repository with a 
WebAssembly build of Csound, compiled using the Emscripten LLVM toolchain.
This build uses the new WebAudio AudioWorklet for superior performance with 
fewer audio issues. Features include:

* A number of C++ plugin opcodes (here, statically linked).

* A new JavaScript interface to Csound that follows, as exactly as possible,
  the interface defined by the Csound class in `csound.hpp` and also
  implemented in CsoundOboe in `csound_oboe.hpp` for the Csound for Android
  app, and in `csound.node`.

* Additional Csound API methods exposed to JavaScript.

* A new WebAssembly build of the C++ CsoundAC ("Csound Algorithmic 
  Composition") library.

Please log any bug reports or requests for enhancements at
https://github.com/gogins/csound-extended/issues.

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit log.

## Installation

Download the latest version of `csound-extended-wasm-version.zip` from the 
[release page here](https://github.com/gogins/csound-extended/releases). Unzip 
it and it is ready to use.

## Examples

Some of the examples herein will run from 
https://gogins.github.io/csound-examples. For more information, see my 
[csound-examples](https://github.com/gogins/csound-examples) repository.


## Building

See the main README.md of csound-extended for build instructions.

# Csound for WebAssembly

Authors: Edward Costello, Steven Yi, Henri Manson, Michael Gogins

## Introduction

THis directory builds, packages, and tests csound-extended for WebAssembly. 
This build replaces `CsoundObj.js` from the core Csound repository with two  
WebAssembly builds of Csound, `csound_extended.js` (using ScriptProcessorNode) 
and `CsoundAudioNode.js` (using the new standard AudioWorklet), which 
feature:

* A number of C++ plugin opcodes (here, statically linked).

* A new JavaScript interface to Csound that follows, as exactly as possible, 
  the interface defined by the Csound class in `csound.hpp` and also 
  implemented in CsoundOboe in `csound_oboe.hpp` for the Csound for Android 
  app, and in `csound.node`.
  
* Additional Csound API methods exposed to JavaScript.
  
See the README.md in the csound-extended directory for build instructions.

The examples herein will run from the [GitHub pages of the csound-extended 
repository](https://gogins.github.io/csound-extended/). To run the examples locally, either clone and build this 
repository, or download the `csound-wasm*.zip` release and unzip it. 

Then, run the self-contained Web server, `httpd.py`, in the WebAssembly 
directory, and navigate in your Web browser to localhost at the port reported 
at server startup.

The `cmask.html` example will run with `csound.node` in NW.js 30.2 or later, 
and will run with `CsoundAudioNode.js` in Chrome 66 or later. It should run 
with `csound_extended.js` in any browser or environment that supports 
WebAssembly.

The `csound_loader.js` script is provided as a convenient method of 
detecting and using available implementations of Csound that run in HTML5 
environments.
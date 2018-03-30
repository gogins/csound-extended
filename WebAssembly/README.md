# Csound for WebAssembly

Authors: Edward Costello, Steven Yi, Henri Manson, Michael Gogins

## Introduction

THis directory builds, packages, and tests csound-extended for WebAssembly. This 
build replaces CsoundObj.js from the core Csound repository with a new WebAssembly 
build of Csound, csound_extended.js, which features:

* A number of C++ opcodes (here, statically linked).

* A new JavaScript interface to Csound that follows, as exactly as possible, 
  the interface defined by CsoundThreaded in `csound_threaded.cpp` and also 
  implemented in CsoundOboe in `csound_oboe.hpp` for the Csound for Android 
  app, and in csound.node.
  
See the README.md in the csound-extended directory for build instructions.

The examples herein will run from the GitHub pages of the csound-extended 
repository. To run the examples locally, either clone and build this 
repository, or download the `csound-wasm` release and unzip it. 

Then, run the self-contained Web server, `httpd.py`, in the WebAssembly directory.
# Csound for WebAssembly

Authors: Edward Costello, Steven Yi, Henri Manson, Michael Gogins

## Introduction

THis directory builds, packages, and tests Csound for WebAssembly. This build 
is based upon the core Csound build for WebAssembly by Edward Costello, Steven 
Yi, Victor Lazzarini, and Henri Manson. To this have been added:

* A number of C++ opcodes (here, statically linked).

* A new JavaScript interface to Csound that follows, as exactly as possible, 
  the interface defined by CsoundThreaded in `csound_threaded.cpp` and also 
  implemented in CsoundOboe in `csound_oboe.hpp` for the Csound for Android 
  app, and in csound.node.
  
* A complete build of CsoundAC for WebAssembly will also be added.

## Requirements

* Install the toolchain from source [using these instructions](http://webassembly.org/getting-started/developers-guide/). 
  Building the toolchain is compute and memory intensive. If the build does not 
  complete or produces a zero size clang file, run the build step using only 
  one thread: `./emsdk install sdk-incoming-64bit binaryen-master-64bit -j1`.

## Build Instructions for WebAssembly

In the `WebAssembly` directory, run `sh build-all.sh`. This will perform the 
following steps, which may also be executed individually:

1. Update the Emscripten toolchain for WebAssembly.
2. Run `source emsdk_env.sh` in the `~/emsdk` directory which executes  
   `export EMSCRIPTEN_ROOT=$EMSCRIPTEN`.
3. Run `sh download_and_build_libsndfile_wasm.sh`. This will compile 
   `libsndfile-wasm.a` in `deps/libsndfile-1.0.25`.
2. Run the `build-wasm.sh` script using `sh build-wasm.sh`. This will create a 
   build folder, run cmake from there with the Csound source,
   then compile Csound with Emscripten for WASM. The script will copy
   the required files into a dist-wasm folder for distribution.

### Release Instructions

1. Run `sh build-wasm.sh`
2. Run `sh update_example_libs_from_dist_wasm.sh`
3. Update CS_VERSION in build-wasm.sh if necessary
4. Run `sh release-wasm.sh`

### Test Instructions

1. Change to the `examples-wasm` directory.
2. Run  `python httpd.py` to run a local Web server.
3. Run a WebAssembly enabled browser such as Chrome. Navigate to 
  `http://localhost:port_number` where `port_number` is the port printed when 
  `httpd.py` starts.
4. You should see a "Web Assembly Csound" page and be able to select an example 
   CSD from the list on the lower right.
5. Click on the _Compile_ and _Perform_ buttons to see if you can hear the CSD.

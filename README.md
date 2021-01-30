# csound-extended
![GitHub All Releases (total)](https://img.shields.io/github/downloads/gogins/csound-extended/total.svg)<br>
Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This repository contains various extensions to Csound that have been moved
out of the core Csound Git repository at https://github.com/csound/csound,
or collected from older projects of mine. These extensions include:

1.  CsoundAC, an algorithmic composition library designed to be used with
    Csound. CsoundAC is written in C++ and has C++, JavaScript, and Python 
    interfaces.

2.  csound.node, a C++ add-on that embeds Csound in the JavaScript context of
    Web pages running in MW.js from https://nwjs.io/.

3.  A port of the algorithmic composition program CMask by Andre Bartetzki
    to WebAssembly and to a Linux Csound plugin opcode.

6.  Csound for WebAssembly, almost all features of Csound as a WebAssembly
    module that will run Csound from a JavaScript interface in any current
    Web browser. This version includes useful plugin opcodes statically
    linked. Some live examples run from
    [here](https://github.com/gogins/csound-extended/tree/develop/docs).

7.  Silencio, a JavaScript algorithmic composition library designed to work
    with all Csound/HTML5 environments. __PLEASE NOTE: This library is now 
    deprecated, please use the WebAssembly build of CsoundAC instead.__

8.  nudruz, a Common Lisp library for algorithmic composition by Drew Krause,
    hosted here with his permission. This is based upon and includes
    the Common Music library for algorithmic composition and the Fomus
    library for automatically notating generated scores.
    
9.  A foreign function interface to the Csound shared library for the Haskell 
    programming language and the Euterpea package for algorithmic composition, 
    in the haskell directory. This is a dynamic FFI and does not require to be 
    pre-built in order to be used.
    
10. Some plugin opcodes for Csound, including bformdec2, chuap, cmask, MVerb, 
    and the STK opcodes.

With regret I must announce that CsoundVST and the vst4cs opcodes are no longer
maintained here. This is due to efforts by Steinberg to force developers to move
to the VST3 SDK. **However, CsoundVST and the vst4cs opcodes are still avaiable 
from me as freeware binaries from https://michaelgogins.tumblr.com/csound_extended.** 

As an alternative to the vst4cs opcodes, I recommend using Rory Walsh's 
[Cabbage](https://github.com/rorywalsh/cabbage) system for creating VST 
plugins using Csound. Or, consider using my 
[csound-vst3-opcodes](https://github.com/gogins/csound-vst3-opcodes) that provide 
VST3 plugin support for Csound on Linux. 

New extensions may be added by me in the future. If you would like to add your
own extensions, enter an issue in this repository or submit a pull request.

This repository uses the core Csound packages, and some other third-party
dependencies, as Git submodules, packages, or direct source downloads. For each
platform, there is one build system.

Please log any bug reports or requests for enhancements at
https://github.com/gogins/csound-extended/issues.

## Examples

Examples for the various aspects of csound-extended are maintained in my 
separate [csound-examples](https://github.com/gogins/csound-examples) 
repository. Some of the examples there will run in WebBrowsers using 
WebAssembly, and these can be viewed at https://gogins.github.io/csound-examples.

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit
log.

## Installation

1.  You must first install Csound on your system, e.g. as instructed at 
    `https://github.com/csound/csound`.

2.  CsoundAC and other Linux binaries and other resources are installed from
    the Debian package released from this repository, e.g.
    `sudo apt install ./csound-extended-dev-version-Linux.deb`.  Please note,
    this package conflicts with the system packages for CsoundAC. The Lisp
    systems are installed in /usr/share/common-lisp/csound-extended-dev/
    but in order to load nudruz.asd you must first install a number of its
    dependencies, listed in nudruz.asd. Some of these can be installed
    as Linux packages, some must be installed by cloning Git repositories,
    some must be installed using Quicklisp, and some must be installed by
    downloading archives. In all cases except for system packages, you must
    create a symbolic link to your ~/.local/share/common-lisp/source/
    directory.

2.  The  silencio library and WebAssembly build of Csound are packaged in
    the csound-extended-wasm-version.zip archive released from this
    repository.

You may also install locally by first building from sources, as described
below. You may then install the software by running `sudo make install` in
the build-linux directory. However, be warned that this installs the
software in /usr/local. 

There are files and directories in the Git repository and in the packages that 
can be used as helpers for csound-extended. You can create symbolic links from 
these files to your home directory or other places.

- `build-env.sh`: Source this to set useful environment variables for the 
  build and runtime environment on Linux. You may need to copy and modify this 
  script.
  
- Create a symbolic link from `csound-extended/.SciTEUser.properties` to your 
  home directory, to create custom commands and editor features in the SciTE 
  text editor. This makes it possible to run various kinds of Csound pieces, 
  and even to build C++ pieces and plugin opcodes, from the editor. Believe 
  me, I tried all the other editors, and this is the one that is both simple 
  and useful.

- `run_nwjs_application.sh`: Create a symbolic link to this script in your 
  home dirctory to assist with running pieces written for csound.node that run 
  in NW.js. This also requires installing the SDK version of NW.js and 
  creating a symbolic link from the installation directory to `nwjs` in your 
  home directory.

- `silencio`: Create a symbolic link to this directory in every directory in 
  which you are writing or running a piece that uses the Silencio library.

## Building

Currently, the supported platforms are Linux and WebAssembly.
The code is generally "cross-platform" in nature and this build system could
be adapted to build for Windows or OS X.

### Build and Install Csound

The system packages for Csound are out of date, so you must perform a local 
build and installation of Csound. Clone the Csound Git repository from 
`https://github.com/csound/csound`, and build and install Csound according to 
the instructions there. 

### Build, Package, and Install csound-extended

First clone the Git repository at https://github.com/gogins/csound-extended.

#### Building on Linux

The build script involves some user interaction for sudo or deletions.
Otherwise, the build is highly automated. Many dependencies are local. All
dependencies are fetched automatically. Most targets are built for release
with debug information. I have tried to keep configuration options, and 
manual configuration steps, to an absolute minimum, all controlled by 
environment variables in `build-env.sh`.

When the build is complete, all targets have been built and the package 
files have been generated.

Manual configuration steps include, but are not necessarily limited to:

1. Node.js and npm must be installed, not from any Linux 
package repository, but according to the instructions for binary archives at 
https://github.com/nodejs/help/wiki/Installation. When that has been done, 
execute `npm install -g node-gyp` and `npm install -g node-addon-api`. Also 
put node-gyp into your executable PATH.

2. Lance Putnum's Gamma library for C++ audio signal 
processing must be cloned from GitHub, built with the addition of the 
`-fPIC` compiler option, and installed (CMake should be able to find it in 
the `/usr/local` tree).

3. The OpenCV library with codecs must be downloaded as source from 
`https://opencv.org/releases/`, built, and installed.

4. The following environment variables MUST be set before building, perhaps in
your .profile script. Obviously, modify the paths as required to suit your
home directory and installation details. These are exported in `build-env.sh` 
which you can source in your .profile script.

```
CSOUND_SRC_ROOT=/home/mkg/csound-extended/dependencies/csound
NODE_PATH=/home/mkg/csound/csound/frontends/nwjs/build/Release
OPCODE6DIR64=/usr/local/lib/csound/plugins64-6.0
RAWWAVE_PATH=/home/mkg/stk/rawwaves
export PATH=/usr/local/lib/node-v12.14.1-linux-x64/bin:${PATH}
unset NODE_ADDON_API_INCLUDE
export NODE_ADDON_API_INCLUDE=/usr/local/lib/node-v12.14.1-linux-x64/lib/node_modules/node-addon-api

```

The very first time you build csound-extended, go to about line 280 in 
CMakeLists.txt and do as it says there:
```
# For your first build on your system, set this to "OFF", build, and install.
# Then, set this to "ON", rebuild, and reinstall. This is a workaround for a 
# bug in how CPack interacts with shlibdeps.
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
```
Change to your csound-extended repository and execute `fresh-build-linux-release.sh`,
which does the following:

1.  Execute `bash update-dependencies.sh`. Do this periodically or whenever
    you think a dependency has changed.

2.  Execute `bash build-linux.sh`. The build compiles all targets and creates
    all packages.

Subsequently, you can perform these steps independently.

To make clean, execute `bash clean-linux.sh`.

To install, change to build-linux and execute `sudo install
./csound-extended-dev-{version}-Linux.deb --reinstall`.

#### Building for WebAssembly

First, install the Emscripten SDK according to instructions at 
`https://emscripten.org/docs/getting_started/downloads.html`.

You will need to make sure that the Eigen library for matrix algebra is 
available to the Emscripten toolchain. The easiest way to do this is to 
install the `libeigen3-dev` system package, and then create a symbolic 
link to the system eigen3 include directory to the Emscripten C++ include 
directory:
```
mkg@xenakis:~/emsdk/upstream/emscripten/system/include/libcxx$ 
mkg@xenakis:~/emsdk/upstream/emscripten/system/include/libcxx$ ln -s /usr/include/eigen3 eigen3
mkg@xenakis:~/emsdk/upstream/emscripten/system/include/libcxx$ ls eigen3
```

Then, to build for WebAssembly for the first time, change to the WebAssembly
subdirectory of this repository and execute:

- The script `bash fresh-build-build.sh`, which performs the following steps:

    - Updates the Emscripten toolchain.
    
    - Executes `bash clean-build-wasm.sh`, which:
    
        * Cleans the previous build.
        
        * Runs `bash download-and-build-libsndfile-wasm.sh.`
        
        * Runs `bash build-wasm.sh`, which:
        
            > Builds the CMask opcodes for Csound. 
            
            > Builds the Csound static library. 
            
            > Builds two Csound JavaScript interface classes.
            
            > Runs `bash release-wasm.sh` to package the build and examples for release. 

Any of the bash scripts in this sequence can be run independently, and will 
continue to the end of the sequence.

#### Building csound.node

If csound.node fails to build: 

1.  You may need to add the NPM bin directory to your PATH variable so that 
    CMake can find node-gyp.
    
2.  You may need to manually configure `csound.node/binding.gyp` to explicly
    include the directory containing `napi.h` more or less as follows:
    ```
    'target_defaults': 
    {
       "cflags!": [ "-fno-exceptions" ],
        "cflags_cc!": [ "-fno-exceptions" ],
        "include_dirs": 
        [
            ## This is theoretically required but causes the build to fail: 
            ## "<!@(node -p \"require('node-addon-api').include\")",
            ## This does work but must be manually configured here:
            "/usr/local/lib/node-v12.14.1-linux-x64/lib/node_modules/node-addon-api",
        ],
    ```



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

2.  A port of the algorithmic composition program CMask by Andre Bartetzki
    to WebAssembly and to a Linux Csound plugin opcode.

3.  Silencio, a JavaScript algorithmic composition library designed to work
    with all Csound/HTML5 environments. __PLEASE NOTE: This library is now 
    deprecated, please use the WebAssembly build of CsoundAC instead.__
  
4.  Some plugin opcodes for Csound, including bformdec2, chuap, cmask, MVerb, 
    and the STK opcodes.
    
With regret I must announce that CsoundVST and the vst4cs opcodes are no longer
maintained here. This is due to efforts by Steinberg to force developers to move
to the VST3 SDK. **However, CsoundVST and the vst4cs opcodes are still avaiable 
from me as freeware binaries from https://michaelgogins.tumblr.com/csound_extended.** 

As an alternative to the vst4cs opcodes, I recommend using Rory Walsh's 
[Cabbage](https://github.com/rorywalsh/cabbage) system for creating VST 
plugins using Csound. Or, consider using my 
[csound-vst3-opcodes](https://github.com/gogins/csound-vst3-opcodes) that provide 
VST3 plugin support for Csound on Linux, and soon on Mac OS and Windows. 

New extensions may be added by me in the future. If you would like to add your
own extensions, enter an issue in this repository or submit a pull request.

This repository uses the core Csound packages, and some other third-party
dependencies, as Git submodules, packages, or direct source downloads. For each
platform, there is one build system.

Please log any bug reports or requests for enhancements at
https://github.com/gogins/csound-extended/issues.

## Examples

Examples (some of which can also serve as tests) for the various aspects of 
csound-extended are maintained in my separate 
[csound-examples](https://github.com/gogins/csound-examples) 
repository. Some of the examples there will run in WebBrowsers using 
WebAssembly, and these can be viewed at 
https://gogins.github.io/csound-examples.

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit
log.

## Installation

1.  You must first install Csound on your system, e.g. as instructed at 
    `https://github.com/csound/csound`.
    
2.  CsoundAC can both import and export MusicXML scores. To enable this,
    you must be first clone the master branch of libmusicxml from 
    `https://github.com/grame-cncm/libmusicxml/tree/dev/build` and then 
    build according to `https://github.com/grame-cncm/libmusicxml/tree/dev/build` 
    and finally `sudo make install`.

3.  CsoundAC and other Linux binaries and other resources are installed from
    the Debian package released from this repository, e.g.
    `sudo apt install ./csound-extended-dev-version-Linux.deb`.  Please note,
    this package conflicts with the system packages for CsoundAC.
    
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
  
- Create a symbolic link from `csound-extended/playpen/.SciTEUser.properties` to your 
  home directory, to create custom commands and editor features in the SciTE 
  text editor. This makes it possible to run various kinds of Csound pieces, 
  and even to build C++ pieces and plugin opcodes, from the editor. Believe 
  me, I tried all the other editors, and this is the one that is both simple 
  and useful. For more information, see `playpen/README.md`.

- `silencio`: Create a symbolic link to this directory in every directory in 
  which you are writing or running a piece that uses the Silencio library.

## Building

Currently, the supported platform is Linux. The code is generally 
"cross-platform" in nature and this build system could be adapted to build for 
Windows or OS X.

### Build and Install Csound

The system packages for Csound are out of date, so you must perform a local 
build and installation of Csound. Clone the Csound Git repository from 
`https://github.com/csound/csound`, and build and install Csound according to 
the instructions there. 

### Build, Package, and Install csound-extended

First clone the Git repository at https://github.com/gogins/csound-extended.

#### Building on Linux

The build is highly automated. Many dependencies are local. Most dependencies 
are fetched automatically. Most targets are built for release with debug 
information. I have tried to keep configuration options, and manual 
configuration steps, to an absolute minimum, all controlled by environment 
variables in `build-env.sh`.

When the build is complete, all targets have been built and the package 
files have been generated.

Manual configuration steps include, but are not necessarily limited to:

1. Lance Putnum's Gamma library for C++ audio signal 
processing must be cloned from GitHub, built with the addition of the 
`-fPIC` compiler option, and installed (CMake should be able to find it in 
the `/usr/local` tree).

2.  CsoundAC can both import and export MusicXML scores. To enable this,
    in the `csound-extended/dependencies/libmusicxml` directory,  
    build according to `https://github.com/grame-cncm/libmusicxml/tree/dev/build` 
    and finally `sudo make install`.

3. The following environment variables MUST be set before building, perhaps in
your `.profile` script. Obviously, modify the paths as required to suit your
home directory and installation details. These are exported in `build-env.sh` 
which you can source in your `.profile` script.

```
CSOUND_SRC_ROOT=/home/mkg/csound-extended/dependencies/csound
OPCODE6DIR64=/usr/local/lib/csound/plugins64-6.0
RAWWAVE_PATH=/home/mkg/stk/rawwaves

```

The very first time you build csound-extended, go to about line 260 in 
CMakeLists.txt and do as it says there:
```
# For your first build on your system, set this to "OFF", build, and install.
# Then, set this to "ON", rebuild, and reinstall. This is a workaround for a 
# bug in how CPack interacts with shlibdeps.
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
```
Change to your csound-extended repository and execute
```
bash fresh-build-linux-release.sh`
```

during which sudo may prompt you for your password. This script does the 
following:

1.  Execute `bash update-dependencies.sh`. Do this periodically or whenever
    you think a dependency has changed.

2.  Execute `bash build-linux.sh`. The build compiles all targets and creates
    all packages.

Subsequently, you can perform these steps independently.

To make clean, execute `bash clean-linux.sh`.

To install, change to build-linux and execute `sudo install
./csound-extended-dev-{version}-Linux.deb --reinstall`.




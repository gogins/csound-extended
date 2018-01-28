# CSOUND-EXTENDED

Version 0.1.0
Michael Gogins

This repository contains various extensions to Csound that have been moved 
out of the core Csound Git repository at https://github.com/csound/csound. 
These extensions include:

1.  CsoundAC, an algorithmic composition library designed to be used with 
    Csound. Csound is written in C++ and has C++, Python, Java, and Lua 
    interfaces.
   
2.  csound.node, a C++ add-on that embeds Csound in the JavaScript context of 
    Web pages running in nwjs from https://nwjs.io/.
   
3.  CsoundHtml5, a lightweight editor and "front end" for Csound that embeds 
    HTML5 capabilities for Csound.

4.  CsoundVST, Csound in the form of a VST plugin.

5.  Csound for Android, almost all features of Csound in an Android app that 
    also integrates Csound with HTML5. Please note, dependencies of Csound 
    for Android are fetched from the core Csound repository, and rebuilt 
    using the Android NDK.

This repository uses the core Csound repository, and some other third-party 
dependencies, as Git submodules, packages, or direct source downloads. There 
is one build system for it all.

## Installation

Currently this project is in an alpha stage of development. Right now building 
from sources is the best way to install it. The build scripts _should_ install 
all required dependencies as well as csound-extended.

## Building

Currently, the only supported operating systems are Linux and Android. The 
code is generally "cross-platform" in nature and this build system could in 
the future be adapted to build for Windows or OS X.
 
### Building on Linux

The build script involves some user interaction for sudo or deletions. 
Otherwise, the build is highly automated. Many dependencies are local. All 
dependencies are fetched automatically. Most targets are built for release 
with debug information. There are few (ideally, no) configuration options. 
When the build is complete, all targets have been installed and a number of 
package files have been produced.

To build on Linux for the first time, change to the root directory and execute 
`fresh-build-linux.sh`, which does the following:

1.  Change to the root directory of this repository.

2.  Execute `bash update-dependencies.sh`. Do this periodically or whenever 
    you think a dependency has changed.
    
3.  Execute `bash build-linux.sh`. For each target, the build produces an 
    installation, some archives, and a Debian package.

4.  To make clean, execute `bash clean-linux.sh`. 

Or, to do all of the above in one step, execute `fresh-build-linux.sh`.

### Building for Android

Building for Android is similar. 

Please note, some NDK dependencies are built in their own subdirectories, 
and some are built in OTHER subdirectories with their own makefiles that 
refer to source files in the ORIGINAL subdirectories. There is a naming 
convention, e.g. `link` is the original Git repository for the Ableton Link 
Kit which we do not build, and `link-opcodes` is our subdirectory which we do 
build and which includes files from the `link` subdirectory.

Prerequisites for building include:

1.  You must install Android Studio 3.0.1, Android SDKs 28, 27.1.1, 23, and 21, 
    GDB, LLDB, the NDK, and build tools 26.0.1.

2.  In order to enable local NDK builds, you must set the following 
    environment variables:
    
    2.1.    `ANDROID_NDK_ROOT` with the full pathname of your Android Native 
            Development kit, typically `$ANDROID_SDK_ROOT/ndk-bundle`.
    2.2.    `ANDROID_SDK_ROOT` with the full pathname of your Android Software 
            Development kit, perhaps something like `~/Android/Sdk`.
    2.3.    `CSOUND_SRC_ROOT` with the full pathname to this repository's 
            `dependencies/csound` subdirectory.
    2.4.    `NDK_MODULE_PATH` with the full pathname to this repository's 
            `CsoundForAndroid/ndk-libraries` subdirectory.

To build for Android on Linux for the first time, change to the 
CsoundForAndroid subdirectory of this repository and execute 
`fresh-build-android.sh`, which does the following:

1.  Execute `bash update-dependencies`. Do this periodically or whenever 
    you think a dependency has changed.
    
2.  Execute `bash build-android.sh`. The build system compiles all native 
    libraries, including the Csound library `libcsoundandroid.so`, required 
    by the Csound for Android app, and copies them to the 
    appropriate subdirectories for further building and packaging.
    
3.  Run Android Studio and load the `CsoundForAndroid/build.gradle` project.

4.  Attach an Android device, enable USB debugging on it, and debug the 
    CsoundApplication project.
    
5.  For a production build, apply to me for the signing key, build for 
    release, and generate a signed `.apk`.

## License

Csound is copyright (c) 1991 Barry Vercoe, John ffitch, and other contributors.
csound-extended is copyright (c) 2018 by Michael Gogins.

Csound and csound-extended are free software; you can redistribute them
and/or modify them under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Csound and csound-extended are distributed in the hope that they will be 
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser 
General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this software; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
02110-1301 USA


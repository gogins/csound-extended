# csound-extended

[![Github All Releases (total)](https://img.shields.io/github/downloads/gogins/csound-extended/total.svg)]()

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This repository contains various extensions to Csound that have been moved 
out of the core Csound Git repository at https://github.com/csound/csound, 
and collected from older projects of mine. These extensions include:

1.  CsoundAC, an algorithmic composition library designed to be used with 
    Csound. Csound is written in C++ and has C++, Python, Java, and Lua 
    interfaces.
   
2.  csound.node, a C++ add-on that embeds Csound in the JavaScript context of 
    Web pages running in MW.js from https://nwjs.io/.
   
3.  CsoundHtml5, a lightweight editor and "front end" for Csound that embeds 
    HTML5 capabilities for Csound. This makes it possible to compose music by 
    writing Web pages (.html files) in which Csound appears as a `csound` 
    JavaScript object that implements a substantial subset of the Csound C++ 
    API and that follows exactly the same semantics.
    
4.  A port of the algorithmic composition program CMask by Andre Bartetzki 
    to WebAssembly and to a Linux Csound plugin opcode.

5.  Csound for Android, almost all features of Csound in an Android app that 
    also integrates Csound with HTML5. Please note, dependencies of Csound 
    for Android are fetched from the core Csound repository, and rebuilt 
    using the Android NDK.
    
6.  Csound for WebAssembly, almost all features of Csound as a WebAssembly 
    module that will run Csound from a JavaScript interface in any current 
    Web browser. This version includes useful plugin opcodes statically 
    linked. Some live examples run from 
    [here](https://github.com/gogins/csound-extended/tree/develop/docs).
    
7.  Silencio, a JavaScript algorithmic composition library designed to work 
    with all Csound/HTML5 environments.
    
9.  An online _playable_ version of the [_**Csound Reference Manual**_](https://gogins.github.io/csound-extended/html/indexframes.html).

10. nudruz, a Common Lisp library for algorithmic composition by Drew Krause, 
    hosted here with his permission. This is based upon and includes 
    the Common Music library for algorithmic composition and the Fomus 
    library for automatically notating generated scores.
    
With regret I must announce that CsoundVST and the vst4cs opcodes are no 
longer maintained here. This is due to efforts by Steinberg to force 
developers to move to the VST3 SDK, which has a more restrictive license that 
I do not wish to work with. However, these are avaiable from me as freeware 
binaries from https://michaelgogins.tumblr.com/csound_extended.
     
New extensions may be added by me in the future. If you would like to add your 
own extensions, enter an issue in this repository or submit a pull request.

This repository uses the core Csound repository, and some other third-party 
dependencies, as Git submodules, packages, or direct source downloads. For 
each platform, there is one build system.

Please log any bug reports or requests for enhancements at 
https://github.com/gogins/csound-extended/issues.

## Examples

Some tests of basic functionality that also serve as examples and can be used 
as templates for new compositions are installed in the 
/usr/share/doc/csound-extended-dev/test-examples directory.

## Changes

The major change for version 1.0.0 is the creation of regular Debian packages.

CsoundAC has gained the ability to run Common Lisp code for generating or 
modifying generated scores.

CsoundAC has been refactored so that the csnd6 library interfaces for Lua 
and Python are not required. Most methods of CppSound have been exposed in the 
MusicModel class, e.g. CppSound::CompileCsdText is exposed as 
MusicModel::cppsoundCompileCsdText.

CsoundForAndroid has been upgraded to target Android API version 26. It also 
has lost the LuaJIT opcodes because the Android NDK must now use clang, but 
LuaJIT cannot be built for Android with clang.

See https://github.com/gogins/csound-extended/commits/develop for the commit log.

## Installation

1.  CsoundAC and other Linux binaries and other resources are installed from 
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
    
3.  The Csound for Android app is available from the Google Play Store, or may 
    be installed from the CsoundApplication-release.apk package released 
    from this repository.
    
You may also install locally by first building from sources, as described 
below. You may then install the software by running `sudo make install` in 
the build-linux directory. However, be warned that this installs the 
software in /usr/local. You will also first need to separately install 
Csound.

# Using a Local Build of Csound on Linux

Before building csound-extended, be aware that you can use csound-extended 
with your own local build of Csound from the Csound git repository.

To do this, first build  **and install** csound-extended using the 
instructions below, which automatically **installs the official Csound packages.** 
Run some pieces that use features you need to test your installation. 

Then, clone the Csound Git repository from `https://github.com/csound/csound`, 
and build Csound according to the instructions there. However, when running CMake, 
change the value of the default CMake install prefix from `/usr/local` to `/usr`, 
like this:
```
cmake .. -DCMAKE_INSTALL_PREFIX=/usr
```
After that of course do the usual `sudo make install` for Csound.

Finbally, re-run your tests to make sure the features you need are still 
working and that the newer Csound is compatible with csound-extended. If not 
all features you need work, e.g. the Python interfaces, do a local build and 
install of csound-extended, **i.e. without updating dependencies** (`bash 
build-linux.sh`), and re-install your local build of csound-extended over 
your existing installation, like this:
```
sudo apt install ./build-linux/csound-extended-dev-1.3.1-Linux.deb --reinstall
```

If ever need to revert to the original packaged version of Csound 
normally used by csound-extended, in your local Csound build directory, do this:
```
sudo xargs rm < install_manifest.txt
```
and then do a fresh build and installation of csound-extended **including 
updating Csound** (`bash fresh-build-linux.sh`).

## Building

Currently, the supported platforms are Linux, Android, and WebAssembly. 
The code is generally "cross-platform" in nature and this build system could 
be adapted to build for Windows or OS X.

Please note, the Linux package for native code (CsoundAC etc.) is built using 
the Ubuntu system packages for Csound, whereas the Csound for Android app and 
the WebAssembly module are built from Csound source code cloned from GitHub.
 
First clone the Git repository at https://github.com/gogins/csound-extended.

### Building on Linux

Currently, building all of csound-extended requires Linux 17.10 (Artful 
Aardvark) or higher, as earlier releases of Linux do not have packages for 
the Qt WebEngine used by CsoundHtml5.

The build script involves some user interaction for sudo or deletions. 
Otherwise, the build is highly automated. Many dependencies are local. All 
dependencies are fetched automatically. Most targets are built for release 
with debug information. There are few (ideally, no) configuration options. 
When the build is complete, all targets have been built and the package 
files have been generated.

The following environment variables MUST be set before building, perhaps in 
your .profile script. Obviously, modify the paths as required to suit your 
home directory and installation details.

```
CSOUND_SRC_ROOT=/home/mkg/csound-extended/dependencies/csound
CSOUND_EXTENDED_VERSION=0.1.2
NODE_PATH=/home/mkg/csound/csound/frontends/nwjs/build/Release
OPCODE6DIR64=/usr/local/lib/csound/plugins64-6.0
RAWWAVE_PATH=/home/mkg/stk/rawwaves
```

If csound.node fails to build, you may need to add the NPM bin directory to 
your PATH variable so that CMake can find node-gyp.

Change to your csound-extended repository and execute `fresh-build-linux.sh`, 
which does the following:

1.  Execute `bash update-dependencies.sh`. Do this periodically or whenever 
    you think a dependency has changed.
    
2.  Execute `bash build-linux.sh`. The build compiles all targets and creates 
    all packages.

Subsequently, you can perform these steps independently.

To make clean, execute `bash clean-linux.sh`. 

To install, change to build-linux and execute `sudo install 
./csound-extended-dev-1.3.1-Linux.deb --reinstall`.

Please note, some NDK dependencies are built in their own subdirectories, 
and some are built in OTHER subdirectories with their own makefiles that 
refer to source files in the ORIGINAL subdirectories. There is a naming 
convention, e.g. `link` is the original Git repository for the Ableton Link 
Kit which we do not build, and `link-opcodes` is our subdirectory which we do 
build and which includes files from the `link` subdirectory.

Prerequisites for building include:

1.  You must install Android Studio 3.0.1, Android SDKs 28, 27.1.1, 23, and 21, 
    GDB, LLDB, the NDK, and build tools 26.0.2.

2.  In order to enable local NDK builds (i.e. in individual subdirectories), 
    you must set the following environment variables, probably in your 
    .profile script:
    
    2.1.    ANDROID_NDK_ROOT with the full pathname of your Android Native 
            Development kit, typically $ANDROID_SDK_ROOT/ndk-bundle.
            
    2.2.    ANDROID_SDK_ROOT with the full pathname of your Android Software 
            Development kit, perhaps something like ~/Android/Sdk.
            
    2.3.    CSOUND_SRC_ROOT with the full pathname to this repository's 
            dependencies/csound subdirectory.
            
    2.4.    NDK_MODULE_PATH with the full pathname to this repository's 
            CsoundForAndroid/ndk-libraries subdirectory.

To build for Android on Linux for the first time, change to the 
CsoundForAndroid subdirectory of this repository and execute 
`bash fresh-build-android.sh`, which does the following:

1.  Execute `bash update-dependencies`. Do this periodically or whenever 
    you think a dependency has changed.
    
2.  Execute `bash build-android.sh`. The build system compiles all native 
    libraries, including the Csound library libcsoundandroid.so, required 
    by the Csound for Android app, and copies them to the 
    appropriate subdirectories for further building and packaging.
    
Run Android Studio and load the CsoundForAndroid/build.gradle project.

Attach an Android device, enable USB debugging on it, and debug the 
CsoundApplication project.
    
For a production build, apply to me for the signing key, build for 
release, and generate a signed .apk.
    
### Building for WebAssembly

To build for WebAssembly for the first time, change to the WebAssembly 
subdirectory of this repository and execute `bash fresh-build.sh`,  which 
does the following:

1.  Updates the Emscripten SDK.

2.  Builds Csound for WebAssembly.

4.  Creates a release package.






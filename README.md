# CSOUND-EXTENDED

Version 0.1.0
Michael Gogins

This repository contains various extensions to Csound that have been moved 
out of the core Csound Git repository at https://github.com/csound/csound. 
These extensions include:

1.  CsoundAC, an algorithmic composition library designed to be used with 
    Csound, with C++, Python, Java, and Lua interfaces.
   
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
is one CMake build for it all.

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


# FAUST OPCODES IN C++ FOR CSOUND

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

[Faust](https://faust.grame.fr/) is a sophisticated language and toolchain for 
writing digital signal processing code, then compiling Faust source code to 
other languages. Faust maintains a clean separation between the "user" layer 
for DSP and the "architecture" layer that runs the DSP code.

Currently, on Ubuntu 20.04, Faust 2.2.0 is available as a system package. The 
`faust2csound` tool from this version does not generate C++ code compatible 
with the current version of Csound. On Ubuntu, up to date versions of Faust 
are available, but only by building from source code. Unfortunately I have 
not been able to build Faust on Linux because `make all;sudo make install` 
fails with many errors that I do not have time fix.

It is fortunately *very* easy to patch version 2.2.0 of Faust to generate the 
correct C++ code for current versions of Csound.

In the "architecture" file for Csound, `/usr/lib/faust/csound.cpp`, near the 
bottom, change the opcode registration code from:
```
extern "C" {
    static OENTRY localops[] = {
        {(char*)sym(OPCODE_NAME), sizeof(dataspace), 0, 7, makeDescription(FAUST_OUTPUTS), makeDescription(FAUST_INPUTS, FAUST_ACTIVES),
            (SUBR)init, NULL, (SUBR)process32bits }
    };
    LINKAGE
}
```
to:
```
extern "C" {
    static OENTRY localops[] = {
        {(char*)sym(OPCODE_NAME), sizeof(dataspace), 0, 3, makeDescription(FAUST_OUTPUTS), makeDescription(FAUST_INPUTS, FAUST_ACTIVES),
            (SUBR)init, (SUBR)process32bits, NULL }
    };
    LINKAGE
}
```

Also, change `#include "csdl.h"` to `#include <csound/csdl.h>`.

Faust will then generate correct C++ code for the current version of Csound.

Here is an example of how to generate a Csound plugin opcode from the Faust 
`piano.dsp` example on Ubuntu Linux.

Create a working directory.

Copy the `piano.dsp` source code file from the Faust distribution into the 
working directory.

Generate C++ source code for a Csound opcode plugin:
```
faust2csound piano.dsp
```
This will generate C++ code and compile it with optimizations.

Test the opcode:
```
csound faust_piano_test.csd
```

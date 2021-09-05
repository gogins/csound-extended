# FAUST OPCODES IN C++ FOR CSOUND
Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

Faust is a sophisticated tool for writing digital signal processing code in 
the Faust language, then compiling the Faust source code to other languages.

Currently, on Ubuntu 20.04, Faust is available as a system package version 
2.2.0. The faust2csound tool from this version does not currently generate C++ 
code compatible with the current version of Csound, but the generated code is 
quite easy to patch.

More up to date versions of Faust are available on Ubuntu only by building 
from source in the master-dev branch, but "make all;sudo make install" fails 
with a number of obvious bugs.

It is fortunately easy to patch version 2.2.0 of Faust to generate the 
correct C++ code for Csound.

In the `/usr/lib/faust/csound.cpp` file, near the bottom, change the opcode 
registration code from:
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

Faust will then generate correct C++ code for Csound.

The following example provides instructions for generating a usable Csound 
plugin opcode from the Faust piano.dsp example, on Ubuntu Linux.

Create a working directory.

Copy the piano.dsp source code file into the working directory.

Generate C++ source code for a Csound opcode plugin:
```
faust2csound piano.dsp
```

Compile the piano.dsp.cpp file:
```
g++ piano.dsp.cpp -DOPCODE_NAME=faust_piano -DUSE_DOUBLE -Dlinux --std=gnu++17 -lstdc++fs -Wno-write-strings -O3 -g -fPIC -shared -I. -iquote /usr/local/include/csound -I$HOME/faust/examples/physicalModeling/faust-stk -I/usr/local/include -I/usr/local/include/csound -I/usr/include/csound -lm -olibfaust_piano.so
```
Copy the plugin to Csound's plugin directory:
```
sudo cp libfaust_piano.so /usr/local/lib/csound/plugins64-6.0/
```
Test the opcode:
```
csound faust_piano_test.csd
```

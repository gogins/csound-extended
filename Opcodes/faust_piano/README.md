# FAUST OPCODES IN C++ FOR CSOUND
Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

Faust is a sophisticated tool for writing digital signal processing code in 
the Faust language, then compiling the Faust source code to other languages.

The faust2csound tool does not currently generate C++ code compatible with 
the current version of Csound, but the generated code is quite easy to patch.

The following example provides instructions for generating a usable Csound 
plugin opcode from the Faust piano.dsp example, on Ubuntu Linux.

Create a working directory.

Copy the piano.dsp source code file into the working directory.

Generate C++ source code for a Csound opcode plugin:
```
faust2csound piano.dsp
```

Near the bottom of the generated piano.dsp.cpp file, change the opcode 
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
# clang

clang - Compiles C/C++ source code at Csound performance time to a loaded 
object module. This object modulle is callable in the running Csound process, 
and can call into the running Csound process using the Csound API. The module 
new Csound opcodes, or call the Csound API to execute arbitrary Csound code.

## Description

The `clang` opcode provides a just-in-time compiler that enables Csound to 
compile C or C++ source code, embedded in the Csound orchestra, to an object 
module; link that module; load that module; and call the Csound API from 
that module.

The `clang` opcode uses the [Clang library and LLVM](https://llvm.org/), and 
is based on the ["Clang C Interpreter Example"](https://github.com/llvm/llvm-project/tree/main/clang/examples/clang-interpreter). 
The `clang` opcode was inspired by the [`faustgen`](https://csound.com/docs/manual/faustgen.html) 
opcode.

## Syntax
```
i_result clang S_source_code, S_compiler_options [, S_link_libraries]
```
## Initialization

*S_source_code* - C or C++ source code, usually a multi-line string literal 
enclosed in `{{` and `}}`.

*S_compiler_options* - Standard gcc/clang compiler options, as would be passed 
on the compiler command line; can be a multi-line string literal enclosed in 
`{{` and `}}`.

*S_link_libraries* - Optional space-delimited list of link libraries, serving 
the same function as the `-l` option for a standalone compiler. Here, however, 
each link library must be its fully qualified filepath. Each library will be 
loaded and linked by llvm before the user's code is JIT compiled.

*i_result* - 0 if the code has been compiled, linked, loaded, and registered; 
non-0 if there is an error. Diagnostics are printed as Csound messages.

## Performance

The `clang` opcode must be invoked in the orchestra header, and is i-time only. 
The opcode is called after `csoundStart` has been called, and at the time that 
Csound is beginning performance by running the init pass in the orchestra 
header (i.e., the init pass for `instr 0`).

Non-standard include directories, link libraries, and compiler options may be 
used, but must be defined in `S_compiler_options` and `S_link_libraries`.

PLEASE NOTE: Only link libraries that are compatible with both gcc and Clang 
may be used. For example, use `-stdlib=libstdc++`. The `<iostream>` header may 
not be used.

The compiled module must define the following C function:
```
int csound_main(CSOUND *csound);
```
Once the `clang` opcode has compiled, linked, and loaded as an object module, 
Csound will call the `csound_main` function in that module. The `csound_main` 
function may then register any opcodes defined in the module using 
`csound->AppendOpcode`, or create new instruments in the Csound orchestra using 
`csound->CompileText`, or indeed do anything that can be done in C, C++, or using 
the Csound API, or using Csound score or orchestra code.

Once an opcode has been registered in `csound_main`, the Csound 
performance may invoke the opcode just like any other opcode.

Once an instrument has been defined in `csound_main`, the 
instrument may be performed by the Csound score just like any other 
instrument. Note that instruments get numbers strictly according to 
the order in which those instruments are defined.

## Example

The `csound clang_opcode_test.csd` file uses the `clang` opcode to compile a piano 
opcode and instrument, a reverb opcode and instrument, and a score generating 
instrument, which then algorithmically generate and render a piece. For the sake 
of clarity, although all of the code could be implemented in one module, the 
following separate modules are defined:

1. A physically modelled piano instrument opcode, written in C++, which is then 
    wrapped in a Csound instrument definition.
   
2. A reverb opcode, written in C++, which is then wrapped in a Csound instrument 
   definition.

3. A score generating opcode, written in C++, which is then wrapped in a Csound 
   instrument definition.
   
The Csound orchestra in this piece uses the signal flow graph opcodes to connect 
the piano instrument to the reverb instrument, and to connect the reverb 
instrument to an output instrument.

The Csound score in this piece invokes the score generating instrument twice at 
an offset in time and pitch, to create a canon at the sixth.

## Installation

1. Install Clang and LLVM from the stable branch, see https://apt.llvm.org/.

2. Compile the `clang_opcode.cpp` file using `build.sh`, which you may have to 
   modify for your system.
   
3. Test by executing `csound clang_opcode_test.csd`. 

# Credits

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com


# clang

clang - Compiles C/C++ source code at Csound performance time to a loaded 
object module, which is callable in the running Csound process. Such code can 
pretty much do anything that C/C++ code can do, including define new Csound 
opcodes, or call the Csound API to execute arbitrary Csound code.

## Description

The `clang` opcode provides a runtime compiler that enables Csound to 
compile C or C++ source code, embedded in the Csound orchestra, to an object 
module; link that module; load that module; and call the Csound API from 
that module.

The `clang` opcode usea the [Clang library and LLVM](https://llvm.org/), and 
is based on the ["Clang C Interpreter Example"](https://github.com/llvm/llvm-project/tree/main/clang/examples/clang-interpreter). 
This opcode was inspired by the [`faustgen`](https://csound.com/docs/manual/faustgen.html) opcode.

## Syntax
```
i_result clang S_source_code [, S_compiler_options]
```
## Initialization

S_source_code - C or C++ source code, usually a multi-line string literal 
enclosed in `{{` and `}}`.

S_compiler_options - Standard gcc/clang compiler options, usually a multi-line 
string literal enclosed in `{{` and `}}`.

i_result - 0 if the code has been compiled, linked, loaded, and registered; 
non-0 if there is an error. Diagnostics are printed as Csound messages.

## Performance

The `clang` opcode must be invoked in the orchestra header, and is i-time only. 

Non-standard include directories, link libraries, and compiler options may be 
used, but must be defined in `S_compiler_options`.

PLEASE NOTE: Only link libraries that are compatible with both gcc and Clang 
may be used. For example, use `-stdlib=libstdc++`.

The compiled module must define and export the following C function:
```
extern "C" int csound_main(CSOUND *csound);
```
Once the `clang` opcode has compiled, linked, and loaded an object module, 
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

## Installation

1. Install Clang and LLVM from the stable branch, see https://apt.llvm.org/.

2. Compile the `clang_opcode.cpp` file using `build.sh`, which you may have to 
   modify for your system.
   
3. Test by executing `csound clang_opcode_test.csd`. This should compile a piano 
   opcode and instrument, a reverb opcode and instrument, and a score generating 
   instrument, which will algorithmically generate and render a piece.

# Credits

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com


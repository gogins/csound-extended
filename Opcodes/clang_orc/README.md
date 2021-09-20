# clang_orc

clang_orc - Compiles C/C++ source code into a module, and executes it at
Csound performance time. The compiled module is callable in the running Csound 
performance, and can call into the running Csound performance using the Csound 
API. The module may define new Csound opcodes, or call the Csound API to 
execute arbitrary Csound code, or do anything that is doable using C or C++.

## Description

The `clang_orc` opcode provides a just-in-time compiler (JIT) that enables 
Csound to compile C or C++ source code, embedded in the Csound orchestra, to a 
module; load and link that module; and call the Csound API from that module.

The `clang_orc` opcode uses the [Clang library and LLVM](https://llvm.org/), and 
is based on the ["Clang C Interpreter Example"](https://github.com/llvm/llvm-project/tree/main/clang/examples/clang-interpreter). 
The `clang` opcode was inspired by the 
[`faustgen`](https://csound.com/docs/manual/faustgen.html) opcode.

The name `clang_orc` alludes both to the Csound orchestra, and to LLVM's 
On-Request Compiler (ORC), that is, the Clang JIT compiler.

## Syntax
```
i_result clang_orc S_entry_point, S_source_code, S_compiler_options [, S_link_libraries]
```
## Initialization

*S_entry_point* - A valid C identifier, unique in the Csound performance, 
for an entry point function that must be defined in the module. This function  
must have the signature `extern "C" int (*)(CSOUND *csound)`. This function has 
full access to any loaded link libraries, as well as to the running instance 
of Csound.

*S_source_code* - C or C++ source code. Can be a multi-line string literal 
enclosed in `{{` and `}}`. Please note, this string is a "heredoc" and, thus, 
any `\` characters in it must be escaped, e.g. one must write `\\n` not '\n' 
for a newline character. 

*S_compiler_options* - Standard gcc/clang compiler options, as would be passed 
on the compiler command line. Can be a multi-line string literal enclosed in 
`{{` and `}}`. If the `-v` option is present, additional diagnostics are 
enabled for the `clang_orc` opcode itself.

*S_link_libraries* - Optional space-delimited list of link libraries, serving 
the same function as the `-l` option for a standalone compiler. Here, however, 
each link library must be its fully qualified filepath. Each library will be 
loaded and linked by LLVM before the user's code is JIT compiled. Can be a 
multi-line string literal enclosed in `{{` and `}}`. 

*i_result* - 0 if the code has been compiled and executed succesfully; 
non-0 if there is an error. Clang and LLVM diagnostics are printed to stderr.

## Performance

The `clang_orc` opcode is i-time only, and is often invoked in the orchestra 
header. Therefore, the opcode entry point is only called after `csoundStart` 
has been called.

Non-standard include directories and compiler options may be used, but must be 
defined in `S_compiler_options`.

Libraries on which the module depends may be used, but must be specified as 
fully qualified filepaths in `S_link_libraries`. The usual compiler option 
`-l` does not work in this context.

PLEASE NOTE: Many shared libraries use the symbol `__dso_handle`, but this is 
not defined in the ORC compiler's startup code. To work around this, manually 
define it in your C/C++ code like this:
```
void* __dso_handle = (void *)&__dso_handle;
```
The module _must_ define a uniquely named C function, which is the entry point to 
the module, in the same way that the `main` function is the entry point to a C 
program, with the following signature:
```
extern "C" int(*)(CSOUND *csound);
```
Once the `clang_orc` opcode has compiled the module, Csound will immediately 
call the entry point function in that module. At that very time, the LLVM JIT 
compiler will compile the source code and link it into the running Csound 
process. The `csound_main` function may register any opcodes defined in the 
module by calling `csound->AppendOpcode`, or create new instruments in the 
Csound orchestra using `csound->CompileText`, or indeed do anything that can 
be done in C, C++, or using the Csound API.

Once an opcode has been added in `csound_main`, the Csound 
performance may invoke the opcode just like any other opcode.

Once an instrument has been defined in `csound_main`, the 
instrument may be performed by the Csound score just like any other 
instrument. Note that instruments get numbers strictly according to 
the order in which those instruments are defined.

The module may do anything else that is doable in C or C++. For example,
the module may use an external shared library to assist with algorithmic 
composition, then translate the generated score to a Csound score, then 
call `csound->ReadScore` to schedule the score for immediate performance.

## Example

The `csound clang_example.csd` file uses the `clang_orc` opcode to 
compile a guitar opcode and instrument, a reverb opcode and instrument, 
and a score generating instrument. For the sake of clarity, although all of 
this code could be implemented in one module, the following separate modules 
are defined:

1. A physically modelled guitar opcode, written in C++, which is then 
    wrapped in a Csound instrument definition.
   
2. A reverb opcode, written in C++, which is then wrapped in a Csound instrument 
   definition.

3. A score generating opcode, written in C++, which is then wrapped in a Csound 
   instrument definition.
   
The Csound orchestra in this piece uses the signal flow graph opcodes to connect 
the guitar instrument to the reverb instrument, and to connect the reverb 
instrument to an output instrument.

The Csound score in this piece invokes the score generating instrument twice at 
an offset in time and pitch, to create a canon at the sixth.

## Installation

1. Install Clang and LLVM from the stable branch, see https://apt.llvm.org/.

2. Compile the `clang_opcode.cpp` file using `build.sh`, which you may need to 
   modify for your system.
   
3. Copy the `clang_opcode.so` file to your `OPCODE6DIR6` directory.
   
4. Test by executing `csound clang_example.csd`. 

# Credits

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com


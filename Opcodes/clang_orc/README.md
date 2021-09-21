# Clang Opcodes

The Clang opcodes provide two facilities. The `clang_compile` opcode compiles C 
or C++ source code, embedded in a Csound orchestra, at performance time. The 
result is a compiled module of low level virtual machine (LLVM) intermediate 
representation (IR) code. When any symbol in that module is accessed, the 
LLVM on-request compiler (ORC) translates the IR code to machine language, 
resolves symbols and relocations, and in short loads and links the code just 
like any other compiled C or C++ object.

The `clang_invoke` opcode enables a compiled module to be invoked from Csound 
code during the Csound performance. Commonly, this is used to implement new 
Csound opcodes in the Csound orchestra that are more powerful, and run faster, 
than user-defined opcodes. It is also used to generate scores or control 
channel values at the beginning of, or during, the performance.

# clang_compile

clang_compile - Compiles C/C++ source code into a module, and executes it at
Csound performance time. The compiled module is callable in the running Csound 
performance, can call into the running Csound performance using the Csound 
API, and can call exported functions in other modules compiled by 
`clang_compile`.

## Description

The `clang_compile` opcode provides an on-request-compiler (ORC) that enables 
Csound to compile C or C++ source code, embedded in the Csound orchestra, to 
a module of LLVM IR code; load and link that module; and call the Csound API, 
other modules, or link libraries from that module. The ORC compiler is a type 
of just-in-time (JIT) compiler, in which the actual translation to machine 
language takes place whenever a symbol in the module is accessed for the first 
time from the ORC compiler's LLVM execution session.

The `clang_compile` opcode uses the [Clang library and LLVM](https://llvm.org/), 
and is based on the 
["Clang C Interpreter Example"](https://github.com/llvm/llvm-project/tree/main/clang/examples/clang-interpreter). 
The `clang_invoke` opcode was inspired by the 
[`faustgen`](https://csound.com/docs/manual/faustgen.html) opcode.

## Syntax
```
i_result clang_compile S_entry_point, S_source_code, S_compiler_options [, S_link_libraries]
```
## Initialization

*S_entry_point* - A valid C identifier, unique in the Csound performance, 
for an entry point function that must be defined in the module. This function 
must have the signature `extern "C" int (*)(CSOUND *csound)`. This function has 
full access to the running instance of Csound via the Csound API members of the 
CSOUND structure, as well as to all symbols in other LLVM modules, and all 
exported symbols in all loaded link libraries.

*S_source_code* - C or C++ source code. Can be a multi-line string literal 
enclosed in `{{` and `}}`. Please note, this string is a "heredoc" and, thus, 
any `\` characters in it must be escaped, e.g. one must write `\\n` not '\n' 
for a newline character. The source code represents one translation unit, but 
it can be as large as needed.

*S_compiler_options* - Standard gcc/clang compiler options, as would be passed 
on the compiler command line. Can be a multi-line string literal enclosed in 
`{{` and `}}`. If the `-v` option is present, additional diagnostics are 
enabled for the `clang_compile` opcode itself.

*S_link_libraries* - Optional space-delimited list of standard or custom link 
libraries, serving the same function as `-l` options for a standalone 
compiler. Here, however, each link library must be specified as a fully 
qualified filepath. Each library will be loaded and linked by LLVM before the 
user's code is JIT compiled. Can be a multi-line string literal enclosed in `{{` 
and `}}`. 

*i_result* - 0 if the code has been compiled and executed succesfully; 
non-0 if there is an error. Clang and LLVM diagnostics are printed to stderr.

## Performance

Compilation and execution occur at initialization time only. Therefore, the 
compiled module's entry point is only called after `csoundStart` has been 
called. If the compilation is done in the orchestra header, i.e. in `instr 0`, 
the execution occurs during Csound's init pass for `instr 0`. If the 
compilation is done from a regular Csound instrument, the execution occurs 
during Csound's init pass for that particular instrument instance.

Non-standard include directories and compiler options may be used, but must be 
defined in `S_compiler_options`.

Libraries on which the module depends may be used, whether system libraries or 
user libraries, but must be specified as fully qualified filepaths in 
`S_link_libraries`. The usual compiler option `-l` does not work in this 
context.

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
Once the `clang_compile` opcode has compiled the module, Csound will immediately 
call the entry point function in that module. At that very time, the LLVM ORC 
compiler will translate the IR code in the module to machine language, resolve 
symbols, perform relocations, and link it into the running Csound process. 

The entry point function may call the Csound API functions that are members of 
the `CSOUND` struct, define classes and structs, or do anything at all else that 
can be done with C or C++ code.

For example, the module may use an external shared library to assist with 
algorithmic composition, then translate the generated score to a Csound score, 
then call `csound->InputMessage` to schedule the score for immediate 
performance.

However, one of the significant uses of `clang_compile` is to compile C/C++
code into classes that can perform the work of Csound opcodes. This is 
done by subclassing the `ClangInvokable` class. See `clang_invoke` for how 
this works and how to use it.

## Example

The `csound clang_example.csd` file uses the `clang_compile` opcode to 
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

# clang_invoke

clang_invoke - creates an instance of a `ClangInvokable` that has been defined 
previously using `clang_compile`, and invokes that instance at i-time, k-time, 
or both.

## Description

Creates an instance of a `ClangInvokable` that has been defined 
previously using `clang_compile`, and invokes that instance at i-time, k-time, 
or both. 

This can be used to define any type of Csound opcode. It can also be used for 
other purposes, e.g. simply as a way to call some function in the ClangInvokable 
module.

## Syntax
```
[m_output_1,...] clang_invoke S_clang_invokeable, i_thread, [, m_input_1,...]
```
## Initialization

*S_clang_invokable* - The name of a class that implements the following C++ 
virtual class:
```
class ClangInvokable {
	public:
	virtual ~ClangInvokable();
	/**
	 * Called once at init time. The inputs are the same as the 
	 * parameters passed to the `clang_invoke` opcode. The outputs become 
	 * the values returned from the `clang_invoke` opcode. Performs the 
	 * same work as `iopadr` in a standard Csound opcode definition.
	 */
	virtual int init(CSOUND *, MYFLT* outputs, const MYFLT *inputs) {};
	/**
	 * Called once every kperiod. The inputs are the same as the 
	 * parameters passed to the `clang_invoke` opcode. The outputs become 
	 * the values returned from the `clang_invoke` opcode. Performs the 
	 * same work as `kopadr` in a standard Csound opcode definition.
	 */
	virtual int kontrol(CSOUND *, MYFLT* outputs, cost MYFLT *inputs) {};
	/**
	 * Called by Csound when the Csound instrument that contains this 
	 * instance of the ClangInvokable is turned off.
	 */
	virtual int noteoff(CSOUND *csound) {};
	protected:
	/**
	 * Which methods are called when, the same as for a standard Csound 
	 * opcode.
	 */
	int thread = 3;
	/**
	 * Back pointer to the `clang_invoke` opcode.
	 */
	std::shared_ptr<ClangInvoke> clang_invoke;
};
```
*i_thread* - The "thread" on which this ClangInvokable will run:

-  1 = The `ClangInvokable::init` method is called, but not the 
   `ClangInvokable::kontrol` method.
-  2 = The `ClangInvokable::kontrol` method is called, but not the 
   `ClangInvokable::init` function. 
`  3 = The `ClangInvokable::init` method is called once at the 
   init pass for the instrument, and the `ClangInvokable::kontrol` 
   method is called once every kperiod during the lifetime of the 
   instrument.

*m_output_1,...* - Any number, up to 40, of any type of Csound parameters, 
i-rate or k-rate. These are copied to the same number and type of outputs 
that were provided by Csound to `clang_invoke`.

*m_input_i,...* - Any number of any type of Csound parameters, i-rate or 
k-rate. These are copied from the same number and type of inputs that were 
provided by Csound to `clang_invoke`.

The *S_clang_invokeable* symbol is looked up in the LLVM execution session 
of the global ORC compiler, and a new instance of the ClangInvokable class 
is created. `clang_invoke` then calls the instances's init method with the 
input and output parameters, and the output values are returned in the 
argument.

Csound will set up the inputs and outputs of `clang_invoke` according to the 
parameters and return values set up by the user code. The user must of course 
ensure that the ClangInvokable has the right numbers, types, and rates for 
these parameters and return values. Because of the variable numbers and types 
of arguments, type checking is virtually impossible.


## Performance

The ClangInvokable instance's `kontrol` method is called once per 
kperiod during the lifetime of the opcode.

When the Csound instrument that has created the `clang_invoke` opcode is 
turned off, the ClangInvokable's `noteoff` method is called. At that  
time, the ClangInvokable can release system resources or free memory.

The ClangInvokable instance is then deleted by the `clang_invoke` opcode.

# Installation

1. Install all available components of Clang and LLVM from the stable branch, 
   see https://apt.llvm.org/.

2. Compile the `clang_opcodes.cpp` file using `build.sh`, which you may need to 
   modify for your system.
   
3. Copy the `clang_opcodes.so` file to your `OPCODE6DIR6` directory, or load 
   it with Csound's `--opcode-lib="./clang_opcodes.so"` option.
   
4. Test by executing `csound clang_example.csd`. 

# Credits

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

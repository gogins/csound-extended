/**
 * # clang
 *
 * Michael Gogins<br>
 * https://github.com/gogins<br>
 * http://michaelgogins.tumblr.com
 *
 * This file implements a Csound opcode that compiles C or C++ source code,
 * embedded in tne Csound orchestra, for any purpose. The compiler is a
 * simplified, embedded instance of the clang/llvm compiler.
 *
 * This code is based on the "compiler_instance C Interpreter Example:"
 * examples/clang-interpreter/main.cpp.
 *
 * ## Syntax
 *```
 * i_result clang S_unique_entry_point, S_source_code, S_compiler_options [, link_libraries]
 *
 * The link_libraries parameter is a space-delimited list of zero or more
 * libraries that llvm will load and that will be callable by the compiled
 * module. This list plays the part of the `-l` options for a standalone
 * compiler/linker. Each library must be specified by its fully qualified
 * filepath.
 * ```
 */

#include "clang/Basic/DiagnosticOptions.h"
#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"

#include <csound/OpcodeBase.hpp>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <mutex>
#include <stdlib.h>
#include <string>

using namespace clang;
using namespace clang::driver;

llvm::ExitOnError exit_on_error;

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// GetMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement GetMainExecutable
// without being given the address of a function in the main executable).
std::string GetExecutablePath(const char *argv_0, void *main_address)
{
    return llvm::sys::fs::getMainExecutable(argv_0, main_address);
}

std::string dylib_name() {
    static int dylib_count = 0;
    dylib_count++;
    char dylib_name_[0x100];
    std::snprintf(dylib_name_, 0x100, "<main-%d>", dylib_count);
    return dylib_name_;
}

namespace llvm
{
namespace orc
{
class JITCompiler
{
private:
    ExecutionSession execution_session;
    std::unique_ptr<TargetMachine> target_machine;
    const DataLayout data_layout;
    MangleAndInterner Mangle{execution_session, data_layout};
    std::string name = dylib_name();
    JITDylib &main_jit_dylib{execution_session.createBareJITDylib(name)};
    RTDyldObjectLinkingLayer object_linking_layer{execution_session, create_memory_manager};
    // llvm::orc::SimpleCompiler is the simplest JIT compiler in
    // the orc namespace, but it does seem to suit our use case.
    IRCompileLayer intermediate_representation_compiler_layer{execution_session, object_linking_layer, std::make_unique<SimpleCompiler>(*target_machine)};
    static std::unique_ptr<SectionMemoryManager> create_memory_manager() {
        return std::make_unique<SectionMemoryManager>();
    }
    JITCompiler(
        std::unique_ptr<TargetMachine> target_machine, DataLayout data_layout,
        std::unique_ptr<DynamicLibrarySearchGenerator> process_symbols_generator)
        : execution_session(cantFail(SelfExecutorProcessControl::Create())), target_machine(std::move(target_machine)), data_layout(std::move(data_layout))
    {
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
        main_jit_dylib.addGenerator(std::move(process_symbols_generator));
        std::fprintf(stderr, "####### clang_compile: main_jit_dylib: name: %s\n", main_jit_dylib.getName().c_str());
    }
public:
    ~JITCompiler()
    {
        std::fprintf(stderr, "####### clang_compile: deleting JITCompiler %p and ending execution session.\n", this);
        if(auto error = execution_session.endSession()) {
            execution_session.reportError(std::move(error));
        }
    }
    static Expected<std::unique_ptr<JITCompiler>> Create()
    {
        auto jit_target_machine_builder = JITTargetMachineBuilder::detectHost();
        if(!jit_target_machine_builder) {
            return jit_target_machine_builder.takeError();
        }
        auto target_machine = jit_target_machine_builder->createTargetMachine();
        if(!target_machine) {
            return target_machine.takeError();
        }
        auto data_layout = (*target_machine)->createDataLayout();
        auto process_symbols_generator = DynamicLibrarySearchGenerator::GetForCurrentProcess(data_layout.getGlobalPrefix());
        if(!process_symbols_generator) {
            return process_symbols_generator.takeError();
        }
        return std::unique_ptr<JITCompiler>(new JITCompiler(std::move(*target_machine), std::move(data_layout), std::move(*process_symbols_generator)));
    }
    const TargetMachine &getTargetMachine() const
    {
        return *target_machine;
    }
    Error addModule(ThreadSafeModule thread_safe_module)
    {
        return intermediate_representation_compiler_layer.add(main_jit_dylib, std::move(thread_safe_module));
    }
    Expected<JITEvaluatedSymbol> findSymbol(const StringRef &name)
    {
        return execution_session.lookup({&main_jit_dylib}, Mangle(name));
    }
    Expected<JITTargetAddress> getSymbolAddress(const StringRef &name)
    {
        auto symbol = findSymbol(name);
        if(!symbol) {
            return symbol.takeError();
        }
        return symbol->getAddress();
    }
};
} // end namespace orc
} // end namespace llvm

/**
 * This refers to the singleton JIT compiler that can link modules and
 * be called from different opcodes.
 */
static std::shared_ptr<llvm::orc::JITCompiler> jit_compiler;

static void tokenize(std::string const &string_, const char delimiter, std::vector<std::string> &tokens)
{
    size_t start;
    size_t end = 0;
    while ((start = string_.find_first_not_of(delimiter, end)) != std::string::npos)
    {
        end = string_.find(delimiter, start);
        tokens.push_back(string_.substr(start, end - start));
    }
}

/**
 * The `clang_compile` opcode will call a uniquely named function that must be
 * defined in the module. The type of this function must be
 * `int (*)(CSOUND *csound)`. This function serves as the entry point to the
 * module, similar to 'main' in a C or C++ program.
 *
 * When the entry point is called, `csoundStart` has _already_ been called,
 * and Csound is performing an init pass, which for `clang_compile` used in the
 * orchestra header will be the first init pass in the orchestra header
 * (which is "instr 0").
 */
extern "C" {
    typedef int (*csound_main_t)(CSOUND *csound);
};

/**
 * Thread-safe facility for storing and removing modules that have been
 * compiled with clang.
 */
struct clang_modules_for_csounds_t
{
    std::mutex mutex_;
    std::multimap<CSOUND *, std::shared_ptr<llvm::Module> > clang_modules_for_csounds_;
    void add(CSOUND *csound, std::shared_ptr<llvm::Module> module) {
        std::lock_guard<std::mutex> guard(mutex_);
        clang_modules_for_csounds_.insert({csound, module});
    }
    void del(CSOUND *csound) {
        std::lock_guard<std::mutex> guard(mutex_);
        clang_modules_for_csounds_.erase(csound);
    }
};

static clang_modules_for_csounds_t &clang_modules_for_csounds() {
    static clang_modules_for_csounds_t clang_modules_for_csounds_;
    return clang_modules_for_csounds_;
};

class ClangCompile : public csound::OpcodeBase<ClangCompile>
{
public:
    // OUTPUTS
    MYFLT *i_result;
    // INPUTS
    STRINGDAT *S_entry_point;
    STRINGDAT *S_source_code;
    STRINGDAT *S_compiler_options;
    STRINGDAT *S_link_libraries;
    // STATE
    /**
     * This is an i-time only opcode. Everything happens in init.
     */
    int init(CSOUND *csound)
    {
        //std::fprintf(stderr, "ClangCompile::init: line %d\n", __LINE__);
        auto diagnostics_enabled = false;
        auto entry_point = csound->strarg2name(csound, (char *)0, S_entry_point->data, (char *)"", 1);
        if (diagnostics_enabled) csound->Message(csound, "####### clang_compile: entry_point: %s\n", entry_point);
        // Create a temporary file containing the source code.
        auto source_code = csound->strarg2name(csound, (char *)0, S_source_code->data, (char *)"", 1);
        const char *temp_directory = std::getenv("TMPDIR");
        if (temp_directory == nullptr) {
            temp_directory = "/tmp";
        }
        char filepath[0x500];
        std::snprintf(filepath, 0x500, "%s/clang_opcode_XXXXXX.cpp", temp_directory);
        auto file_descriptor = mkstemps(filepath, 4);
        auto file_ = fdopen(file_descriptor, "w");
        std::fwrite(source_code, strlen(source_code), sizeof(source_code[0]), file_);
        std::vector<const char*> args;
        std::fclose(file_);
        args.push_back("clang_opcode");
        args.push_back(filepath);
        // Parse the compiler options.
        auto compiler_options = csound->strarg2name(csound, (char *)0, S_compiler_options->data, (char *)"", 1);
        std::vector<std::string> tokens;
        tokenize(compiler_options, ' ', tokens);
        for (int i = 0; i < tokens.size(); ++i) {
            if (tokens[i] == "-v") {
                diagnostics_enabled = true;
            }
            args.push_back(tokens[i].c_str());
        }
        // Compile the source code to a module, and call its
        // csound_main entry point. This just needs to be some symbol in
        // the process; C++ doesn't allow taking the address of ::main.
        void *main_address = (void*)(intptr_t) GetExecutablePath;
        std::string executable_filepath = GetExecutablePath(args[0], main_address);
        if (diagnostics_enabled) csound->Message(csound, "####### clang_compile: executable_filepath: %s\n", executable_filepath.c_str());
        IntrusiveRefCntPtr<DiagnosticOptions> diagnostic_options = new DiagnosticOptions();
        TextDiagnosticPrinter *diagnostic_client = new TextDiagnosticPrinter(llvm::errs(), &*diagnostic_options);
        IntrusiveRefCntPtr<DiagnosticIDs> diagnostic_ids(new DiagnosticIDs());
        DiagnosticsEngine diagnostics_engine(diagnostic_ids, &*diagnostic_options, diagnostic_client);
        // Infer Csound's runtime architecture.
        const std::string process_triple = llvm::sys::getProcessTriple();
        if (diagnostics_enabled) csound->Message(csound, "####### clang_compile: triple: %s\n", process_triple.c_str());
        llvm::Triple triple(process_triple);
        // Use ELF on Windows-32 and MingW for now.
#ifndef CLANG_INTERPRETER_COFF_FORMAT
        if(triple.isOSBinFormatCOFF()) {
            triple.setObjectFormat(llvm::Triple::ELF);
        }
#endif
        exit_on_error.setBanner("Csound JIT compiler ");
        Driver clang_driver(executable_filepath, triple.str(), diagnostics_engine);
        clang_driver.setTitle("Csound JIT compiler ");
        clang_driver.setCheckInputsExist(false);
        // FIXME: This is a hack to try to force the driver to do something we can
        // recognize. We need to extend the driver library to support this use model
        // (basically, exactly one input, and the operation mode is hard wired).
        ///SmallVector<const char *, 16> args(argv, argv + argc);
        args.push_back("-fsyntax-only");
        for (auto arg : args) {
            if (diagnostics_enabled) csound->Message(csound, "arg: %s\n", arg);
        }
        // TODO: Change this to in-memory?
        std::unique_ptr<Compilation> compilation(clang_driver.BuildCompilation(args));
        if(!compilation) {
            return 0;
        }
        // FIXME: This is copied from ASTUnit.cpp; simplify and eliminate.
        // We expect to get back exactly one command job, if we didn't something
        // failed. Extract that job from the compilation.
        const driver::JobList &compilation_jobs = compilation->getJobs();
        if(compilation_jobs.size() != 1 || !isa<driver::Command>(*compilation_jobs.begin())) {
            SmallString<256> message;
            llvm::raw_svector_ostream OS(message);
            compilation_jobs.Print(OS, "; ", true);
            diagnostics_engine.Report(diag::err_fe_expected_compiler_job) << OS.str();
            return 1;
        }
        const driver::Command &command = cast<driver::Command>(*compilation_jobs.begin());
        if(llvm::StringRef(command.getCreator().getName()) != "clang") {
            diagnostics_engine.Report(diag::err_fe_expected_clang_command);
            return 1;
        }
        // Initialize a compiler invocation object from the clang (-cc1) arguments.
        const llvm::opt::ArgStringList &compiler_args = command.getArguments();
        std::unique_ptr<CompilerInvocation> compiler_invocation(new CompilerInvocation);
        CompilerInvocation::CreateFromArgs(*compiler_invocation, compiler_args, diagnostics_engine);
        // Show the invocation, with -v.
        if(compiler_invocation->getHeaderSearchOpts().Verbose) {
            llvm::errs() << "clang invocation:\n";
            compilation_jobs.Print(llvm::errs(), "\n", true);
            llvm::errs() << "\n";
        }
        // FIXME: This is copied from cc1_main.cpp; simplify and eliminate.
        // Create a compiler instance to handle the actual work.
        CompilerInstance compiler_instance;
        compiler_instance.setInvocation(std::move(compiler_invocation));
        // Create the compiler's actual diagnostics engine.
        compiler_instance.createDiagnostics();
        if(!compiler_instance.hasDiagnostics()) {
            return 1;
        }
        // Infer the builtin include path if unspecified.
        if(compiler_instance.getHeaderSearchOpts().UseBuiltinIncludes && compiler_instance.getHeaderSearchOpts().ResourceDir.empty()) {
            compiler_instance.getHeaderSearchOpts().ResourceDir = CompilerInvocation::GetResourcesPath(args[0], main_address);
        }
        std::unique_ptr<CodeGenAction> emit_llvm_action(new EmitLLVMOnlyAction());
        if(!compiler_instance.ExecuteAction(*emit_llvm_action)) {
            return 1;
        }
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        int result = 255;
        std::unique_ptr<llvm::LLVMContext> llvm_context(emit_llvm_action->takeLLVMContext());
        std::unique_ptr<llvm::Module> module = emit_llvm_action->takeModule();
        //std::fprintf(stderr, "ClangCompile::init: line %d\n", __LINE__);
        if(module) {
            // Load and link all libraries required.
            auto link_libraries = csound->strarg2name(csound, (char *)0, S_link_libraries->data, (char *)"", 1);
            std::vector<std::string> link_libraries_list;
            tokenize(link_libraries, ' ', link_libraries_list);
            for (auto link_library : link_libraries_list) {
                llvm::sys::DynamicLibrary::LoadLibraryPermanently(link_library.c_str());
            }
            // The JIT compiler is global for the Csound performance and for all opcodes.
            if (!jit_compiler) {
                jit_compiler = exit_on_error(llvm::orc::JITCompiler::Create());
            }
            exit_on_error(jit_compiler->addModule(llvm::orc::ThreadSafeModule(std::move(module), std::move(llvm_context))));
            // It seems the actual compilation to machine language happens
            // just when a symbol is accessed for the first time.
            auto csound_main = (int (*)(CSOUND *)) exit_on_error(jit_compiler->getSymbolAddress(entry_point));
            if (diagnostics_enabled) csound->Message(csound, "####### clang_compile: invoking \"%s\" at %p:\n", entry_point, csound_main);
            result = csound_main(csound);
            if (diagnostics_enabled) csound->Message(csound, "####### clang_compile: \"%s\" returned: %d\n", entry_point, result);
            //~ // On success, store the compiled module until Csound exits.
            //~ std::shared_ptr<llvm::Module> object_module = std::move(module);
            //~ if (object_module) {
            //~ clang_modules_for_csounds().add(csound, object_module);
            //~ }
        }
        //std::fprintf(stderr, "ClangCompile::init ended at: line %d\n", __LINE__);
        return OK;
    };
};

#include "clang_invokable.hpp"

/**
 * Assuming that `clang_compile` has already compiled a module that
 * implements a `ClangInvokable`, creates an instance of that
 * `ClangInvokable` and invokes it.
 */
class ClangInvoke : public csound::OpcodeNoteoffBase<ClangInvoke>
{
public:
    // OUTPUTS
    MYFLT *outputs[40];
    // INPUTS
    STRINGDAT *S_invokable_factory;
    MYFLT *i_thread;
    MYFLT *inputs[VARGMAX];
    // STATE
    int thread = (int) *i_thread;
    std::shared_ptr<ClangInvokable> clang_invokable;
    int init(CSOUND *csound)
    {
        int result = OK;
        // Look up factory.
        auto invokable_factory_name = csound->strarg2name(csound, (char *)0, S_invokable_factory->data, (char *)"", 1);
        auto invokable_factory = (ClangInvokable *(*)()) exit_on_error(jit_compiler->getSymbolAddress(invokable_factory_name));
        // Create instance.clang_invokable.reset(
        clang_invokable.reset(invokable_factory());
        if (thread == 1) {
            return result;
        }
        // Invoke the instance.
        result = clang_invokable->init(csound, &opds, outputs, inputs);
        return result;
    }
    int kontrol(CSOUND *csound)
    {
        int result = OK;
        if (thread == 0) {
            return result;
        }
        result = clang_invokable->kontrol(csound, outputs, inputs);
        return result;

    }
    int noteff(CSOUND *csound) {
        int result = OK;
        result = clang_invokable->noteoff(csound);
        clang_invokable = nullptr;
        return result;
    }
};

extern "C" {

    PUBLIC int csoundModuleInit_clang_opcodes(CSOUND *csound)
    {
        int status = csound->AppendOpcode(csound,
                                          (char *)"clang_compile",
                                          sizeof(ClangCompile),
                                          0,
                                          1,
                                          (char *)"i",
                                          (char *)"SSW",
                                          (int (*)(CSOUND*,void*)) ClangCompile::init_,
                                          (int (*)(CSOUND*,void*)) 0,
                                          (int (*)(CSOUND*,void*)) 0);
        status += csound->AppendOpcode(csound,
                                          (char *)"clank_invoke",
                                          sizeof(ClangInvoke),
                                          0,
                                          3,
                                          (char *)"mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm",
                                          (char *)"M",
                                          (int (*)(CSOUND*,void*)) ClangInvoke::init_,
                                          (int (*)(CSOUND*,void*)) ClangInvoke::kontrol_,
                                          (int (*)(CSOUND*,void*)) 0);
        return status;
    }

    PUBLIC int csoundModuleDestroy_clang_opcodes(CSOUND *csound)
    {
        //~clang_modules_for_csounds().del(csound);
        llvm::llvm_shutdown();
        return 0;
    }

#ifndef INIT_STATIC_MODULES
    PUBLIC int csoundModuleCreate(CSOUND *csound)
    {
        return 0;
    }

    PUBLIC int csoundModuleInit(CSOUND *csound)
    {
        return csoundModuleInit_clang_opcodes(csound);
    }

    PUBLIC int csoundModuleDestroy(CSOUND *csound)
    {
        return csoundModuleDestroy_clang_opcodes(csound);
    }
#endif
}

/**
 * clang
 *
 * Michael Gogins<br>
 * https://github.com/gogins<br>
 * http://michaelgogins.tumblr.com
 *
 * This file implements a Csound opcode that compiles C /C++ source code,
 * embedded in tne Csound orchestra, for any purpose.
 *
 * This code is based on the "compiler_instance C Interpreter Example:"
 * examples/clang-interpreter/main.cpp.
 *
 * i_result clang S_source_code [, S_compiler_options]
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

using namespace clang;
using namespace clang::driver;

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// GetMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement GetMainExecutable
// without being given the address of a function in the main executable).
std::string GetExecutablePath(const char *argv_0, void *main_address)
{
    return llvm::sys::fs::getMainExecutable(argv_0, main_address);
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
                JITDylib &main_jit_dylib{execution_session.createBareJITDylib("<main>")};
                RTDyldObjectLinkingLayer object_linking_layer{execution_session, create_memory_manager};
                IRCompileLayer intermediate_representation_compiler_layer{execution_session, object_linking_layer, std::make_unique<SimpleCompiler>(*target_machine)};
                static std::unique_ptr<SectionMemoryManager> create_memory_manager()
                {
                    return std::make_unique<SectionMemoryManager>();
                }
                JITCompiler(
                    std::unique_ptr<TargetMachine> target_machine, DataLayout data_layout,
                    std::unique_ptr<DynamicLibrarySearchGenerator> process_symbols_generator)
                    : execution_session(cantFail(SelfExecutorProcessControl::Create())), target_machine(std::move(target_machine)), data_layout(std::move(data_layout))
                {
                    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
                    main_jit_dylib.addGenerator(std::move(process_symbols_generator));
                }
            public:
                ~JITCompiler()
                {
                    if(auto Err = execution_session.endSession()) {
                        execution_session.reportError(std::move(Err));
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
                Error addModule(ThreadSafeModule M)
                {
                    return intermediate_representation_compiler_layer.add(main_jit_dylib, std::move(M));
                }
                Expected<JITEvaluatedSymbol> findSymbol(const StringRef &Name)
                {
                    return execution_session.lookup({&main_jit_dylib}, Mangle(Name));
                }
                Expected<JITTargetAddress> getSymbolAddress(const StringRef &Name)
                {
                    auto Sym = findSymbol(Name);
                    if(!Sym) {
                        return Sym.takeError();
                    }
                    return Sym->getAddress();
                }
        };
    } // end namespace orc
} // end namespace llvm

/**
 * The clang opcode will call the following function that must be defined
 * in the to be compiled module. The csound_main function should call
 * csound->AppendOpcode for each opcode defined in the module, and do any
 * other work, if necessary calling the Csound API members of the CSOUND
 * object.
 *
 * When this function is called, csoundStart has been compiled, and Csound 
 * is beginning its init pass by calling all i-rate opcodes in the orchestra 
 * header ("instr 0").
 * pass for 
 */
extern "C" {
    typedef int (*csound_main)(CSOUND *csound);
};

class clang_opcode_t : public csound::OpcodeBase<clang_opcode_t>
{
    public:
        // OUTPUTS
        MYFLT *i_result;
        // INPUTS
        STRINGDAT *S_source_code;
        STRINGDAT *S_compiler_options;
        // STATE
        char *source_code;
        char *compiler_options;
        int init(CSOUND *csound)
        {
            // First, compile the module.
            // Then, call its "csound_main" so it can register opcodes, etc.
            //module->
            // Finally, keep the compiled module around until this instance of
            // Csound exits.
            //modules_for_csounds(csound).push_back(module);
            return OK;
        };
};

llvm::ExitOnError exit_on_error;

int main(int argc, const char **argv)
{
    // This just needs to be some symbol in the binary; C++ doesn't
    // allow taking the address of ::main however.
    void *main_address = (void*)(intptr_t) GetExecutablePath;
    std::string executable_path = GetExecutablePath(argv[0], main_address);
    IntrusiveRefCntPtr<DiagnosticOptions> diagnostic_options = new DiagnosticOptions();
    TextDiagnosticPrinter *diagnostic_client = new TextDiagnosticPrinter(llvm::errs(), &*diagnostic_options);
    IntrusiveRefCntPtr<DiagnosticIDs> diagnostic_ids(new DiagnosticIDs());
    DiagnosticsEngine diagnostics_engine(diagnostic_ids, &*diagnostic_options, diagnostic_client);
    const std::string process_triple = llvm::sys::getProcessTriple();
    llvm::Triple triple(process_triple);
    // Use ELF on Windows-32 and MingW for now.
#ifndef CLANG_INTERPRETER_COFF_FORMAT
    if(triple.isOSBinFormatCOFF()) {
        triple.setObjectFormat(llvm::Triple::ELF);
    }
#endif
    exit_on_error.setBanner("Csound runtime C compiler");
    Driver clang_driver(executable_path, triple.str(), diagnostics_engine);
    clang_driver.setTitle("Csound runtime C compiler");
    clang_driver.setCheckInputsExist(false);
    // FIXME: This is a hack to try to force the driver to do something we can
    // recognize. We need to extend the driver library to support this use model
    // (basically, exactly one input, and the operation mode is hard wired).
    SmallVector<const char *, 16> args(argv, argv + argc);
    args.push_back("-fsyntax-only");
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
        compiler_instance.getHeaderSearchOpts().ResourceDir = CompilerInvocation::GetResourcesPath(argv[0], main_address);
    }
    // Create and execute the frontend to generate an LLVM bitcode module.
    std::unique_ptr<CodeGenAction> emit_llvm_action(new EmitLLVMOnlyAction());
    if(!compiler_instance.ExecuteAction(*emit_llvm_action)) {
        return 1;
    }
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    int result = 255;
    std::unique_ptr<llvm::LLVMContext> llvm_context(emit_llvm_action->takeLLVMContext());
    std::unique_ptr<llvm::Module> module = emit_llvm_action->takeModule();
    // Now generate the native code from the bitcode.
    if(module) {
        auto jit_compiler = exit_on_error(llvm::orc::JITCompiler::Create());
        exit_on_error(jit_compiler->addModule(llvm::orc::ThreadSafeModule(std::move(module), std::move(llvm_context))));
        auto Main = (int (*)(...))exit_on_error(jit_compiler->getSymbolAddress("main"));
        result = Main();
        auto What = (int (*)(...))exit_on_error(jit_compiler->getSymbolAddress("my_hook"));
        result = What("This is what.");
        printf("result: %d\n", result);
    }
    llvm::llvm_shutdown();
    return result;
}
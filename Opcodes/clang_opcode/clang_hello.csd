<CsoundSyntheizer>
<CsLicense>
This .csd file tests the new Clang JIT compiler opcode for Csound.
</CsLicense>
<CsOptions>
-m195 -otest.wav
</CsOptions>
<CsInstruments>

prints "I'm about to try compiling a simple test C++ module....\n"

gS_source_code = {{

#include <csound/csdl.h>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

void* __dso_handle = (void *)&__dso_handle;

extern "C" int csound_main(CSOUND *csound) {
    csound->Message(csound, "\\n********\\nHello, world! This is csound_main with csound: %p.\\n", csound);
    std::vector<std::string> strings;
    strings.push_back("A test string...");
    csound->Message(csound, "\\nProof that a lot of libstdc++ stuff works: strings.size(): %ld strings[0]: %s\\n", strings.size(), strings[0].c_str());
    std::cerr << "Now that we have manually defined our own __dso_handle, this proves std::cerr works as well!\\n********\\n" << std::endl;
    return 0;
};

}}

gi_result clang_orc "csound_main", gS_source_code, "-std=c++14 -I/usr/local/include/csound -stdlib=libstdc++", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so /usr/lib/gcc/x86_64-linux-gnu/9/libgcc_s.so /usr/lib/x86_64-linux-gnu/libm.so /usr/lib/x86_64-linux-gnu/libpthread.so"

</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>

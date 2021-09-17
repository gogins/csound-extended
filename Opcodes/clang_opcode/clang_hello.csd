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

void*   __dso_handle = (void*) &__dso_handle;

extern "C" int csound_main(CSOUND *csound) {
    csound->Message(csound, "\\n********\\nHello, world! This is csound_main with csound: %p.\\n********\\n\\n", csound);
    std::vector<std::string> strings;
    strings.push_back("A test string...");
    csound->Message(csound, "\\nProof that a lot of stdc++ stuff works: strings.size(): %ldd strings[0]: %s\\n", strings.size(), strings[0].c_str());
    std::cerr << "Now that we have defined our own __dso_handle, std:cerr works as well!" << std::endl;

    return 0;
};

}}

;gi_result clang gS_source_code, "-v -lgcc -lgcc_s -stdlib=libstdc++ -fno-use-cxa-atexit"
gi_result clang gS_source_code, "-v -fPIC -std=c++14", "/usr/lib/gcc/x86_64-linux-gnu/9/libstdc++.so"

</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>

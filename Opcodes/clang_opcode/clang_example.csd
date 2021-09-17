<CsoundSyntheizer>
<CsLicense>
This .csd file tests the new JIT compiler opcode for Csound.
</CsLicense>
<CsOptions>
-m195 -otest.wav
</CsOptions>
<CsInstruments>

prints "I'm about to try compiling a simple test C++ module.\n"

gS_source_code = {{

#include <csound/csdl.h>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

extern "C" int csound_main(CSOUND *csound) {
        csound->Message(csound, "Hello, World! This is csound_main with csound: %p.\\n", csound);
        //std::cerr << "And this is std::cerr!" << std::endl;
        return 0;
};

}}

gi_result clang "csound_main", gS_source_code, "-v"

</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>

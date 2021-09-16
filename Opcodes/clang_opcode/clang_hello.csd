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
//#include <iostream>
#include <sstream>
#include <string>
#include <vector>

extern "C" int csound_main(CSOUND *csound) {
    csound->Message(csound, "Hello, world! This is csound_main with csound: %p.\\n", csound);
    //std::cerr << "And now I'm calling std::cerr!" << std::endl;
    return 0;
};

}}

;gi_result clang gS_source_code, "-v -lgcc -lgcc_s -stdlib=libstdc++ -fno-use-cxa-atexit"
gi_result clang gS_source_code, "-v"

gS_source_code_2 = {{

#include <csound/csdl.h>
#include <cstdio>
#include <cstdlib>
//#include <iostream>
#include <sstream>
#include <string>
#include <vector>

extern "C" int csound_main(CSOUND *csound) {
    csound->Message(csound, "Hello, world #2! This is csound_main #2 with csound: %p.\\n", csound);
    //std::cerr << "And now I'm calling std::cerr!" << std::endl;
    return 0;
};

}}

;gi_result clang gS_source_code_2, "-v -lgcc -lgcc_s -stdlib=libstdc++ -fno-use-cxa-atexit"
gi_result clang gS_source_code_2, "-v"

</CsInstruments>
<CsScore>
f 0 30
</CsScore>
</CsoundSynthesizer>

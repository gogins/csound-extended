#include <vector>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <sstream>
#include <csound/csound.hpp>

extern "C" {

    int csound_main(CSOUND *csound) {
        std::fprintf(stderr, "Hello, World! This is \"csound_main\" with csound: %p\n", csound);
        return 0;
    }
};

int main(int argc, const char **argv) {
    std::string hello = "Hello, world, from LLVM land!\n";
    std::vector<std::string> vector_ = {hello};
    std::printf("%s", vector_.front().c_str());
    auto result = csound_main(nullptr);
    return 0;
}
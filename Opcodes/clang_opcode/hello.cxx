#include <vector>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <sstream>

extern "C" {
    int my_hook(const char *what) {
        int result = 3;
        //std::cout << "what is: " << what << " and should return: " << result << std::endl;
        return result;
    }
}

int main(int argc, const char **argv) {
    std::string hello = "Hello, world, from LLVM land!\n";
    std::vector<std::string> vector_ = {hello};
    std::printf("%s", vector_.front().c_str());
    return 0;
}
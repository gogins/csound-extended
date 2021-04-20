#include <Silence.hpp>

auto script = R"(
c = .9
y = 0
for i in range(100):
    y1 = y * c * 4 * (1 - y);
    y = y1
    insno = 1
    time = i / 8.
    duration = .5
    midi_key = y * 60. + 36.
    midi_velocity = 60.
    print("i ", insno, time, duration, midi_key, midi_velocity)
)";

int main(int argc, const char **argv) 
{
    csound::System::setMessageLevel(15);
    csound::ExternalNode externalNode;
    externalNode.setCommand("/usr/bin/python");
    externalNode.setScript(script);
    externalNode.generate();
    std::cout << "Generated:" << std::endl;
    std::cout << externalNode.getScore().getCsoundScore() << std::endl;
    return 0;
}
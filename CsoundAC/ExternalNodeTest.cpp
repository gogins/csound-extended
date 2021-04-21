#include <Silence.hpp>

auto script = R"(
import math

c = .98
y = 0.5
bass = 36
range_ = 60
for i in range(100):
    y1 = c * y * (1 - y) * 4
    y = y1
    midi_key = math.floor(bass + (y * range_)) 
    insno = 1
    time_ = i / 8.
    duration = .5
    midi_velocity = 60.
    print("i ", insno, time_, duration, midi_key, midi_velocity)
)";

int main(int argc, const char **argv) 
{
    csound::System::setMessageLevel(7);
    csound::ExternalNode externalNode;
    externalNode.setCommand("/usr/bin/python3.9");
    externalNode.setScript(script);
    externalNode.generate();
    std::cout << "Generated:" << std::endl;
    std::cout << externalNode.getScore().getCsoundScore() << std::endl;
    return 0;
}
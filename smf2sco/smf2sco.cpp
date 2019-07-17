#include <Score.hpp>
#include <cstdio>
#include <string>
#include <fstream>

/**
 * Read a standard MIDI file in format 0 or format 1, and translate 
 * it to a Csound score in "silence" format.
 */
int main(int argc, const char **argv)
{
    if (argc < 3) {
        std::fprintf(stderr, "Usage: smf2sco [input.mid] [output.sco]\n");
    }
    csound::Score score;
    score.load(argv[1]);
    std::fstream stream(argv[2]);
    // Delete non-sounding notes, and add 1 to MIDI channel numbers.
    for (int i = 0; i < score.size(); i++) {
        csound::Event event = score[i];
        if (event.isNoteOn() == true) {
            double insno = event.getChannel();
            insno = insno + 1;
            event.setInstrument(insno);
            auto istatement = event.toCsoundIStatement(12);
            stream.write(istatement.data(), istatement.size());
        }
    }
    stream.close();
    return 0;
}


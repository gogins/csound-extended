#include <csound/csound_threaded.hpp>
#include <iostream>

const char csd_text[] = R"(
<CsoundSynthesizer>
<CsOptions>
</CsOptions>
<CsInstruments>
sr          =           96000
ksmps       =           1
nchnls      =           2
0dbfs       =           4000

instr xtratim
i_attack = .05
i_release = .15
xtratim i_attack + i_release

k_release release
if k_release == 0 then
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
printf "sustain p3: %8.4f time: %9.4f\n", 1, p3, timeinsts()
else
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
printf "release p3: %8.4f time: %9.4f\n", 1, p3, timeinsts()
endif

a_envelope linseg 0, i_attack, 1, p3, 1, i_release, 0
i_frequency = cpsmidinn(p4)
i_amplitude = ampdb(p5)
a_signal poscil3 i_amplitude, i_frequency
outs a_signal * a_envelope, a_signal * a_envelope
endin
instr p3_plus_attack_plus_release
i_attack = .05
i_release = .15
i_sustain = p3

p3 = i_attack + i_sustain + i_release

k_release release
if k_release == 0 then
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
printf "sustain p3: %8.4f time: %9.4f\n", 1, p3, timeinsts()
else
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
printf "release p3: %8.4f time: %9.4f\n", 1, p3, timeinsts()
endif

a_envelope linseg 0, i_attack, 1, i_sustain, 1, i_release, 0
i_frequency = cpsmidinn(p4)
i_amplitude = ampdb(p5)
a_signal poscil3 i_amplitude, i_frequency
outs a_signal * a_envelope, a_signal * a_envelope
endin

// This is the pattern to use.

instr noteon_noteoff
i_attack = .05
i_release = .15
xtratim i_attack + i_release
a_envelope linsegr 0, i_attack, 1, p3, 1, i_release, 0
prints "%-24.24s i %9.4f t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f #%3d\n", nstrstr(p1), p1, p2, p3, p4, p5, p7, active(p1)
i_frequency = cpsmidinn(p4)
i_amplitude = ampdb(p5)
a_signal poscil3 i_amplitude, i_frequency
outs a_signal * a_envelope, a_signal * a_envelope
endin

</CsInstruments>
<CsScore>
i 1 1 5 60 60 0 .5
i 1 7 5 60 60 0 .5
i 1 8 5 64 60 0 .5
i 1 9 5 67 60 0 .5
s 15
i 2 1 5 60 60 0 .5
i 2 7 5 60 60 0 .5
i 2 8 5 64 60 0 .5
i 2 9 5 67 60 0 .5
s 15
i 3.1  1 -5 60 60 0 .5
i 3.2  7 -5 60 60 0 .5
i 3.3  8 -5 64 60 0 .5
i 3.4  9 -5 67 60 0 .5
i -3.1  6 -5 60 60 0 .5
i -3.2 12 -5 60 60 0 .5
i -3.3 13 -5 64 60 0 .5
i -3.4 14 -5 67 60 0 .5
s 15
i 3 1 5 60 60 0 .5
i 3 7 5 60 60 0 .5
i 3 8 5 64 60 0 .5
i 3 9 5 67 60 0 .5
s 15
i 3 1 .05 60 60 0 .5
i 3 7 .05 60 60 0 .5
i 3 8 .05 64 60 0 .5
i 3 9 .05 67 60 0 .5
s 15

</CsScore>
</CsoundSynthesizer>
)";

int main(int argc, char *argv[])
{
    Csound csound;
    csound.SetOption("-d");
    csound.SetOption("-m160");
    csound.SetOption("-otest.wav");
    csound.SetOption("-+msg_color=0");
    std::fprintf(stderr, "csound.Compile...\n");
    csound.CompileCsdText(csd_text);
    std::fprintf(stderr, "csound.Start...\n");
    csound.Start();
    std::fprintf(stderr, "csound.Perform...\n");
    csound.Perform();
    std::fprintf(stderr, "csound.Cleanup...\n");
    csound.Cleanup();
    std::fprintf(stderr, "Finished.\n");
    return 0;
}

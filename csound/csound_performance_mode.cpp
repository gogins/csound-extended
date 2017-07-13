/**
 * Compile with:
 * Linux:      g++ csound_performance_mode.cpp -o csound_performance_mode -std=c++11 -O2 -g -lcsound64
 * mingw64:    g++ csound_performance_mode.cpp -o csound_performance_mode -std=c++11 -O2 -g -LC:/Program_Files/Csound6_x64/bin -lcsound64 -IC:/Program_Files/Csound6_x64/include
 * MSVC amd64: Create a Visual Studio command-line project just for this file.
 */

#include <csound/csound.h>
#include <sndfile/sndfile.h>
#include <csound/csPerfThread.cpp>
#include <cstdio>
#include <iostream>

auto about = R"(
C S O U N D   P E R F O R M A N C E   M O D E
By Michael Gogins

This program demonstrates Csound performance modes ('real-time' vs. 'non-real-time').
It attempts to answer questions that remain after reading documentation and source code.
)";
const char *options = "-d -m3 -o dac";
const char *orc = R"(
sr          =           88200
ksmps       =           100
nchnls      =           2

gk_Harpsichord_level init 0
gk_HarpsichordPan init .5
gi_Harptable ftgen 0, 0, 65536, 7, -1, 1024, 1, 1024, -1
instr Harpsichord
//////////////////////////////////////////////
// Original by James Kelley.
// Adapted by Michael Gogins.
//////////////////////////////////////////////
insno = p1
itime = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p7
ipan = .25
idepth = p8
iheight = p9
ipcs = p10
ihomogeneity = p11
iattack = .005
isustain = p3
irelease = .3
p3 = iattack + isustain + irelease
iHz = cpsmidinn(ikey)
kHz = k(iHz)
iamplitude = ampdb(ivelocity)
aenvelope 	 transeg 1.0, 20.0, -10.0, 0.05
apluck 	 pluck 1, kHz, iHz, 0, 1
aharp 	 poscil 1, kHz, gi_Harptable
aharp2 	 balance apluck, aharp
asignal			 = (apluck + aharp2) * iamplitude * aenvelope * 10
adeclick linsegr 0, iattack, 1, isustain, 1, irelease, 0
kgain = ampdb(gk_Harpsichord_level)
asignal = asignal * adeclick * kgain
aoutleft, aoutright pan2 asignal, ipan
outs aoutleft, aoutright
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endin

gi_Buzzer_overlap init .1
gk_Buzzer_harmonics init 2
gk_Buzzer_level init 0
gk_Buzzer_distort_factor init 0.4
gk_Buzzer_first_harmonic init 0.4
gi_Buzzer_sine ftgen 0, 0, 65536, 10, 1
instr Buzzer
insno = p1
itime = p2
iduration = p3
ikey = p4
ivelocity = p5
iphase = p7
ipan = .25
idepth = p8
iheight = p9
ipcs = p10
ihomogeneity = p11
iamp = ampdb(ivelocity) * 4
iattack = gi_Buzzer_overlap
idecay = gi_Buzzer_overlap
isustain = p3
p3 = iattack + isustain + idecay
aenvelope transegr 0.0, iattack / 2.0, 1.5, iamp / 2.0, iattack / 2.0, -1.5, iamp, isustain, 0.0, iamp, idecay / 2.0, 1.5, iamp / 2.0, idecay / 2.0, -1.5, 0
ihertz = cpsmidinn(ikey)
asignal buzz aenvelope, ihertz, gk_Buzzer_harmonics, gi_Buzzer_sine
asignal = asignal * 3
aleft, aright pan2 asignal, ipan
adamping linseg 0, 0.03, 1, p3 - 0.1, 1, 0.07, 0
kgain = ampdb(gk_Buzzer_level)
aleft = adamping * aleft * kgain
aright = adamping * aright * kgain
outs aleft, aright
prints "instr %4d t %9.4f d %9.4f k %9.4f v %9.4f p %9.4f\n", p1, p2, p3, p4, p5, p7
endin
)";

const char *sco = R"(
i 1 .1 .5 60 60
i 2 .2 .5 64 60
i 2 .3 .5 67 60
i 1 .4 .5 71 60
; The following line causes immediate exit in real-time performance.
; e 7
)";
const char *csd_template = R"(
<CsoundSynthesizer>
<CsOptions>
%s
</CsOptions>
<CsInstruments>
%s
</CsInstruments>
<CsScore>
%s
</CsScore>
</CsoundSynthesizer>
)";

int main(int argc, const char *argv[])
{
    std::cout << about << std::endl;
    char csd[0x10000];
    std::sprintf(csd, csd_template, options, orc, sco);
    CSOUND *csound = nullptr;
	CsoundPerformanceThread *csoundPerformanceThread = nullptr;
	double score_time = 0;
    double score_duration = 0;
    int result = 0;
    int kperiods = 0;
    std::cout << R"(

csoundCompileCsdText
csoundStart
csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundCompileCsdText(csound, csd);
    csoundStart(csound);
    while ((result = csoundPerformKsmps(csound)) == 0) {
        score_duration = csoundGetScoreTime(csound) + 1;
    };
    std::cout << "result: " << result << std::endl;
    std::cout << "score_duration: " << score_duration << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundStart
csoundCompileCsdText
csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundStart(csound);
    csoundCompileCsdText(csound, csd);
    while ((result = csoundPerformKsmps(csound)) == 0) {
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            // Does not cause csoundPerformKsmps to return non-zero!
            // std::cout << "csoundStop" << std::endl;
            // csoundStop(csound);
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundStart
csoundCompileOrc
csoundReadScore
csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundStart(csound);
    csoundCompileOrc(csound, orc);
    csoundReadScore(csound, sco);
    while ((result = csoundPerformKsmps(csound)) == 0) {
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundCompileOrc
csoundReadScore
csoundStart
csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundCompileOrc(csound, orc);
    csoundReadScore(csound, sco);
    csoundStart(csound);
    while ((result = csoundPerformKsmps(csound)) == 0) {
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundReadScore
csoundCompileOrc
csoundStart
csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundReadScore(csound, sco);
    csoundCompileOrc(csound, orc);
    csoundStart(csound);
    while ((result = csoundPerformKsmps(csound)) == 0) {
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundCompileOrc
csoundStart
csoundPerformKsmps
csoundReadScore after 1st csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundCompileOrc(csound, orc);
    csoundStart(csound);
    score_time = 0;
    while ((result = csoundPerformKsmps(csound)) == 0) {
        if (score_time == 0) {
            csoundReadScore(csound, sco);
        }
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundStart
csoundCompileOrc
csoundPerformKsmps
csoundReadScore after 1 csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundStart(csound);
    csoundCompileOrc(csound, orc);
    score_time = 0;
    while ((result = csoundPerformKsmps(csound)) == 0) {
        if (score_time == 0) {
            csoundReadScore(csound, sco);
        }
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundCompileOrc
csoundStart
csoundPerformKsmps
csoundReadScore after 10 csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundStart(csound);
    csoundCompileOrc(csound, orc);
    score_time = 0;
    kperiods = 0;
    while ((result = csoundPerformKsmps(csound)) == 0) {
        kperiods++;
        if (kperiods == 10) {
            csoundReadScore(csound, sco);
        }
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundCompileOrc
csoundPerformKsmps
csoundReadScore after 10 csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundCompileOrc(csound, orc);
    score_time = 0;
    kperiods = 0;
    while ((result = csoundPerformKsmps(csound)) == 0) {
        kperiods++;
        if (kperiods == 10) {
            csoundReadScore(csound, sco);
        }
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
    std::cout << R"(

csoundCompileOrc
csoundStart
csoundStart
csoundPerformKsmps
csoundReadScore after 10 csoundPerformKsmps

    )";
    csound = csoundCreate(0);
    csoundSetOption(csound, "-d");
    csoundSetOption(csound, "-m3");
    csoundSetOption(csound, "-odac");
    csoundCompileOrc(csound, orc);
    csoundStart(csound);
    csoundStart(csound);
    score_time = 0;
    kperiods = 0;
    while ((result = csoundPerformKsmps(csound)) == 0) {
        kperiods++;
        if (kperiods == 10) {
            csoundReadScore(csound, sco);
        }
        score_time = csoundGetScoreTime(csound);
        if (score_time > score_duration) {
            std::cout << "Passed time." << std::endl;
            break;
        }
    };
    std::cout << "result: " << result << std::endl;
    csoundDestroy(csound);
	std::cout << R"(

csoundCreate
csoundPerformanceThread = new CsoundPerformanceThread(csound)
csoundCompileOrc
csoundReadScore
csoundStart
csoundPerformanceThread->Play()
csoundPerformanceThread->Join()

    )";
	csound = csoundCreate(0);
	csoundPerformanceThread = new CsoundPerformanceThread(csound);
	csoundSetOption(csound, "-d");
	csoundSetOption(csound, "-m3");
	csoundSetOption(csound, "-odac");
	result = csoundCompileOrc(csound, orc);
	result = csoundReadScore(csound, sco);
	result = csoundStart(csound);
	csoundPerformanceThread->Play();
	csoundPerformanceThread->Join();
	delete csoundPerformanceThread;
	csoundDestroy(csound);
	std::cout << R"(

csoundCreate
csoundPerformanceThread = new CsoundPerformanceThread(csound)
csoundCompileOrc
csoundReadScore
csoundPerformanceThread->Play()
csoundStart
csoundPerformanceThread->Join()

    )";
    csound = csoundCreate(0);
	csoundPerformanceThread = new CsoundPerformanceThread(csound);
	csoundSetOption(csound, "-d");
	csoundSetOption(csound, "-m3");
	csoundSetOption(csound, "-odac");
	result = csoundCompileOrc(csound, orc);
	result = csoundReadScore(csound, sco);
	csoundPerformanceThread->Play();
	result = csoundStart(csound);
	csoundPerformanceThread->Join();
	delete csoundPerformanceThread;
    csoundDestroy(csound);
    std::cout << "Finished." << std::endl;
    return 0;
}

#include "ChordSpace.hpp"
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <string>

#pragma GCC diagnostic ignored "-Wformat"

typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Matrix;
typedef Eigen::Matrix<double, Eigen::Dynamic, 1> Vector;

static bool printPitv = true;
static int testSector = 0;

static bool is_k_dual(const csound::Chord &chord_) {
    auto chord = chord_.eOP();
    bool k_dual = false;
    auto under_k = chord.K().eOPTT().eOP();
    if (under_k != chord) {
        k_dual = true;
        std::fprintf(stderr, "chord:      %s %s\n", chord.toString().c_str(), chord.name().c_str());
        std::fprintf(stderr, "is dual to: %s %s\n\n", under_k.toString().c_str(), under_k.name().c_str());
    }
    return k_dual;
}

static void print_dualities(std::vector<csound::Chord> &chords) {
    for (auto &chord : chords) {
        is_k_dual(chord);
    }
}

int main(int argc, char **argv) {
    std::fprintf(stderr, "G E N E R A L I Z E D   C O N T E X T U A L   G R O U P   D U A L I T I E S\n\n");
    std::cerr << csound::chord_space_version() << std::endl;
    std::fprintf(stderr, "Identify OPTT that are non-invariant (dual, similar to major/minor) under K.\n\n");
    auto chordspace_optts_3 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>(3, 12., 1., testSector);
    print_dualities(chordspace_optts_3);
    auto chordspace_optts_4 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>(4, 12., 1., testSector);
    print_dualities(chordspace_optts_4);
    auto chordspace_optts_5 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>(5, 12., 1., testSector);
    print_dualities(chordspace_optts_5);
    std::fprintf(stderr, "Identify scale degrees that are non-invariant (dual) under K.\n\n");
    auto major_scale = csound::scaleForName("C major");
    for (int scale_degree = 1; scale_degree < 8; ++scale_degree) {
        for (int arity = 3; arity < 7; ++arity) {
            auto chord = major_scale.chord(scale_degree, arity);
            std::fprintf(stderr, "scale: %s degree: %3d voices: %3d\n", major_scale.name().c_str(), scale_degree, arity);
            is_k_dual(chord);
        }
    }
    auto minor_scale = csound::scaleForName("C minor");
    for (int scale_degree = 1; scale_degree < 8; ++scale_degree) {
        for (int arity = 3; arity < 7; ++arity) {
            auto chord = minor_scale.chord(scale_degree, arity);
            std::fprintf(stderr, "scale: %s degree: %3d voices: %3d\n", minor_scale.name().c_str(), scale_degree, arity);
            is_k_dual(chord);
        }
    }
    std::fprintf(stderr, "Finished.\n");
    return 0;
}
 

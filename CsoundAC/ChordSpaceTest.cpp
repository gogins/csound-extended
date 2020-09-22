 #include "ChordSpace.hpp"
#include <System.hpp>
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <string>

#pragma GCC diagnostic ignored "-Wformat"

static bool printPass = true;
static bool failureExits = false;
static int passCount = 0;
static int failureCount = 0;
static int testCount = 0;
static int exitAfterFailureCount = 1;
static int maximumVoiceCountToTest = 4;

static void pass(std::string message) {
    passCount = passCount + 1;
    testCount = passCount + failureCount;
    if (printPass) {
        std::fprintf(stderr, "\nPASSED (passed: %-9d failed: %-9d of %9d): %s\n", passCount, failureCount, testCount, message.c_str());
    }
}

static void fail(std::string message) {
    failureCount = failureCount + 1;
    testCount = passCount + failureCount;
    std::fprintf(stderr, "================================================================================================================\n");
    std::fprintf(stderr, "FAILED (passed: %-9d failed: %-9d of %d): %s", passCount, failureCount, testCount, message.c_str());
    std::fprintf(stderr, "================================================================================================================\n");
    if (failureExits && (failureCount >= exitAfterFailureCount)) {
        std::exit(-1);
    }
}

static bool test(bool passes, std::string message) {
    if (passes) {
        pass(message + "\n");
    } else {
        fail(message + "\n");
    }
    return passes;
}

static void printSet(std::string name, const std::set<csound::Chord> &chords) {
    int i = 1;
    for (auto it = chords.begin(); it != chords.end(); ++it, ++i) {
        std::fprintf(stderr, "%s %5d: %s\n", name.c_str(), i, it->toString().c_str());
    }
}

static bool equals(const csound::HyperplaneEquation &a, const csound::HyperplaneEquation &b) {
    if (a.unit_normal_vector.rows() == b.unit_normal_vector.rows() == false) {
        csound::System::error("equals: size mismatch: %d %d\n", a.unit_normal_vector.rows(), b.unit_normal_vector.rows());
        return false;
    }
    for (int row = 0; row < a.unit_normal_vector.rows(); ++row) {
        if (csound::eq_epsilon(a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0)) == false) {
            csound::System::error("equals: unit normal vector element mismatch: %.17g %.17g\n", a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0));
            return false;
        }
    }
    if (csound::eq_epsilon(a.constant_term, b.constant_term) == false) {
        csound::System::error("equals: constant term mismatch: %.17g %,17g\n", a.constant_term, b.constant_term);
        return false;
    }
    return true;
}

static void Hyperplane_Equation_for_Test_Points() {
    std::vector<csound::Chord> points;
    points.push_back(csound::Chord(std::vector<double>({ 4,  0, -1,  0})));
    points.push_back(csound::Chord(std::vector<double>({ 1,  2,  3, -1})));
    points.push_back(csound::Chord(std::vector<double>({ 0, -1,  2,  0})));
    points.push_back(csound::Chord(std::vector<double>({-1,  1, -1,  1})));
    // From inversion_flats.py for these points by SVD from vectors:
    csound::HyperplaneEquation expected;
    expected.unit_normal_vector.resize(4, 1);
    expected.unit_normal_vector << 0.20864865369890548, 0.12839917150701868, 0.32099792876754685, 0.9148440969875088;
    expected.constant_term = 0.5135966860280752;
    csound::HyperplaneEquation actual = hyperplane_equation(points, false);
    bool passes = equals(expected, actual);
    test(passes, __func__);    
}

/*

INVERSION FLAT FOR 4 VOICES BY SINGULAR VALUE DECOMPOSITION...

original points:
 [[0, 0, 0, 0], [0, 0, 0, 12], [0, 1, 2, 9], [0, 3, 6, 9], [0, 0, 0, 7]]
points:
 [[0.0, 0.0, 0.0, 0.0], [-3.0, -3.0, -3.0, 9.0], [-3.0, -2.0, -1.0, 6.0], [-4.5, -1.5, 1.5, 4.5], [-1.75, -1.75, -1.75, 5.25]]
subtrahend: [-1.75, -1.75, -1.75, 5.25]
vectors:
 [[ 1.75  1.75  1.75 -5.25]
 [-1.25 -1.25 -1.25  3.75]
 [-1.25 -0.25  0.75  0.75]
 [-2.75  0.25  3.25 -0.75]]
U:
 [[-0.80008406 -0.08951901 -0.55315943 -0.21416464]
 [ 0.57148861  0.06394215 -0.81639104 -0.05308325]
 [ 0.1019949   0.32961289  0.15737939 -0.92530217]
 [-0.1512062   0.93768496 -0.0524598   0.30843406]]
singular values: [7.56051967e+00 4.45404787e+00 5.81478041e-16 2.30311389e-16]
V:
 [[-0.24154219 -0.28805006 -0.33455793  0.86415018]
 [-0.72456242 -0.01898676  0.6865889   0.05696028]
 [ 0.42867847  0.62353653  0.42867847  0.49363116]
 [ 0.48259863 -0.72654584  0.48259863  0.07955047]]
normal_vector: [ 0.48259863 -0.72654584  0.48259863  0.07955047]
norm: 1.0000000000000002
Unit normal vector:
0.4825986251911143
-0.7265458392918117
0.4825986251911144
0.0795504703634725
unit_normal_vector: [ 0.48259863 -0.72654584  0.48259863  0.07955047]
constant_term: 8.326672684688674e-16
 
*/

static csound::HyperplaneEquation Hyperplane_Equation_for_4_Voices() {
    //From _Science_ draft 6.4.5 (a):
    std::vector<csound::Chord> points;
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  6,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  6,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  5,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  2,  6,  8})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  5,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  4,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  3,  6,  9})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  2,  5,  8})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  4,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  3,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  4,  6, 10})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  3,  5,  9})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  2,  4,  8})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  3,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  2,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  5,  6, 11})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  4,  5, 10})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  3,  4,  9})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  2,  3,  8})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  2,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  1,  6})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  6,  6, 12})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  5,  5, 11})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  4,  4, 10})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  3,  3,  9})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  2,  2,  8})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  1,  1,  7})));
    points.push_back(csound::Chord(std::vector<double>({ 0,  0,  0,  6})));
    //~ csound::HyperplaneEquation expected;
    //~ expected.unit_normal_vector.resize(4, 1);
    //~ expected.unit_normal_vector << 0.4825986251911143, -0.7265458392918117, 0.4825986251911144, 0.0795504703634725;
    //~ expected.constant_term = 8.326672684688674e-16;
    csound::HyperplaneEquation actual = hyperplane_equation(points, true);
    //~ bool passes = equals(expected, actual);
    //~ test(passes, __func__);    
    return actual;
}

/**
 * The idea here is that in a given fundamental domain of OPT, the centroid of 
 * the OPTI subset should be perfectly reflected as the centroid of the ~OPTI 
 * subset, and the vector from one centroid to the other should be normal to 
 * the inversion flat.
 */
namespace csound {
    
static HyperplaneEquation hyperplane_equation_from_centroids(int dimensions) {
    auto opts = fundamentalDomainByIsNormal<EQUIVALENCE_RELATION_RPT>(dimensions, OCTAVE(), 1.);
    Chord opti_centroid(dimensions);
    Chord not_opti_centroid(dimensions);
    Chord center = opti_centroid.center();
    int opti_count = 0;
    int not_opti_count = 0;
    for (auto opt : opts) {
        if (opt.iseOPTI() == true) {
            opti_count = opti_count + 1;
            for (int voice = 0; voice < dimensions; ++voice) {
                auto sum = opti_centroid.getPitch(voice) + opt.getPitch(voice);
                opti_centroid.setPitch(voice, sum);
            }
        } else {
            not_opti_count = not_opti_count + 1;
            for (int voice = 0; voice < dimensions; ++voice) {
                auto sum = not_opti_centroid.getPitch(voice) + opt.getPitch(voice);
                not_opti_centroid.setPitch(voice, sum);
            }
        }
    }
    for (int voice = 0; voice < dimensions; ++voice) {
        opti_centroid.setPitch(voice, opti_centroid.getPitch(voice) / opti_count);
        not_opti_centroid.setPitch(voice, not_opti_centroid.getPitch(voice) / not_opti_count);
    }
    auto normal_vector = voiceleading(opti_centroid, not_opti_centroid);
    auto norm = normal_vector.col(0).norm();
    HyperplaneEquation hyperplane_equation_;
    hyperplane_equation_.unit_normal_vector = normal_vector.col(0) / norm;
    auto temp = center.col(0).adjoint() * hyperplane_equation_.unit_normal_vector;    
    hyperplane_equation_.constant_term = temp(0, 0);
    std::fprintf(stderr, "hyperplane_equation_from_centroids: center:\n");
    for(int i = 0; i < dimensions; i++) {
        std::fprintf(stderr, "  %9.4f\n", center.getPitch(i));
    }
    std::fprintf(stderr, "hyperplane_equation_from_centroids: unit_normal_vector:\n");
    for(int i = 0; i < dimensions; i++) {
        std::fprintf(stderr, "  %9.4f\n", hyperplane_equation_.unit_normal_vector(i, 0));
    }
    std::fprintf(stderr, "hyperplane_equation_from_centroids: constant_term: %9.4f\n", hyperplane_equation_.constant_term);
    return hyperplane_equation_;
}

};

static bool test_chord_type(int dimensions) {
    bool passes = true;
    auto ops = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RP>(dimensions, csound::OCTAVE(), 1.);
    for (auto op : ops) {
        auto opt = op.eOPTT();
        auto chord_type_ = opt.chord_type();
        auto tt_of_chord_type = chord_type_.eTT();
        if (true) {
            std::fprintf(stderr, "test_chord_types:\n  OP:               %s %s\n  OPT:              %s\n  chord_type:       %s\n  TT of chord_type: %s\n", 
                op.toString().c_str(), op.name().c_str(),
                opt.toString().c_str(),
                chord_type_.toString().c_str(),
                tt_of_chord_type.toString().c_str());
        }
        if (opt.equals(tt_of_chord_type) == false) {
            std::fprintf(stderr, ">> Oops! OPT of %s != TT of chord type %s\n", opt.toString().c_str(), tt_of_chord_type.toString().c_str());
            passes = false;
        }
    }
    return passes;
}

static void test_chord_types() {
    for (int dimensions = 3; dimensions < 5; ++dimensions) {
        test(test_chord_type(dimensions), "chord_type: OPTs of chords should equal OPTs of chord.chord_types.");
    }
}

static void testChordSpaceGroup(const csound::ChordSpaceGroup &chordSpaceGroup, std::string chordName) {
    std::fprintf(stderr, "BEGAN test ChordSpaceGroup for %s...\n", chordName.c_str());
    csound::Chord originalChord = csound::chordForName(chordName);
    csound::Chord optti = originalChord.eOPTTI();
    std::fprintf(stderr, "Original chord:\n%s\n", originalChord.information().c_str());
    Eigen::VectorXi pitv = chordSpaceGroup.fromChord(originalChord, true);
    csound::Chord reconstitutedChord = chordSpaceGroup.toChord(pitv[0], pitv[1], pitv[2], pitv[3], true)[0];
    std::fprintf(stderr, "Reconstituted chord:\n%s\n", reconstitutedChord.information().c_str());
    test(originalChord == reconstitutedChord, "Reconstituted chord must be the same as the original chord.\n");
    csound::Chord revoicedOriginalChord = originalChord;
    revoicedOriginalChord.setPitch(1,  revoicedOriginalChord.getPitch(1) + 12.);
    revoicedOriginalChord.setPitch(2,  revoicedOriginalChord.getPitch(2) + 24.);
    std::fprintf(stderr, "Revoiced original chord:\n%s\n", revoicedOriginalChord.information().c_str());
    pitv = chordSpaceGroup.fromChord(revoicedOriginalChord, true);
    csound::Chord reconstitutedRevoicedChord = chordSpaceGroup.toChord_vector(pitv, true)[0];
    std::fprintf(stderr, "Reconstituted revoiced chord:\n%s\n", reconstitutedRevoicedChord.information().c_str());
    test(revoicedOriginalChord == reconstitutedRevoicedChord, "Reconstituted revoiced chord must be the same as the original revoiced chord.\n");
    csound::Chord invertedChord = originalChord.I().eOP();
    std::fprintf(stderr, "Inverted original chord:\n%s\n", invertedChord.information().c_str());
    pitv = chordSpaceGroup.fromChord(invertedChord);
    csound::Chord reconstitutedInvertedChord = chordSpaceGroup.toChord_vector(pitv, true)[0];
    std::fprintf(stderr, "Reconstituted inverted chord:\n%s\n", reconstitutedInvertedChord.information().c_str());
    test(invertedChord == reconstitutedInvertedChord,"Reconstituted inverted chord must be the same as the original inverted chord.\n");
    std::fprintf(stderr, "ENDED test ChordSpaceGroup for %s.\n", chordName.c_str());
    std::fprintf(stderr, "\n");
}

static void testFChordSpaceGroup(int initialVoiceCount, int finalVoiceCount) {
    double range = 48.0;
    for (int voiceCount = initialVoiceCount; voiceCount <= finalVoiceCount; ++voiceCount) {
        bool passes = true;
        csound::ChordSpaceGroup chordSpaceGroup;
        std::fprintf(stderr, "Testing all of ChordSpaceGroup: voices: %d  range: %f\n", voiceCount, range);
        std::fprintf(stderr, "Testing ChordSpaceGroup to chords and back...\n");
        chordSpaceGroup.initialize(voiceCount, range);
        chordSpaceGroup.list(true, true, true);
        for (int P = 0; P < chordSpaceGroup.countP; ++P) {
            for (int I = 0; I < chordSpaceGroup.countI; ++I) {
                for (int T = 0; T < chordSpaceGroup.countT; ++T) {
                    for (int V = 0; V < chordSpaceGroup.countV; ++V) {
                        csound::Chord fromPITV = chordSpaceGroup.toChord(P, I, T, V)[0];
                        if (printPass) std::fprintf(stderr, "%8d     %8d     %8d     %8d => %s\n", P, I, T, V, fromPITV.toString().c_str());
                        Eigen::VectorXi pitv = chordSpaceGroup.fromChord(fromPITV);
                        csound::Chord frompitv = chordSpaceGroup.toChord(pitv(0), pitv(1), pitv(2), pitv(3))[0];
                        if (printPass) std::fprintf(stderr, "%8d     %8d     %8d     %8d <= %s\n",pitv(0), pitv(1), pitv(2), pitv(3), frompitv.toString().c_str());
                        bool equals = (fromPITV == frompitv);
                        if (!equals) {
                            chordSpaceGroup.toChord(P, I, T, V, true);
                            std::fprintf(stderr, "fromPITV (toChord):\n%s\n", fromPITV.information().c_str());
                            chordSpaceGroup.fromChord(fromPITV, true);
                            std::fprintf(stderr, "frompitv (fromChord):\n%s\n", frompitv.information().c_str());
                            passes = false;
                        }
                        test(equals, "fromPITV must match frompitv.\n");
                        if (printPass) std::fprintf(stderr, "\n");
                    }
                }
            }
        }
        std::fprintf(stderr, "Testing chords to ChordSpaceGroup and back...\n\n");
        bool passes2 = true;
        auto eops = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RP>(voiceCount, csound::OCTAVE(), 1.0);
        for(auto it = eops.begin(); it != eops.end(); ++it) {
            auto chord = *it;
            auto origin = chord;
            for(;;) {
                Eigen::VectorXi pitv = chordSpaceGroup.fromChord(chord);
                if (printPass) std::fprintf(stderr, "%8d     %8d     %8d     %8d <= %s\n", pitv(0), pitv(1), pitv(2), pitv(3), chord.toString().c_str());
                csound::Chord fromPITV = chordSpaceGroup.toChord(pitv(0), pitv(1), pitv(2), pitv(3))[0];
                if (printPass) std::fprintf(stderr, "%8d     %8d     %8d     %8d => %s\n", pitv(0), pitv(1), pitv(2), pitv(3), fromPITV.toString().c_str());
                bool equals = (fromPITV == chord);
                if (!equals) {
                    std::fprintf(stderr, "Original chord (fromChord):\n%s\n", chord.information().c_str());
                    std::fprintf(stderr, "New chord (toChord):\n%s\n", fromPITV.information().c_str());
                    passes2 = false;
                }
                test(equals, "Original chord must match chord from original chord's PITV.\n");
                if (printPass) std::fprintf(stderr, "\n");
                // This was going too far... cut off sooner and all seems well.
                if (csound::next(chord, origin, range - 1.0, csound::OCTAVE()) == false)
                {
                    break;
                }
            }
        }
    }
}

std::vector<std::string> equivalenceRelationsToTest = {"RP", "RPTg", "RPI", "RPTI", "RPTgI"};
typedef csound::Chord(*normalize_t)(const csound::Chord &, double, double);
typedef bool (*isNormal_t)(const csound::Chord &, double, double);
typedef bool (*isEquivalent_t)(const csound::Chord &, const csound::Chord &, double, double);
typedef std::set<csound::Chord> (*fundamentalDomainByNormalize_t)(int, double, double);
typedef std::set<csound::Chord> (*fundamentalDomainByIsNormal_t)(int, double, double);
std::map<std::string, normalize_t> normalizesForEquivalenceRelations;
std::map<std::string, isNormal_t> isNormalsForEquivalenceRelations;
std::map<std::string, isEquivalent_t> isEquivalentsForEquivalenceRelations;
std::map<std::string, std::set<std::string> > equivalenceRelationsForCompoundEquivalenceRelations;
std::map<std::string, fundamentalDomainByNormalize_t> fundamentalDomainByNormalizesForEquivalenceRelations;
std::map<std::string, fundamentalDomainByIsNormal_t> fundamentalDomainByIsNormalsForEquivalenceRelations;

/**
 * The correctness and consistency of the equivalence relations are tested as
 * follows. We identify the elements of a representative fundamental domain
 * for an equivalence relation R both by sending all chords in chord space to
 * R's representative fundamental domain ("made_equivalents") and by 
 * identifying each chord in the space that belongs to R 
 * ("found_equivalents"). Because of singularities in the orbifolds, 
 * "found_equivalents" may have more chords than "made_equivalents." Then, we 
 * test that the following conditions obtain:
 * (1) Each element in "made_equivalents" also returns true for "is_equivalent".
 * (2) Each element in "found_equivalents" is also found in "made_equivalents".
 */
static bool testNormalsAndEquivalents(std::string equivalence,
                                      std::set<csound::Chord> &made_equivalents,
                                      std::set<csound::Chord> &found_equivalents,
                                      double range,
                                      double g) {
    char buffer[0x200];
    auto is_equivalent = isNormalsForEquivalenceRelations[equivalence];
    std::fprintf(stderr, "\nequivalence: %s  normalized: %ld  is_normal: %ld  range: %f  g: %f\n", equivalence.c_str(), made_equivalents.size(), found_equivalents.size(), range, g);
    auto make_equivalent = normalizesForEquivalenceRelations[equivalence];
    auto isEquivalent = isEquivalentsForEquivalenceRelations[equivalence];
    bool passes1 = true;
    for (auto made_equivalent = made_equivalents.begin(); made_equivalent != made_equivalents.end(); ++made_equivalent) {
        if (is_equivalent(*made_equivalent, range, g) == false) {
            passes1 = false;
            std::fprintf(stderr, "testNormalsAndEquivalents: %s range %f g %f: 'made_equivalents' %s is not 'is_equivalent'.\n",
                          equivalence.c_str(),
                          range,
                          g,
                          made_equivalent->toString().c_str());
        }
    }
    std::sprintf(buffer, "testNormalsAndEquivalents: %s range %f g %f: all 'made_equivalents' chords must return 'true' for 'is_equivalent'.\n",
                 equivalence.c_str(),
                 range,
                 g);
    test(passes1, buffer);
    bool passes2 = true;
    for (auto found_equivalent = found_equivalents.begin(); found_equivalent != found_equivalents.end(); ++found_equivalent) {
        ///std::fprintf(stderr, ">>> %s 'found_equivalents:' %s.\n", equivalence.c_str(), found_equivalent->toString().c_str());
        bool equivalenceFound = false;
        for (auto made_equivalent = made_equivalents.begin(); made_equivalent != made_equivalents.end(); ++made_equivalent) {
            if (isEquivalent(*found_equivalent, *made_equivalent, range, g) == true) {
                equivalenceFound = true;
                ///std::fprintf(stderr, "    %s 'made_equivalents:'  %s.\n", equivalence.c_str(), made_equivalent->toString().c_str());
            }
        }
        if (equivalenceFound == false) {
            passes2 = false;
            std::sprintf(buffer, "testNormalsAndEquivalents: %s range %f g %f: no 'made_equivalent' found for 'found_equivalent': %s\n",
                         equivalence.c_str(),
                         range,
                         g,
                         found_equivalent->toString().c_str());
            fail(buffer);
        }
    }
    std::sprintf(buffer, "normals and equivalents: %s range %f g %f: each 'found_equivalent' must have equivalent in 'made_equivalents'.\n",
                 equivalence.c_str(),
                 range,
                 g);
    pass(buffer);
    return passes1 && passes2;
}

static bool testEquivalenceRelation(std::string equivalenceRelation, int voiceCount, double range, double g) {
    bool passes = true;
    char buffer[0x200];
    auto normalsForEquivalenceRelation = fundamentalDomainByNormalizesForEquivalenceRelations[equivalenceRelation](voiceCount, range, g);
    std::sprintf(buffer, "%-8s 'made_equivalents': ", equivalenceRelation.c_str());
    printSet(buffer, normalsForEquivalenceRelation);
    std::sprintf(buffer, "%-8s 'found_equivalents':", equivalenceRelation.c_str());
    auto equivalentsForEquivalenceRelation = fundamentalDomainByIsNormalsForEquivalenceRelations[equivalenceRelation](voiceCount, range, g);
    printSet(buffer, equivalentsForEquivalenceRelation);
    if (!testNormalsAndEquivalents(equivalenceRelation,
                                   normalsForEquivalenceRelation,
                                   equivalentsForEquivalenceRelation,
                                   range,
                                   g)) {
        passes = false;
    }
    if (equivalenceRelation == "RPTgI") {
        if (voiceCount == 3) {
            if (equivalentsForEquivalenceRelation.size() != 19) {
                std::sprintf(buffer, "%-8s 'found_equivalents' size should be 19 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
                std::fprintf(stderr, buffer);
            }
            passes = false;
        }
        if (voiceCount == 4) {
            if (equivalentsForEquivalenceRelation.size() != 83) {
                std::sprintf(buffer, "%-8s 'found_equivalents' size should be 83 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
                std::fprintf(stderr, buffer);
            }
            passes = false;    
        }
    }
    return passes;
}

static bool testEquivalenceRelations(int voiceCount, double range, double g) {
    bool passes = true;
    std::fprintf(stderr, "\nTesting equivalence relations for %d voices over range %f with g %f...\n\n", voiceCount, range, g);
    for (auto equivalenceRelationI = equivalenceRelationsToTest.begin();
            equivalenceRelationI != equivalenceRelationsToTest.end();
            ++equivalenceRelationI) {
        if (!testEquivalenceRelation(*equivalenceRelationI, voiceCount, range, g)) {
            passes = false;
        }
    }
    return passes;
}

void printRPIStuff(const csound::Chord &chord)
{
    std::fprintf(stderr, "%s\n", chord.information().c_str());
}

void testRPIStuff(const csound::Chord &chord)
{
    std::fprintf(stderr, "\nTESTING RPI STUFF\n");
    std::fprintf(stderr, "chord...\n");
    printRPIStuff(chord);
    auto chordRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(chord, 12.0, 1.0);
    auto inverse = chord.I().eP();
    std::fprintf(stderr, "inverse...\n");
    printRPIStuff(inverse);
    auto inverseRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(inverse, 12.0, 1.0);
    std::fprintf(stderr, "inverseRP...\n");
    printRPIStuff(inverseRP);
    auto midpoint = csound::midpoint(chord.eP(), inverseRP.eP());
    std::fprintf(stderr, "midpoint of chord and inverseRP: %s\n", midpoint.eOP().toString().c_str());
    std::fprintf(stderr, "inverse of midpoint:             %s\n", midpoint.I().eOP().toString().c_str());
    auto inverseInverseRP = inverseRP.I();
    std::fprintf(stderr, "inverseInverseRP...\n");
    printRPIStuff(inverseInverseRP);
    auto inverseRPinverseRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(inverseInverseRP, 12.0, 1.0);
    std::fprintf(stderr, "inverseRPinverseRP...\n");
    printRPIStuff(inverseRPinverseRP);
    test(inverseRPinverseRP == chordRP, "chordRP must equal inverseRPinverseRP.");
}

int main(int argc, char **argv) {
    std::fprintf(stderr, "C H O R D S P A C E   U N I T   T E S T S\n\n");
    Hyperplane_Equation_for_Test_Points();
    Hyperplane_Equation_for_4_Voices();
#if defined(USE_OLD_EQUIVALENCES)
    std::fprintf(stderr, "Using OLD implementation of equivalence relations.\n\n");
#else
    std::fprintf(stderr, "Using NEW implementation of equivalence relations.\n\n");
#endif
    normalizesForEquivalenceRelations["R"] =        csound::normalize<csound::EQUIVALENCE_RELATION_R>;
    normalizesForEquivalenceRelations["P"] =        csound::normalize<csound::EQUIVALENCE_RELATION_P>;
    normalizesForEquivalenceRelations["T"] =        csound::normalize<csound::EQUIVALENCE_RELATION_T>;
    normalizesForEquivalenceRelations["Tg"] =       csound::normalize<csound::EQUIVALENCE_RELATION_Tg>;
    normalizesForEquivalenceRelations["I"] =        csound::normalize<csound::EQUIVALENCE_RELATION_I>;
    normalizesForEquivalenceRelations["RP"] =       csound::normalize<csound::EQUIVALENCE_RELATION_RP>;
    normalizesForEquivalenceRelations["RPT"] =      csound::normalize<csound::EQUIVALENCE_RELATION_RPT>;
    normalizesForEquivalenceRelations["RPTg"] =     csound::normalize<csound::EQUIVALENCE_RELATION_RPTg>;
    normalizesForEquivalenceRelations["RPI"] =      csound::normalize<csound::EQUIVALENCE_RELATION_RPI>;
    normalizesForEquivalenceRelations["RPTI"] =     csound::normalize<csound::EQUIVALENCE_RELATION_RPTI>;
    normalizesForEquivalenceRelations["RPTgI"] =    csound::normalize<csound::EQUIVALENCE_RELATION_RPTgI>;
    isNormalsForEquivalenceRelations["R"] =         csound::isNormal<csound::EQUIVALENCE_RELATION_R>;
    isNormalsForEquivalenceRelations["P"] =         csound::isNormal<csound::EQUIVALENCE_RELATION_P>;
    isNormalsForEquivalenceRelations["T"] =         csound::isNormal<csound::EQUIVALENCE_RELATION_T>;
    isNormalsForEquivalenceRelations["Tg"] =        csound::isNormal<csound::EQUIVALENCE_RELATION_Tg>;
    isNormalsForEquivalenceRelations["I"] =         csound::isNormal<csound::EQUIVALENCE_RELATION_I>;
    isNormalsForEquivalenceRelations["RP"] =        csound::isNormal<csound::EQUIVALENCE_RELATION_RP>;
    isNormalsForEquivalenceRelations["RPT"] =       csound::isNormal<csound::EQUIVALENCE_RELATION_RPT>;
    isNormalsForEquivalenceRelations["RPTg"] =      csound::isNormal<csound::EQUIVALENCE_RELATION_RPTg>;
    isNormalsForEquivalenceRelations["RPI"] =       csound::isNormal<csound::EQUIVALENCE_RELATION_RPI>;
    isNormalsForEquivalenceRelations["RPTI"] =      csound::isNormal<csound::EQUIVALENCE_RELATION_RPTI>;
    isNormalsForEquivalenceRelations["RPTgI"] =     csound::isNormal<csound::EQUIVALENCE_RELATION_RPTgI>;
    isEquivalentsForEquivalenceRelations["R"] =     csound::isEquivalent<csound::EQUIVALENCE_RELATION_R>;
    isEquivalentsForEquivalenceRelations["P"] =     csound::isEquivalent<csound::EQUIVALENCE_RELATION_P>;
    isEquivalentsForEquivalenceRelations["T"] =     csound::isEquivalent<csound::EQUIVALENCE_RELATION_T>;
    isEquivalentsForEquivalenceRelations["Tg"] =    csound::isEquivalent<csound::EQUIVALENCE_RELATION_Tg>;
    isEquivalentsForEquivalenceRelations["I"] =     csound::isEquivalent<csound::EQUIVALENCE_RELATION_I>;
    isEquivalentsForEquivalenceRelations["RP"] =    csound::isEquivalent<csound::EQUIVALENCE_RELATION_RP>;
    isEquivalentsForEquivalenceRelations["RPT"] =   csound::isEquivalent<csound::EQUIVALENCE_RELATION_RPT>;
    isEquivalentsForEquivalenceRelations["RPTg"] =  csound::isEquivalent<csound::EQUIVALENCE_RELATION_RPTg>;
    isEquivalentsForEquivalenceRelations["RPI"] =   csound::isEquivalent<csound::EQUIVALENCE_RELATION_RPI>;
    isEquivalentsForEquivalenceRelations["RPTI"] =  csound::isEquivalent<csound::EQUIVALENCE_RELATION_RPTI>;
    isEquivalentsForEquivalenceRelations["RPTgI"] = csound::isEquivalent<csound::EQUIVALENCE_RELATION_RPTgI>;
    equivalenceRelationsForCompoundEquivalenceRelations["RP"] =      {"R", "P"};
    equivalenceRelationsForCompoundEquivalenceRelations["RPT"] =     {"R", "P", "T"}; // V?
    equivalenceRelationsForCompoundEquivalenceRelations["RPTg"] =    {"R", "P", "Tg"}; // V?
    equivalenceRelationsForCompoundEquivalenceRelations["RPI"] =     {"R", "P"};
    equivalenceRelationsForCompoundEquivalenceRelations["RPTgI"] =   {"RPTg", "RP", "R", "P", "Tg"}; // V?
    fundamentalDomainByNormalizesForEquivalenceRelations["R"] =        csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_R>;
    fundamentalDomainByNormalizesForEquivalenceRelations["P"] =        csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_P>;
    fundamentalDomainByNormalizesForEquivalenceRelations["T"] =        csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_T>;
    fundamentalDomainByNormalizesForEquivalenceRelations["Tg"] =       csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_Tg>;
    fundamentalDomainByNormalizesForEquivalenceRelations["I"] =        csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_I>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RP"] =       csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RP>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RPT"] =      csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPT>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RPTg"] =     csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPTg>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RPI"] =      csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPI>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RPTI"] =     csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPTI>;
    fundamentalDomainByNormalizesForEquivalenceRelations["RPTgI"] =    csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPTgI>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["R"] =           csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_R>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["P"] =           csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_P>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["T"] =           csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_T>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["Tg"] =          csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_Tg>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["I"] =           csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_I>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RP"] =          csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RP>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RPT"] =         csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPT>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RPTg"] =        csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTg>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RPI"] =         csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPI>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RPTI"] =        csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTI>;
    fundamentalDomainByIsNormalsForEquivalenceRelations["RPTgI"] =       csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTgI>;

    csound::Chord original;
    original.resize(3);
    original.setPitch(0, -8.0);
    original.setPitch(1,  4.0);
    original.setPitch(2,  4.0);

    // normals for T and equals for T are of course very very different, modify test for this.

    auto normalized = csound::normalize<csound::EQUIVALENCE_RELATION_R>(original, 24.0, 1.0);
    std::fprintf(stderr, "R:     original: %s  normalized: %s:\n", original.toString().c_str(), normalized.toString().c_str());

    original.setPitch(0,  3.0);
    original.setPitch(1,  4.0);
    original.setPitch(2,  4.0);
    testRPIStuff(original);

    original.setPitch(0, -5.0);
    original.setPitch(1, -2.0);
    original.setPitch(2,  7.0);
    testRPIStuff(original);

    original.setPitch(0, -7.0);
    original.setPitch(1,  3.0);
    original.setPitch(2,  5.0);
    testRPIStuff(original);

    original.setPitch(0, -7.0);
    original.setPitch(1,  3.0);
    original.setPitch(2,  3.0);
    testRPIStuff(original);

    original.setPitch(0, -7.0);
    original.setPitch(1,  3.0);
    original.setPitch(2,  5.0);
    testRPIStuff(original);

    original.setPitch(0, -5.0);
    original.setPitch(1,  2.0);
    original.setPitch(2,  7.0);
    testRPIStuff(original);

    original.setPitch(0, -3.0);
    original.setPitch(1,  7.0);
    original.setPitch(2,  7.0);
    testRPIStuff(original);

    original.setPitch(0, -5.0);
    original.setPitch(1,  5.0);
    original.setPitch(2,  7.0);
    testRPIStuff(original);

    original.setPitch(0, -8.0);
    original.setPitch(1,  4.0);
    original.setPitch(2,  4.0);
    testRPIStuff(original);

    original.setPitch(0, -5.0);
    original.setPitch(1,  5.0);
    original.setPitch(2,  5.0);
    testRPIStuff(original);

    original.setPitch(0, -4.0);
    original.setPitch(1,  1.0);
    original.setPitch(2,  3.0);
    testRPIStuff(original);

    original.setPitch(0, -3.0);
    original.setPitch(1, -1.0);
    original.setPitch(2,  4.0);
    testRPIStuff(original);

    original.setPitch(0, -3.0);
    original.setPitch(1, -1.0);
    original.setPitch(2,  4.0);
    testRPIStuff(original);

    original.setPitch(0,  0.0);
    original.setPitch(1,  0.0);
    original.setPitch(2,  1.0);
    testRPIStuff(original);

    original.setPitch(0,  0.0);
    original.setPitch(1,  1.0);
    original.setPitch(2,  1.0);
    testRPIStuff(original);

    original.setPitch(0,  0.0);
    original.setPitch(1,  0.0);
    original.setPitch(2,  1.0);
    testRPIStuff(original);

    original.setPitch(0, -3.0);
    original.setPitch(1,  3.0);
    original.setPitch(2,  9.0);
    testRPIStuff(original);

    original.setPitch(0, -1.0);
    original.setPitch(1, -1.0);
    original.setPitch(2, 12.0);
    testRPIStuff(original);

    auto index = csound::indexForOctavewiseRevoicing(original, 48.0, true);
    index = csound::indexForOctavewiseRevoicing(original.eOP(), 48.0, true);
    //int x;
    //std::cin >> x;

    std::fprintf(stderr, "\nBehavior of std::fmod and std::remainder:\n\n");
    for (double pitch = -24.0; pitch < 24.0; pitch += 1.0) {
        double modulusFmod = std::fmod(pitch, csound::OCTAVE());
        double modulusRemainder = std::remainder(pitch, csound::OCTAVE());
        double pc = csound::epc(pitch);
        double modulus = csound::modulo(pitch, csound::OCTAVE());
        std::fprintf(stderr, "Pitch: %9.4f  modulo: %9.4f  std::fmod: %9.4f  std::remainder: %9.4f  epc: %9.4f\n", pitch, modulus, modulusFmod, modulusRemainder, pc);
    }
    csound::Chord pcs = csound::chordForName("C major").epcs();
    std::fprintf(stderr, "Should be C major scale:\n%s\n", pcs.information().c_str());
    for (double pitch = 36.0; pitch < 96.0; pitch += 1.0) {
        double conformed = csound::conformToPitchClassSet(pitch, pcs);
        std::fprintf(stderr, "pitch: %9.4f  conformed: %9.4f\n", pitch, conformed);
    }
    std::cerr << "EPSILON: " << csound::EPSILON() << std::endl;
    std::cerr << "epsilonFactor: " << csound::epsilonFactor() << std::endl;
    std::cerr << "EPSILON * epsilonFactor: " << csound::EPSILON() * csound::epsilonFactor() << std::endl;
    std::cerr << "csound::eq_epsilon(0.15, 0.15): " << csound::eq_epsilon(0.15, 0.15) << std::endl;
    std::cerr << "csound::eq_epsilon(0.1500001, 0.15): " << csound::eq_epsilon(0.1500001, 0.15) << std::endl;
    std::cerr << "csound::gt_epsilon(14.0, 12.0): " << csound::gt_epsilon(14.0, 12.0) << std::endl;
    std::cerr << "csound::ge_epsilon(14.0, 12.0): " << csound::ge_epsilon(14.0, 12.0) << std::endl;
    csound::Chord chord;
    chord.resize(3);
    std::cerr << "Default chord: " << chord.toString() << std::endl;
    std::cerr << "chord.count(0.0): " << chord.count(0.0) << std::endl;
    csound::Chord other;
    other.resize(3);
    other.setPitch(1, 2);
    std::cerr << "Other chord: " << other.toString() << std::endl;
    std::cerr << "other.count(0.0): " << other.count(0.0) << std::endl;
    std::cerr << "(chord == other): " << (chord == other) << std::endl;
    std::cerr << "other.contains(2.0): " << other.contains(2.0) << std::endl;
    std::cerr << "other.contains(2.00000001): " << other.contains(2.00000001) << std::endl;
    std::vector<double> result = other.min();
    std::cerr << "other.min(): " << result[0] << ", " << result[1] << ", " << result.size() << std::endl;
    std::cerr << "other.minimumInterval(): " << other.minimumInterval() << std::endl;
    std::cerr << "other.maximumInterval(): " << other.maximumInterval() << std::endl;
    csound::Chord clone = other;
    std::cerr << "clone = other: " << clone.toString() << std::endl;
    clone.setPitch(1, .5);
    std::cerr << "clone: " << clone.toString() << std::endl;
    csound::Chord floor = clone.floor();
    std::cerr << "floor: " << floor.toString() << std::endl;
    csound::Chord ceiling = clone.ceiling();
    std::cerr << "ceiling: " << ceiling.toString() << std::endl;
    chord.setPitch(0, 1);
    chord.setPitch(1, 1);
    std::cerr << "chord: " << chord.toString() << std::endl;
    std::cerr << "chord.distanceToOrigin(): " << chord.distanceToOrigin() << std::endl;
    std::cerr << "chord.distanceToUnisonDiagonal(): " << chord.distanceToUnisonDiagonal() << std::endl;
    std::cerr << "chord.center(): " << chord.center().toString() << std::endl;
    std::cerr << "chord.T(3): " << chord.T(3).toString() << std::endl;
    std::cerr << "chord.I(): " << chord.I().toString() << std::endl;
    std::cerr << "csound::epc(13.2): " << csound::epc(13.2) << std::endl;
    std::cerr << "chord.isepcs(): " << chord.isepcs() << std::endl;
    chord.setPitch(2, 14);
    std::cerr << "chord: " << chord.toString() << std::endl;
    std::cerr << "chord.isepcs(): " << chord.isepcs() << std::endl;
    csound::Chord epcs = chord.epcs();
    std::cerr << "chord.epcs(): " << epcs.toString() << std::endl;
    std::cerr << "csound::epc(14.0): " << csound::epc(14.0) << std::endl;
    csound::Chord transposed = chord.T(5.0);
    std::cerr << "chord::T(5.0): " << transposed.toString() << std::endl;
    std::cerr << "transposed.iset(): " << transposed.iset() << std::endl;
    csound::Chord et = transposed.et();
    std::cerr << "et = transposed.et(): " << et.toString() << std::endl;
    std::cerr << "transposed.iseO(): " << transposed.iseO() << std::endl;
    csound::Chord eO = transposed.eO();
    std::cerr << "transposed.eO(): " << eO.toString() << std::endl;
    std::cerr << "eO.iseO(): " << eO.iseO() << std::endl;
    std::cerr << "eO.iseP(): " << eO.iseP() << std::endl;
    csound::Chord eP = eO.eP();
    std::cerr << "eP = eO.eP(): " << eP.toString() << std::endl;
    std::cerr << "eP.iseT(): " << eP.iseT() << std::endl;
    csound::Chord eT= eP.eT();
    std::cerr << "eT = eP.eT(): " << eT.toString() << std::endl;
    std::cerr << "eT.iseT(): " << eT.iseT() << std::endl;
    csound::Chord eTT= eP.eTT();
    std::cerr << "eTT = eP.eTT(): " << eTT.toString() << std::endl;
    std::cerr << "eTT.iseT(): " << eTT.iseTT() << std::endl;
    std::cerr << "eT.iseTT(): " << eT.iseTT() << std::endl;
    std::cerr << "eTT: " << eTT.toString() << std::endl;
    csound::Chord inverse = eTT.I();
    std::cerr << "csound::Chord inverse = eTT.I(): " << inverse.toString() << std::endl;
    csound::Chord inverseOfInverse = inverse.I();
    std::cerr << "csound::Chord inverseOfInverse = inverse.I(): " << inverseOfInverse.toString() << std::endl;
    std::cerr << "inverse.iseI(): " << inverse.iseI() << std::endl;
    csound::Chord eI = eTT.eI();
    std::cerr << "csound::Chord eI = eTT.eI(): " << eI.toString() << std::endl;
    std::cerr << "eI.iseI(): " << eI.iseI() << std::endl;
    std::cerr << "eTT.iseI(): " << eTT.iseI() << std::endl;
    std::cerr << "(inverse < eTT): " << (inverse < eTT) << std::endl;
    std::cerr << "(eTT < inverse): " << (eTT < inverse) << std::endl;
    std::cerr << "chord: " << chord.toString() << std::endl;
    std::cerr << "chord.cycle(): " << chord.cycle().toString() << std::endl;
    std::cerr << "chord.cycle(2): " << chord.cycle(2).toString() << std::endl;
    std::cerr << "chord.cycle().cycle(): " << chord.cycle().cycle().toString() << std::endl;
    std::cerr << "eI: " << eI.toString() << std::endl;
    std::cerr << "eI.cycle(-1): " << eI.cycle(-1).toString() << std::endl;
    std::vector<csound::Chord> permutations = chord.permutations();
    std::cerr << "Permutations, iseV, eV:" << std::endl;
    for (size_t i = 0; i < permutations.size(); i++) {
        const csound::Chord permutation = permutations[i];
        const csound::Chord &eV = permutation.eV();
        std::cerr << i << " of " << permutations.size() << ": " << permutation.toString() << "  iseV: " << permutation.iseV() << "  eV: " << eV.toString() << "  iseP: " <<  permutation.iseP() << std::endl;
    }
    std::vector<csound::Chord> voicings = chord.voicings();
    std::cerr << "voicing, iseV, eV:" << std::endl;
    for (size_t i = 0; i < voicings.size(); i++) {
        const csound::Chord voicing = voicings[i];
        const csound::Chord &eV = voicing.eV();
        std::cerr << i << " of " << voicings.size() << ": " << voicing.toString() << "  iseV: " << voicing.iseV() << "  eV: " << eV.toString() << std::endl;
    }
    std::string tosplit = "C     D     E        G           B";
    //auto prior_level = csound::System::setMessageLevel(15);
    // Must be 'false' because this is a chord not a scale, and if it were a 
    // scale, infinite looping because tosplit is not in order.
    csound::fill("C", 0., "M9", tosplit, false);
    //csound::System::setMessageLevel(prior_level);
    csound::Chord C7 = csound::chordForName("C7");
    std::cerr << "Should be C7:" << std::endl << C7.information().c_str() << std::endl;
    csound::Chord G7 = csound::chordForName("G7");
    std::cerr << "Should be G7:" << std::endl << G7.information().c_str() << std::endl;
    csound::Chord CM9 = csound::chordForName("CM9");
    std::cerr << "Should be CM9:" << std::endl << CM9.information().c_str() << std::endl;
    CM9.setPitch(0, 0.);
    CM9.setPitch(1, 4.);
    CM9.setPitch(2, 7.);
    CM9.setPitch(3,-1.);
    CM9.setPitch(4, 2.);
    std::cerr << "Should be CM9:" << std::endl << CM9.information().c_str() << std::endl;
    csound::Chord Dm9 = csound::chordForName("Dm9");
    std::cerr << "Should be Dm9:" << std::endl << Dm9.information().c_str() << std::endl;
    Dm9.setPitch(0, 2.);
    Dm9.setPitch(1, 5.);
    Dm9.setPitch(2, 9.);
    Dm9.setPitch(3, 0.);
    Dm9.setPitch(4, 4.);
    std::cerr << "Should be Dm9:" << std::endl << Dm9.information().c_str() << std::endl;
    csound::Chord chordForName_ = csound::chordForName("CM9");
    std::fprintf(stderr, "chordForName(%s): %s\n", "CM9", chordForName_.information().c_str());
    std::fprintf(stderr, "\nTesting all equivalence relations...\n\n");
    for (int voiceCount = 3; voiceCount <= maximumVoiceCountToTest; ++voiceCount) {
        testEquivalenceRelations(voiceCount, csound::OCTAVE(), 1.0);
    }
    auto optgiByNormalize = csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPTgI>(4, 12.0, 1.0);
    printSet("optgiByNormalize", optgiByNormalize);
    auto optgiByIsNormal = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTgI>(4, 12.0, 1.0);
    printSet("optgiByIsNormal", optgiByIsNormal);
    if (optgiByNormalize == optgiByIsNormal) {
        std::fprintf(stderr, "optgiByNormalize == optgiByIsNormal\n");
    } else {
        std::fprintf(stderr, "optgiByNormalize != optgiByIsNormal\n");
    }
    
    //csound::Chord original;
    csound::Chord reflected;
    csound::Chord spun_back;
    
    std::cout << "HYPERPLANE EQUATIONS FOR DIMENSIONS" << std::endl;
    for (int i = 3; i < 11 ; ++i) {
        auto hpd = csound::hyperplane_equation_from_dimensionality(i);
        //~ auto hpc = csound::hyperplane_equation_from_centroids(i);
    }

#if 0
    csound::ChordSpaceGroup chordSpaceGroup;
    chordSpaceGroup.createChordSpaceGroup(4, csound::OCTAVE() * 5.0, 1.0);
    chordSpaceGroup.list(true, true, true);
    testChordSpaceGroup(chordSpaceGroup, "Gb7");
    std::fprintf(stderr, "\nTesting all of chord space groups...\n\n");
    testAllOfChordSpaceGroup(3, maximumVoiceCountToTest);
#endif     

    csound::Chord c1({-7, 2, 5});
    std::cout << c1.information() << std::endl;
    csound::Chord c2({-5, -2, 7});
    std::cout << c2.information() << std::endl;
    
    test_chord_types();
    
    original = csound::chordForName({0, 4, 7});
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    csound::HyperplaneEquation hyperplane_equation = csound::get_hyperplane_equation(3);
    reflected = csound::reflect_by_householder(original, hyperplane_equation.unit_normal_vector);
    std::cout << "reflect_by_householder:" << std::endl;
    std::cout << reflected.information() << std::endl;
    reflected = reflect_in_inversion_flat(original);
    std::cout << "reflect_in_inversion_flat:" << std::endl;
    std::cout << reflected.information() << std::endl;
    spun_back = reflected.eOPTT();
    std::cout << "spun_back:" << std::endl;
    std::cout << spun_back.information() << std::endl;

    original = csound::chordForName("C7");
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    reflected = reflect_in_inversion_flat(original);
    std::cout << "reflected:" << std::endl;
    std::cout << reflected.information() << std::endl;
    spun_back = reflected.eOPTT();
    std::cout << "spun_back:" << std::endl;
    std::cout << spun_back.information() << std::endl;    
    
    std::fprintf(stderr, "\nFINISHED.\n\n");
    return 0;
}

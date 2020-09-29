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
        csound::System::message("\nPASSED (passed: %-9d failed: %-9d of %9d): %s\n", passCount, failureCount, testCount, message.c_str());
    }
}

static void fail(std::string message) {
    failureCount = failureCount + 1;
    testCount = passCount + failureCount;
    csound::System::message("================================================================================================================\n");
    csound::System::message("FAILED (passed: %-9d failed: %-9d of %d): %s", passCount, failureCount, testCount, message.c_str());
    csound::System::message("================================================================================================================\n");
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
        csound::System::message("%s %5d: %s\n", name.c_str(), i, it->toString().c_str());
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
    csound::HyperplaneEquation actual = hyperplane_equation_by_singular_value_decomposition(points, false);
    bool passes = equals(expected, actual);
    test(passes, __func__);    
}

//~ /**
 //~ * The idea here is that in a given fundamental domain of OPT, the centroid of 
 //~ * the OPTI subset should be perfectly reflected as the centroid of the ~OPTI 
 //~ * subset, and the vector from one centroid to the other should be normal to 
 //~ * the inversion flat.
 //~ */
//~ namespace csound {
    
//~ static HyperplaneEquation hyperplane_equation_from_centroids(int dimensions) {
    //~ auto opts = fundamentalDomainByIsNormal<EQUIVALENCE_RELATION_RPT>(dimensions, OCTAVE(), 1.);
    //~ Chord opti_centroid(dimensions);
    //~ Chord not_opti_centroid(dimensions);
    //~ Chord center = opti_centroid.center();
    //~ int opti_count = 0;
    //~ int not_opti_count = 0;
    //~ for (auto opt : opts) {
        //~ if (opt.iseOPTI() == true) {
            //~ opti_count = opti_count + 1;
            //~ for (int voice = 0; voice < dimensions; ++voice) {
                //~ auto sum = opti_centroid.getPitch(voice) + opt.getPitch(voice);
                //~ opti_centroid.setPitch(voice, sum);
            //~ }
        //~ } else {
            //~ not_opti_count = not_opti_count + 1;
            //~ for (int voice = 0; voice < dimensions; ++voice) {
                //~ auto sum = not_opti_centroid.getPitch(voice) + opt.getPitch(voice);
                //~ not_opti_centroid.setPitch(voice, sum);
            //~ }
        //~ }
    //~ }
    //~ for (int voice = 0; voice < dimensions; ++voice) {
        //~ opti_centroid.setPitch(voice, opti_centroid.getPitch(voice) / opti_count);
        //~ not_opti_centroid.setPitch(voice, not_opti_centroid.getPitch(voice) / not_opti_count);
    //~ }
    //~ auto normal_vector = voiceleading(opti_centroid, not_opti_centroid);
    //~ auto norm = normal_vector.col(0).norm();
    //~ HyperplaneEquation hyperplane_equation_;
    //~ hyperplane_equation_.unit_normal_vector = normal_vector.col(0) / norm;
    //~ auto temp = center.col(0).adjoint() * hyperplane_equation_.unit_normal_vector;    
    //~ hyperplane_equation_.constant_term = temp(0, 0);
    //~ csound::System::message("hyperplane_equation_from_centroids: center:\n");
    //~ for(int i = 0; i < dimensions; i++) {
        //~ csound::System::message("  %9.4f\n", center.getPitch(i));
    //~ }
    //~ csound::System::message("hyperplane_equation_from_centroids: unit_normal_vector:\n");
    //~ for(int i = 0; i < dimensions; i++) {
        //~ csound::System::message("  %9.4f\n", hyperplane_equation_.unit_normal_vector(i, 0));
    //~ }
    //~ csound::System::message("hyperplane_equation_from_centroids: constant_term: %9.4f\n", hyperplane_equation_.constant_term);
    //~ return hyperplane_equation_;
//~ }

//~ };

static bool test_chord_type(int dimensions) {
    bool passes = true;
    auto ops = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RP>(dimensions, csound::OCTAVE(), 1.);
    for (auto op : ops) {
        auto opt = op.eOPTT();
        auto chord_type_ = opt.chord_type();
        auto tt_of_chord_type = chord_type_.eTT();
        if (false) {
            csound::System::message("test_chord_types:\n  OP:               %s %s\n  OPT:              %s\n  chord_type:       %s\n  TT of chord_type: %s\n", 
                op.toString().c_str(), op.name().c_str(),
                opt.toString().c_str(),
                chord_type_.toString().c_str(),
                tt_of_chord_type.toString().c_str());
        }
        if (opt.equals(tt_of_chord_type) == false) {
            csound::System::message(">> Oops! OPT of %s != TT of chord type %s\n", opt.toString().c_str(), tt_of_chord_type.toString().c_str());
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

static void test_chord_space_group(const csound::ChordSpaceGroup &chordSpaceGroup, std::string chordName) {
    csound::System::message("BEGAN test ChordSpaceGroup for %s...\n", chordName.c_str());
    csound::Chord originalChord = csound::chordForName(chordName);
    csound::Chord optti = originalChord.eOPTTI();
    csound::System::message("Original chord:\n%s\n", originalChord.information().c_str());
    Eigen::VectorXi pitv = chordSpaceGroup.fromChord(originalChord, true);
    csound::Chord reconstitutedChord = chordSpaceGroup.toChord(pitv[0], pitv[1], pitv[2], pitv[3], true)[0];
    csound::System::message("Reconstituted chord:\n%s\n", reconstitutedChord.information().c_str());
    test(originalChord == reconstitutedChord, "Reconstituted chord must be the same as the original chord.\n");
    csound::Chord revoicedOriginalChord = originalChord;
    revoicedOriginalChord.setPitch(1,  revoicedOriginalChord.getPitch(1) + 12.);
    revoicedOriginalChord.setPitch(2,  revoicedOriginalChord.getPitch(2) + 24.);
    csound::System::message("Revoiced original chord:\n%s\n", revoicedOriginalChord.information().c_str());
    pitv = chordSpaceGroup.fromChord(revoicedOriginalChord, true);
    csound::Chord reconstitutedRevoicedChord = chordSpaceGroup.toChord_vector(pitv, true)[0];
    csound::System::message("Reconstituted revoiced chord:\n%s\n", reconstitutedRevoicedChord.information().c_str());
    test(revoicedOriginalChord == reconstitutedRevoicedChord, "Reconstituted revoiced chord must be the same as the original revoiced chord.\n");
    csound::Chord invertedChord = originalChord.I().eOP();
    csound::System::message("Inverted original chord:\n%s\n", invertedChord.information().c_str());
    pitv = chordSpaceGroup.fromChord(invertedChord);
    csound::Chord reconstitutedInvertedChord = chordSpaceGroup.toChord_vector(pitv, true)[0];
    csound::System::message("Reconstituted inverted chord:\n%s\n", reconstitutedInvertedChord.information().c_str());
    test(invertedChord == reconstitutedInvertedChord,"Reconstituted inverted chord must be the same as the original inverted chord.\n");
    csound::System::message("ENDED test ChordSpaceGroup for %s.\n", chordName.c_str());
    csound::System::message("\n");
}

static void test_chord_space_group(int initialVoiceCount, int finalVoiceCount) {
    double range = 48.0;
    for (int voiceCount = initialVoiceCount; voiceCount <= finalVoiceCount; ++voiceCount) {
        bool passes = true;
        csound::ChordSpaceGroup chordSpaceGroup;
        csound::System::message("Testing all of ChordSpaceGroup: voices: %d  range: %f\n", voiceCount, range);
        csound::System::message("Testing ChordSpaceGroup to chords and back...\n");
        chordSpaceGroup.initialize(voiceCount, range);
        chordSpaceGroup.list(true, true, true);
        for (int P = 0; P < chordSpaceGroup.countP; ++P) {
            for (int I = 0; I < chordSpaceGroup.countI; ++I) {
                for (int T = 0; T < chordSpaceGroup.countT; ++T) {
                    for (int V = 0; V < chordSpaceGroup.countV; ++V) {
                        csound::Chord fromPITV = chordSpaceGroup.toChord(P, I, T, V)[0];
                        if (printPass) csound::System::message("%8d     %8d     %8d     %8d => %s\n", P, I, T, V, fromPITV.toString().c_str());
                        Eigen::VectorXi pitv = chordSpaceGroup.fromChord(fromPITV);
                        csound::Chord frompitv = chordSpaceGroup.toChord(pitv(0), pitv(1), pitv(2), pitv(3))[0];
                        if (printPass) csound::System::message("%8d     %8d     %8d     %8d <= %s\n",pitv(0), pitv(1), pitv(2), pitv(3), frompitv.toString().c_str());
                        bool equals = (fromPITV == frompitv);
                        if (!equals) {
                            chordSpaceGroup.toChord(P, I, T, V, true);
                            csound::System::message("fromPITV (toChord):\n%s\n", fromPITV.information().c_str());
                            chordSpaceGroup.fromChord(fromPITV, true);
                            csound::System::message("frompitv (fromChord):\n%s\n", frompitv.information().c_str());
                            passes = false;
                        }
                        test(equals, "fromPITV must match frompitv.\n");
                        if (printPass) csound::System::message("\n");
                    }
                }
            }
        }
        csound::System::message("Testing chords to ChordSpaceGroup and back...\n\n");
        bool passes2 = true;
        auto eops = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RP>(voiceCount, csound::OCTAVE(), 1.0);
        for(auto it = eops.begin(); it != eops.end(); ++it) {
            auto chord = *it;
            auto origin = chord;
            for(;;) {
                Eigen::VectorXi pitv = chordSpaceGroup.fromChord(chord);
                if (printPass) csound::System::message("%8d     %8d     %8d     %8d <= %s\n", pitv(0), pitv(1), pitv(2), pitv(3), chord.toString().c_str());
                csound::Chord fromPITV = chordSpaceGroup.toChord(pitv(0), pitv(1), pitv(2), pitv(3))[0];
                if (printPass) csound::System::message("%8d     %8d     %8d     %8d => %s\n", pitv(0), pitv(1), pitv(2), pitv(3), fromPITV.toString().c_str());
                bool equals = (fromPITV == chord);
                if (!equals) {
                    csound::System::message("Original chord (fromChord):\n%s\n", chord.information().c_str());
                    csound::System::message("New chord (toChord):\n%s\n", fromPITV.information().c_str());
                    passes2 = false;
                }
                test(equals, "Original chord must match chord from original chord's PITV.\n");
                if (printPass) csound::System::message("\n");
                // This was going too far... cut off sooner and all seems well.
                if (csound::next(chord, origin, range - 1.0, csound::OCTAVE()) == false)
                {
                    break;
                }
            }
        }
    }
}

std::vector<std::string> equivalenceRelationsToTest = {"RP", "RPT", "RPTg", "RPTI", "RPTgI"};
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
    csound::System::message("\nequivalence: %s  normalized: %ld  is_normal: %ld  range: %f  g: %f\n", equivalence.c_str(), made_equivalents.size(), found_equivalents.size(), range, g);
    auto make_equivalent = normalizesForEquivalenceRelations[equivalence];
    auto isEquivalent = isEquivalentsForEquivalenceRelations[equivalence];
    bool passes1 = true;
    for (auto made_equivalent = made_equivalents.begin(); made_equivalent != made_equivalents.end(); ++made_equivalent) {
        if (is_equivalent(*made_equivalent, range, g) == false) {
            passes1 = false;
            csound::System::message("testNormalsAndEquivalents: %s range %f g %f: 'made_equivalents' %s is not 'is_equivalent'.\n",
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
        ///csound::System::message(">>> %s 'found_equivalents:' %s.\n", equivalence.c_str(), found_equivalent->toString().c_str());
        bool equivalenceFound = false;
        for (auto made_equivalent = made_equivalents.begin(); made_equivalent != made_equivalents.end(); ++made_equivalent) {
            if (isEquivalent(*found_equivalent, *made_equivalent, range, g) == true) {
                equivalenceFound = true;
                ///csound::System::message("    %s 'made_equivalents:'  %s.\n", equivalence.c_str(), made_equivalent->toString().c_str());
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
    //~ std::sprintf(buffer, "%-8s 'made_equivalents': ", equivalenceRelation.c_str());
    //~ printSet(buffer, normalsForEquivalenceRelation);
    auto equivalentsForEquivalenceRelation = fundamentalDomainByIsNormalsForEquivalenceRelations[equivalenceRelation](voiceCount, range, g);
    //~ std::sprintf(buffer, "%-8s 'found_equivalents':", equivalenceRelation.c_str());
    //~ printSet(buffer, equivalentsForEquivalenceRelation);
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
                csound::System::message("%-8s 'found_equivalents' size should be 19 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
            }
            passes = false;
        }
        if (voiceCount == 4) {
            if (equivalentsForEquivalenceRelation.size() != 83) {
                csound::System::message("%-8s 'found_equivalents' size should be 83 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
             }
            passes = false;    
        }
    }
    return passes;
}

static bool testEquivalenceRelations(int voiceCount, double range, double g) {
    bool passes = true;
    csound::System::message("\nTesting equivalence relations for %d voices over range %f with g %f...\n\n", voiceCount, range, g);
    for (auto equivalenceRelationI = equivalenceRelationsToTest.begin();
            equivalenceRelationI != equivalenceRelationsToTest.end();
            ++equivalenceRelationI) {
        if (!testEquivalenceRelation(*equivalenceRelationI, voiceCount, range, g)) {
            passes = false;
        }
    }
    return passes;
}

//~ void printRPIStuff(const csound::Chord &chord)
//~ {
    //~ csound::System::message("%s\n", chord.information().c_str());
//~ }

//~ void testRPIStuff(const csound::Chord &chord)
//~ {
    //~ csound::System::message("\nTESTING RPI STUFF\n");
    //~ csound::System::message("chord...\n");
    //~ printRPIStuff(chord);
    //~ auto chordRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(chord, 12.0, 1.0);
    //~ auto inverse = chord.I().eP();
    //~ csound::System::message("inverse...\n");
    //~ printRPIStuff(inverse);
    //~ auto inverseRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(inverse, 12.0, 1.0);
    //~ csound::System::message("inverseRP...\n");
    //~ printRPIStuff(inverseRP);
    //~ auto midpoint = csound::midpoint(chord.eP(), inverseRP.eP());
    //~ csound::System::message("midpoint of chord and inverseRP: %s\n", midpoint.eOP().toString().c_str());
    //~ csound::System::message("inverse of midpoint:             %s\n", midpoint.I().eOP().toString().c_str());
    //~ auto inverseInverseRP = inverseRP.I();
    //~ csound::System::message("inverseInverseRP...\n");
    //~ printRPIStuff(inverseInverseRP);
    //~ auto inverseRPinverseRP = csound::normalize<csound::EQUIVALENCE_RELATION_RP>(inverseInverseRP, 12.0, 1.0);
    //~ csound::System::message("inverseRPinverseRP...\n");
    //~ printRPIStuff(inverseRPinverseRP);
    //~ test(inverseRPinverseRP == chordRP, "chordRP must equal inverseRPinverseRP.");
//~ }

void printchord(const char *label, const csound::Chord &chord) {
    auto iseo = " ";
    if (chord.iseO()) {
        iseo = "O";
    }
    auto isep = " ";
    if (chord.iseP()) {
        isep = "P";
    }
    auto iset = " ";
    if (chord.iseT()) {
        iset = "T";
    }
    auto isev = " ";
    if (chord.iseV()) {
        isev = "V";
    }
    auto iseopt = "   ";
    if (chord.iseOPT()) {
        iseopt = "OPT";
    }
    auto iseopti = "    ";
    if (chord.iseOPTI()) {
        iseopti = "OPTI";
    }
    fprintf(stderr, "%s: %s %s %s %s %s %s %s\n", label, chord.toString().c_str(), iseo, isep, iset, isev, iseopt, iseopti);  
}


int main(int argc, char **argv) {
    csound::System::message("C H O R D S P A C E   U N I T   T E S T S\n\n");
    Hyperplane_Equation_for_Test_Points();
#if defined(USE_OLD_EQUIVALENCES)
    csound::System::message("Using OLD implementation of equivalence relations.\n\n");
#else
    csound::System::message("Using NEW implementation of equivalence relations.\n\n");
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
    csound::System::message("R:     original: %s  normalized: %s:\n", original.toString().c_str(), normalized.toString().c_str());

    //~ original.setPitch(0,  3.0);
    //~ original.setPitch(1,  4.0);
    //~ original.setPitch(2,  4.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -5.0);
    //~ original.setPitch(1, -2.0);
    //~ original.setPitch(2,  7.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -7.0);
    //~ original.setPitch(1,  3.0);
    //~ original.setPitch(2,  5.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -7.0);
    //~ original.setPitch(1,  3.0);
    //~ original.setPitch(2,  3.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -7.0);
    //~ original.setPitch(1,  3.0);
    //~ original.setPitch(2,  5.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -5.0);
    //~ original.setPitch(1,  2.0);
    //~ original.setPitch(2,  7.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -3.0);
    //~ original.setPitch(1,  7.0);
    //~ original.setPitch(2,  7.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -5.0);
    //~ original.setPitch(1,  5.0);
    //~ original.setPitch(2,  7.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -8.0);
    //~ original.setPitch(1,  4.0);
    //~ original.setPitch(2,  4.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -5.0);
    //~ original.setPitch(1,  5.0);
    //~ original.setPitch(2,  5.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -4.0);
    //~ original.setPitch(1,  1.0);
    //~ original.setPitch(2,  3.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -3.0);
    //~ original.setPitch(1, -1.0);
    //~ original.setPitch(2,  4.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -3.0);
    //~ original.setPitch(1, -1.0);
    //~ original.setPitch(2,  4.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0,  0.0);
    //~ original.setPitch(1,  0.0);
    //~ original.setPitch(2,  1.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0,  0.0);
    //~ original.setPitch(1,  1.0);
    //~ original.setPitch(2,  1.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0,  0.0);
    //~ original.setPitch(1,  0.0);
    //~ original.setPitch(2,  1.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -3.0);
    //~ original.setPitch(1,  3.0);
    //~ original.setPitch(2,  9.0);
    //~ testRPIStuff(original);

    //~ original.setPitch(0, -1.0);
    //~ original.setPitch(1, -1.0);
    //~ original.setPitch(2, 12.0);
    //~ testRPIStuff(original);

    auto index = csound::indexForOctavewiseRevoicing(original, 48.0, true);
    index = csound::indexForOctavewiseRevoicing(original.eOP(), 48.0, true);
    //int x;
    //std::cin >> x;

    csound::System::message("\nBehavior of std::fmod and std::remainder:\n\n");
    for (double pitch = -24.0; pitch < 24.0; pitch += 1.0) {
        double modulusFmod = std::fmod(pitch, csound::OCTAVE());
        double modulusRemainder = std::remainder(pitch, csound::OCTAVE());
        double pc = csound::epc(pitch);
        double modulus = csound::modulo(pitch, csound::OCTAVE());
        csound::System::message("Pitch: %9.4f  modulo: %9.4f  std::fmod: %9.4f  std::remainder: %9.4f  epc: %9.4f\n", pitch, modulus, modulusFmod, modulusRemainder, pc);
    }
    csound::Chord pcs = csound::chordForName("C major").epcs();
    csound::System::message("Should be C major scale:\n%s\n", pcs.information().c_str());
    for (double pitch = 36.0; pitch < 96.0; pitch += 1.0) {
        double conformed = csound::conformToPitchClassSet(pitch, pcs);
        csound::System::message("pitch: %9.4f  conformed: %9.4f\n", pitch, conformed);
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
    csound::System::message("chordForName(%s): %s\n", "CM9", chordForName_.information().c_str());
    
    csound::System::message("\nTesting equivalence relations...\n\n");
    for (int voiceCount = 3; voiceCount <= maximumVoiceCountToTest; ++voiceCount) {
        testEquivalenceRelations(voiceCount, csound::OCTAVE(), 1.0);
    }
    
    //~ auto optgiByNormalize = csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPTgI>(4, 12.0, 1.0);
    //~ printSet("optgiByNormalize", optgiByNormalize);
    //~ auto optgiByIsNormal = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTgI>(4, 12.0, 1.0);
    //~ printSet("optgiByIsNormal", optgiByIsNormal);
    //~ if (optgiByNormalize == optgiByIsNormal) {
        //~ csound::System::message("optgiByNormalize == optgiByIsNormal\n");
    //~ } else {
        //~ csound::System::message("optgiByNormalize != optgiByIsNormal\n");
    //~ }
    
    //csound::Chord original;
    csound::Chord reflected;
    csound::Chord spun_back;
    
    std::cout << "HYPERPLANE EQUATIONS FOR DIMENSIONS\n" << std::endl;
    for (int i = 3; i < 6 ; ++i) {
        std::cerr << "\nDIMENSIONALITY\n" << std::endl;
        auto hpd = csound::hyperplane_equation_from_dimensionality(i, true);
        //~ std::cerr << "\nCENTROIDS\n" << std::endl;
        //~ auto hpc = csound::hyperplane_equation_from_centroids(i);
        std::cerr << "\nRANDOM INVERSION FLAT\n" << std::endl;
        auto hpr = csound::hyperplane_equation_from_random_inversion_flat(i);
    }

#if 0
    csound::ChordSpaceGroup chordSpaceGroup;
    chordSpaceGroup.createChordSpaceGroup(4, csound::OCTAVE() * 5.0, 1.0);
    chordSpaceGroup.list(true, true, true);
    testChordSpaceGroup(chordSpaceGroup, "Gb7");
    csound::System::message("\nTesting all of chord space groups...\n\n");
    testAllOfChordSpaceGroup(3, maximumVoiceCountToTest);

    csound::Chord c1({-7, 2, 5});
    std::cout << c1.information() << std::endl;
    csound::Chord c2({-5, -2, 7});
    std::cout << c2.information() << std::endl;
    
    test_chord_types();
    
    auto prior_level = csound::System::setMessageLevel(15);
    
    original = csound::Chord({0, 3, 7}).eOPT();
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    reflected = csound::reflect_by_householder(original);
    std::cout << "reflect_by_householder:" << std::endl;
    std::cout << reflected.information() << std::endl;
    reflected = reflect_in_inversion_flat(original);
    std::cout << "reflect_in_inversion_flat:" << std::endl;
    std::cout << reflected.information() << std::endl;
    spun_back = reflected.eOPT();
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
#endif     

 
    //csound::hyperplane_equation_from_dimensionality(3, true, 2);
    
    csound::System::message("\n");

    auto opts = csound::fundamentalDomainByNormalize<csound::EQUIVALENCE_RELATION_RPT>(3, 12., 1.);
    csound::System::message("\n");
    auto index_ = 0;
    for (auto opt : opts) {
        csound::System::message("unison %d\n", index_);
        index_ = index_ + 1;
        printchord("opt      ", opt);
        auto opt_i = csound::reflect_in_unison_diagonal(opt);
        printchord("opt_i    ", opt_i);
        auto opt_i_opt = opt_i.eOPT();
        printchord("opt_i_opt", opt_i);
        csound::System::message("\n");
    }
    
    index_ = 0;
    for (auto opt : opts) {
        csound::System::message("center %d\n", index_);
        index_ = index_ + 1;
        printchord("opt      ", opt);
        auto opt_i = csound::reflect_in_center(opt);
        printchord("opt_i    ", opt_i);
        auto opt_i_opt = opt_i.eOPT();
        printchord("opt_i_opt", opt_i);
        csound::System::message("\n");
    }
    
    index_ = 0;
    for (auto opt : opts) {
        csound::System::message("flat %d\n", index_);
        index_ = index_ + 1;
        printchord("opt      ", opt);
        auto opt_i = csound::reflect_in_inversion_flat(opt);
        printchord("opt_i    ", opt_i);
        auto opt_i_opt = opt_i.eOPT();
        printchord("opt_i_opt", opt_i);
        csound::System::message("\n");
    }

    index_ = 0;
    for (auto opt : opts) {
        csound::System::message("house %d\n", index_);
        index_ = index_ + 1;
        printchord("opt      ", opt);
        auto opt_i = csound::reflect_by_householder(opt);
        printchord("opt_i    ", opt_i);
        auto opt_i_opt = opt_i.eOPT();
        printchord("opt_i_opt", opt_i);
        csound::System::message("\n");
    }


    return 0;
}

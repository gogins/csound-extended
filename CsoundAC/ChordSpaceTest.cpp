#include "ChordSpace.hpp"
#include <System.hpp>
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <string>


#pragma GCC diagnostic ignored "-Wformat"

typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Matrix;
typedef Eigen::Matrix<double, Eigen::Dynamic, 1> Vector;

static bool printPass = true;
static bool failureExits = false;
static int passCount = 0;
static int failureCount = 0;
static int testCount = 0;
static int exitAfterFailureCount = 1;

static bool pass(std::string message) {
    passCount = passCount + 1;
    testCount = passCount + failureCount;
    if (printPass) {
        csound::System::message("\nPASSED (passed: %-9d failed: %-9d of %9d): %s\n", passCount, failureCount, testCount, message.c_str());
    }
    return true;
}

static bool fail(std::string message) {
    failureCount = failureCount + 1;
    testCount = passCount + failureCount;
    csound::System::message("================================================================================================================\n");
    csound::System::message("FAILED (passed: %-9d failed: %-9d of %d): %s", passCount, failureCount, testCount, message.c_str());
    csound::System::message("================================================================================================================\n");
    if (failureExits && (failureCount >= exitAfterFailureCount)) {
        std::exit(-1);
    }
    return false;
}

static void summary() {
    testCount = passCount + failureCount;
    std::time_t time_ = std::time(nullptr);
    auto datetime = std::asctime(std::localtime(&time_));
    csound::System::message("\n================================================================================================================\n");
    csound::System::message("SUMMARY  Passed: %-9d  Failed: %-9d  Total: %d  Completed: %s", passCount, failureCount, testCount, datetime); 
    csound::System::message("================================================================================================================\n");
}

static bool test(bool passes, std::string message) {
    if (passes) {
        pass(message);
    } else {
        fail(message);
    }
    return passes;
}

static void printSet(std::string name, const std::vector<csound::Chord> &chords) {
    csound::System::message("%s\n", name.c_str());
    std::multimap<csound::Chord, csound::Chord> sorted;
    for (auto &e : chords) {
        sorted.insert({e.normal_form(), e});
    }
    int i = 1;
    for (auto &value : sorted) {
        auto &c = value.second;
        csound::System::message("%s  chord[%04d]: %s  OPTT: %s  OPTTI: %s opti_sector: ", c.normal_form().toString().c_str(), i, c.toString().c_str(), c.eOPTT().toString().c_str(), c.eOPTTI().toString().c_str());
        auto opti_sectors_ = c.opti_domain_sector();
        for (auto opti_sector : opti_sectors_) {
            csound::System::message("%2d (%4.1f)", opti_sector, opti_sector / 2.);
        }
        csound::System::message("\n");
        i = i + 1;
    }
}

static bool equals(const csound::HyperplaneEquation &a, const csound::HyperplaneEquation &b) {
    if (a.unit_normal_vector.rows() == b.unit_normal_vector.rows() == false) {
        csound::System::error("equals: size mismatch: %d %d\n", a.unit_normal_vector.rows(), b.unit_normal_vector.rows());
        return false;
    }
    for (int row = 0; row < a.unit_normal_vector.rows(); ++row) {
        if (csound::eq_tolerance(a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0)) == false) {
            csound::System::error("equals: unit normal vector element mismatch: %.17g %.17g\n", a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0));
            return false;
        }
    }
    if (csound::eq_tolerance(a.constant_term, b.constant_term) == false) {
        csound::System::error("equals: constant term mismatch: %.17g %,17g\n", a.constant_term, b.constant_term);
        return false;
    }
    return true;
}

static void Hyperplane_Equation_for_Test_Points() {
    std::vector<csound::Chord> points;
    points.push_back(csound::Chord(std::vector<double>({ 4.,  0., -1.,  0.})));
    points.push_back(csound::Chord(std::vector<double>({ 1.,  2.,  3., -1.})));
    points.push_back(csound::Chord(std::vector<double>({ 0., -1.,  2.,  0.})));
    points.push_back(csound::Chord(std::vector<double>({-1.,  1., -1.,  1.})));
    // From inversion_flats.py for these points by SVD from vectors:
    csound::HyperplaneEquation expected;
    expected.unit_normal_vector.resize(4, 1);
    expected.unit_normal_vector << 0.20864865369890548, 0.12839917150701868, 0.32099792876754685, 0.9148440969875088;
    expected.constant_term = 0.5135966860280752;
    csound::HyperplaneEquation actual = hyperplane_equation_from_singular_value_decomposition(points, false);
    bool passes = equals(expected, actual);
    test(passes, __func__);    
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
typedef csound::Chord(*normalize_t)(const csound::Chord &, double, double, int);
typedef bool (*isNormal_t)(const csound::Chord &, double, double, int);
typedef std::vector<csound::Chord> (*fundamentalDomainByNormalize_t)(int, double, double);
typedef std::vector<csound::Chord> (*fundamentalDomainByIsNormal_t)(int, double, double);
std::map<std::string, normalize_t> normalizesForEquivalenceRelations;
std::map<std::string, isNormal_t> isNormalsForEquivalenceRelations;
std::map<std::string, std::set<std::string> > equivalenceRelationsForCompoundEquivalenceRelations;
std::map<std::string, fundamentalDomainByNormalize_t> fundamentalDomainByNormalizesForEquivalenceRelations;
std::map<std::string, fundamentalDomainByIsNormal_t> fundamentalDomainByIsNormalsForEquivalenceRelations;

static bool testNormalsAndEquivalents(std::string equivalence,
                                      std::vector<csound::Chord> &made_equivalents,
                                      std::vector<csound::Chord> &found_equivalents,
                                      double range,
                                      double g) {
    char buffer[0x200];
    auto is_equivalent = isNormalsForEquivalenceRelations[equivalence];
    csound::System::message("\nequivalence: %s  normalized: %ld  is_normal: %ld  range: %f  g: %f\n", equivalence.c_str(), made_equivalents.size(), found_equivalents.size(), range, g);
    auto make_equivalent = normalizesForEquivalenceRelations[equivalence];
    bool passes1 = true;
    int count = 1;
    for (auto made_equivalent = made_equivalents.begin(); made_equivalent != made_equivalents.end(); ++made_equivalent) {
        std::sprintf(buffer, "MADE EQUIVALENT %d\n", count);
        test(made_equivalent->test(), std::string(buffer));
        count = count + 1;
    }
    count = 1;
    for (auto found_equivalent = found_equivalents.begin(); found_equivalent != found_equivalents.end(); ++found_equivalent) {
        std::sprintf(buffer, "FOUND EQUIVALENT %d\n", count);
        test(found_equivalent->test(), std::string(buffer));
        count = count + 1;
     }
 }

static bool testEquivalenceRelation(std::string equivalenceRelation, int voiceCount, double range, double g) {
    bool passes = true;
    char buffer[0x200];
    auto normalsForEquivalenceRelation = fundamentalDomainByNormalizesForEquivalenceRelations[equivalenceRelation](voiceCount, range, g);
    auto equivalentsForEquivalenceRelation = fundamentalDomainByIsNormalsForEquivalenceRelations[equivalenceRelation](voiceCount, range, g);
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
                passes = false;
                test(passes, "Size of found equivalents not correct for 3 voices.");
            } else {
                test(true, "Size of found equivalents is correct for 3 voices.");
            }
         }
        if (voiceCount == 4) {
            if (equivalentsForEquivalenceRelation.size() != 83) {
                csound::System::message("%-8s 'found_equivalents' size should be 83 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
                passes = false;    
                test(passes, "Size of found equivalents not correct for 4 voices.");
            } else {
                test(true, "Size of found equivalents is correct for 4 voices.");
            }
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

int main(int argc, char **argv) {
    csound::System::message("C H O R D S P A C E   U N I T   T E S T S\n\n");
    double mp_double_small = .00000000000000000001;
    double mp_double_large = 1e40;
    std::cerr << "ulp(mp_double_small): " << boost::math::ulp(mp_double_small) << std::endl;
    std::cerr << "ulp(mp_double_large): " << boost::math::ulp(mp_double_large) << std::endl;
    std::cerr << "csound::eq_tolerance(0.15, 0.15): " << csound::eq_tolerance(0.15, 0.15) << std::endl;
    std::cerr << "csound::eq_tolerance(0.1500000000000000001, 0.15): " << csound::eq_tolerance(0.1500000000000000001, 0.15) << std::endl;
    std::cerr << "csound::eq_tolerance(0.1500000000001, 0.15): " << csound::eq_tolerance(0.1500000000001, 0.15) << std::endl;
    std::cerr << "csound::gt_tolerance(14.0, 12.0): " << csound::gt_tolerance(14.0, 12.0) << std::endl;
    std::cerr << "csound::ge_tolerance(14.0, 12.0): " << csound::ge_tolerance(14.0, 12.0) << std::endl;
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

    csound::System::message("\nBehavior of std::fmod and std::remainder:\n\n");
    for (double pitch = -24.0; pitch < 24.0; pitch += 1.0) {
        double modulusFmod = std::fmod(pitch, csound::OCTAVE());
        double modulusRemainder = std::remainder(pitch, csound::OCTAVE());
        double pc = csound::epc(pitch);
        double modulus = csound::modulo(pitch, csound::OCTAVE());
        csound::System::message("Pitch: %9.4f  modulo: %9.4f  std::fmod: %9.4f  std::remainder: %9.4f  epc: %9.4f\n", pitch, modulus, modulusFmod, modulusRemainder, pc);
    }
    csound::Chord cmt = csound::chordForName("CM").epcs();
    csound::System::message("Should be C major triad:\n%s\n", cmt.information().c_str());
    csound::Chord pcs = csound::chordForName("C major").epcs();
    csound::System::message("Should be C major scale:\n%s\n", pcs.information().c_str());
    for (double pitch = 36.0; pitch < 96.0; pitch += 1.0) {
        double conformed = csound::conformToPitchClassSet(pitch, pcs);
        csound::System::message("pitch: %9.4f  conformed: %9.4f\n", pitch, conformed);
    }
    {
        ///csound::SCOPED_DEBUGGING scoped_debugging;
        csound::Chord chord({-4.,8.,8.});
        std::cerr << chord.information() << std::endl;
    }
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
    
    
    auto prior_level = csound::System::setMessageLevel(15);
    
    original = csound::Chord({0, 3, 7}).eOPT();
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    reflected = csound::reflect_by_householder(original);
    std::cout << "reflect_by_householder:" << std::endl;
    std::cout << reflected.information() << std::endl;
    reflected =  (original);
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

    csound::System::message("\nTesting equivalence relations...\n\n");
    for (int voiceCount = 3; voiceCount <= 4; ++voiceCount) {
        testEquivalenceRelations(voiceCount, csound::OCTAVE(), 1.0);
    }

    std::vector<csound::Chord> science_opttis_4;
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 0.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 1.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 2.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 3.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 11.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 0., 12.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 1.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 2.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 3.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 1., 11.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 2.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 3.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 2., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 3.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 3., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 4., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 4., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 4., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 4., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 4., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 5., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 5., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 5., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 0., 6., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 3.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 2., 11.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 4.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 3., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 4., 5.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 4., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 4., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 4., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 4., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 5., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 5., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 5., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 1., 6., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 4., 6.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 4., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 4., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 4., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 4., 10.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 5., 7.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 5., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 5., 9.}));
    science_opttis_4.push_back(csound::Chord({0., 2., 6., 8.}));
    science_opttis_4.push_back(csound::Chord({0., 3., 6., 9.}));
    printSet("Science OPTTIs", science_opttis_4);
    
    auto my_optts_4 = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTg>(4, csound::OCTAVE(), 1.);
    printSet("My OPTTs", my_optts_4);

    auto my_opttis_4 = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTgI>(4, csound::OCTAVE(), 1.);
    printSet("My OPTTIs", my_opttis_4);

    std::vector<csound::Chord> science_chord_types_3;
    science_chord_types_3.push_back(csound::Chord({0., 0., 0.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 1.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 2.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 3.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 4.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 0., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 5., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 4., 4.}));
    science_chord_types_3.push_back(csound::Chord({0., 3., 3.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 2.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 1.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 2.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 3.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 4.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 1., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 5., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 4., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 3., 4.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 3.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 4.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 2., 7.}));
    science_chord_types_3.push_back(csound::Chord({0., 4., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 3., 5.}));
    science_chord_types_3.push_back(csound::Chord({0., 3., 6.}));
    science_chord_types_3.push_back(csound::Chord({0., 3., 7.}));
    science_chord_types_3.push_back(csound::Chord({0., 4., 7.}));
    science_chord_types_3.push_back(csound::Chord({0., 4., 8.}));
    printSet("Science chord types", science_chord_types_3);
    
    auto myoptts_3 = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTg>(3, 12., 1.);
    printSet("My OPTTs", myoptts_3);
 
    auto myopttis_3 = csound::fundamentalDomainByIsNormal<csound::EQUIVALENCE_RELATION_RPTgI>(3, 12., 1.);
    printSet("My OPTTIs", myopttis_3);
    
    summary();
    return 0;
}
 
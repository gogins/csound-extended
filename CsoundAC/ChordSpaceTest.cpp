#include "ChordSpace.hpp"
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <string>

#pragma GCC diagnostic ignored "-Wformat"

typedef Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> Matrix;
typedef Eigen::Matrix<double, Eigen::Dynamic, 1> Vector;

static bool printPass = true;
static bool printPitv = false;
static bool failureExits = false ;
static int passCount = 0;
static int failureCount = 0;
static int testCount = 0;
static int exitAfterFailureCount = 5;
static int testSector = 0;

static bool pass(std::string message) {
    passCount = passCount + 1;
    testCount = passCount + failureCount;
    if (printPass) {
        csound::message("\nPASSED (passed: %-9d failed: %-9d of %9d): %s\n", passCount, failureCount, testCount, message.c_str());
    }
    return true;
}

static bool fail(std::string message) {
    failureCount = failureCount + 1;
    testCount = passCount + failureCount;
    csound::message("================================================================================================================\n");
    csound::message("FAILED (passed: %-9d failed: %-9d of %d): %s\n", passCount, failureCount, testCount, message.c_str());
    csound::message("================================================================================================================\n");
    if (failureExits && (failureCount >= exitAfterFailureCount)) {
        std::exit(-1);
    }
    return false;
}

static void summary() {
    testCount = passCount + failureCount;
    std::time_t time_ = std::time(nullptr);
    auto datetime = std::asctime(std::localtime(&time_));
    csound::message("\n================================================================================================================\n");
    csound::message("SUMMARY  Passed: %-9d  Failed: %-9d  Total: %d  Completed: %s", passCount, failureCount, testCount, datetime); 
    csound::message("================================================================================================================\n");
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
    csound::message("%s\n", name.c_str());
    std::multimap<csound::Chord, csound::Chord> sorted;
    for (auto &e : chords) {
        sorted.insert({e.normal_form(), e}); 
    }
    int i = 1;
    for (auto &value : sorted) {
        auto &c = value.second;
        csound::message("normal: %s  chord[%04d]: %s  OPTT: %s  OPTTI: %s opti_sector: ", c.normal_form().toString().c_str(), i, c.toString().c_str(), c.eOPTT().toString().c_str(), c.eOPTTI().toString().c_str());
        auto opti_sectors_ = c.opti_domain_sectors();
        for (auto opti_sector : opti_sectors_) {
            csound::message("%2d (%4.1f)", opti_sector, opti_sector / 2.);
        }
        csound::message("\n");
        i = i + 1;
    }
}

static bool equals(const csound::HyperplaneEquation &a, const csound::HyperplaneEquation &b) {
    if (a.unit_normal_vector.rows() == b.unit_normal_vector.rows() == false) {
        csound::error("equals: size mismatch: %d %d\n", a.unit_normal_vector.rows(), b.unit_normal_vector.rows());
        return false;
    }
    for (int row = 0; row < a.unit_normal_vector.rows(); ++row) {
        if (csound::eq_tolerance(a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0)) == false) {
            csound::error("equals: unit normal vector element mismatch: %.17g %.17g\n", a.unit_normal_vector(row, 0), b.unit_normal_vector(row, 0));
            return false;
        }
    }
    if (csound::eq_tolerance(a.constant_term, b.constant_term) == false) {
        csound::error("equals: constant term mismatch: %.17g %,17g\n", a.constant_term, b.constant_term);
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

static void test_pitv(const csound::PITV &pitv_, std::string chordName) {
    csound::message("BEGAN test PITV for %s...\n", chordName.c_str());
    csound::Chord originalChord = csound::chordForName(chordName);
    csound::Chord optti = originalChord.eOPTTI();
    csound::message("Original chord:\n%s\n", originalChord.information().c_str());
    Eigen::VectorXi pitv = pitv_.fromChord(originalChord, printPitv);
    csound::Chord reconstitutedChord = pitv_.toChord(pitv[0], pitv[1], pitv[2], pitv[3], printPitv)[0];
    csound::message("Reconstituted chord:\n%s\n", reconstitutedChord.information().c_str());
    test(originalChord == reconstitutedChord, "Reconstituted chord must be the same as the original chord.");
    csound::Chord revoicedOriginalChord = originalChord;
    revoicedOriginalChord.setPitch(1,  revoicedOriginalChord.getPitch(1) + 12.);
    revoicedOriginalChord.setPitch(2,  revoicedOriginalChord.getPitch(2) + 24.);
    csound::message("Revoiced original chord:\n%s\n", revoicedOriginalChord.information().c_str());
    pitv = pitv_.fromChord(revoicedOriginalChord, printPitv);
    csound::Chord reconstitutedRevoicedChord = pitv_.toChord_vector(pitv, printPitv)[0];
    csound::message("Reconstituted revoiced chord:\n%s\n", reconstitutedRevoicedChord.information().c_str());
    test(revoicedOriginalChord == reconstitutedRevoicedChord, "Reconstituted revoiced chord must be the same as the original revoiced chord.");
    csound::Chord invertedChord = originalChord.I().eOP();
    csound::message("Inverted original chord:\n%s\n", invertedChord.information().c_str());
    pitv = pitv_.fromChord(invertedChord, printPitv);
    csound::Chord reconstitutedInvertedChord = pitv_.toChord_vector(pitv, true)[0];
    csound::message("Reconstituted inverted chord:\n%s\n", reconstitutedInvertedChord.information().c_str());
    test(invertedChord == reconstitutedInvertedChord,"Reconstituted inverted chord must be the same as the original inverted chord.");
    csound::message("ENDED test PITV for %s.\n", chordName.c_str());
    csound::message("\n");
}

static void test_pitv(int initialVoiceCount, int finalVoiceCount) {
    double range = 60.0;
    for (int voiceCount = initialVoiceCount; voiceCount <= finalVoiceCount; ++voiceCount) {
        bool passes = true;
        csound::PITV pitv;
        csound::message("Testing all of PITV: voices: %d  range: %f\n", voiceCount, range);
        csound::message("Testing PITV to chord and back...\n");
        pitv.initialize(voiceCount, range, 1., true);
        pitv.list(true, true, true);
        for (int P = 0; P < pitv.countP; ++P) {
            for (int T = 0; T < pitv.countT; ++T) {
                for (int V = 0; V < pitv.countV; ++V) {
                    for (int I = 0; I < pitv.countI; ++I) {
                        if (printPass) csound::message("PITV => chord from PITV\n");
                        csound::Chord chord_from_pitv = pitv.toChord(P, I, T, V, printPitv)[0];
                        if (printPass) csound::message("chord from PITV => PITV from chord\n");
                        Eigen::VectorXi pitv_from_chord = pitv.fromChord(chord_from_pitv, printPitv);
                        if (printPass) csound::message("PITV from chord => chord from PITV from chord\n");
                        csound::Chord chord_from_pitv_from_chord = pitv.toChord(pitv_from_chord(0), pitv_from_chord(1), pitv_from_chord(2), pitv_from_chord(3), printPitv)[0];
                        bool equals = (chord_from_pitv == chord_from_pitv_from_chord);
                        if (!equals) {
                            csound::message("chord_from_pitv (toChord):\n%s\n", chord_from_pitv.information().c_str());
                            csound::message("chord_from_pitv_from_chord (fromChord):\n%s\n", chord_from_pitv_from_chord.information().c_str());
                            passes = false;
                        }
                        test(equals, "chord_from_pitv must match chord_from_pitv_from_chord.");
                        if (printPass) csound::message("\n\n");
                    }
                }
            }
        }
        csound::message("Testing chord to PITV and back...\n\n");
        bool passes2 = true;
        auto eops = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RP>(voiceCount, csound::OCTAVE(), 1., testSector);
        for(auto it = eops.begin(); it != eops.end(); ++it) {
            auto chord = it->T(24.);
            auto origin = chord;
            for(;;) {
                Eigen::VectorXi pitv_from_chord = pitv.fromChord(chord, printPitv);
                if (printPass) csound::message("pitv_from_chord:            %8d     %8d     %8d     %8d <= %s\n", pitv_from_chord(0), pitv_from_chord(1), pitv_from_chord(2), pitv_from_chord(3), chord.toString().c_str());
                csound::Chord chord_from_pitv_from_chord = pitv.toChord(pitv_from_chord(0), pitv_from_chord(1), pitv_from_chord(2), pitv_from_chord(3), printPitv)[0];
                if (printPass) csound::message("chord_from_pitv_from_chord: %8d     %8d     %8d     %8d => %s\n", pitv_from_chord(0), pitv_from_chord(1), pitv_from_chord(2), pitv_from_chord(3), chord_from_pitv_from_chord.toString().c_str());
                bool equals = (chord_from_pitv_from_chord == chord);
                if (!equals) {
                    csound::message("Original chord (fromChord):\n%s\n", chord.information().c_str());
                    csound::message("New chord (toChord):\n%s\n", chord_from_pitv_from_chord.information().c_str());
                }
                if (pitv_from_chord(3) == -1) {
                    csound::message("Chord is out of PITV range...\n");
                } else {
                    test(equals, "Original chord must match chord from original chord's PITV.");
                    passes2 = false;
                }
                if (printPass) csound::message("\n");
                // This was going too far... cut off sooner and all seems well.
                if (csound::next(chord, origin, range - 1.0, csound::OCTAVE()) == false) {
                    break;
                }
            }
        }
    }
}

std::vector<std::string> equivalenceRelationsToTest = {"RP", "RPT", "RPTg", "RPTI", "RPTgI"};
typedef csound::Chord(*equate_t)(const csound::Chord &, double, double, int);
typedef bool (*predicate_t)(const csound::Chord &, double, double, int);
typedef std::vector<csound::Chord> (*fundamentalDomainByEquate_t)(int, double, double, int);
typedef std::vector<csound::Chord> (*fundamentalDomainByPredicate_t)(int, double, double, int, bool);
std::map<std::string, equate_t> equatesForEquivalenceRelations;
std::map<std::string, predicate_t> predicatesForEquivalenceRelations;
std::map<std::string, std::set<std::string> > equivalenceRelationsForCompoundEquivalenceRelations;
std::map<std::string, fundamentalDomainByEquate_t> fundamentalDomainByEquateForEquivalenceRelations;
std::map<std::string, fundamentalDomainByPredicate_t> fundamentalDomainByPredicateForEquivalenceRelations;

static bool testNormalsAndEquivalents(std::string equivalence,
                                      std::vector<csound::Chord> &made_equivalents,
                                      std::vector<csound::Chord> &found_equivalents,
                                      double range,
                                      double g) {
    char buffer[0x200];
    auto is_equivalent = predicatesForEquivalenceRelations[equivalence];
    csound::message("\nequivalence: %s  normalized: %ld  is_normal: %ld  range: %f  g: %f\n", equivalence.c_str(), made_equivalents.size(), found_equivalents.size(), range, g);
    auto make_equivalent = equatesForEquivalenceRelations[equivalence];
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
    auto normalsForEquivalenceRelation = fundamentalDomainByEquateForEquivalenceRelations[equivalenceRelation](voiceCount, range, g, testSector);
    auto equivalentsForEquivalenceRelation = fundamentalDomainByPredicateForEquivalenceRelations[equivalenceRelation](voiceCount, range, g, testSector, false);
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
                csound::message("%-8s 'found_equivalents' size should be 19 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
                passes = false;
                test(passes, "Size of found equivalents not correct for 3 voices.");
            } else {
                test(true, "Size of found equivalents is correct for 3 voices.");
            }
         }
        if (voiceCount == 4) {
            if (equivalentsForEquivalenceRelation.size() != 83) {
                csound::message("%-8s 'found_equivalents' size should be 83 but is %ld.\n", equivalenceRelation.c_str(), equivalentsForEquivalenceRelation.size());
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
    csound::message("\nTesting equivalence relations for %d voices over range %f with g %f...\n\n", voiceCount, range, g);
    for (auto equivalenceRelationI = equivalenceRelationsToTest.begin();
            equivalenceRelationI != equivalenceRelationsToTest.end();
            ++equivalenceRelationI) {
        if (!testEquivalenceRelation(*equivalenceRelationI, voiceCount, range, g)) {
            passes = false;
        }
    }
    return passes;
}

/**
 * Puts the set difference of A \ B, if any, into difference.
 */
static void setDifference(const std::string &a_name, std::vector<csound::Chord> &A, const std::string &b_name,std::vector<csound::Chord> &B, std::vector<csound::Chord> &difference) {
    auto comparator = [](auto &a, auto &b) {
        auto an = a.eOPTT(0);
        an.clamp();
        auto bn = b.eOPTT(0);
        bn.clamp();
        if ((an < bn) == true) {
            ///std::cerr << "less" << std::endl;
            return true;
        } else {
            ///std::cerr << "not less" << std::endl;
            return false;
        }
    };
    std::sort(A.begin(), A.end(), comparator);
    std::sort(B.begin(), B.end(), comparator);
    std::multimap<std::string, csound::Chord> map_a;
    for (csound::Chord &chord : A) {
        std::string key = chord.eOPTT().normal_form().toString();
        map_a.insert({key, chord});
    }
    std::multimap<std::string, csound::Chord> map_b;
    for (csound::Chord chord : B) {
        std::string key = chord.eOPTT().normal_form().toString();
        map_b.insert({key, chord});
    }
    difference.clear();
    int a_i = 0;
    int b_i = 0;
    for (auto a_it = map_a.begin(); a_it != map_a.end(); ++a_it) {
        auto b_it = map_b.find(a_it->first);
        auto const &a = a_it->second;
        if (b_it == map_b.end()) {
            std::fprintf(stderr, "%s[%d]:\n",  a_name.c_str(), a_i);
            std::fprintf(stderr, "    normal_form:        %s\n", a.normal_form().toString().c_str());
            std::fprintf(stderr, "    prime_form:         %s\n", a.prime_form().toString().c_str());
            std::fprintf(stderr, "    inverse_prime_form: %s\n", a.inverse_prime_form().toString().c_str());
            std::fprintf(stderr, "    eppcs:              %s\n", a.eppcs().toString().c_str());
            std::fprintf(stderr, "    chord:              %s\n", print_chord(a));
            std::fprintf(stderr, "    OPTT:               %s\n", print_chord(a.eOPTT()));
            std::fprintf(stderr, "    OPTTI:              %s\n\n\n\n\n\n\n", print_chord(a.eOPTTI()));
            difference.push_back(a_it->second);
            ++a_i;
        } else {
            auto const &b = b_it->second;
            std::fprintf(stderr, "%s[%d]:\n",  a_name.c_str(), a_i);
            std::fprintf(stderr, "    normal_form:        %s\n", a.normal_form().toString().c_str());
            std::fprintf(stderr, "    prime_form:         %s\n", a.prime_form().toString().c_str());
            std::fprintf(stderr, "    inverse_prime_form: %s\n", a.inverse_prime_form().toString().c_str());
            std::fprintf(stderr, "    eppcs:              %s\n", a.eppcs().toString().c_str());
            std::fprintf(stderr, "    chord:              %s\n", print_chord(a));
            std::fprintf(stderr, "    OPTT:               %s\n", print_chord(a.eOPTT()));
            std::fprintf(stderr, "    OPTTI:              %s\n", print_chord(a.eOPTTI()));
            std::fprintf(stderr, "  %s[%d]:\n",  b_name.c_str(), b_i);
            std::fprintf(stderr, "    normal_form:        %s\n", b.normal_form().toString().c_str());
            std::fprintf(stderr, "    prime_form:         %s\n", b.prime_form().toString().c_str());
            std::fprintf(stderr, "    inverse_prime_form: %s\n", b.inverse_prime_form().toString().c_str());
            std::fprintf(stderr, "    eppcs:              %s\n", b.eppcs().toString().c_str());
            std::fprintf(stderr, "    chord:              %s\n", print_chord(b));
            std::fprintf(stderr, "    OPTT:               %s\n", print_chord(b.eOPTT()));
            std::fprintf(stderr, "    OPTTI:              %s\n\n", print_chord(b.eOPTTI()));
            ++a_i;
            ++b_i;
        }
    }
    std::sort(difference.begin(), difference.end(), comparator);
 }

int main(int argc, char **argv) {
    csound::message("C H O R D S P A C E   U N I T   T E S T S\n\n");
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

    auto chordx = csound::chordForName("CM7");
    auto dominantx = csound::chordForName("G7");
    csound::message("CM7:\n%s\n", chordx.information().c_str());
    csound::message("G7:\n%s\n", dominantx.information().c_str());

    equatesForEquivalenceRelations["R"] =        csound::equate<csound::EQUIVALENCE_RELATION_R>;
    equatesForEquivalenceRelations["P"] =        csound::equate<csound::EQUIVALENCE_RELATION_P>;
    equatesForEquivalenceRelations["T"] =        csound::equate<csound::EQUIVALENCE_RELATION_T>;
    equatesForEquivalenceRelations["Tg"] =       csound::equate<csound::EQUIVALENCE_RELATION_Tg>;
    equatesForEquivalenceRelations["I"] =        csound::equate<csound::EQUIVALENCE_RELATION_I>;
    equatesForEquivalenceRelations["RP"] =       csound::equate<csound::EQUIVALENCE_RELATION_RP>;
    equatesForEquivalenceRelations["RPT"] =      csound::equate<csound::EQUIVALENCE_RELATION_RPT>;
    equatesForEquivalenceRelations["RPTg"] =     csound::equate<csound::EQUIVALENCE_RELATION_RPTg>;
    equatesForEquivalenceRelations["RPI"] =      csound::equate<csound::EQUIVALENCE_RELATION_RPI>;
    equatesForEquivalenceRelations["RPTI"] =     csound::equate<csound::EQUIVALENCE_RELATION_RPTI>;
    equatesForEquivalenceRelations["RPTgI"] =    csound::equate<csound::EQUIVALENCE_RELATION_RPTgI>;
    predicatesForEquivalenceRelations["R"] =         csound::predicate<csound::EQUIVALENCE_RELATION_R>;
    predicatesForEquivalenceRelations["P"] =         csound::predicate<csound::EQUIVALENCE_RELATION_P>;
    predicatesForEquivalenceRelations["T"] =         csound::predicate<csound::EQUIVALENCE_RELATION_T>;
    predicatesForEquivalenceRelations["Tg"] =        csound::predicate<csound::EQUIVALENCE_RELATION_Tg>;
    predicatesForEquivalenceRelations["I"] =         csound::predicate<csound::EQUIVALENCE_RELATION_I>;
    predicatesForEquivalenceRelations["RP"] =        csound::predicate<csound::EQUIVALENCE_RELATION_RP>;
    predicatesForEquivalenceRelations["RPT"] =       csound::predicate<csound::EQUIVALENCE_RELATION_RPT>;
    predicatesForEquivalenceRelations["RPTg"] =      csound::predicate<csound::EQUIVALENCE_RELATION_RPTg>;
    predicatesForEquivalenceRelations["RPI"] =       csound::predicate<csound::EQUIVALENCE_RELATION_RPI>;
    predicatesForEquivalenceRelations["RPTI"] =      csound::predicate<csound::EQUIVALENCE_RELATION_RPTI>;
    predicatesForEquivalenceRelations["RPTgI"] =     csound::predicate<csound::EQUIVALENCE_RELATION_RPTgI>;
    equivalenceRelationsForCompoundEquivalenceRelations["RP"] =      {"R", "P"};
    equivalenceRelationsForCompoundEquivalenceRelations["RPT"] =     {"R", "P", "T"}; // V?
    equivalenceRelationsForCompoundEquivalenceRelations["RPTg"] =    {"R", "P", "Tg"}; // V?
    equivalenceRelationsForCompoundEquivalenceRelations["RPI"] =     {"R", "P"};
    equivalenceRelationsForCompoundEquivalenceRelations["RPTgI"] =   {"RPTg", "RP", "R", "P", "Tg"}; // V?
    fundamentalDomainByEquateForEquivalenceRelations["R"] =        csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_R>;
    fundamentalDomainByEquateForEquivalenceRelations["P"] =        csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_P>;
    fundamentalDomainByEquateForEquivalenceRelations["T"] =        csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_T>;
    fundamentalDomainByEquateForEquivalenceRelations["Tg"] =       csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_Tg>;
    fundamentalDomainByEquateForEquivalenceRelations["I"] =        csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_I>;
    fundamentalDomainByEquateForEquivalenceRelations["RP"] =       csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RP>;
    fundamentalDomainByEquateForEquivalenceRelations["RPT"] =      csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RPT>;
    fundamentalDomainByEquateForEquivalenceRelations["RPTg"] =     csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RPTg>;
    fundamentalDomainByEquateForEquivalenceRelations["RPI"] =      csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RPI>;
    fundamentalDomainByEquateForEquivalenceRelations["RPTI"] =     csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RPTI>;
    fundamentalDomainByEquateForEquivalenceRelations["RPTgI"] =    csound::fundamentalDomainByTransformation<csound::EQUIVALENCE_RELATION_RPTgI>;
    fundamentalDomainByPredicateForEquivalenceRelations["R"] =           csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_R>;
    fundamentalDomainByPredicateForEquivalenceRelations["P"] =           csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_P>;
    fundamentalDomainByPredicateForEquivalenceRelations["T"] =           csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_T>;
    fundamentalDomainByPredicateForEquivalenceRelations["Tg"] =          csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_Tg>;
    fundamentalDomainByPredicateForEquivalenceRelations["I"] =           csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_I>;
    fundamentalDomainByPredicateForEquivalenceRelations["RP"] =          csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RP>;
    fundamentalDomainByPredicateForEquivalenceRelations["RPT"] =         csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPT>;
    fundamentalDomainByPredicateForEquivalenceRelations["RPTg"] =        csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>;
    fundamentalDomainByPredicateForEquivalenceRelations["RPI"] =         csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPI>;
    fundamentalDomainByPredicateForEquivalenceRelations["RPTI"] =        csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTI>;
    fundamentalDomainByPredicateForEquivalenceRelations["RPTgI"] =       csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTgI>;
        
    auto chordspace_optts_3 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>(3, 12., 1., testSector);
    //~ printSet("My OPTTs", chordspace_optts_3);
 
    auto chordspace_opttis_3 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTgI>(3, 12., 1., testSector);
    //~ printSet("My OPTTIs", chordspace_opttis_3);
    
    auto chordspace_optts_4 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTg>(4, csound::OCTAVE(), 1., testSector);
    //~ printSet("My OPTTs", chordspace_optts_4);

    auto chordspace_opttis_4 = csound::fundamentalDomainByPredicate<csound::EQUIVALENCE_RELATION_RPTgI>(4, csound::OCTAVE(), 1., testSector);
    //~ printSet("My OPTTIs", chordspace_opttis_4);
    
    csound::message("\nBehavior of std::fmod and std::remainder:\n\n");
    for (double pitch = -24.0; pitch < 24.0; pitch += 1.0) {
        double modulusFmod = std::fmod(pitch, csound::OCTAVE());
        double modulusRemainder = std::remainder(pitch, csound::OCTAVE());
        double pc = csound::epc(pitch);
        double modulus = csound::modulo(pitch, csound::OCTAVE());
        csound::message("Pitch: %9.4f  modulo: %9.4f  std::fmod: %9.4f  std::remainder: %9.4f  epc: %9.4f\n", pitch, modulus, modulusFmod, modulusRemainder, pc);
    }
    csound::Chord cmt = csound::chordForName("CM").epcs();
    csound::message("Should be C major triad:\n%s\n", cmt.information().c_str());
    csound::Chord pcs = csound::chordForName("C major").epcs();
    csound::message("Should be C major scale:\n%s\n", pcs.information().c_str());
    for (double pitch = 36.0; pitch < 96.0; pitch += 1.0) {
        double conformed = csound::conformToPitchClassSet(pitch, pcs);
        csound::message("pitch: %9.4f  conformed: %9.4f\n", pitch, conformed);
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
    //auto prior_level = csound::setMessageLevel(15);
    // Must be 'false' because this is a chord not a scale, and if it were a 
    // scale, infinite looping because tosplit is not in order.
    csound::fill("C", 0., "M9", tosplit, false);
    //csound::setMessageLevel(prior_level);
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
    csound::message("chordForName(%s): %s\n", "CM9", chordForName_.information().c_str());

    csound::message("\nTesting equivalence relations...\n\n");
    for (int voiceCount = 3; voiceCount <= 4; ++voiceCount) {
        testEquivalenceRelations(voiceCount, csound::OCTAVE(), 1.0);
    }
    
    csound::PITV pitv_3;
    pitv_3.initialize(3, 60., 1., true);
    pitv_3.list(true, true);
    
    csound::PITV pitv_4;
    pitv_4.initialize(4, 60., 1., true);
    pitv_4.list(true, true);
    
    test_pitv(pitv_4, "D#7b5");
    test_pitv(3, 4);

    std::vector<csound::Chord> science_optts_3;
    science_optts_3.push_back(csound::Chord({0., 0., 0.}));
    science_optts_3.push_back(csound::Chord({0., 0., 1.}));
    science_optts_3.push_back(csound::Chord({0., 0., 2.}));
    science_optts_3.push_back(csound::Chord({0., 0., 3.}));
    science_optts_3.push_back(csound::Chord({0., 0., 4.}));
    science_optts_3.push_back(csound::Chord({0., 0., 5.}));
    science_optts_3.push_back(csound::Chord({0., 0., 6.}));
    science_optts_3.push_back(csound::Chord({0., 5., 5.}));
    science_optts_3.push_back(csound::Chord({0., 4., 4.}));
    science_optts_3.push_back(csound::Chord({0., 3., 3.}));
    science_optts_3.push_back(csound::Chord({0., 2., 2.}));
    science_optts_3.push_back(csound::Chord({0., 1., 1.}));
    science_optts_3.push_back(csound::Chord({0., 1., 2.}));
    science_optts_3.push_back(csound::Chord({0., 1., 3.}));
    science_optts_3.push_back(csound::Chord({0., 1., 4.}));
    science_optts_3.push_back(csound::Chord({0., 1., 5.}));
    science_optts_3.push_back(csound::Chord({0., 1., 6.}));
    science_optts_3.push_back(csound::Chord({0., 5., 6.}));
    science_optts_3.push_back(csound::Chord({0., 4., 5.}));
    science_optts_3.push_back(csound::Chord({0., 3., 4.}));
    science_optts_3.push_back(csound::Chord({0., 2., 3.}));
    science_optts_3.push_back(csound::Chord({0., 2., 4.}));
    science_optts_3.push_back(csound::Chord({0., 2., 5.}));
    science_optts_3.push_back(csound::Chord({0., 2., 6.}));
    science_optts_3.push_back(csound::Chord({0., 2., 7.}));
    science_optts_3.push_back(csound::Chord({0., 4., 6.}));
    science_optts_3.push_back(csound::Chord({0., 3., 5.}));
    science_optts_3.push_back(csound::Chord({0., 3., 6.}));
    science_optts_3.push_back(csound::Chord({0., 3., 7.}));
    science_optts_3.push_back(csound::Chord({0., 4., 7.}));
    science_optts_3.push_back(csound::Chord({0., 4., 8.}));
    //~ printSet("Science OPTTIs", science_optts_3);
    
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
    //~ printSet("Science OPTTIs", science_opttis_4);
    
    std::vector<csound::Chord> difference;
    setDifference("ScienceOPTTS", science_optts_3, "ChordSpaceOPTTs", chordspace_optts_3, difference);
    //~ for (const auto &chord : difference) {
        //~ std::cerr << "Chords in Science not in ChordSpace:" << std::endl << std::endl;
        //~ std::cerr << chord.information() << std::endl << std::endl;
    //~ }
    setDifference("ChordSpaceOPTTS", chordspace_optts_3, "ScienceOPTTs", science_optts_3, difference);
    //~ for (const auto &chord : difference) {
        //~ std::cerr << "Chords in ChordSpace not in Science:" << std::endl << std::endl;
        //~ std::cerr << chord.information() << std::endl << std::endl;
    //~ }
    setDifference("ScienceOPTTIs", science_opttis_4, "ChordSpaceOPTTIs", chordspace_opttis_4, difference);
    //~ for (const auto &chord : difference) {
        //~ std::cerr << "Chords in Science not in ChordSpace:" << std::endl << std::endl;
        //~ std::cerr << chord.information() << std::endl << std::endl;
    //~ }
    setDifference("ChordSpaceOPTTIs", chordspace_opttis_4, "ScienceOPTTIs", science_opttis_4, difference);
    //~ for (const auto &chord : difference) {
        //~ std::cerr << "Chords in ChordSpace not in Science:" << std::endl << std::endl;
        //~ std::cerr << chord.information() << std::endl << std::endl;
    //~ }
    
    auto test_chord1 = csound::Chord({0., 1., 2., 6.}).eT();
    std::cerr << test_chord1.information() << std::endl;
    auto test_chord2 = csound::Chord({0., 1., 2., 8.}).eT();
    std::cerr << test_chord2.information() << std::endl;

#if 1    
    csound::Chord c1({-7, 2, 5});
    std::cout << c1.information() << std::endl;
    csound::Chord c2({-5, -2, 7});
    std::cout << c2.information() << std::endl;
    
    auto prior_level = csound::message_level(15);
    
    auto original = csound::Chord({0, 3, 7}).eOPT();
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    auto reflected = csound::reflect_by_householder(original);
    std::cout << "reflect_by_householder:" << std::endl;
    std::cout << reflected.information() << std::endl;
    reflected =  (original);
    std::cout << "reflect_in_inversion_flat:" << std::endl;
    std::cout << reflected.information() << std::endl;
    auto spun_back = reflected.eOPT();
    std::cout << "spun_back:" << std::endl;
    std::cout << spun_back.information() << std::endl;

    original = csound::chordForName("C7");
    std::cout << "original:" << std::endl;
    std::cout << original.information() << std::endl;
    reflected = reflect_in_inversion_flat(original, testSector);
    std::cout << "reflected:" << std::endl;
    std::cout << reflected.information() << std::endl;
    spun_back = reflected.eOPTT();
    std::cout << "spun_back:" << std::endl;
    std::cout << spun_back.information() << std::endl;    
#endif    

    summary();
    return 0;
}
 
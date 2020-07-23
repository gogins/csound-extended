/*
 * C S O U N D
 *
 * L I C E N S E
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef CHORD_SPACE_H
#define CHORD_SPACE_H
#define EIGEN_INITIALIZE_MATRICES_BY_ZERO
#include "Platform.hpp"
#ifdef SWIG
%module CsoundAC
%{
#include <algorithm>
#include <cfloat>
#include <climits>
#include <cmath>
#include <cstdarg>
#include <eigen3/Eigen/Dense>
#include <Event.hpp>
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include <Score.hpp>
#include <System.hpp>
#include <set>
#include <sstream>
#include <vector>
%}
%include "std_string.i"
%include "std_vector.i"
#else
#include <algorithm>
#include <cfloat>
#include <climits>
#include <cmath>
#include <cstdarg>
#include <eigen3/Eigen/Dense>
#include "Event.hpp"
#include <functional>
#include <iostream>
#include <iterator>
#include <map>
#include "Score.hpp"
#include "System.hpp"
#include <set>
#include <sstream>
#include <vector>
#endif

namespace csound {
/**
This library, part of CsoundAC, implements a geometric approach to some common
operations on chords in neo-Riemannian music theory for use in score
generating procedures:

--  Identifying whether a chord belongs to some equivalence class of music
    theory, or sending a chord to its equivalent within a representative
    ("normal") fundamental domain of some equivalence relation. The
    equivalence relations are octave (O), permutational (P), transpositional,
    (T), inversional (I), and their compounds OP, OPT (set-class or chord
    type), and OPTI (similar to prime form), among others.

--  Causing chord progressions to move strictly within an orbifold that
    generates some equivalence class.

--  Implementing chord progressions based on the L, P, R, D, K, and Q
    operations of neo-Riemannian theory (thus implementing some aspects of
    "harmony").

--  Implementing chord progressions performed within a more abstract
    equivalence class by means of the closest voice-leading within a less
    abstract equivalence class (thus implementing some fundamentals of
    "counterpoint").
    
--  Implementing "functional" or "Roman numeral" operations performed 
    using scales and scale degrees.
    
DEFINITIONS

Pitch is the perception of a distinct sound frequency. It is a logarithmic
perception; octaves, which sound 'equivalent' in some sense, represent
doublings or halvings of frequency.

Pitches and intervals are represented as real numbers. Middle C is 60 and the
octave is 12. Our usual system of 12-tone equal temperament, as well as MIDI
key numbers, are completely represented by the whole numbers; any and all
other pitches can be represented simply by using fractions.

A voice is a distinct sound that is heard as having a pitch.

A chord is simply a set of voices heard at the same time, represented here
as a point in a chord space having one dimension of pitch for each voice
in the chord.

A scale is a chord with a tonic pitch-class as its first and lowest voice, 
all other voices being pitches, not pitch-classes, sorted in ascending order.

For the purposes of algorithmic composition in CsoundAC, a score is considered
to be a sequence of more or less fleeting chords.

EQUIVALENCE RELATIONS AND CLASSES

An equivalence relation identifies elements of a set as belonging to
classes. For example the octave is an equivalence relation that identifies
C1, C2, and C3 as belonging to the equivalence class C. Operations that send
elements to their equivalents induce quotient spaces or orbifolds, where
the equivalence operation identifies points on one face of the orbifold with
points on an opposing face. The fundamental domain of the equivalence relation
is the space "within" the orbifold.

Plain chord space has no equivalence relation. Ordered chords are represented
as vectors in parentheses (p1, ..., pN). Unordered chords are represented as
sorted vectors in braces {p1, ..., pN}. Unordering is itself an equivalence
relation -- permutational equivalence.

The following equivalence relations apply to pitches and chords, and exist in
different orbifolds. Equivalence relations can be combined (Callendar, Quinn,
and Tymoczko, "Generalized Voice-Leading Spaces," _Science_ 320, 2008), and
the more equivalence relations are combined, the more abstract is the
resulting orbifold compared to the parent space.

In most cases, a chord space can be divided into a number, possibly
infinite, of geometrically equivalent fundamental domains for the same
equivalence relation. Therefore, here we use the notion of 'representative'
or 'normal' fundamental domain. For example, the representative fundamental
domain of unordered sequences, out of all possible orderings, consists of all
sequences in their ordinary sorted order. It is important, in the following,
to identify representative fundamental domains that combine properly, e.g.
such that the representative fundamental domain of OP / the representative
fundamental domain of PI equals the representative fundamental domain of OPI.
And this in turn may require accounting for duplicate elements of the
representative fundamental domain caused by reflections or singularities in
the orbifold.

C       Cardinality equivalence, e.g. {1, 1, 2} == {1, 2}. _Not_ assuming
        cardinality equivalence ensures that there is a proto-metric in plain
        chord space that is inherited by all child chord spaces. Cardinality
        equivalence is never assumed here, because we are working in chord
        spaces of fixed dimensionality; e.g. we represent the note middle C
        not only as {60}, but also as {60, 60, ..., 60}.

O       Octave equivalence. The fundamental domain is defined by the pitches
        in a chord spanning the range of an octave or less, and summing to
        an octave or less.

P       Permutational equivalence. The fundamental domain is defined by a
        "wedge" of plain chord space in which the voices of a chord are always
        sorted by pitch.

T       Transpositional equivalence, e.g. {1, 2} == {7, 8}. The fundamental
        domain is defined as a plane in chord space at right angles to the
        diagonal of unison chords. Represented by the chord always having a
        sum of pitches equal to 0.

Tg      Transpositional equivalence "rounded off" to the nearest generator
        of transposition (in 12 tone equal temperament, this is one semitone).

I       Inversional equivalence. Care is needed to distinguish the
        mathematician's sense of 'invert', which means 'pitch-space inversion'
        or 'reflect in a point', from the musician's sense of 'invert', which
        varies according to context but in practice often means 'registral
        inversion' or 'revoice by adding an octave to the lowest tone of a
        chord.' Here, we use 'invert' and 'inversion' in the mathematician's
        sense, and we use the terms 'revoice' and 'voicing' for the musician's
        'invert' and 'inversion'. The inversion point for any inversion lies
        on the unison diagonal. A fundamental domain is defined as any half of
        chord space that is bounded by a plane containing the inversion point.
        Represented as the chord having the first interval between voices be
        smaller than or equal to the final interval (recursing for chords of
        more than 3 voices).

PI      Inversional equivalence with permutational equivalence. The
        'inversion flat' of unordered chord space is a hyperplane consisting
        of all those unordered chords that are invariant under inversion. A
        fundamental domain is defined by any half space bounded by a
        hyperplane containing the inversion flat. It is represented as that
        half of the space on or lower than the hyperplane defined by the
        inversion flat and the unison diagonal.

OP      Octave equivalence with permutational equivalence. Tymoczko's orbifold
        for chords; i.e. chords with a fixed number of voices in a harmonic
        context. The fundamental domain is defined as a hyperprism one octave
        long with as many sides as voices and the ends identified by octave
        equivalence and one cyclical permutation of voices, modulo the
        unordering. In OP for trichords in 12TET, the augmented triads run up
        the middle of the prism, the major and minor triads are in 6
        alternating columns around the augmented triads, the two-pitch chords
        form the 3 sides, and the one-pitch chords form the 3 edges that join
        the sides.
        
OPT     The layer of the OP prism as close as possible to the origin, modulo
        the number of voices. Chord type. Note that CM and Cm are different
        OPT. Because the OP prism is canted down from the origin, at least one
        pitch in each OPT chord (excepting the origin itself) is negative.

OPI     The OP prism modulo inversion, i.e. 1/2 of the OP prism. The
        representative fundamental consits of those chords less than or equal
        to their inversions modulo OP.

OPTI    The OPT layer modulo inversion, i.e. 1/2 of the OPT layer.
        Set-class. Note that CM and Cm are the same OPTI.

OPERATIONS

Each of the above equivalence relations is, of course, an operation that sends
chords outside a fundamental domain to chords inside the fundamental domain.

We define the following additional operations:

T(p, x)         Translate p by x.

I(p [, x])      Reflect p in x, by default the origin.

P               Send a major triad to the minor triad with the same root,
                or vice versa (Riemann's parallel transformation).

L               Send a major triad to the minor triad one major third higher,
                or vice versa (Riemann's Leittonwechsel or leading-tone
                exchange transformation).

R               Send a major triad to the minor triad one minor third lower,
                or vice versa (Riemann's relative transformation).

D               Send a triad to the next triad a perfect fifth lower
                (dominant transformation).

P, L, and R have been extended as follows, see Fiore and Satyendra,
"Generalized Contextual Groups", _Music Theory Online_ 11, August 2008:

K(c)            Interchange by inversion;
                K(c) := I(c, c[1] + c[2]).
                This is a generalized form of P; for major and minor triads,
                it is exactly the same as P, but it also works with other
                chord types.

Q(c, n, m)      Contexual transposition;
                Q(c, n, m) := T(c, n) if c is a T-form of m,
                or T(c, -n) if c is an I-form of M. Not a generalized form
                of L or R; but, like them, K and Q generate the T-I group.
                
*/

static int DEBUGGING = false;

/**
 * Returns n!
 */
inline SILENCE_PUBLIC double factorial(double n) {
    double result = 1.0;
    for (int i = 0; i <= n; ++i) {
        result = result * i;
    }
    return result;
}

inline SILENCE_PUBLIC double EPSILON() {
    static double epsilon = 1.0;
    if (epsilon == 1.0) {
        for (;;) {
            epsilon = epsilon / 2.0;
            double nextEpsilon = epsilon / 2.0;
            double onePlusNextEpsilon = 1.0 + nextEpsilon;
            if (onePlusNextEpsilon == 1.0) {
                break;
            }
        }
    }
    return epsilon;
}

inline SILENCE_PUBLIC double &epsilonFactor() {
    static double epsilonFactor = 1000.0;
    return epsilonFactor;
}

inline SILENCE_PUBLIC bool eq_epsilon(double a, double b) {
    if (std::abs(a - b) < (EPSILON() * epsilonFactor())) {
        return true;
    } else {
        return false;
    }

}

inline SILENCE_PUBLIC double gt_epsilon(double a, double b) {
    if (eq_epsilon(a, b)) {
        return false;
    } else {
        return (a > b);
    }
}

inline SILENCE_PUBLIC double lt_epsilon(double a, double b) {
    if (eq_epsilon(a, b)) {
        return false;
    } else {
        return (a < b);
    }
}

inline SILENCE_PUBLIC double ge_epsilon(double a, double b) {
    if (eq_epsilon(a, b)) {
        return true;
    } else {
        return (a > b);
    }
}

inline SILENCE_PUBLIC double le_epsilon(double a, double b) {
    if (eq_epsilon(a, b)) {
        return true;
    } else {
        return (a < b);
    }
}

/**
 * The size of the octave, defined to be consistent with
 * 12 tone equal temperament and MIDI.
 */
inline SILENCE_PUBLIC double OCTAVE() {
    return 12.0;
}

inline SILENCE_PUBLIC double MIDDLE_C() {
    return 60.0;
}

inline SILENCE_PUBLIC double C4() {
    return MIDDLE_C();
}

/**
 * Returns the pitch transposed by semitones, which may be any scalar.
 * NOTE: Does NOT return an equivalent under any requivalence relation.
 */
inline SILENCE_PUBLIC double T(double pitch, double semitones) {
    return pitch + semitones;
}

/**
 * Returns the pitch reflected in the center, which may be any pitch.
 * NOTE: Does NOT return an equivalent under any requivalence relation.
 */
inline SILENCE_PUBLIC double I(double pitch, double center = 0.0) {
    return center - pitch;
}

/**
 * Returns the remainder of the dividend divided by the divisor,
 * according to the Euclidean definition.
 */
inline SILENCE_PUBLIC double modulo(double dividend, double divisor) {
    double quotient = 0.0;
    if (divisor < 0.0) {
        quotient = std::ceil(dividend / divisor);
    }
    if (divisor > 0.0) {
        quotient = std::floor(dividend / divisor);
    }
    double remainder = dividend - (quotient * divisor);
    return remainder;
}

/**
 * Returns the equivalent of the pitch under pitch-class equivalence, i.e.
 * the pitch is in the interval [0, OCTAVE). Implemented using the Euclidean
 * definition.
 */
inline SILENCE_PUBLIC double epc(double pitch) {
    double pc = modulo(pitch, OCTAVE());
    return pc;
}

/**
 * Enums for all defined equivalence relations,
 * used to specialize template functions.
 * If relation R takes no range argument,
 * it defaults to a range of one octave.
 * T is transposition to layer 0,
 * Tg is transposition to the layer close as one
 * can get to layer 0 but all chord pitches are
 * generated by g (default = 1 semitone).
 *
 * NOTE: Not all of these are currently implemented.
 */
typedef enum {
    EQUIVALENCE_RELATION_r = 0,
    EQUIVALENCE_RELATION_R,
    EQUIVALENCE_RELATION_P,
    EQUIVALENCE_RELATION_T,
    EQUIVALENCE_RELATION_Tg,
    EQUIVALENCE_RELATION_I,
    EQUIVALENCE_RELATION_V,
    EQUIVALENCE_RELATION_RP,
    EQUIVALENCE_RELATION_RT,
    EQUIVALENCE_RELATION_RTg,
    EQUIVALENCE_RELATION_RI,
    EQUIVALENCE_RELATION_PT,
    EQUIVALENCE_RELATION_PTg,
    EQUIVALENCE_RELATION_PI,
    EQUIVALENCE_RELATION_TI,
    EQUIVALENCE_RELATION_RPT,
    EQUIVALENCE_RELATION_RPTg,
    EQUIVALENCE_RELATION_RPI,
    EQUIVALENCE_RELATION_RTI,
    EQUIVALENCE_RELATION_RTgI,
    EQUIVALENCE_RELATION_RPTI,
    EQUIVALENCE_RELATION_RPTgI,
} EQUIVALENCE_RELATIONS;

static const char* namesForEquivalenceRelations[] = {
    "r",
    "R",
    "P",
    "T",
    "Tg",
    "I",
    "V",
    "RP",
    "RT",
    "RTg",
    "RI",
    "PT",
    "PTg",
    "PI",
    "TI",
    "RPT",
    "RPTg",
    "RPI",
    "RTI",
    "RTgI",
    "RPTI",
    "RPTgI"
};

// Forward declarations:

SILENCE_PUBLIC std::vector<std::string> split(std::string);
class SILENCE_PUBLIC Chord;
class SILENCE_PUBLIC Scale;
SILENCE_PUBLIC double euclidean(const csound::Chord &a, const csound::Chord &b);
SILENCE_PUBLIC double pitchClassForName(std::string name);
SILENCE_PUBLIC std::string nameForPitchClass(double pitch);
SILENCE_PUBLIC std::string nameForChord(const Chord &chord);
SILENCE_PUBLIC const Chord &chordForName(std::string name);
SILENCE_PUBLIC std::string nameForScale(const Scale &scale);
SILENCE_PUBLIC const Scale &scaleForName(std::string name);
SILENCE_PUBLIC std::multimap<Chord, std::string> &namesForChords();
SILENCE_PUBLIC std::multimap<Scale, std::string> &namesForScales();
SILENCE_PUBLIC std::map<std::string, Scale> &scalesForNames();
SILENCE_PUBLIC void add_chord(std::string, const Chord &chord);
SILENCE_PUBLIC void add_scale(std::string, const Scale &scale);
SILENCE_PUBLIC std::set<Chord> &unique_chords();
SILENCE_PUBLIC std::set<Scale> &unique_scales();

// Equivalence relations are implemented first as template functions at namespace scope,
// and then as class member functions delegating to the corresponding namespace functions.

template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isNormal(const Chord &chord,
        double range, double g);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isNormal(const Chord &chord,
        double range);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isNormal(const Chord &chord);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isEquivalent(const Chord &a,
        const Chord &b,
        double range, double g);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isEquivalent(const Chord &a,
        const Chord &b,
        double range);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isEquivalent(const Chord &a,
        const Chord &b);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC Chord normalize(const Chord &chord,
        double range, double g);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC Chord normalize(const Chord &chord,
        double range);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC Chord normalize(const Chord &chord);

template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC std::set<Chord> allNormalizedFundamentalDomain(int voices, double range, double g);
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC std::set<Chord> uniqueNormalizedFundamentalDomain(int voices, double range, double g);

/**
 * Returns a chord with the specified number of voices all set to a first
 * pitch, useful as an iterator.
 */
SILENCE_PUBLIC Chord iterator(int voiceN, double first);
/**
 * Increment a chord voicewise through chord space,
 * from a low point on the unison diagonal through a high point
 * on the unison diagonal. g is the generator of transposition.
 * It may be necessary to set the chord to the low point to start.
 */
SILENCE_PUBLIC bool next(Chord &odometer, const Chord &low, double high, double g = 1.0);
SILENCE_PUBLIC bool operator == (const Chord &a, const Chord &b);
SILENCE_PUBLIC bool operator < (const Chord &a, const Chord &b);
SILENCE_PUBLIC bool operator <= (const Chord &a, const Chord &b);
SILENCE_PUBLIC bool operator > (const Chord &a, const Chord &b);
SILENCE_PUBLIC bool operator >= (const Chord &a, const Chord &b);
/**
 * Chords represent simultaneously sounding pitches. The pitches are
 * represented as semitones with 0 at the origin and middle C as 60.
 * Each voice also has a duration, velocity, channel, and pan.
 * Eigen matrices are accessed (row, column) and stored as column
 * vectors, so a Chord is accessed (voice (same as row), attribute).
 */
class SILENCE_PUBLIC Chord : public Eigen::MatrixXd {
public:
    enum {
        PITCH = 0,
        DURATION = 1,
        LOUDNESS = 2,
        INSTRUMENT = 3,
        PAN = 4,
        COUNT = 5
    };
    Chord() {
        resize(0);
    }
    Chord(const Chord &other) {
        *this = other;
    }
    Chord(const std::vector<double> &other) {
        *this = other;
    }
    virtual Chord &operator = (const Chord &other) {
        Eigen::MatrixXd::operator=(other);
        return *this;
    }
    virtual Chord &operator = (const std::vector<double> &other) {
        auto voices_n = other.size();
        resize(voices_n);
        for (size_t voice = 0; voice < voices_n; ++voice) {
            setPitch(voice, other[voice]);
        }
        return *this;
    }
    virtual operator std::vector<double>() const {
        std::vector<double> result;
        result.resize(voices());
        for (size_t voice = 0; voice < voices(); ++voice) {
            result.push_back(getPitch(voice));
        }
        return result;
    }
#if __cpplusplus >= 201103L
    Chord &operator = (Chord &&other) = default;
#endif
    virtual ~Chord() {
    }
    virtual size_t voices() const {
        return rows();
    }
    virtual void resize(size_t voiceN) {
        Eigen::MatrixXd::resize(voiceN, COUNT);
    }
    /**
     * Returns a string representation of the chord's pitches (only).
     * Quadratic complexity, but short enough not to matter.
     */
    virtual std::string toString() const {
        char buffer[0x1000];
        std::stringstream stream;
        for (size_t voice = 0; voice < voices(); ++voice) {
            std::snprintf(buffer, 0x100, "%12.7f", getPitch(voice));
            if (voice > 0) {
                stream << " ";
            }
            stream << buffer;
        }
        return stream.str();
    }
    /**
     * Rebuilds the chord's pitches (only) from a line of text.
     */
    virtual void fromString(std::string text) {
        double scalar;
        std::vector<double> vector_;
        std::stringstream stream(text);
        while (stream >> scalar) {
            vector_.push_back(scalar);
        }
        resize(vector_.size());
        for (int i = 0, n = vector_.size(); i < n; ++i) {
            setPitch(i, vector_[i]);
        }
    }
    virtual double getPitch(int voice) const {
        return coeff(voice, PITCH);
    }
    virtual double &getPitchReference(int voice) {
        return coeffRef(voice, PITCH);
    }
    virtual void setPitch(int voice, double value) {
        coeffRef(voice, PITCH) = value;
    }
    virtual double getDuration(int voice = 0) const {
        return coeff(voice, DURATION);
    }
    virtual void setDuration(double value, int voice = -1) {
        if (voice == -1) {
            for (voice = 0; voice < rows(); ++voice) {
                coeffRef(voice, DURATION) = value;
            }
        } else {
            coeffRef(voice, DURATION) = value;
        }
    }
    virtual double getLoudness(int voice = 0) const {
        return coeff(voice, LOUDNESS);
    }
    virtual void setLoudness(double value, int voice = -1) {
        if (voice == -1) {
            for (voice = 0; voice < rows(); ++voice) {
                coeffRef(voice, LOUDNESS) = value;
            }
        } else {
            coeffRef(voice, LOUDNESS) = value;
        }
    }
    virtual double getInstrument(int voice = 0) const {
        return coeff(voice, INSTRUMENT);
    }
    virtual void setInstrument(double value, int voice = -1) {
        if (voice == -1) {
            for (voice = 0; voice < rows(); ++voice) {
                coeffRef(voice, INSTRUMENT) = value;
            }
        } else {
            coeffRef(voice, INSTRUMENT) = value;
        }
    }
    virtual double getPan(int voice = 0) const {
        return coeff(voice, PAN);
    }
    virtual void setPan(double value, int voice = -1) {
        if (voice == -1) {
            for (voice = 0; voice < rows(); ++voice) {
                coeffRef(voice, PAN) = value;
            }
        } else {
            coeffRef(voice, PAN) = value;
        }
    }
    virtual size_t count(double pitch) const {
        size_t n = 0;
        for (size_t voice = 0; voice < voices(); ++voice) {
            if (eq_epsilon(getPitch(voice), pitch)) {
                n++;
            }
        }
        return n;
    }
    /**
     * Returns whether or not the chord contains the pitch.
     */
    virtual bool contains(double pitch_) const {
        for (size_t voice = 0; voice < voices(); voice++) {
            if (eq_epsilon(getPitch(voice), pitch_)) {
                return true;
            }
        }
        return false;
    }
    /**
    * Returns the lowest pitch in the chord,
    * and also its voice index.
    */
    virtual std::vector<double> min() const {
        std::vector<double> result;
        result.resize(2);
        int voice = 0;
        double pitch = getPitch(voice);
        result[0] = pitch;
        result[1] = double(voice);
        for (int voice = 1; voice < voices(); voice++) {
            double pitch = getPitch(voice);
            if (lt_epsilon(pitch, result[0])) {
                result[0] = pitch;
                result[1] = double(voice);
            }
        }
        return result;
    }
    /**
    * Returns the highest pitch in the chord,
    * and also its voice index.
    */
    virtual std::vector<double> max() const {
        std::vector<double> result(2);
        int voice = 0;
        double pitch = getPitch(voice);
        result[0] = pitch;
        result[1] = double(voice);
        for (voice = 1; voice < voices(); voice++) {
            pitch = getPitch(voice);
            if (gt_epsilon(pitch, result[0])) {
                result[0] = pitch;
                result[1] = double(voice);
            }
        }
        return result;
    }
    virtual double minimumInterval() const {
        double minimumInterval_ = std::abs(getPitch(0) - getPitch(1));
        for (size_t v1 = 0; v1 < voices(); v1++) {
            for (size_t v2 = 0; v2 < voices(); v2++) {
                double interval = std::abs(getPitch(v1) - getPitch(v2));
                if (lt_epsilon(interval, minimumInterval_)) {
                    minimumInterval_ = interval;
                }
            }
        }
        return minimumInterval_;
    }
    virtual double maximumInterval() const {
        double maximumInterval_ = std::abs(getPitch(0) - getPitch(1));
        for (size_t v1 = 0; v1 < voices(); v1++) {
            for (size_t v2 = 0; v2 < voices(); v2++) {
                double interval = std::abs(getPitch(v1) - getPitch(v2));
                if (gt_epsilon(interval, maximumInterval_)) {
                    maximumInterval_ = interval;
                }
            }
        }
        return maximumInterval_;
    }
    /**
     * Returns a new chord whose pitches are the floors of this chord's pitches.
     */
    virtual Chord floor() const {
        Chord clone = *this;
        for (size_t voice = 0; voice  < voices(); voice++) {
            clone.setPitch(voice, std::floor(getPitch(voice)));
        }
        return clone;
    }
    /**
     * Returns a new chord whose pitches are the ceilings of this chord's pitches.
     */
    virtual Chord ceiling() const {
        Chord clone = *this;
        for (size_t voice = 0; voice  < voices(); voice++) {
            clone.setPitch(voice, std::ceil(getPitch(voice)));
        }
        return clone;
    }
    /**
     * Returns the origin of the chord's space.
     */
    virtual Chord origin() const {
        Chord clone;
        clone.resize(voices());
        return clone;
    }
    /**
     * Returns the Euclidean distance of this chord from its space's
     * origin.
     */
    virtual double distanceToOrigin() const {
        Chord origin_ = origin();
        return euclidean(*this, origin_);
    }
    /**
     * Returns the sum of the pitches in the chord.
     */
    virtual double layer() const {
        double sum = 0.0;
        for (size_t voice = 0; voice < voices(); ++voice) {
            sum += getPitch(voice);
        }
        return sum;
    }
    /**
     * Returns the Euclidean distance from this chord
     * to the unison diagonal of its chord space.
     */
    virtual double distanceToUnisonDiagonal() const {
        Chord unison = origin();
        double pitch = layer() / double(voices());
        for (size_t voice = 0; voice < voices(); voice ++) {
            unison.setPitch(voice, pitch);
        }
        return euclidean(*this, unison);
    }
    /**
     * Returns the maximally even chord in the chord's space,
     * e.g. the augmented triad for 3 dimensions.
     */
    virtual Chord maximallyEven() const {
        Chord clone = *this;
        double g = OCTAVE() / double(voices());
        for (size_t voice = 0; voice < voices(); voice++) {
            clone.setPitch(voice,  double(voice) * g);
        }
        return clone;
    }
    /**
     * Transposes the chord by the indicated interval (may be a fraction).
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord T(double interval) const {
        Chord clone = *this;
        for (size_t voice = 0; voice < voices(); voice++) {
            clone.setPitch(voice, csound::T(getPitch(voice), interval));
        }
        return clone;
    }
    /**
     * Transposes the chord by the indicated voiceleading (passed as a Chord 
     * of directed intervals). 
     * NOTE: Does NOT return an equivalent under any equivalence relation.
     */
    virtual Chord T_voiceleading(const Chord &voiceleading) {
        Chord clone = *this;
        for (size_t voice = 0; voice < voices(); voice++) {
            clone.setPitch(voice, getPitch(voice) + voiceleading.getPitch(voice));
        }
        return clone;
    }
    /**
     * Returns the transpositions (as a Chord of directed intervals) that 
     * takes this chord to the destination chord.
     * NOTE: Makes no assumption that both chords are in the same equivalence 
     * class.
     */
    virtual Chord voiceleading(const Chord &destination) const {
        Chord voiceleading_ = *this;
        for (size_t voice = 0; voice < voices(); voice++) {
            voiceleading_.setPitch(voice, destination.getPitch(voice) - getPitch(voice));
        }
        return voiceleading_;
    }
    /**
     * Inverts the chord by another chord that is on the unison diagonal, by
     * default the origin.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord I(double center = 0.0) const {
        Chord inverse = *this;
        for (size_t voice = 0; voice < voices(); voice++) {
            inverse.setPitch(voice, csound::I(getPitch(voice), center));
        }
        return inverse;
    }
    /**
     * Returns a copy of the chord cyclically permuted by a stride, by default 1.
     * The direction of rotation is by default the same as musicians' first
     * inversion, second inversion, and so on; but negative sign will reverse
     * the direction of rotation.
     * + 1 is pop the front and push it on the back, shifting the middle down.
     * 0 1 2 3 4 => 1 2 3 4 0
     * - 1 is pop the back and push it on the front, shifting the middle up.
     * 0 1 2 3 4 => 4 0 1 2 3
     */
    virtual Chord cycle(int stride = 1) const {
        Chord permuted = *this;
        int voicesToPopAndPush = std::abs(stride) % voices();
        int voicesToShift = voices() - voicesToPopAndPush;
        if (stride < 0) {
            permuted.bottomRows(voicesToShift) = topRows(voicesToShift);
            permuted.topRows(voicesToPopAndPush) = bottomRows(voicesToPopAndPush);
        }
        if (stride > 0) {
            permuted.topRows(voicesToShift) = bottomRows(voicesToShift);
            permuted.bottomRows(voicesToPopAndPush) = topRows(voicesToPopAndPush);
        }
        return permuted;
    }
    /**
     * Returns the permutations of the pitches in a chord. The permutations
     * starting from any particular permutation are always returned in the same order.
     */
    virtual std::vector<Chord> permutations() const {
        std::vector<Chord> permutations_;
        Chord permutation = *this;
        permutations_.push_back(permutation);
        for (size_t i = 1; i < voices(); i++) {
            permutation = permutation.cycle();
            permutations_.push_back(permutation);
        }
        std::sort(permutations_.begin(), permutations_.end());
        return permutations_;
    }
    /**
     * Returns a copy of the chord 'inverted' in the musician's sense,
     * i.e. revoiced by cyclically permuting the chord and
     * adding (or subtracting) an octave to the highest (or lowest) voice.
     * The revoicing will move the chord up or down in pitch.
     * A positive direction is the same as a musician's first inversion,
     * second inversion, etc.
     */
    virtual Chord v(int direction = 1) const {
        Chord chord = *this;
        int head = voices() - 1;
        while (direction > 0) {
            chord = chord.cycle(1);
            chord.setPitch(head, chord.getPitch(head) + OCTAVE());
            direction--;
        }
        while (direction < 0) {
            chord = chord.cycle(-1);
            chord.setPitch(0, chord.getPitch(0) + OCTAVE());
            direction++;
        }
        return chord;
    }
    /**
     * Returns all the 'inversions' (in the musician's sense)
     * or octavewise revoicings of the chord.
     */
    virtual std::vector<Chord> voicings() const {
        Chord chord = *this;
        std::vector<Chord> voicings;
        voicings.push_back(chord);
        for (size_t voicing = 1; voicing < voices(); voicing++) {
            chord = chord.v();
            voicings.push_back(chord);
        }
        return voicings;
    }
    /**
     * Returns whether the chord is within the fundamental domain of
     * pitch-class equivalence, i.e. is a pitch-class set.
     */
    virtual bool isepcs() const {
        for (size_t voice = 0; voice < voices(); voice++) {
            if (!eq_epsilon(getPitch(voice), epc(getPitch(voice)))) {
                return false;
            }
        }
        return true;
    }
    /**
     * Returns the equivalent of the chord under pitch-class equivalence,
     * i.e. the pitch-class set of the chord.
     */
    virtual Chord epcs() const {
        Chord chord = *this;
        for (size_t voice = 0; voice < voices(); voice++) {
            chord.setPitch(voice, epc(getPitch(voice)));
        }
        return chord;
    }
    /**
     * Returns whether the chord is within the fundamental domain of
     * transposition to 0.
     */
    virtual bool iset() const {
        Chord et_ = et();
        if (!(*this == et_)) {
            return false;
        }
        return true;
    }
    /**
     * Returns the equivalent of the chord within the fundamental domain of
     * transposition to 0.
     */
    virtual Chord et() const {
        double min_ = min()[0];
        return T(-min_);
    }
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of the indicated range equivalence.
     */
    virtual bool iseR(double range_) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave equivalence.
     */
    virtual bool iseO() const {
        return iseR(OCTAVE());
    }
    /**
     * Returns the equivalent of the chord within the representative
     * fundamental domain of a range equivalence.
     */
    virtual Chord eR(double range) const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave equivalence.
     */
    virtual Chord eO() const {
        return eR(OCTAVE());
    }
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of permutational equivalence.
     */
    virtual bool iseP() const;
    /**
     * Returns the equivalent of the chord within the representative
     * fundamental domain of permutational equivalence.	The implementation
     * uses a bubble sort to swap out of order voices in the Eigen matrix.
     */
    virtual Chord eP() const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of transpositional equivalence.
     */
    virtual bool iseT() const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of transpositonal equivalence.
     */
    virtual Chord eT() const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of transpositonal equivalence and the equal temperament generated
     * by g. I.e., returns the chord transposed such that its layer is 0 or, under
     * transposition, the positive layer closest to 0. NOTE: Does NOT return the
     * result under any other equivalence class.
     */
    virtual Chord eTT(double g = 1.0) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of translational equivalence and the equal temperament generated by g.
     */
    virtual bool iseTT(double g = 1.0) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of inversional equivalence.
     */
    virtual bool iseI_chord(Chord *inverse) const;
    virtual bool iseI() const {
        return iseI_chord(nullptr);
    }
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of inversional equivalence.
     */
    virtual Chord eI() const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range and permutational equivalence.
     */
    virtual bool iseRP(double range) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave and permutational equivalence.
     */
    virtual bool iseOP() const {
        return iseRP(OCTAVE());
    }
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range and permutational equivalence.
     */
    virtual Chord eRP(double range) const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave and permutational equivalence.
     */
    virtual Chord eOP() const {
        return eRP(OCTAVE());
    }
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of voicing equivalence.
     */
    virtual bool iseV() const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of voicing equivalence.
     */
    virtual Chord eV() const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range, permutational, and transpositional equivalence.
     */
    virtual bool iseRPT(double range) const;
    virtual bool iseRPTT(double range, double g = 1.0) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, and transpositional equivalence.
     */
    virtual bool iseOPT() const {
        return iseRPT(OCTAVE());
    }
    virtual bool iseOPTT(double g = 1.0) const {
        return iseRPTT(OCTAVE(), g);
    }
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, and transpositional equivalence; the same
     * as set-class type, or chord type.
     */
    virtual Chord eRPT(double range) const;
    virtual Chord eRPTT(double range, double g = 1.0) const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave, permutational, and transpositional equivalence.
     */
    virtual Chord eOPT() const {
        return eRPT(OCTAVE());
    }
    virtual Chord eOPTT(double g = 1.0) const {
        return eRPTT(OCTAVE(), g);
    }
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range, permutational, and inversional equivalence.
     */
    virtual bool iseRPI(double range) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, and inversional equivalence.
     */
    virtual bool iseOPI() const {
        return iseRPI(OCTAVE());
    }
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, and inversional equivalence.
     */
    virtual Chord eRPI(double range) const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave, permutational, and inversional equivalence.
     */
    virtual Chord eOPI() const {
        return eRPI(OCTAVE());
    }
    /** Returns whether the chord is within the representative fundamental domain
     * of range, permutational, transpositional, and inversional equivalence.
     */
    virtual bool iseRPTI(double range) const;
    virtual bool iseRPTTI(double range) const;
    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, transpositional, and inversional equivalence.
     */
    virtual bool iseOPTI() const {
        return iseRPTI(OCTAVE());
    }
    virtual bool iseOPTTI() const {
        return iseRPTTI(OCTAVE());
    }
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, transpositional, and inversional
     * equivalence.
     */
    virtual Chord eRPTI(double range) const;
    virtual Chord eRPTTI(double range) const;
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, transpositional, and inversional
     * equivalence.
     */
    virtual Chord eOPTI() const {
        return eRPTI(OCTAVE());
    }
    virtual Chord eOPTTI() const {
        return eRPTTI(OCTAVE());
    }
    virtual std::string name() const {
        std::string name_ = nameForChord(*this);
        return name_;
    }
    virtual std::string information() const;
    /**
     * Move 1 voice of the chord.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord move(int voice, double interval) const {
        Chord chord = *this;
        chord.setPitch(voice, csound::T(getPitch(voice), interval));
        return chord;
    }
    /**
     * Performs the neo-Riemannian Lettonwechsel transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrL() const {
        // TODO: Is this right for anything but triads and sevenths?
        Chord cv = eV();
        Chord cvt = eV().et();
        if (cvt.getPitch(1) == 4.0) {
            cv.setPitch(0, cv.getPitch(0) - 1.0);
        } else {
            if (cvt.getPitch(1) == 3.0) {
                cv.setPitch(2, cv.getPitch(2) + 1.0);
            }
        }
        return cv;
    }
    /**
     * Performs the neo-Riemannian parallel transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrP() const {
        // TODO: Is this right for anything but triads and sevenths?
        Chord cv = eV();
        Chord cvt = eV().et();
        if (cvt.getPitch(1) == 4.0) {
            cv.setPitch(1, cv.getPitch(1) - 1.0);
        } else {
            if (cvt.getPitch(1) == 3.0) {
                cv.setPitch(1, cv.getPitch(1) + 1.0);
            }
        }
        return cv;
    }
    /**
     * Performs the neo-Riemannian relative transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrR() const {
        // TODO: Is this right for anything but triads and sevenths?
        Chord cv = eV();
        Chord cvt = eV().et();
        if (cvt.getPitch(1) == 4.0) {
            cv.setPitch(2, cv.getPitch(2) + 2.0);
        } else {
            if (cvt.getPitch(1) == 3.0) {
                cv.setPitch(0, cv.getPitch(0) - 2.0);
            }
        }
        return cv;
    }
    /**
     * Performs the neo-Riemannian Nebenverwandt transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrN() const {
        return nrR().nrL().nrP();
    }
    /**
     * Performs the neo-Riemannian Slide transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrS() const {
        return nrL().nrP().nrR();
    }
    /**
     * Performs the neo-Riemannian hexatonic pole transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrH() const {
        return nrL().nrP().nrL();
    }
    /**
     * Performs the neo-Riemannian dominant transformation.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord nrD() const {
        return T(-7.0);
    }
    /**
     * Returns the chord inverted by the sum of its first two voices.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord K(double range = OCTAVE()) const {
        Chord chord = *this;
        if (chord.voices() < 2) {
            return chord;
        }
        // Unordered and in [0, 12).
        Chord epc = epcs();
        double center = epc.getPitch(0) + epc.getPitch(1);
        return I(center);
    }
    /**
     * Returns whether the chord is a transpositional form of Y with interval size g.
     * Only works in equal temperament.
     */
    virtual bool Tform(const Chord &Y, double g = 1.0) const {
        Chord eopx = epcs().eP();
        double i = 0.0;
        while (i < OCTAVE()) {
            Chord ty = Y.T(i);
            Chord eopty = ty.epcs().eP();
            if (eopx == eopty) {
                return true;
            }
            i = i + g;
        }
        return false;
    }
    /**
     * Returns whether the chord is an inversional form of Y with interval size g.
     * Only works in equal temperament.
     */
    virtual bool Iform(const Chord &Y, double g = 1.0) const {
        Chord eopx = epcs().eP();
        double i = 0.0;
        while (i < OCTAVE()) {
            Chord iy = Y.I(i);
            Chord eopiy = iy.epcs().eP();
            if (eopx == eopiy) {
                return true;
            }
            i = i + g;
        }
        return false;
    }
    /**
     * Returns the contextual transposition of the chord by x with respect to m
     * with minimum interval size g.
     * NOTE: Does NOT return an equivalent under any requivalence relation.
     */
    virtual Chord Q(double x, const Chord &m, double g = 1.0) const {
        if (Tform(m, g)) {
            return T(x);
        }
        if (Iform(m, g)) {
            return T(-x);
        }
        return *this;
    }
    /**
     * Creates a complete "note on" Event for the
     * indicated voice of the chord. If the optional
     * duration, channel, velocity, and pan parameters
     * are not passed, then the Chord's own values for
     * these are used.
     */
    virtual Event note(int voice,
                       double time_,
                       double duration_ = DBL_MAX,
                       double channel_ = DBL_MAX,
                       double velocity_ = DBL_MAX,
                       double pan_ = DBL_MAX) const {
        Event note;
        note.setTime(time_);
        note.setKey(getPitch(voice));
        if (duration_ != DBL_MAX) {
            note.setDuration(duration_);
        } else {
            note.setDuration(getDuration(voice));
        }
        if (channel_ != DBL_MAX) {
            note.setInstrument(channel_);
        } else {
            note.setInstrument(getInstrument(voice));
        }
        if (velocity_ != DBL_MAX) {
            note.setVelocity(velocity_);
        } else {
            note.setVelocity(getLoudness(voice));
        }
        if (pan_ != DBL_MAX) {
            note.setPan(pan_);
        } else {
            note.setPan(getPan(voice));
        }
        return note;
    }
    /**
     * Returns an individual note for each voice of the chord.
     * If the optional
     * duration, channel, velocity, and pan parameters
     * are not passed, then the Chord's own values for
     * these are used.
     */
    virtual Score notes(double time_,
                        double duration_ = DBL_MAX,
                        double channel_ = DBL_MAX,
                        double velocity_ = DBL_MAX,
                        double pan_ = DBL_MAX) const {
        Score score;
        for (int voice = 0; voice < voices(); ++voice) {
            Event event = note(voice, time_, duration_, channel_, velocity_, pan_);
            score.append(event);
        }
        return score;
    }
    /**
     * Returns an individual note for each voice of the chord.
     * If the optional
     * duration, channel, velocity, and pan parameters
     * are not passed, then the Chord's own values for
     * these are used.
     */
    virtual void toScore(Score &score,
                         double time_, bool voiceIsInstrument=true) const {
        for (int voice = 0; voice < voices(); ++voice) {
            double instrument = double(voice);
            if (!voiceIsInstrument) {
                instrument = getInstrument(voice);
            }
            score.append(time_,
                         getDuration(voice),
                         144.0,
                         instrument,
                         getPitch(voice),
                         getLoudness(voice),
                         0.0,
                         getPan(voice));
        }
    }
    /**
     * Returns the ith arpeggiation, current voice, and corresponding revoicing
     * of the chord. Positive arpeggiations start with the lowest voice of the
     * chord and revoice up; negative arpeggiations start with the highest voice
     * of the chord and revoice down.
     */
    virtual Chord a(int arpeggiation, double &resultPitch, int &resultVoice) const {
        Chord resultChord = v(arpeggiation);
        if (arpeggiation < 0) {
            resultVoice = resultChord.voices() - 1;
        } else {
            resultVoice = 0;
        }
        resultPitch = resultChord.getPitch(resultVoice);
        return resultChord;
    }
    virtual bool equals(const Chord &other) const {
        return *this == other;
    }
};

inline SILENCE_PUBLIC bool operator == (const Chord &a, const Chord &b) {
    if (&a == &b) {
        return true;
    }
    if (a.voices() != b.voices()) {
        return false;
    }
    for (size_t voice = 0; voice < a.voices(); ++voice) {
        if (!eq_epsilon(a.getPitch(voice), b.getPitch(voice))) {
            return false;
        }
    }
    return true;
}

inline SILENCE_PUBLIC bool operator < (const Chord &a, const Chord &b) {
    size_t n = std::min(a.voices(), b.voices());
    for (size_t voice = 0; voice < n; voice++) {
        if (lt_epsilon(a.getPitch(voice), b.getPitch(voice))) {
            return true;
        }
        if (gt_epsilon(a.getPitch(voice), b.getPitch(voice))) {
            return false;
        }
    }
    if (a.voices() < b.voices()) {
        return true;
    }
    return false;
}

inline SILENCE_PUBLIC bool operator <= (const Chord &a, const Chord &b) {
    if (a == b) {
        return true;
    }
    return (a < b);
}

inline SILENCE_PUBLIC bool operator > (const Chord &a, const Chord &b) {
    size_t n = std::min(a.voices(), b.voices());
    for (size_t voice = 0; voice < n; voice++) {
        if (gt_epsilon(a.getPitch(voice), b.getPitch(voice))) {
            return true;
        }
        if (lt_epsilon(a.getPitch(voice), b.getPitch(voice))) {
            return false;
        }
    }
    if (a.voices() > b.voices()) {
        return true;
    }
    return false;
}

inline SILENCE_PUBLIC bool operator >= (const Chord &a, const Chord &b) {
    if (a == b) {
        return true;
    }
    return (a > b);
}

/**
 * Returns the Euclidean distance between chords a and b,
 * which must have the same number of voices.
 */
inline SILENCE_PUBLIC double euclidean(const Chord &a, const Chord &b) {
    double sumOfSquaredDifferences = 0.0;
    for (size_t voice = 0, voices = a.voices(); voice < voices; ++voice) {
        sumOfSquaredDifferences += std::pow((a.getPitch(voice) - b.getPitch(voice)), 2.0);
    }
    return std::sqrt(sumOfSquaredDifferences);
}

/**
 * Returns the chord that is the midpoint between two chords,
 * which must have the same number of voices.
 */
inline SILENCE_PUBLIC Chord midpoint(const Chord &a, const Chord &b) {
    Chord midpoint_ = a;
    for (int voice = 0, voices = a.voices(); voice < voices; ++voice) {
        double voiceSum = a.getPitch(voice) + b.getPitch(voice);
        double voiceMidpoint = voiceSum / 2.0;
        midpoint_.setPitch(voice, voiceMidpoint);
    }
    System::debug("a: %s  b: %s  mid: %s\n", a.toString().c_str(), b.toString().c_str(), midpoint_.toString().c_str());
    return midpoint_;
}


inline SILENCE_PUBLIC bool next(Chord &iterator_, const Chord &origin, double range, double g) {
    int leastSignificantVoice = iterator_.voices() - 1;
    int mostSignificantVoice = 0;
    // Increment, as in an odometer.
    iterator_.setPitch(leastSignificantVoice, iterator_.getPitch(leastSignificantVoice) + g);
    // If necessary, carry the increment to the next most significant voice.
    for (int voice = leastSignificantVoice; voice > mostSignificantVoice; --voice) {
        if (gt_epsilon(iterator_.getPitch(voice), (origin.getPitch(voice) + range))) {
            iterator_.setPitch(voice, origin.getPitch(voice));
            iterator_.setPitch(voice - 1, iterator_.getPitch(voice - 1) + g);
        }
    }
    if (gt_epsilon(iterator_.getPitch(mostSignificantVoice), (origin.getPitch(mostSignificantVoice) + range))) {
        return false;
    }
    return true;
}

/**
 * Returns a chord with the specified number of voices all set to a first
 * pitch, useful as an iterator.
 */
inline SILENCE_PUBLIC Chord iterator(int voiceN, double first) {
    Chord odometer;
    odometer.resize(voiceN);
    for (int voice = 0; voice < voiceN; ++voice) {
        odometer.setPitch(voice, first);
    }
    return odometer;
}

/**
 * TODO: Change this to use strictly the representative fundamental domains.
 * Each iteration must be sent to the representative fundamental domain, then
 * added to the set.
 */
inline SILENCE_PUBLIC std::vector<Chord> allOfEquivalenceClass(int voiceN, std::string equivalence, double g = 1.0) {
    std::set<Chord> equivalentChords;
    int chordCount = 0;
    int equivalentChordCount = 0;
    Chord origin = iterator(voiceN, -13.0);
    Chord iterator_ = origin;
    if (equivalence == "OP") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOP() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    if (equivalence == "OPT") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOPT() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    if (equivalence == "OPTT") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOPTT() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    if (equivalence == "OPI") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOPI() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    if (equivalence == "OPTI") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOPTI() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    if (equivalence == "OPTTI") {
        while (next(iterator_, origin, 13.0, g) == true) {
            chordCount++;
            Chord chord = iterator_.eP();
            if (chord.iseOPTTI() == true) {
                equivalentChordCount++;
                equivalentChords.insert(chord);
            }
        }
    }
    std::vector<Chord> result;
    std::copy(equivalentChords.begin(), equivalentChords.end(), std::back_inserter(result));
    return result;
}
/**
 * Returns the voice-leading between chords a and b,
 * i.e. what you have to add to a to get b, as a
 * chord of directed intervals.
 */
inline SILENCE_PUBLIC Chord voiceleading(const Chord &a, const Chord &b) {
    Chord voiceleading_ = a;
    for (int voice = 0; voice < a.voices(); ++voice) {
        voiceleading_.setPitch(voice, b.getPitch(voice) - a.getPitch(voice));
    }
    return voiceleading_;
}

/**
 * Returns whether the voiceleading
 * between chords a and b contains a parallel fifth.
 */
inline SILENCE_PUBLIC bool parallelFifth(const Chord &a, const Chord &b) {
    Chord voiceleading_ = voiceleading(a, b);
    if (voiceleading_.count(7) > 1) {
        return true;
    } else {
        return false;
    }
}

/**
 * Returns the smoothness of the voiceleading between
 * chords a and b by L1 norm.
 */
inline SILENCE_PUBLIC double voiceleadingSmoothness(const Chord &a, const Chord &b) {
    double L1 = 0.0;
    for (int voice = 0; voice < a.voices(); ++voice) {
        L1 = L1 + std::abs(b.getPitch(voice) - a.getPitch(voice));
    }
    return L1;
}

/**
 * Returns which of the voiceleadings (source to d1, source to d2)
 * is the smoother (shortest moves), optionally avoiding parallel fifths.
 */
inline SILENCE_PUBLIC Chord voiceleadingSmoother(const Chord &source, const Chord &d1, const Chord &d2, bool avoidParallels = false, double range = OCTAVE()) {
    if (avoidParallels) {
        if (parallelFifth(source, d1)) {
            return d2;
        }
        if (parallelFifth(source, d2)) {
            return d1;
        }
    }
    double s1 = voiceleadingSmoothness(source, d1);
    double s2 = voiceleadingSmoothness(source, d2);
    if (s1 <= s2) {
        return d1;
    } else {
        return d2;
    }
}
/**
 * Returns which of the voiceleadings (source to d1, source to d2)
 * is the simpler (fewest moves), optionally avoiding parallel fifths.
 */
inline SILENCE_PUBLIC Chord voiceleadingSimpler(const Chord &source, const Chord &d1, const Chord &d2, bool avoidParallels = false) {
    if (avoidParallels) {
        if (parallelFifth(source, d1)) {
            return d2;
        }
        if (parallelFifth(source, d2)) {
            return d1;
        }
    }
    // TODO: Verify this.
    int s1 = voiceleading(source, d1).count(0.0);
    int s2 = voiceleading(source, d2).count(0.0);
    if (s1 > s2) {
        return d1;
    }
    if (s2 > s1) {
        return d2;
    }
    return d1;
}

/**
 * Returns which of the voiceleadings (source to d1, source to d2)
 * is the closer (first smoother, then simpler), optionally avoiding parallel fifths.
 */
inline SILENCE_PUBLIC Chord voiceleadingCloser(const Chord &source, const Chord &d1, const Chord &d2, bool avoidParallels = false) {
    if (avoidParallels) {
        if (parallelFifth(source, d1)) {
            return d2;
        }
        if (parallelFifth(source, d2)) {
            return d1;
        }
    }
    double s1 = voiceleadingSmoothness(source, d1);
    double s2 = voiceleadingSmoothness(source, d2);
    if (s1 < s2) {
        return d1;
    }
    if (s2 > s1) {
        return d2;
    }
    return voiceleadingSimpler(source, d1, d2, avoidParallels);
}

/**
 * Returns the voicing of the destination which has the closest voice-leading
 * from the source within the range, optionally avoiding parallel fifths.
 */
inline SILENCE_PUBLIC Chord voiceleadingClosestRange(const Chord &source, const Chord &destination, double range, bool avoidParallels = false) {
    Chord destinationOP = destination.eOP();
    Chord d = destinationOP;
    Chord origin = source.eOP();
    Chord odometer = origin;
    while (next(odometer, origin, range, OCTAVE())) {
        Chord revoicing = odometer;
        for (int voice = 0; voice < revoicing.voices(); ++voice) {
            revoicing.setPitch(voice, revoicing.getPitch(voice) + destinationOP.getPitch(voice));
        }
        d = voiceleadingCloser(source, d, revoicing, avoidParallels);
    }
    return d;
}

/**
 * Returns the pitch from the chord that is closest to the pitch.
 */
inline SILENCE_PUBLIC double closestPitch(double pitch, const Chord &chord) {
    std::map<double, double> pitchesForDistances;
    for (int voice = 0; voice < chord.voices(); ++voice) {
        double chordPitch = chord.getPitch(voice);
        double distance = std::fabs(chordPitch - pitch);
        pitchesForDistances[distance] = chordPitch;
    }
    return pitchesForDistances.begin()->second;
}

/**
* Conform the pitch to the pitch-class set, but in its original register.
*/
inline SILENCE_PUBLIC double conformToPitchClassSet(double pitch, const Chord &pcs) {
    double pc_ = epc(pitch);
    double closestPc = closestPitch(pc_, pcs);
    double register_ = std::floor(pitch / OCTAVE()) * OCTAVE();
    double closestPitch = register_ + closestPc;
    return closestPitch;
}

/**
 * If the Event is a note, moves its pitch
 * to the closest pitch of the chord.
 * If octaveEquivalence is true (the default),
 * the pitch-class of the note is moved to the closest pitch-class
 * of the chord, i.e. keeping the note more or less in its original register;
 * otherwise, the pitch of the note is moved to the closest
 * absolute pitch of the chord.
 */
inline SILENCE_PUBLIC void conformToChord(Event &event, const Chord &chord, bool octaveEquivalence = true) {
    if (!event.isNoteOn()) {
        return;
    }
    double pitch = event.getKey();
    if (octaveEquivalence) {
        Chord pcs = chord.epcs();
        pitch = conformToPitchClassSet(pitch, pcs);
    } else {
        pitch = closestPitch(pitch, chord);
    }
    event.setKey(pitch);
}

/**
 * Inserts the notes of the chord into the score at the specified time.
 */
inline SILENCE_PUBLIC void insert(Score &score,
                                  const Chord &chord,
                                  double time_) {
    chord.toScore(score, time_);
}

/**
 * Returns a slice of the Score starting at the start time and extending up
 * to but not including the end time. The slice contains pointers to the Events
 * in the Score.
 */
inline SILENCE_PUBLIC std::vector<Event *> slice(Score &score, double startTime, double endTime) {
    std::vector<Event *> result;
    for (int i = 0, n = score.size(); i < n; ++i) {
        Event *event = &score[i];
        if (event->isNoteOn()) {
            double eventStart = event->getTime();
            if (eventStart >= startTime && eventStart < endTime) {
                result.push_back(event);
            }
        }
    }
    return result;
}

/**
 * For all the notes in the Score
 * beginning at or later than the start time,
 * and up to but not including the end time,
 * moves the pitch of the note to belong to the chord, using the
 * conformToChord function.
 */
inline SILENCE_PUBLIC void apply(Score &score, const Chord &chord, double startTime, double endTime, bool octaveEquivalence = true) {
    std::vector<Event *> slice_ = slice(score, startTime, endTime);
    for (int i = 0; i < slice_.size(); ++i) {
        Event &event = *slice_[i];
        conformToChord(event, chord, octaveEquivalence);
    }
}

/**
 * Returns a chord containing all the pitches of the score
 * beginning at or later than the start time,
 * and up to but not including the end time.
 */
inline SILENCE_PUBLIC Chord gather(Score &score, double startTime, double endTime) {
    std::vector<Event *> slice_ = slice(score, startTime, endTime);
    std::set<double> pitches;
    for (int i = 0; i < slice_.size(); ++i) {
        pitches.insert(slice_[i]->getKey());
    }
    Chord chord;
    chord.resize(pitches.size());
    int voice = 0;
    for (std::set<double>::iterator it = pitches.begin(); it != pitches.end(); ++it) {
        chord.setPitch(voice, *it);
        voice++;
    }
    return chord;
}

inline SILENCE_PUBLIC int octavewiseRevoicings(const Chord &chord,
        double range = OCTAVE()) {
    Chord origin = chord.eOP();
    Chord odometer = origin;
    // Enumerate the permutations.
    int voicings = 0;
    while (next(odometer, origin, range, OCTAVE())) {
        voicings = voicings + 1;
    }
    System::debug("octavewiseRevoicings: chord:    %s\n", chord.toString().c_str());
    System::debug("octavewiseRevoicings: eop:      %s\n", chord.eOP().toString().c_str());
    System::debug("octavewiseRevoicings: odometer: %s\n", odometer.toString().c_str());
    System::debug("octavewiseRevoicings: voicings: %5d\n", voicings);
    return voicings;
}

SILENCE_PUBLIC Chord octavewiseRevoicing(const Chord &chord, int revoicingNumber_, double range, bool debug=false);
/**
 * Returns the index of the octavewise revoicing that this chord is,
 * relative to its OP equivalent, within the indicated range. Returns
 * -1 if there is no such chord within the range.
 */
SILENCE_PUBLIC int indexForOctavewiseRevoicing(const Chord &chord, double range, bool debug=false);

/*
template<int EQUIVALENCE_RELATION> SILENCE_PUBLIC bool isNormal(const Chord &chord, double range, double g) {
	Chord normal = normalize<EQUIVALENCE_RELATION>(chord, range, g);
	if (normal == chord) {
		return true;
	} else {
		return false;
	}
}
*/

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC bool isNormal(const Chord &chord, double range) {
    bool result = isNormal<EQUIVALENCE_RELATION>(chord, range, 1.0);
    return result;
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC bool isNormal(const Chord &chord) {
    bool result = isNormal<EQUIVALENCE_RELATION>(chord, OCTAVE());
    return result;
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC bool isEquivalent(const Chord &a, const Chord &b, double range, double g) {
    if (isNormal<EQUIVALENCE_RELATION>(a, range, g) == false) {
        return false;
    }
    if (isNormal<EQUIVALENCE_RELATION>(b, range, g) == false) {
        return false;
    }
    Chord normalA = normalize<EQUIVALENCE_RELATION>(a, range, g);
    Chord normalB = normalize<EQUIVALENCE_RELATION>(b, range, g);
    if (normalA == normalB) {
        return true;
    } else {
        return false;
    }
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC bool isEquivalent(const Chord &a, const Chord &b, double range) {
    return isEquivalent<EQUIVALENCE_RELATION>(a, b, range, 1.0);
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC bool isEquivalent(const Chord &a, const Chord &b) {
    return isEquivalent<EQUIVALENCE_RELATION>(a, b, OCTAVE());
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC Chord normalize(const Chord &chord, double range) {
    return normalize<EQUIVALENCE_RELATION>(chord, range, 1.0);
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC Chord normalize(const Chord &chord) {
    return normalize<EQUIVALENCE_RELATION>(chord, OCTAVE());
}

//	EQUIVALENCE_RELATION_r

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_r>(const Chord &chord, double range, double g) {
    for (int voice = 0; voice < chord.voices(); ++voice) {
        double pitch = chord.getPitch(voice);
        if (le_epsilon(0.0, pitch) == false) {
            return false;
        }
        if (lt_epsilon(pitch, range) == false) {
            return false;
        }
    }
    return true;
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_r>(const Chord &chord, double range, double g) {
    Chord normal = chord;
    for (int voice = 0; voice < chord.voices(); ++voice) {
        double pitch = chord.getPitch(voice);
        pitch = modulo(pitch, range);
        normal.setPitch(voice, pitch);
    }
    return normal;
}

//	EQUIVALENCE_RELATION_R

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_R>(const Chord &chord, double range, double g) {
    double max = chord.max()[0];
    double min = chord.min()[0];
    if (le_epsilon(max, (min + range)) == false) {
        return false;
    }
    double layer = chord.layer();
    if (le_epsilon(0.0, layer) == false) {
        return false;
    }
    if (lt_epsilon(layer, range) == false) {
        return false;
    }
    return true;
}

inline bool Chord::iseR(double range_) const {
    return isNormal<EQUIVALENCE_RELATION_R>(*this, range_, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_R>(const Chord &chord, double range, double g) {
    Chord normal = normalize<EQUIVALENCE_RELATION_r>(chord, range, g);
    //Chord normal = chord;
    //while (le_epsilon(0.0, normal.layer()) == false) {
    //	std::vector<double> minimum = normal.min();
    //	normal.setPitch(minimum[1], minimum[0] + range);
    //}
    while (lt_epsilon(normal.layer(), range) == false) {
        std::vector<double> maximum = normal.max();
        normal.setPitch(maximum[1], maximum[0] - range);
    }
    return normal;
}

inline Chord Chord::eR(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_R>(*this, range, 1.0);
}

//	EQUIVALENCE_RELATION_P

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_P>(const Chord &chord, double range, double g) {
    for (size_t voice = 1; voice < chord.voices(); voice++) {
        if (gt_epsilon(chord.getPitch(voice - 1), chord.getPitch(voice))) {
            return false;
        }
    }
    return true;
}

inline bool Chord::iseP() const {
    return isNormal<EQUIVALENCE_RELATION_P>(*this, OCTAVE(), 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_P>(const Chord &chord, double range, double g) {
    Chord normal = chord;
    bool sorted = false;
    while (!sorted) {
        sorted = true;
        for (int voice = 1; voice < normal.voices(); voice++) {
            if (gt_epsilon(normal.getPitch(voice - 1), normal.getPitch(voice))) {
                sorted = false;
                normal.row(voice - 1).swap(normal.row(voice));
            }
        }
    }
    return normal;
}

inline Chord Chord::eP() const {
    return csound::normalize<EQUIVALENCE_RELATION_P>(*this, OCTAVE(), 1.0);
}

/**
 * Returns the named chord as a scale, that is, starting with the chord in OP, 
 * and sorting it from the tonic pitch-class on up. This enables 
 * transformations in tonal harmony such as transposing by scale degree. If no 
 * Chord exists for the name, an empty Chord is returned.
 */
inline SILENCE_PUBLIC Chord scale(std::string name) {
    System::debug("scale: for name: %s\n", name.c_str());
    auto scale = chordForName(name);
    if (scale.size() == 0) {
        return scale;
    }
    auto parts = split(name);
    auto tonic = pitchClassForName(parts.front());
    System::debug("scale: tonic: %9.4f\n", tonic);
    System::debug("scale: initially: %s\n", scale.toString().c_str());
    while (eq_epsilon(scale.getPitch(0), tonic) == false) {
        scale = scale.v();
        System::debug("scale: revoicing: %s\n", scale.toString().c_str());
    }
    return scale;
}

/**
 * Returns the chord, in scale order, for the specified degree of the scale.
 * The chord can be composed of seconds, thirds, or larger intervals, and 
 * can have two or more voices. The scale can have any number of pitch-classes  
 * and any interval content; it simply has to consists of pitches (not 
 * pitch-classes) sorted from the tonic pitch-class on up.
 * PLEASE NOTE: Scale degree is 1-based. A "third" is denoted "3" but is two 
 * scale degrees, and so on.
 */
inline SILENCE_PUBLIC Chord chord(const Chord &scale, int scale_degree, int chord_voices, int interval = 3) {
    int scale_index = scale_degree - 1;
    int scale_interval = interval - 1;
    Chord result;
    result.resize(chord_voices);
    double octave = 0.;
    for (int chord_voice = 0; chord_voice < chord_voices; ++chord_voice) {
        if (scale_index >= scale.voices()) {
            scale_index = scale_index - scale.voices();
            octave = octave + OCTAVE();
        }
        auto pitch = scale.getPitch(scale_index) + octave;
        result.setPitch(chord_voice, pitch);
        scale_index = scale_index + scale_interval;
    }
    return result;
}

/**
 * Returns the chord, in scale order, transposed within the scale by the 
 * indicated number of scale degrees, which can be positive or negative.
 * The original chord may be in any order or voicing. By default,
 * chords are generated by thirds, but they can be at any interval in scale 
 * degrees. If the original chord does not belong to the scale, an empty 
 * Chord is returned.
 */
inline SILENCE_PUBLIC Chord transpose_degrees(const Chord &scale, const Chord &original_chord, int transposition_degrees, int interval = 3) {
    int scale_degrees = scale.voices();
    int chord_voices = original_chord.voices();
    Chord original_eop = original_chord.eOP();
    for (int original_chord_index = 0; original_chord_index < scale_degrees; ++original_chord_index) {
        System::debug("transpose_degrees: original_chord_index: %d scale_degrees: %d\n", original_chord_index, scale_degrees);
        Chord transposed = csound::chord(scale, original_chord_index + 1, chord_voices, interval);
        Chord transposed_eop = transposed.eOP();
        System::debug("original_eop: %s\ntransposed_eop: %s\n", original_eop.information().c_str(), transposed_eop.information().c_str());
        if (original_eop == transposed_eop) {
            // Found the scale index of the original chord, now get the transposed chord.
            int target_index = original_chord_index + transposition_degrees;
            System::debug("found chord, target_index: %d original_chord_index: %d transposition_degrees: %d\n", target_index, original_chord_index, transposition_degrees);
            // Transposition has sign. If negative, wrap.
            while (target_index < 0) {
                target_index = target_index + scale_degrees;
            }
            System::debug("wrapped target_index: %d original_chord_index: %d transposition_degrees: %d\n", target_index, original_chord_index, transposition_degrees);
            Chord transposed_chord = csound::chord(scale, target_index + 1, chord_voices, interval);
            System::debug("transposed_chord: %s\n", transposed_chord.toString().c_str());
            return transposed_chord;
        }
    }
    Chord empty_chord;
    empty_chord.resize(0);
    return empty_chord;
}

/**
 * Scale as a class; must be created with the name of the scale. Inherits 
 * from Chord. Note that inherited Chord member functions such as T and I 
 * return Chords, not Scales.
 */
class SILENCE_PUBLIC Scale : public Chord {
    public:
        /**
         * Default constructor, an empty Scale.
         */
        Scale() {
            resize(0);
        }
        /**
         * Creates a Scale by name, e.g. 'C major'. If the named Scale does 
         * not already exist, an empty Scale without a name is created.
         */
        Scale(std::string name) {
            const Chord temporary = csound::scale(name);
            Eigen::MatrixXd::operator=(temporary);
            if (temporary.voices() > 0) {
                auto space_at = name.find(' ');
                type_name = name.substr(space_at + 1);
            }
        }
        /** 
         * Creates a Scale with a new name as a set of pitches. These must 
         * start in octave 0 and be in ascending order, but otherwise may have 
         * any value in semitones or fractions of semitones; this permits the 
         * construction of new scales with any temperament and with any 
         * interval content. If a Scale with the proposed name already exists, 
         * that Scale is returned. New Scales are also stored as new named 
         * Scales.
         */
        Scale(std::string name, const Chord &scale_pitches) {
            Scale temporary(name);
            if (temporary.voices() > 0) 
            {
                *this = temporary;
                return;
            } 
            resize(scale_pitches.size());
            for (int index = 0; index < voices(); ++index) {
                setPitch(index, scale_pitches.getPitch(index));
            }
            add_scale(name, *this);
        }
        /** 
         * Creates a Scale with a new name as a set of pitches. These must 
         * start in octave 0 and be in ascending order, but otherwise may have 
         * any value in semitones or fractions of semitones; this permits the 
         * construction of new scales with any temperament and with any 
         * interval content. If a Scale with the proposed name already exists, 
         * that Scale is replaced. New Scales are also stored as new named 
         * Scales.
         */
        Scale(std::string name, const std::vector<double> &scale_pitches) {
            resize(scale_pitches.size());
            for (int index = 0; index < voices(); ++index) {
                setPitch(index, scale_pitches[index]);
            }
            add_scale(name, *this);
        }
        virtual ~Scale() {};
        virtual Scale &operator = (const Scale &other) {
            Eigen::MatrixXd::operator=(dynamic_cast<const Chord &>(other));
            type_name = other.getTypeName();
            return *this;
        }
        /** 
         * Returns the chord for the indicated scale degree, number of voices
         * in the chord, and interval in scale degrees of the chord (defaults 
         * to thirds, or 3; the actual number of scale steps between chord 
         * pitches is interval - 1).
         */
        virtual Chord chord(int scale_degree, int voices, int interval = 3) const {
            return csound::chord(*this, scale_degree, voices, interval);
        }
        /**
         * Returns a Chord transposed by the indicated number of scale 
         * degrees; the chord as passed must belong to this Scale, and the
         * interval must be the same as that used to generate the Chord; 
         * (defaults to thirds, or 3; the actual number of scale steps between 
         * chord pitches is interval - 1).
         */
        virtual Chord transpose_degrees(const Chord &chord, int scale_degrees, int interval = 3) const {
            return csound::transpose_degrees(*this, chord, scale_degrees, interval);
        }
        /**
         * Returns the number of semitones (may be whole or fractional) from 
         * the tonic (as 0) of this Scale to the indicated scale degree, which 
         * is wrapped around by octave equivalence.
         */
        virtual double semitones_for_degree(int scale_degree) const {
            int scale_degrees = voices();
            while(scale_degree < 1) {
                scale_degree = scale_degree + scale_degrees;
            }
            while (scale_degree > scale_degrees) {
                scale_degree = scale_degree - scale_degrees;
            }
            double pitch_of_tonic = tonic();
            double pitch_of_scale_degree = getPitch(scale_degree - 1);
            double semitones = pitch_of_scale_degree - pitch_of_tonic;
            return semitones;
        }
        /** 
         * Returns a copy of this Scale transposed to the indicated 
         * _scale degree_. 
         */
        virtual Scale transpose_to_degree(int degrees) const {
            System::debug("Scale::transpose_to_degree(%9.4f)...\n", degrees);
            double semitones = semitones_for_degree(degrees);
            return transpose(semitones);
        }
        /** 
         * Returns a copy of this Scale transposed by the indicated number of 
         * _semitones_.
         */
        virtual Scale transpose(double semitones) const {
            Chord transposed_pitches = T(semitones);
            // Make sure the copy starts in octave 0.
            while (lt_epsilon(transposed_pitches.getPitch(0), 0) == true) {
                transposed_pitches = transposed_pitches.T(OCTAVE());
            }
            while (ge_epsilon(transposed_pitches.getPitch(0), OCTAVE()) == true) {
                transposed_pitches = transposed_pitches.T( - OCTAVE());
            }
            System::debug("Scale::transpose: transposed_pitches(%f): %s\n", semitones, transposed_pitches.toString().c_str());
            // Create the copy with the name of the new tonic.
            System::debug("Scale::transpose: original name: %s\n", name().c_str());
            auto tonic_name = nameForPitchClass(transposed_pitches.getPitch(0));
            Scale transposed_scale;
            transposed_scale.type_name = getTypeName();
            transposed_scale.resize(voices());
            for (int voice = 0; voice < voices(); ++voice) {
                transposed_scale.setPitch(voice, transposed_pitches.getPitch(voice));
            }
            System::debug("Scale::transpose: new name: %s\n", transposed_scale.name().c_str());
            System::debug("Scale::transpose: result: %s\n", transposed_scale.information().c_str());
            return transposed_scale;
        }
        /**
         * Returns the name of this Scale.
         */
        virtual std::string name() const {
            return nameForPitchClass(tonic()) + " " + type_name;
        }
        /**
         * Returns the type name, e.g. "major" or "whole tone," of this.
         * This name will probably be invalid if the interval structure of 
         * this has been changed, e.g. by inversion.  
         */
        virtual std::string getTypeName() const {
            return type_name;
        }
        /**
         * Returns the pitch-class that is the tonic or root of this Scale.
         */
        virtual double tonic() const {
            return getPitch(0);
        }
        /**
         * Returns the scale degree of the Chord in this Scale; if the 
         * Chord does not belong to this Scale, -1 is returned.
         */
        virtual int degree(const Chord &chord_, int interval = 3) const {
            int chord_voices = chord_.voices();
            int scale_degrees = voices();
            Chord eop = chord_.eOP();
            for (int scale_degree = 1; scale_degree <= scale_degrees; ++scale_degree) {
                Chord chord_for_degree_eop = chord(scale_degree, chord_voices, interval).eOP();
                if (eop == chord_for_degree_eop) {
                    return scale_degree;
                }
            }
            return -1;
        }
        /**
         * For any Chord belonging to this Scale, returns in the argument a 
         * list of other Scales to which that Chord also belongs. Switching to 
         * one of these Scales will perform some sort of modulation. The list 
         * of scale type names restricts the types of Scale that will be 
         * returned.
         */
        virtual void modulations_for_scale_types(std::vector<Scale> &result, const Chord &current_chord, int voices_, const std::vector<std::string> &type_names) const {
            result.clear();
            int current_degree = degree(current_chord);
            if (current_degree == -1) {
                return;
            }
            if (voices_ == -1) {
                voices_ = current_chord.voices();
            }
            Chord chord_ = chord(current_degree, voices_);
            for (auto scale : unique_scales()) {
                if (scale.degree(chord_) != -1) {
                    if (std::find(type_names.begin(), type_names.end(), scale.getTypeName()) != type_names.end()) {
                        if (std::find(result.begin(), result.end(), scale) == result.end()) {
                            result.push_back(scale);
                        }
                    }
                }
            }
        }
        /**
         * Returns a list of common modulations, that is, other major or 
         * harmonic minor Scales to which the Chord belongs; optionally the 
         * Chord can first be resized (e.g. from a 9th chord to a triad) in 
         * order to find more or fewer possible modulations.
         */
        virtual std::vector<Scale> modulations(const Chord &chord, int voices = -1) const {
            std::vector<Scale> result;
            std::vector<std::string> type_names;
            type_names.push_back("major");
            type_names.push_back("harmonic minor");
            modulations_for_scale_types(result, chord, voices, type_names);
            return result;
        }
        /**
         * Returns the _relative_ tonicizations of the Chord, that is, the 
         * scales for which that Chord could be mutated to have the secondary 
         * function, if that is possible. The list of scale types is used to 
         * restrict the types of Scales that are returned.
         */
        virtual void relative_tonicizations_for_scale_types(std::vector<Scale> &result, const Chord &current_chord, int secondary_function, int voices, const std::vector<std::string> &type_names) const {
            result.clear();
            int current_degree = degree(current_chord);
            System::debug("Scale::relative_tonicizations: chord: %.20s (%s) degree: %3d\n", current_chord.name().c_str(), current_chord.toString().c_str(), current_degree);
            if (current_degree == -1) {
                return;
            }
             if (voices == -1) {
                voices = current_chord.voices();
            }
            Chord chord_ = chord(current_degree, voices);
            System::debug("Scale::relative_tonicizations: resized: %.20s (%s) degree: %3d\n", chord_.name().c_str(), chord_.toString().c_str(), current_degree);
            std::vector<Scale> modulations_ = modulations(chord_);
            for (auto modulation : modulations_) {
                int degree_in_modulation = modulation.degree(chord_);
                if (degree_in_modulation == secondary_function) {
                    if (std::find(result.begin(), result.end(), modulation) == result.end()) {
                        System::debug("Scale::relative_tonicizations: modulation: %.20s (%s) degree of chord in modulation: %3d\n", modulation.name().c_str(), modulation.toString().c_str(), degree_in_modulation);
                        result.push_back(modulation);
                    }
                }
            }
        }
        /**
         * Returns a list of common relative tonicizations for the Chord, that 
         * is, the other major or harmonic minor Scales for which that Chord 
         * could be mutated to have the secondary function. If that is not 
         * possible, an empty result is returned.
         */
        virtual std::vector<Scale> relative_tonicizations(const Chord &current_chord, int secondary_function = 5, int voices = -1) const {
            std::vector<Scale> result;
            std::vector<std::string> scale_types = {"major", "harmonic minor"};
            relative_tonicizations_for_scale_types(result, current_chord, secondary_function, voices, scale_types);
            return result;
        }
        /**
         * Returns the current Chord mutated, if possible, to one or more 
         * function(s) with respect to another Chord in its Scale. Not 
         * "secondary function of this chord," but "this chord as secondary 
         * function of another (tonicized) chord." If that is not 
         * possible, an empty Chord is returned. The number of voices 
         * defaults to that of the current Chord. Can be used to generate 
         * secondary dominants (function = 5), secondary supertonics 
         * (function = 2), secondary subtonics (function = 6), and so on.
         * It is then up to the user to perform an appropriate progression 
         * by number of scale degrees in the original Scale.
         */
        virtual std::vector<Chord> secondary(const Chord &current_chord, int secondary_function = 5, int voices_ = -1) const {
            if (voices_ == -1) {
                voices_ = current_chord.voices();
            }
            std::vector<Scale> relative_tonicizations_ = relative_tonicizations(current_chord, secondary_function);
            std::vector<Chord> result;
            for (auto tonicization : relative_tonicizations_) {
                Chord mutation = tonicization.chord(secondary_function, voices_);
                    if (std::find(result.begin(), result.end(), mutation) == result.end()) {
                        result.push_back(mutation);
                    }
            }
            return result;
        }
        /**
         * Returns all major or minor Scales for which the current Chord is 
         * the tonic (scale degree 1). The number of voices defaults to that 
         * of the current Chord, but may be larger or smaller.
         * NOTE: Here, tonicizations are modulations in which the Chord has 
         * degree 1, i.e. is the tonic chord.
         */
        virtual std::vector<Scale> tonicizations(const Chord &current_chord, int voices = -1) const {
            std::vector<Scale> result;
            int current_degree = degree(current_chord);
            System::debug("Scale::tonicizations: chord: %.20s (%s) degree: %3d\n", current_chord.name().c_str(), current_chord.toString().c_str(), current_degree);
            if (current_degree == -1) {
                return result;
            }
            if (voices == -1) {
                voices = current_chord.voices();
            }
            Chord chord_ = chord(current_degree, voices);
            System::debug("Scale::tonicizations: resized: %.20s (%s) degree: %3d\n", chord_.name().c_str(), chord_.toString().c_str(), current_degree);
            std::vector<Scale> modulations_ = modulations(chord_);
            for (auto modulation : modulations_) {
                int degree_in_modulation = modulation.degree(chord_);
                if (degree_in_modulation == 1) {
                    if (std::find(result.begin(), result.end(), modulation) == result.end()) {
                        System::debug("Scale::tonicizations: modulation: %.20s (%s) degree of chord in modulation: %3d\n", modulation.name().c_str(), modulation.toString().c_str(), degree_in_modulation);
                        result.push_back(modulation);
                    }
                }
            }
            return result;
        }
    protected:
        std::string type_name;
};

//	EQUIVALENCE_RELATION_T

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_T>(const Chord &chord, double range, double g) {
    double layer_ = chord.layer();
    if (!(eq_epsilon(layer_, 0.0))) {
        return false;
    } else {
        return true;
    }
}

inline bool Chord::iseT() const {
    return isNormal<EQUIVALENCE_RELATION_T>(*this, OCTAVE(), 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_T>(const Chord &chord, double range, double g) {
    Chord normal = chord;
    double layer_ = normal.layer();
    double sumPerVoice = layer_ / double(normal.voices());
    normal = normal.T(-sumPerVoice);
    return normal;
}

inline Chord Chord::eT() const {
    return csound::normalize<EQUIVALENCE_RELATION_T>(*this, OCTAVE(), 1.0);
}

//	EQUIVALENCE_RELATION_Tg

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_Tg>(const Chord &chord, double range, double g) {
    Chord normal = normalize<EQUIVALENCE_RELATION_T>(chord, range, g);
    // Make it work for any g not just 1: double transposition = std::ceil(normal.getPitch(0)) - normal.getPitch(0);
    double ng = std::ceil(normal.getPitch(0) / g);
    double transposition = (ng * g) - normal.getPitch(0);
    normal = normal.T(transposition);
    return normal;
}

inline Chord Chord::eTT(double g) const {
    return csound::normalize<EQUIVALENCE_RELATION_Tg>(*this, OCTAVE(), g);
}

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_Tg>(const Chord &chord, double range, double g) {
    Chord normalP = normalize<EQUIVALENCE_RELATION_P>(chord, range, g);
    Chord normalPTg = normalize<EQUIVALENCE_RELATION_Tg>(normalP, range, g);
    if (normalP == normalPTg) {
        return true;
    } else {
        return false;
    }
}

inline bool Chord::iseTT(double g) const {
    return isNormal<EQUIVALENCE_RELATION_Tg>(*this, OCTAVE(), g);
}

//	EQUIVALENCE_RELATION_I

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_I>(const Chord &chord, double range, double g) {
    int lowerVoice = 1;
    int upperVoice = chord.voices() - 1;
    // Compare the intervals in a chord with those in its inverse,
    // starting with the "first." This is NOT the same as
    // whether the chord is less than or equal to its inverse.
    while (lowerVoice < upperVoice) {
        int lowerInterval = chord.getPitch(lowerVoice) - chord.getPitch(lowerVoice - 1);
        int upperInterval = chord.getPitch(upperVoice) - chord.getPitch(upperVoice - 1);
        if (lt_epsilon(lowerInterval, upperInterval)) {
            return true;
        }
        if (gt_epsilon(lowerInterval, upperInterval)) {
            return false;
        }
        lowerVoice = lowerVoice + 1;
        upperVoice = upperVoice - 1;
    }
    return true;
}

inline bool Chord::iseI_chord(Chord *inverse) const {
    return isNormal<EQUIVALENCE_RELATION_I>(*this, OCTAVE(), 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_I>(const Chord &chord, double range, double g) {
    if (isNormal<EQUIVALENCE_RELATION_I>(chord, range, g)) {
        return chord;
    } else {
        return chord.I();
    }
}

inline Chord Chord::eI() const {
    return csound::normalize<EQUIVALENCE_RELATION_I>(*this, OCTAVE(), 1.0);
}

//	EQUIVALENCE_RELATION_V

// Returns whether the chord is within the representative fundamental 
// domain of voicing equivalence. I think this identifies which simplex 
// the voicing is in. In Tymoczko's 1-based notation:
// x[1] + 12 - x[N] <= x[i + 1] - x[i], 1 <= i < N - 1
// In 0-based notation:
// x[0] + 12 - x[N-1] <= x[i + 1] - x[i], 0 <= i < N - 2
template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_V>(const Chord &chord, double range, double g) {
    double outer_interval = chord.getPitch(0) + range - chord.getPitch(chord.voices() - 1);
    for (size_t voice = 0, n = chord.voices() - 2; voice < n; ++voice) {
        double inner_interval = chord.getPitch(voice + 1) - chord.getPitch(voice);
        if (le_epsilon(outer_interval, inner_interval) == false) {
            return false;
        }
    }
    return true;
}

inline bool Chord::iseV() const {
    return isNormal<EQUIVALENCE_RELATION_V>(*this, OCTAVE(), 1.0);
}

// Tymoczko: Consider the dominant seventh chord -- take C7, which is {0, 4, 
// 7, 10}. Now put the thing in ascending order (0, 4, 7, 10).  Now rotate 
// until the smallest interval is between the first and second notes (10, 0, 
// 4, 7). [This implies octavewise revoicing so the chord is always ordered.]
template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_V>(const Chord &chord, double range, double g) {
    const std::vector<Chord> voicings = chord.voicings();
    for (size_t i = 0; i < voicings.size(); i++) {
        const Chord &voicing = voicings[i];
        if (isNormal<EQUIVALENCE_RELATION_V>(voicing, range, g)) {
            return voicing;
        }
    }
    throw "normalize<EQUIVALENCE_RELATION_V>: no voicing equivalent found.\n";
}

inline Chord Chord::eV() const {
    return csound::normalize<EQUIVALENCE_RELATION_V>(*this, OCTAVE(), 1.0);
}

//  EQUIVALENCE_RELATION_RP

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RP>(const Chord &chord, double range, double g) {
    if (!isNormal<EQUIVALENCE_RELATION_P>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_R>(chord, range, g)) {
        return false;
    }
    return true;
}

inline bool Chord::iseRP(double range) const {
    return isNormal<EQUIVALENCE_RELATION_RP>(*this, range, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RP>(const Chord &chord, double range, double g) {
    Chord normal = normalize<EQUIVALENCE_RELATION_R>(chord, range, g);
    normal = normalize<EQUIVALENCE_RELATION_P>(normal, range, g);
    return normal;
}

inline Chord Chord::eRP(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_RP>(*this, range, 1.0);
}

//	EQUIVALENCE_RELATION_RT

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RT>(const Chord &chord, double range, double g) {
    Chord normalR = normalize<EQUIVALENCE_RELATION_R>(chord, range, g);
    std::vector<Chord> voicings_ = normalR.voicings();
    for (size_t voice = 0; voice < normalR.voices(); voice++) {
        const Chord &voicing = voicings_[voice];
        if (isNormal<EQUIVALENCE_RELATION_V>(voicing, range, g)) {
            return normalize<EQUIVALENCE_RELATION_T>(voicing, range, g);
        }
    }
    throw "Shouldn't come here.";
}

//	EQUIVALENCE_RELATION_RTg

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RTg>(const Chord &chord, double range, double g) {
    Chord normalR = normalize<EQUIVALENCE_RELATION_R>(chord, range, g);
    std::vector<Chord> voicings_ = normalR.voicings();
    for (size_t voice = 0; voice < normalR.voices(); voice++) {
        const Chord &voicing = voicings_[voice];
        const Chord normalTg = normalize<EQUIVALENCE_RELATION_Tg>(voicing, range, g);
        if (isNormal<EQUIVALENCE_RELATION_V>(normalTg, range, g)) {
            return normalTg;
        }
    }
    throw "Shouldn't come here.";
}

//	EQUIVALENCE_RELATION_RI

//	EQUIVALENCE_RELATION_PT

//	EQUIVALENCE_RELATION_PTg

//	EQUIVALENCE_RELATION_PI

//	EQUIVALENCE_RELATION_TI

//	EQUIVALENCE_RELATION_RPT

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RPT>(const Chord &chord, double range, double g) {
    if (!isNormal<EQUIVALENCE_RELATION_R>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_P>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_T>(chord, range, g)) {
        return false;
    }
    // TODO: Should this be here?
    if (!isNormal<EQUIVALENCE_RELATION_V>(chord, range, g)) {
        return false;
    }
    return true;
}

inline bool Chord::iseRPT(double range) const {
    return isNormal<EQUIVALENCE_RELATION_RPT>(*this, range, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RPT>(const Chord &chord, double range, double g) {
    Chord normalRP = normalize<EQUIVALENCE_RELATION_RP>(chord, range, g);
    std::vector<Chord> voicings_ = normalRP.voicings();
    for (size_t i = 0; i < voicings_.size(); i++) {
        const Chord &voicing = voicings_[i];
        if (isNormal<EQUIVALENCE_RELATION_V>(voicing, range, g)) {
            return normalize<EQUIVALENCE_RELATION_T>(voicing, range, g);
        }
    }
    throw "Shouldn't come here.";
}

inline Chord Chord::eRPT(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_RPT>(*this, range, 1.0);
}

//	EQUIVALENCE_RELATION_RPTg

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RPTg>(const Chord &chord, double range, double g) {
    if (!isNormal<EQUIVALENCE_RELATION_R>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_P>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_Tg>(chord, range, g)) {
        return false;
    }
    // TODO: Should this be here?
    if (!isNormal<EQUIVALENCE_RELATION_V>(chord, range, g)) {
        return false;
    }
    return true;
}

inline bool Chord::iseRPTT(double range, double g) const {
    return isNormal<EQUIVALENCE_RELATION_RPTg>(*this, range, g);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RPTg>(const Chord &chord, double range, double g) {
    Chord normalRP = normalize<EQUIVALENCE_RELATION_RP>(chord, range, g);
    std::vector<Chord> voicings_ = normalRP.voicings();
    for (size_t i = 0; i < voicings_.size(); i++) {
        const Chord &voicing = voicings_[i];
        Chord normalTg = normalize<EQUIVALENCE_RELATION_Tg>(voicing, range, g);
        if (isNormal<EQUIVALENCE_RELATION_V>(normalTg, range, g) == true) {
            return normalTg;
        }
    }
    throw "Shouldn't come here.";
}

inline Chord Chord::eRPTT(double range, double g) const {
    return csound::normalize<EQUIVALENCE_RELATION_RPTg>(*this, range, g);
}

//	EQUIVALENCE_RELATION_RPI

/*
That Ic acts on R 2 / P by reflection in the line of Ic -invariant P-classes, or the
inversion flat, points to a general principle of the action of Ic on any of the spaces from
sections four and five: Ic corresponds to a reflection in the flat of chords or equivalence
classes of chords that remain invariant under Ic . Therefore, the operative question when
considering Ic -equivalence on any space is "Where are the Ic -invariant points?" Once
we know the answer to this question, we know the precise action of Ic on the original
space. We have already seen this principle at work in the quotient spaces R n / Ic : there is
precisely one fixed point, A, such that Ic ( A) = A , and R n is transformed into R n / Ic by
gluing together points with their reflection in A. (Callender, Quinn, and Tymoczko, draft.)
*/

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RPI>(const Chord &chord, double range, double g) {
    if (isNormal<EQUIVALENCE_RELATION_RP>(chord, range, g) == false) {
        return false;
    }
    Chord inverse = chord.I();
    Chord inverseRP = normalize<EQUIVALENCE_RELATION_RP>(inverse, range, g);
    if (chord <= inverseRP) {
        return true;
    }
    return false;
}

inline bool Chord::iseRPI(double range) const {
    return isNormal<EQUIVALENCE_RELATION_RPI>(*this, range, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RPI>(const Chord &chord, double range, double g) {
    if (isNormal<EQUIVALENCE_RELATION_RPI>(chord, range, g) == true) {
        return chord;
    }
    Chord normalRP = normalize<EQUIVALENCE_RELATION_RP>(chord, range, g);
    Chord normalRPInverse = normalRP.I();
    Chord normalRPInverseRP = normalize<EQUIVALENCE_RELATION_RP>(normalRPInverse, range, g);
    if (normalRP <= normalRPInverseRP) {
        return normalRP;
    } else {
        return normalRPInverseRP;
    }
}

inline Chord Chord::eRPI(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_RPI>(*this, range, 1.0);
}

//	EQUIVALENCE_RELATION_RTI

//	EQUIVALENCE_RELATION_RTgI

//	EQUIVALENCE_RELATION_RPTI

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RPTI>(const Chord &chord, double range, double g) {
    if (!isNormal<EQUIVALENCE_RELATION_P>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_R>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_T>(chord, range, g)) {
        return false;
    }
    if (!isNormal<EQUIVALENCE_RELATION_V>(chord, range, g)) {
        return false;
    }
    return true;
}

inline bool Chord::iseRPTI(double range) const {
    return isNormal<EQUIVALENCE_RELATION_RPTI>(*this, range, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RPTI>(const Chord &chord, double range, double g) {
    Chord normalRPT = normalize<EQUIVALENCE_RELATION_RPT>(chord, range, g);
    if (isNormal<EQUIVALENCE_RELATION_I>(normalRPT, range, g) == true) {
        return normalRPT;
    } else {
        Chord normalI = normalize<EQUIVALENCE_RELATION_RPI>(normalRPT, range, g);
        Chord normalRPT = normalize<EQUIVALENCE_RELATION_RPT>(normalI, range, g);
        return normalRPT;
    }
}

inline Chord Chord::eRPTI(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_RPTI>(*this, range, 1.0);
}

//	EQUIVALENCE_RELATION_RPTgI

template<> inline SILENCE_PUBLIC bool isNormal<EQUIVALENCE_RELATION_RPTgI>(const Chord &chord, double range, double g) {
    if (isNormal<EQUIVALENCE_RELATION_RPTg>(chord, range, g) == false) {
        return false;
    }
    Chord inverse = chord.I();
    Chord normalRPTg = normalize<EQUIVALENCE_RELATION_RPTg>(inverse, range, g);
    if (chord <= normalRPTg) {
        return true;
    }
    return false;
}

inline bool Chord::iseRPTTI(double range) const {
    return isNormal<EQUIVALENCE_RELATION_RPTgI>(*this, range, 1.0);
}

template<> inline SILENCE_PUBLIC Chord normalize<EQUIVALENCE_RELATION_RPTgI>(const Chord &chord, double range, double g) {
    Chord normalRPTg = normalize<EQUIVALENCE_RELATION_RPTg>(chord, range, g);
    Chord inverse = normalRPTg.I();
    Chord inverseNormalRPTg = normalize<EQUIVALENCE_RELATION_RPTg>(inverse, range, g);
    if (normalRPTg <= inverseNormalRPTg) {
        return normalRPTg;
    }
    return inverseNormalRPTg;
}

inline Chord Chord::eRPTTI(double range) const {
    return csound::normalize<EQUIVALENCE_RELATION_RPTgI>(*this, range, 1.0);
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC std::set<Chord> fundamentalDomainByIsNormal(int voiceN, double range, double g)
{
    std::set<Chord> fundamentalDomain;
    int upperI = 2 * (range + 1);
    int lowerI = - (range + 1);
    Chord iterator_ = iterator(voiceN, lowerI);
    Chord origin = iterator_;
    int chords = 0;
    while (next(iterator_, origin, upperI, g) == true) {
        chords++;
        bool iterator_is_normal = isNormal<EQUIVALENCE_RELATION>(iterator_, range, g);
        Chord normalized = normalize<EQUIVALENCE_RELATION>(iterator_, range, g);
        bool normalized_is_normal = isNormal<EQUIVALENCE_RELATION>(normalized, range, g);
        if (DEBUGGING && normalized_is_normal == false) {
            std::cerr << "Inconsistent equivalence class! " << normalized.toString().c_str() << std::endl;
        }
        if (iterator_is_normal == true) {
            auto result = fundamentalDomain.insert(iterator_);
            if (DEBUGGING && result.second == true) {
                System::message("By isNormal  %-8s: chord: %6d  domain: %6d  range: %7.2f  g: %7.2f  iterator: %s  isNormal: %d  normalized: %s  isNormal: %d\n",
                      namesForEquivalenceRelations[EQUIVALENCE_RELATION],
                      chords,
                      fundamentalDomain.size(),
                      range,
                      g,
                      iterator_.toString().c_str(),
                      iterator_is_normal,
                      normalized.toString().c_str(),
                      normalized_is_normal);
            }
        }
    }
    return fundamentalDomain;
    
}

template<int EQUIVALENCE_RELATION> inline SILENCE_PUBLIC std::set<Chord> fundamentalDomainByNormalize(int voiceN, double range, double g)
{
    std::set<Chord> fundamentalDomain;
    int upperI = 2 * (range + 1);
    int lowerI = - (range + 1);
    Chord iterator_ = iterator(voiceN, lowerI);
    Chord origin = iterator_;
    int chords = 0;
    while (next(iterator_, origin, upperI, g) == true) {
        chords++;
        bool iterator_is_normal = isNormal<EQUIVALENCE_RELATION>(iterator_, range, g);
        Chord normalized = normalize<EQUIVALENCE_RELATION>(iterator_, range, g);
        bool normalized_is_normal = isNormal<EQUIVALENCE_RELATION>(normalized, range, g);
        if (DEBUGGING && normalized_is_normal == false) {
            std::cerr << "Inconsistent equivalence class! " << normalized.toString().c_str() << std::endl;
        }
        auto result = fundamentalDomain.insert(normalized);
        if (DEBUGGING && result.second == true) {
            System::message("By normalize %-8s: chord: %6d  domain: %6d  range: %7.2f  g: %7.2f  iterator: %s  isNormal: %d  normalized: %s  isNormal: %d\n",
                  namesForEquivalenceRelations[EQUIVALENCE_RELATION],
                  chords,
                  fundamentalDomain.size(),
                  range,
                  g,
                  iterator_.toString().c_str(),
                  iterator_is_normal,
                  normalized.toString().c_str(),
                  normalized_is_normal);
        }
    }
    return fundamentalDomain;
}

inline SILENCE_PUBLIC const std::map<std::string, double> &pitchClassesForNames() {
    static bool pitchClassesForNamesInitialized = false;
    static std::map<std::string, double> pitchClassesForNames_;
    if (!pitchClassesForNamesInitialized) {
        pitchClassesForNamesInitialized = true;
        pitchClassesForNames_["Ab"] =   8.;
        pitchClassesForNames_["A" ] =   9.;
        pitchClassesForNames_["A#"] =  10.;
        pitchClassesForNames_["Bb"] =  10.;
        pitchClassesForNames_["B" ] =  11.;
        pitchClassesForNames_["B#"] =   0.;
        pitchClassesForNames_["Cb"] =  11.;
        pitchClassesForNames_["C" ] =   0.;
        pitchClassesForNames_["C#"] =   1.;
        pitchClassesForNames_["Db"] =   1.;
        pitchClassesForNames_["D" ] =   2.;
        pitchClassesForNames_["D#"] =   3.;
        pitchClassesForNames_["Eb"] =   3.;
        pitchClassesForNames_["E" ] =   4.;
        pitchClassesForNames_["E#"] =   5.;
        pitchClassesForNames_["Fb"] =   4.;
        pitchClassesForNames_["F" ] =   5.;
        pitchClassesForNames_["F#"] =   6.;
        pitchClassesForNames_["Gb"] =   6.;
        pitchClassesForNames_["G" ] =   7.;
        pitchClassesForNames_["G#"] =   8.;
    }
    return const_cast<std::map<std::string, double> &>(pitchClassesForNames_);
}

inline SILENCE_PUBLIC double pitchClassForName(std::string name) {
    const std::map<std::string, double> &pitchClassesForNames_ = pitchClassesForNames();
    std::map<std::string, double>::const_iterator it = pitchClassesForNames_.find(name);
    if (it == pitchClassesForNames_.end()) {
        return DBL_MAX;
    } else {
        return it->second;
    }
}

/**
 * Returns the name of the pitch-class of the pitch.
 * The first of enharmonic names is always used, sorry. 
 * If there is no matching name, an empty string is returned.
 */
inline SILENCE_PUBLIC std::string nameForPitchClass(double pitch) {
    auto pc = epc(pitch);
    const std::map<std::string, double> &pitchClassesForNames_ = pitchClassesForNames();
    for (auto it = pitchClassesForNames_.begin(); it != pitchClassesForNames_.end(); ++it) {
        if (eq_epsilon(it->second, pc) == true) {
            return it->first;
        }
    }
    return "";
}

inline SILENCE_PUBLIC std::multimap<Chord, std::string> &namesForChords() {
    static std::multimap<Chord, std::string> namesForChords_;
    return namesForChords_;
}

inline SILENCE_PUBLIC std::map<std::string, Chord> &chordsForNames() {
    static std::map<std::string, Chord> chordsForNames_;
    return chordsForNames_;
}

inline SILENCE_PUBLIC std::multimap<Scale, std::string> &namesForScales() {
    static std::multimap<Scale, std::string> namesForScales_;
    return namesForScales_;
}

inline SILENCE_PUBLIC std::map<std::string, Scale> &scalesForNames() {
    static std::map<std::string, Scale> scalesForNames_;
    return scalesForNames_;
}

inline SILENCE_PUBLIC std::set<Chord> &unique_chords() {
    static std::set<Chord> unique_chords_;
    return unique_chords_;
}

inline SILENCE_PUBLIC std::set<Scale> &unique_scales() {
    static std::set<Scale> unique_scales_;
    return unique_scales_;
}

inline SILENCE_PUBLIC void add_chord(std::string name, const Chord &chord) {
    unique_chords().insert(chord);
    chordsForNames().insert(std::make_pair(name, chord));
    namesForChords().insert(std::make_pair(chord, name));
}

inline SILENCE_PUBLIC void add_scale(std::string name, const Scale &scale) {
    unique_scales().insert(scale);
    scalesForNames().insert(std::make_pair(name, scale));
    namesForScales().insert(std::make_pair(scale, name));
}

inline SILENCE_PUBLIC std::vector<std::string> split(std::string string_) {
    std::vector<std::string> tokens;
    std::istringstream iss(string_);
    std::copy(std::istream_iterator<std::string>(iss),
              std::istream_iterator<std::string>(),
              std::back_inserter<std::vector<std::string> >(tokens));
    return tokens;
}

inline void fill(std::string rootName, double rootPitch, std::string typeName, std::string typePitches, bool is_scale = false) {
    Chord chord;
    std::string chordName = rootName + typeName;
    std::vector<std::string> splitPitches = split(typePitches);
    System::debug("chordName: %s = rootName: %s  rootPitch: %f  typeName: %s  typePitches: %s\n", chordName.c_str(), rootName.c_str(), rootPitch, typeName.c_str(), typePitches.c_str());
    chord.resize(splitPitches.size());
    for (int voice = 0, voiceN = splitPitches.size(); voice < voiceN; ++voice) {
        double pitch = pitchClassForName(splitPitches[voice]);
        System::debug("voice: %3d  pc: %-4s  pitch: %9.4f\n", voice, splitPitches[voice].c_str(), pitch);
        chord.setPitch(voice, pitch);
    }
    System::debug("chord type: %s\n", chord.toString().c_str());
    chord = chord.T(rootPitch);
    Chord eOP_ = chord.eOP();
    System::debug("eOP_:   %s  chordName: %s\n", eOP_.toString().c_str(), chordName.c_str());
    ///chordsForNames()[chordName] = eOP_;
    ///namesForChords()[eOP_] = chordName;
    add_chord(chordName, eOP_);
    if (is_scale == true) {
        Scale scale(chordName, chord);
        ///scalesForNames()[chordName] = scale;
        ///namesForScales()[scale] = chordName;
        add_scale(chordName, scale);
    }
}

inline void initializeNames() {
    static bool initializeNamesInitialized = false;
    if (!initializeNamesInitialized) {
        initializeNamesInitialized = true;
        System::debug("Initializing chord names...\n");
        const std::map<std::string, double> &pitchClassesForNames_ = pitchClassesForNames();
        for (std::map<std::string, double>::const_iterator it = pitchClassesForNames_.begin();
                it != pitchClassesForNames_.end();
                ++it) {
            const std::string &rootName = it->first;
            const double &rootPitch = it->second;
            System::debug("rootName: %-3s  rootPitch: %9.5f\n", rootName.c_str(), rootPitch);
            fill(rootName, rootPitch, " minor second",     "C  C#                             ");
            fill(rootName, rootPitch, " major second",     "C     D                           ");
            fill(rootName, rootPitch, " minor third",      "C        Eb                       ");
            fill(rootName, rootPitch, " major third",      "C           E                     ");
            fill(rootName, rootPitch, " perfect fourth",   "C              F                  ");
            fill(rootName, rootPitch, " tritone",          "C                 F#              ");
            fill(rootName, rootPitch, " perfect fifth",    "C                    G            ");
            fill(rootName, rootPitch, " augmented fifth",  "C                       G#        ");
            fill(rootName, rootPitch, " sixth",            "C                          A      ");
            fill(rootName, rootPitch, " minor seventh  ",  "C                             Bb  ");
            fill(rootName, rootPitch, " major seventh",    "C                                B");
            // Scales.
            fill(rootName, rootPitch, " major",            "C     D     E  F     G     A     B", true);
            fill(rootName, rootPitch, " minor",            "C     D  Eb    F     G  Ab    Bb  ", true);
            fill(rootName, rootPitch, " natural minor",    "C     D  Eb    F     G  Ab    Bb  ", true);
            fill(rootName, rootPitch, " harmonic minor",   "C     D  Eb    F     G  Ab       B", true);
            fill(rootName, rootPitch, " chromatic",        "C  C# D  D# E  F  F# G  G# A  A# B", true);
            fill(rootName, rootPitch, " whole tone",       "C     D     E     F#    G#    A#  ", true);
            fill(rootName, rootPitch, " diminished",       "C     D  D#    F  F#    G# A     B", true);
            fill(rootName, rootPitch, " pentatonic",       "C     D     E        G     A      ", true);
            fill(rootName, rootPitch, " pentatonic major", "C     D     E        G     A      ", true);
            fill(rootName, rootPitch, " pentatonic minor", "C        Eb    F     G        Bb  ", true);
            fill(rootName, rootPitch, " augmented",        "C        Eb E        G  Ab    Bb  ", true);
            fill(rootName, rootPitch, " Lydian dominant",  "C     D     E     Gb G     A  Bb  ", true);
            fill(rootName, rootPitch, " 3 semitone",       "C        D#       F#       A      ", true);
            fill(rootName, rootPitch, " 4 semitone",       "C           E           G#        ", true);
            fill(rootName, rootPitch, " blues",            "C     D  Eb    F  Gb G        Bb  ", true);
            fill(rootName, rootPitch, " bebop",            "C     D     E  F     G     A  Bb B", true);
            // Modes.
            fill(rootName, rootPitch, " Ionian",           "C     D     E  F     G     A     B", true);
            fill(rootName, rootPitch, " Dorian",           "C     D  Eb    F     G     A  Bb  ", true);
            fill(rootName, rootPitch, " Phrygian",         "C  Db    Eb    F     G  Ab    Bb  ", true);
            fill(rootName, rootPitch, " Lydian",           "C     D     E     F# G     A     B", true);
            fill(rootName, rootPitch, " Mixolydian",       "C     D     E  F     G     A  Bb  ", true);
            fill(rootName, rootPitch, " Aeolian",          "C     D  Eb    F     G  Ab    Bb  ", true);
            fill(rootName, rootPitch, " Locrian",          "C  Db    Eb    F  Gb    Ab    Bb B", true);
            // Major chords.
            fill(rootName, rootPitch, "M",                 "C           E        G            ");
            fill(rootName, rootPitch, "6",                 "C           E        G     A      ");
            fill(rootName, rootPitch, "69",                "C     D     E        G     A      ");
            fill(rootName, rootPitch, "69b5",              "C     D     E     Gb       A      ");
            fill(rootName, rootPitch, "M7",                "C           E        G           B");
            fill(rootName, rootPitch, "M9",                "C     D     E        G           B");
            fill(rootName, rootPitch, "M11",               "C     D     E  F     G           B");
            fill(rootName, rootPitch, "M#11",              "C     D     E  F#    G           B");
            fill(rootName, rootPitch, "M13",               "C     D     E  F     G     A     B");
            // Minor chords.
            fill(rootName, rootPitch, "m",                 "C        Eb          G            ");
            fill(rootName, rootPitch, "m6",                "C        Eb          G     A      ");
            fill(rootName, rootPitch, "m69",               "C     D  Eb          G     A      ");
            fill(rootName, rootPitch, "m7",                "C        Eb          G        Bb  ");
            fill(rootName, rootPitch, "m7b9",              "C  Db    Eb          G        Bb  ");
            fill(rootName, rootPitch, "m7b9b5",            "C  Db    Eb       Gb          Bb  ");
            fill(rootName, rootPitch, "m#7",               "C        Eb          G           B");
            fill(rootName, rootPitch, "m7b5",              "C        Eb       Gb          Bb  ");
            fill(rootName, rootPitch, "m9",                "C     D  Eb          G        Bb  ");
            fill(rootName, rootPitch, "m9#7",              "C     D  Eb          G           B");
            fill(rootName, rootPitch, "m11",               "C     D  Eb    F     G        Bb  ");
            fill(rootName, rootPitch, "m#11",              "C     D  Eb    F     G        Bb  ");
            fill(rootName, rootPitch, "m13",               "C     D  Eb    F     G     A  Bb  ");
            // Augmented chords.
            fill(rootName, rootPitch, "+",                 "C            E         G#         ");
            fill(rootName, rootPitch, "7#5",               "C            E         G#     Bb  ");
            fill(rootName, rootPitch, "7b9#5",             "C  Db        E         G#     Bb  ");
            fill(rootName, rootPitch, "9#5",               "C     D      E         G#     Bb  ");
            // Diminished chords.
            fill(rootName, rootPitch, "o",                 "C        Eb       Gb              ");
            fill(rootName, rootPitch, "o7",                "C        Eb       Gb       A      ");
            // Suspended chords.
            fill(rootName, rootPitch, "6sus",              "C              F     G     A      ");
            fill(rootName, rootPitch, "69sus",             "C     D        F     G     A      ");
            fill(rootName, rootPitch, "7sus",              "C              F     G        Bb  ");
            fill(rootName, rootPitch, "9sus",              "C     D        F     G        Bb  ");
            fill(rootName, rootPitch, "M7sus",             "C              F     G           B");
            fill(rootName, rootPitch, "M9sus",             "C     D        F     G           B");
            // Dominant chords.
            fill(rootName, rootPitch, "7",                 "C            E       G        Bb  ");
            fill(rootName, rootPitch, "7b5",               "C            E    Gb          Bb  ");
            fill(rootName, rootPitch, "7b9",               "C  Db        E       G        Bb  ");
            fill(rootName, rootPitch, "7b9b5",             "C  Db        E    Gb          Bb  ");
            fill(rootName, rootPitch, "9",                 "C     D      E       G        Bb  ");
            fill(rootName, rootPitch, "9#11",              "C     D      E F#    G        Bb  ");
            fill(rootName, rootPitch, "13",                "C     D      E F     G     A  Bb  ");
            fill(rootName, rootPitch, "13#11",             "C     D      E F#    G     A  Bb  ");
        }
    }
}

/**
 * Returns all enharmonic names for the Chord, if any exists. If none exists, 
 * an empty result is returned.
 */
inline SILENCE_PUBLIC std::vector<std::string> namesForChord(const Chord &chord) {
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        initializeNames();
    }
    std::multimap<Chord, std::string> &namesForChords_ = namesForChords();
    std::vector<std::string> result;
    auto matches = namesForChords_.equal_range(chord);
    for (auto it = matches.first; it != matches.second; ++it) {
        result.push_back(it->second);
    }
    return result;
}

/**
 * Returns the first valid name for the Chord. If none exists, an empty result 
 * is returned.
 */
inline SILENCE_PUBLIC std::string nameForChord(const Chord &chord) {
    auto result = namesForChord(chord);
    if (result.size() == 0) {
        return "";
    } else {
        return result[0];
    }
}

inline SILENCE_PUBLIC const Chord &chordForName(std::string name) {
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        initializeNames();
    }
    const std::map<std::string, Chord> &chordsForNames_ = chordsForNames();
    std::map<std::string, Chord>::const_iterator it = chordsForNames_.find(name);
    if (it == chordsForNames_.end()) {
        static Chord chord;
        chord.resize(0);
        return chord;
    } else {
        return it->second;
    }
}

/**
 * Returns all enharmonic names for the Scale, if any exists. If none exists, 
 * an empty result is returned.
 */
inline SILENCE_PUBLIC std::vector<std::string> namesForScale(const Scale &scale) {
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        initializeNames();
    }
    std::multimap<Scale, std::string> &namesForScales_ = namesForScales();
    std::vector<std::string> result;
    auto matches = namesForScales_.equal_range(scale);
    for (auto it = matches.first; it != matches.second; ++it) {
        result.push_back(it->second);
    }
    return result;
}

/**
 * Returns the first valid name for the Scale.
 */
inline SILENCE_PUBLIC std::string nameForScale(const Scale &scale) {
    auto result = namesForScale(scale);
    if (result.size() == 0) {
        return "";
    } else {
        return result[0];
    }
}

inline SILENCE_PUBLIC const Scale &scaleForName(std::string name) {
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        initializeNames();
    }
    const std::map<std::string, Scale> &scalesForNames_ = scalesForNames();
    std::map<std::string, Scale>::const_iterator it = scalesForNames_.find(name);
    if (it == scalesForNames_.end()) {
        static Scale scale;
        scale.resize(0);
        return scale;
    } else {
        return it->second;
    }
}

/**
 * Orthogonal additive groups for unordered chords of given arity under range
 * equivalence (RP): prime form or P, inversion or I, transposition or T, and
 * voicing or V. P x I x T = OP, P x I x T x V = RP. Therefore, an
 * operation on P, I, T, or V may be used to independently transform the
 * respective symmetry of any chord. Some of these operations will reflect
 * in RP. Please note: some equivalence classes define quotient spaces
 * with singularities, meaning that more than one chord where the space is 
 * glued may have the same equivalent. Hence, for each P there must be one and 
 * only one chord in the representative fundamental domain of the group, yet 
 * each of several chords at any singular point of the fundamental domain must
 * have the same P.
 */
class SILENCE_PUBLIC ChordSpaceGroup {
public:
    virtual ~ChordSpaceGroup() {};
    /**
     * Number of voices in the chord space.
     */
    int N;
    virtual int getN() const {
        return N;
    }
    /**
     * The generator of transposition.
     */
    double g;
    virtual int getG() const {
        return g;
    }
    /**
     * The zero-based range of the chord space.
     */
    double range;
    virtual int getRange() const {
        return range;
    }
    int countP;
    virtual int getCountP() const {
        return countP;
    }
    int countI;
    virtual int getCountI() const {
        return countI;
    }
    int countT;
    virtual int getCountT() const {
        return countT;
    }
    int countV;
    virtual int getCountV() const {
        return countV;
    }
    /**
     * Ordered table of all OPTTI chords for g.
     */
    std::vector<Chord> opttisForIndexes;
    std::map<Chord, int> indexesForOpttis;
    /**
     * Ordered table of all octavewise permutations
     * in RP (note: not OP).
     */
    std::vector<Chord> voicingsForIndexes;
    std::map<Chord, int> indexesForVoicings;
    virtual void preinitialize(int N_, double range_, double g_ = 1.0) {
        System::inform("ChordSpaceGroup.preinitialize...\n");
        opttisForIndexes.clear();
        indexesForOpttis.clear();
        voicingsForIndexes.clear();
        indexesForVoicings.clear();
        N = N_;
        range = range_;
        g = g_;
        countP = 0;
        countI = 2;
        countT = OCTAVE() / g;
        Chord chord;
        chord.resize(N);
        countV = octavewiseRevoicings(chord, range);
    }
    virtual void initialize(int N_, double range_, double g_ = 1.0) {
        System::inform("ChordSpaceGroup.initialize...\n");
        preinitialize(N_, range_, g_);
        std::set<Chord> representative_opttis = fundamentalDomainByNormalize<EQUIVALENCE_RELATION_RPTgI>(N, OCTAVE(), g);
        System::inform("ChordSpaceGroup.initialize: representative_opttis: %6d\n", representative_opttis.size());
        std::set<Chord> equivalent_opttis = fundamentalDomainByIsNormal<EQUIVALENCE_RELATION_RPTgI>(N, OCTAVE(), g);
        System::inform("ChordSpaceGroup.initialize: equivalent_opttis:     %6d\n", equivalent_opttis.size());
        for (auto it = representative_opttis.begin(); it != representative_opttis.end(); ++it) {
            opttisForIndexes.push_back(*it);
        }
        countP = opttisForIndexes.size();
        for (auto equivalent_it = equivalent_opttis.begin(); equivalent_it != equivalent_opttis.end(); ++equivalent_it) {
            const Chord &representative = equivalent_it->eOPTTI();
            auto representative_it = std::find(opttisForIndexes.begin(), opttisForIndexes.end(), representative);
            if (representative_it == opttisForIndexes.end()) {
                System::error("ChordSpaceGroup::initialize: error: representative OPTTI missing: %s\n", representative.information().c_str());
            } else {
                auto index = std::distance(opttisForIndexes.begin(), representative_it);
                indexesForOpttis[*equivalent_it] = index;
            }
        }
        System::inform("ChordSpaceGroup.initialize: indexesForOpttis:      %6d\n", indexesForOpttis.size());
        System::inform("ChordSpaceGroup.initialize.\n");
    }
    virtual void list(bool listheader = true, bool listopttis = false, bool listvoicings = false) const {
        if (listheader) {
            System::inform("ChordSpaceGroup.voices: %8d\n", N);
            System::inform("ChordSpaceGroup.range : %13.4f\n", range);
            System::inform("ChordSpaceGroup.g     : %13.4f\n", g);
            System::inform("ChordSpaceGroup.countP: %8d\n", countP);
            System::inform("ChordSpaceGroup.countI: %8d\n", countI);
            System::inform("ChordSpaceGroup.countT: %8d\n", countT);
            System::inform("ChordSpaceGroup.countV: %8d\n", countV);
        }
        if (listopttis) {
            for (auto &entry : indexesForOpttis) {
                System::inform("index: %5d  optti: %s\n", entry.second, entry.first.toString().c_str());
            }
        }
        // Doesn't currently do anything as these collections are not currently initialized.
        if (listvoicings) {
            for (int i = 0, n = voicingsForIndexes.size(); i < n; ++i) {
                const Chord &voicing = voicingsForIndexes[i];
                int index = indexesForVoicings.at(voicing);
                System::inform("voicing index: %5d  voicing: %s  index from voicing: %5d\n", i,  voicing.toString().c_str(), index);
            }
        }
    }
    virtual std::string createFilename(int voices, double range, double g = 1.0) const {
        std::string extension = ".txt";
        char buffer[0x200];
        std::sprintf(buffer, "ChordSpaceGroup_V%d_R%d_g%d.txt", voices, int(range), int(1000 * g));
        return buffer;
    }
    /**
     * Loads the group if found, creates and saves it otherwise.
     */
    virtual void createChordSpaceGroup(int voices, double range, double g = 1.0) {
        std::string filename = createFilename(voices, range, g);
        std::fstream stream;
        stream.open(filename.c_str());
        if (!stream.is_open()) {
            System::inform("No data in ChordSpaceGroup file \"%s\", initializing and saving...\n", filename.c_str());
            stream.close();
            stream.open(filename.c_str(), std::fstream::out);
            initialize(voices, range, g);
            save(stream);
        } else {
            System::inform("Loading ChordSpaceGroup data from file \"%s\"...\n", filename.c_str());
            load(stream);
        }
        stream.close();
    }
    virtual void save(std::fstream &stream) const {
        stream << "N " << N << std::endl;
        stream << "range " << range << std::endl;
        stream << "g " << g << std::endl;
        for (int i = 0, n = opttisForIndexes.size(); i < n; ++i) {
            stream << opttisForIndexes[i].toString().c_str() << std::endl;
        }
    }
    virtual void load(std::fstream &stream) {
        std::string junk;
        stream >> junk >> N;
        stream >> junk >> range;
        stream >> junk >> g;
        preinitialize(N, range, g);
        char buffer[0x500];
        for (;;) {
            stream.getline(buffer, 0x500);
            if (stream.eof()) {
                break;
            }
            Chord chord;
            chord.fromString(buffer);
            if (chord.voices() > 1) {
                opttisForIndexes.push_back(chord);
                indexesForOpttis[chord] = opttisForIndexes.size() - 1;
            }
        }
        countP = opttisForIndexes.size();
    }
    /**
     * Returns the indices of prime form, inversion, transposition,
     * and voicing for a chord, as the first 4 elements, respectively,
     * of a homogeneous vector.
     *
     * Please note: where are there singularities
     * in the quotient spaces for chords, there may be several chords that
     * belong to the same equivalence class. In such cases, any of several 
     * chords at a singular point of the fundamental domain will return the 
     * same P.
     */
    Eigen::VectorXi fromChord(const Chord &chord, bool printme = false) const {
        bool isNormalOP = csound::isNormal<EQUIVALENCE_RELATION_RP>(chord, OCTAVE(), g);
        if (printme) {
            System::debug("fromChord...\n");
            System::debug("chord:          %s  %d\n", chord.toString().c_str(), isNormalOP);
        }
        Chord normalOP;
        if (isNormalOP) {
            normalOP = chord;
        } else {
            normalOP = csound::normalize<EQUIVALENCE_RELATION_RP>(chord, OCTAVE(), g);
        }
        if (printme) {
            System::debug("normalOP:       %s  %d\n", normalOP.toString().c_str(), csound::isNormal<EQUIVALENCE_RELATION_RP>(normalOP, OCTAVE(), g));
        }
        Chord normalOPTg = csound::normalize<EQUIVALENCE_RELATION_RPTg>(chord, OCTAVE(), g);
        if (printme) {
            System::debug("normalOPTg:     %s\n", normalOPTg.toString().c_str());
        }
        int T_ = 0;
        for (double t = 0.0; t < OCTAVE(); t += g) {
            Chord normalOPTg_t = normalOPTg.T(t);
            normalOPTg_t = csound::normalize<EQUIVALENCE_RELATION_RP>(normalOPTg_t, OCTAVE(), g);
            if (printme) {
                System::debug("normalOPTg_t:   %s    %f\n", normalOPTg_t.toString().c_str(), t);
            }
            if (normalOPTg_t == normalOP) {
                if (printme) {
                    System::debug("equals\n");
                }
                T_ = t;
                break;
            }
        }
        // Breaks here, this form may not be indexed.
        // Try iterating over opttis and comparing eO, eP, eT, eI separately.
        // Alternatively, put in same index for equivalent opttis.
        Chord normalOPTgI = csound::normalize<EQUIVALENCE_RELATION_RPTgI>(chord, OCTAVE(), g);
        std::map<Chord, int>::const_iterator it = indexesForOpttis.find(normalOPTgI);
        if (it == indexesForOpttis.end()) {
            // Falling through here means there is a bug that I want to know about.
            System::debug("Error: normalOPTgI %s not found! Please report an issue, this should not appear.\n");
            exit(1);
        }
        int P_ = it->second;
        if (printme) {
            System::debug("normalOPTgI:    %s    %d\n", normalOPTgI.toString().c_str(), P_);
        }
        int I_;
        if (normalOPTg == normalOPTgI) {
            I_ = 0;
        } else {
            I_ = 1;
        }
        int V_ = indexForOctavewiseRevoicing(chord, range, printme);
        if (V_ == -1) {
            V_ = 0;
        }
        Eigen::VectorXi pitv(4);
        pitv(0) = P_;
        pitv(1) = I_;
        pitv(2) = T_;
        pitv(3) = V_;
        if (printme) {
            System::debug("PITV:       %8d     %8d     %8d     %8d\n", pitv(0), pitv(1), pitv(2), pitv(3));
            System::debug("fromChord.\n");
        }
        return pitv;
    }
    /**
     * Returns the chord for the indices of prime form, inversion,
     * transposition, and voicing. The chord is not in RP; rather, each voice of
     * the chord's OP may have zero or more octaves added to it.
     *
     * Please note: where are there singularities
     * in the quotient spaces for chords, there may be several chords that
     * belong to the same equivalence class. In such cases, each P will return 
     * just one chord from the representative fundamental domain.
     */
    std::vector<Chord> toChord(int P, int I, int T, int V, bool printme = false) const {
        P = P % countP;
        I = I % countI;
        T = T % countT;
        V = V % countV;
        if (printme) {
            System::debug("toChord...\n");
            System::debug("PITV:       %8d     %8d     %8d     %8d\n", P, I, T, V);
        }
        Chord normalOPTgI = opttisForIndexes[P];
        if (printme) {
            System::debug("normalOPTgI:    %s\n", normalOPTgI.toString().c_str());
        }
        Chord normalOPTg;
        if (I == 0) {
            normalOPTg = normalOPTgI;
        } else {
            Chord inverse = normalOPTgI.I();
            normalOPTg = csound::normalize<EQUIVALENCE_RELATION_RPTg>(inverse, OCTAVE(), g);
        }
        if (printme) {
            System::debug("normalOPTg:     %s\n", normalOPTg.toString().c_str());
        }
        Chord normalOPTg_t = normalOPTg.T(T);
        if (printme) {
            System::debug("normalOPTg_t:   %s\n", normalOPTg_t.toString().c_str());
        }
        Chord normalOP = csound::normalize<EQUIVALENCE_RELATION_RP>(normalOPTg_t, OCTAVE(), g);
        if (printme) {
            System::debug("normalOP:       %s\n", normalOP.toString().c_str());
        }
        Chord revoicing = octavewiseRevoicing(normalOP, V, range, printme);
        std::vector<Chord> result(3);
        result[0] = revoicing;
        result[1] = normalOPTgI;
        result[2] = normalOP;
        if (printme) {
            System::debug("revoicing:      %s\n", result[0].toString().c_str());
            System::debug("toChord.\n");
        }
        return result;
    }
    std::vector<Chord> toChord_vector(const Eigen::VectorXi &pitv, bool printme = false) const {
        return toChord(pitv(0), pitv(1), pitv(2), pitv(3), printme);
    }
};

inline std::string Chord::information() const {
    char buffer[0x1000];
    if (voices() < 1) {
        return "Empty chord.";
    }
    Chord normalOP =    csound::normalize<EQUIVALENCE_RELATION_RP>(*this);
    std::string chordName = name(); ///nameForChord(normalOP);
    Chord inverse =     I();
    Chord normalO =     csound::normalize<EQUIVALENCE_RELATION_R>(*this, OCTAVE(), 1.0);
    Chord normalP =     csound::normalize<EQUIVALENCE_RELATION_P>(*this, OCTAVE(), 1.0);
    Chord normalT =     csound::normalize<EQUIVALENCE_RELATION_T>(*this, OCTAVE(), 1.0);
    Chord normalt =     normalT.et();
    Chord normalI =     csound::normalize<EQUIVALENCE_RELATION_I>(*this, OCTAVE(), 1.0);
///    Chord normalV =     csound::normalize<EQUIVALENCE_RELATION_V>(*this, OCTAVE(), 1.0);
///    Chord normalvt =    normalV.et();
    Chord normalpcs =   epcs().eP();
    Chord normalOPT =   csound::normalize<EQUIVALENCE_RELATION_RPT>(*this, OCTAVE(), 1.0);
    Chord normalOPTg =  csound::normalize<EQUIVALENCE_RELATION_RPTg>(*this, OCTAVE(), 1.0);
    Chord normalopt =   normalOPT.et();
    Chord normalOPI =   csound::normalize<EQUIVALENCE_RELATION_RPI>(*this, OCTAVE(), 1.0);
    Chord normalOPTI =  csound::normalize<EQUIVALENCE_RELATION_RPTI>(*this, OCTAVE(), 1.0);
    Chord normalopti =  normalOPTI.et();
    Chord normalOPTgI = csound::normalize<EQUIVALENCE_RELATION_RPTgI>(*this, OCTAVE(), 1.0);
    std::sprintf(buffer,"pitches:  %s  %s\n"
                 "I:        %s\n"
                 "eO:       %s  iseO:    %d\n"
                 "eP:       %s  iseP:    %d\n"
                 "eT:       %s  iseT:    %d\n"
                 "          %s\n"
                 "eI:       %s  iseI:    %d\n"
///                 "eV:       %s  iseV:    %d\n"
///                 "          %s\n"
                 "eOP:      %s  iseOP:   %d\n"
                 "pcs:      %s\n"
                 "eOPT:     %s  iseOPT:  %d\n"
                 "eOPTT:    %s  iseOPTT: %d\n\n"
                 "          %s\n"
                 "eOPI:     %s  iseOPI:  %d\n"
                 "eOPTI:    %s  iseOPTI: %d\n"
                 "eOPTTI:   %s  iseOPTTI:%d\n"
                 "          %s\n"
                 "layer:     %6.2f",
                 toString().c_str(),             chordName.c_str(),
                 inverse.toString().c_str(),
                 normalO.toString().c_str(),     isNormal<EQUIVALENCE_RELATION_R>(*this, OCTAVE(), 1.0),
                 normalP.toString().c_str(),     isNormal<EQUIVALENCE_RELATION_P>(*this, OCTAVE(), 1.0),
                 normalT.toString().c_str(),     isNormal<EQUIVALENCE_RELATION_T>(*this, OCTAVE(), 1.0),
                 normalt.toString().c_str(),
                 normalI.toString().c_str(),     isNormal<EQUIVALENCE_RELATION_I>(*this, OCTAVE(), 1.0),
///                 normalV.toString().c_str(),     isNormal<EQUIVALENCE_RELATION_V>(*this, OCTAVE(), 1.0),
///                 normalvt.toString().c_str(),
                 normalOP.toString().c_str(),    isNormal<EQUIVALENCE_RELATION_RP>(*this, OCTAVE(), 1.0),
                 normalpcs.toString().c_str(),
                 normalOPT.toString().c_str(),   isNormal<EQUIVALENCE_RELATION_RPT>(*this, OCTAVE(), 1.0),
                 normalOPTg.toString().c_str(),  isNormal<EQUIVALENCE_RELATION_RPTg>(*this, OCTAVE(), 1.0),
                 normalopt.toString().c_str(),
                 normalOPI.toString().c_str(),   isNormal<EQUIVALENCE_RELATION_RPI>(*this, OCTAVE(), 1.0),
                 normalOPTI.toString().c_str(),  isNormal<EQUIVALENCE_RELATION_RPTI>(*this, OCTAVE(), 1.0),
                 normalOPTgI.toString().c_str(), isNormal<EQUIVALENCE_RELATION_RPTgI>(*this, OCTAVE(), 1.0),
                 normalopti.toString().c_str(),
                 layer());
    return buffer;
}

inline SILENCE_PUBLIC Chord octavewiseRevoicing(const Chord &chord, int revoicingNumber_, double range, bool debug) {
    int revoicingN = octavewiseRevoicings(chord, range);
    if (revoicingN == 0) {
        revoicingN = 1;
    }
    int revoicingNumber = revoicingNumber_ % revoicingN;
    Chord origin = csound::normalize<EQUIVALENCE_RELATION_RP>(chord, OCTAVE(), 1.0);
    Chord revoicing = origin;
    int revoicingI = 0;
    while (true) {
        System::debug("octavewiseRevoicing %d (%d) of %s in range %7.3f: %5d: %s\n",
              revoicingNumber,
              revoicingNumber_,
              chord.toString().c_str(),
              range,
              revoicingI,
              revoicing.toString().c_str());
         if (revoicingI == revoicingNumber) {
            return revoicing;
        }
        (void) next(revoicing, origin, range, OCTAVE());
        revoicingI++;
    }
    return origin;
}

/**
 * Returns the index of the octavewise revoicing that this chord is,
 * relative to its OP equivalent, within the indicated range. Returns
 * -1 if there is no such chord within the range.
 */
inline SILENCE_PUBLIC int indexForOctavewiseRevoicing(const Chord &chord, double range, bool debug) {
    int revoicingN = octavewiseRevoicings(chord, range);
    Chord origin = csound::normalize<EQUIVALENCE_RELATION_RP>(chord, OCTAVE(), 1.0);
    Chord revoicing = origin;
    int revoicingI = 0;
    while (true) {
        System::debug("indexForOctavewiseRevoicing of %s in range %7.3f: %5d of %5d: %s\n",
              chord.toString().c_str(),
              range,
              revoicingI,
              revoicingN,
              revoicing.toString().c_str());
        if (revoicing == chord) {
            return revoicingI;
        }
        (void) next(revoicing, origin, range, OCTAVE());
        revoicingI++;
        if (revoicingI > revoicingN) {
            return -1;
        }
    }
}

/**
 * Score equipped with chords. The notes in the score may be conformed to the
 * chord that obtains at the time of the notes. The times and durations of
 * notes and chords are rescaled together. This is done by finding minimum and
 * maximum times by counting both note times and chord times.
 */
class SILENCE_PUBLIC ChordScore : public Score {
public:
    std::map<double, Chord> chords_for_times;
    virtual void insertChord(double tyme, const Chord chord) {
        chords_for_times[tyme] = chord;
    }
    /**
     * Returns a pointer to the first chord that starts at or after the
     * specified time. If there is no such chord, a null pointer is returned.
     */
    virtual Chord *getChord(double time_) {
        auto it = chords_for_times.lower_bound(time_);
        if (it != chords_for_times.end()) {
            return &it->second;
        } else {
            return nullptr;
        }
    }

    /**
     * Conforms the pitch-classes of the events in this to the closest
     * pitch-class of the chord, if any, that obtains at that time.
     */
    virtual void conformToChords(bool tie_overlaps = true, bool octave_equivalence = true) {
        sort();
        if (chords_for_times.begin() != chords_for_times.end()) {
            for (auto event_iterator = begin(); event_iterator != end(); ++event_iterator) {
                auto chord_iterator = chords_for_times.lower_bound(event_iterator->getTime());
                if (chord_iterator != chords_for_times.end()) {
                    conformToChord(*event_iterator, chord_iterator->second, octave_equivalence);
                }
            }
        }
        if (tie_overlaps == true) {
            tieOverlappingNotes(true);
        }
    }

    void getScale(std::vector<Event> &score, int dimension, size_t beginAt, size_t endAt, double &minimum, double &range)
    {
        if(beginAt == endAt) {
            return;
        }
        const Event &beginEvent = score[beginAt];
        double maximum = beginEvent[dimension];
        const Event &endEvent = score[endAt - 1];
        minimum = endEvent[dimension];
        if(dimension == Event::TIME) {
            const Event &e = score[beginAt];
            maximum = std::max(e.getTime(), e.getTime() + e.getDuration());
            minimum = std::min(e.getTime(), e.getTime() + e.getDuration());
            double beginning;
            double ending;
            for( ; beginAt != endAt; ++beginAt) {
                const Event &event = score[beginAt];
                beginning = std::min(event.getTime(), event.getTime() + event.getDuration());
                ending = std::max(event.getTime(), event.getTime() + event.getDuration());
                if(ending > maximum) {
                    maximum = ending;
                } else if(beginning < minimum) {
                    minimum = beginning;
                }
            }
            // Also take into account chord times.
            auto chord_begin = chords_for_times.begin();
            auto chord_rbegin = chords_for_times.rbegin();
            if (chord_begin != chords_for_times.end() && chord_rbegin != chords_for_times.rend()) {
                minimum = std::min(minimum, chord_begin->first);
                maximum = std::max(maximum, chord_rbegin->first);
            }
        } else {
            for( ; beginAt != endAt; ++beginAt) {
                const Event &event = score[beginAt];
                if(event[dimension] > maximum) {
                    maximum = event[dimension];
                }
                if(event[dimension] < minimum) {
                    minimum = event[dimension];
                }
            }
        }
        range = maximum - minimum;
    }

    void setScale(std::vector<Event> &score,
                  int dimension,
                  bool rescaleMinimum,
                  bool rescaleRange,
                  size_t beginAt,
                  size_t endAt,
                  double targetMinimum,
                  double targetRange)
    {
        if(!(rescaleMinimum || rescaleRange)) {
            return;
        }
        if(beginAt == endAt) {
            return;
        }
        double actualMinimum;
        double actualRange;
        getScale(score, dimension, beginAt, endAt, actualMinimum, actualRange);
        double scale;
        if(actualRange == 0.0) {
            scale = 1.0;
        } else {
            scale = targetRange / actualRange;
        }
        for( ; beginAt != endAt; ++beginAt) {
            Event &event = score[beginAt];
            event[dimension] = event[dimension] - actualMinimum;
            if(rescaleRange) {
                event[dimension] = event[dimension] * scale;
            }
            if(rescaleMinimum) {
                event[dimension] = event[dimension] + targetMinimum;
            } else {
                event[dimension] = event[dimension] + actualMinimum;
            }
        }
        // Also rescale chord times.
        if (dimension == Event::TIME) {
            std::map<double, Chord> temp;
            for (auto it = chords_for_times.begin(); it != chords_for_times.end(); ++it) {
                double tyme = it->first;
                const Chord &chord = it->second;
                tyme = tyme - actualMinimum;
                if (rescaleRange) {
                    tyme = tyme * scale;
                }
                if (rescaleMinimum) {
                    tyme = tyme + targetMinimum;
                } else {
                    tyme = tyme + actualMinimum;
                }
                temp[tyme] = chord;
            }
            chords_for_times = temp;
        }
    }

    double getDuration()
    {
        double start = 0.0;
        double end = 0.0;
        for (int i = 0, n = size(); i < n; ++i) {
            const Event &event = at(i);
            if (i == 0) {
                start = event.getTime();
                end = event.getOffTime();
            } else {
                if (event.getTime() < start) {
                    start = event.getTime();
                }
                if (event.getOffTime() > end) {
                    end = event.getOffTime();
                }
            }
        }
        auto chord_begin = chords_for_times.begin();
        auto chord_rbegin = chords_for_times.rbegin();
        if (chord_begin != chords_for_times.end() && chord_rbegin != chords_for_times.rend()) {
            start = std::min(start, chord_begin->first);
            end = std::max(end, chord_rbegin->first);
        }
        return end - start;
    }

    void setDuration(double targetDuration)
    {
        double currentDuration = getDuration();
        if (currentDuration == 0.0) {
            return;
        }
        double factor = targetDuration / currentDuration;
        for (size_t i = 0, n = size(); i < n; i++) {
            Event &event = (*this)[i];
            double time_ = event.getTime();
            double duration = event.getDuration();
            event.setTime(time_ * factor);
            event.setDuration(duration * factor);
        }
        std::map<double, Chord> temp;
        for (auto it = chords_for_times.begin(); it != chords_for_times.end(); ++it) {
            double tyme = it->first;
            const Chord &chord = it->second;
            tyme = tyme * factor;
            temp[tyme] = chord;
        }
        chords_for_times = temp;
    }
};

} // End of namespace csound.
#endif

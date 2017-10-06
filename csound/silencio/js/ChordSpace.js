/**
C H O R D S P A C E

Copyright (C) 2014 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an algorithmic music composition library for Csound.

REGARDING BLUE

Steven Yi's Java program blue, for composing with Csound, uses the Nashorn
JavaScript runtime and does not support the DOM or other objects found in a
Web browser's JavaScript context. But, if Silencio.js is the first script loaded
by Nashorn, proxies for such of those objects as are used by Silencio, such as
`console.log`, are monkey-patched in.

*/
if (typeof console === 'undefined') {
    var console = {'log': print};
}

(function() {

    // All JavaScript dependencies of ChordSpace.js:
    // var Silencio = require("Silencio");

    var ChordSpace = {};

    ChordSpace.EPSILON = 1;
    ChordSpace.epsilonFactor = 1000;

    while (true) {
        ChordSpace.EPSILON = ChordSpace.EPSILON / 2;
        var nextEpsilon = ChordSpace.EPSILON / 2;
        var onePlusNextEpsilon = 1 + nextEpsilon;
        if (onePlusNextEpsilon == 1) {
            console.log('ChordSpace EPSILON: ' + ChordSpace.EPSILON);
            break;
        }
    }

    /**
     * C H O R D S P A C E
     *
     * Copyright 2010, 2011, 2015, 2016 by Michael Gogins.
     *
     * This software is licensed under the terms of the GNU Lesser General Public
     * License.
     *
     * This package, part of Silencio, implements a geometric approach to some common
     * operations on chords in neo-Riemannian music theory for use in score
     * generating procedures:
     *
     * --  Identifying whether a chord belongs to some equivalence class of music
     *     theory, or sending a chord to its equivalent within a representative
     *     fundamental domain of some equivalence class. The equivalence classes are
     *     octave (O), permutational (P), transpositional, (T), inversional (I), and
     *     their compounds OP, OPT (set-class or chord type), and OPTI (prime form).
     *
     * --  Causing chord progressions to move strictly within an orbifold that
     *     defined by some equivalence class.
     *
     * --  Implementing chord progressions based on the L, P, R, D, K, Q, and J
     *     operations of neo-Riemannian theory (thus implementing some aspects of
     *     "harmony").
     *
     * --  Implementing chord progressions performed within a more abstract
     *     equivalence class by means of the best-formed voice-leading within a less
     *     abstract equivalence class (thus implementing some fundamentals of
     *     "counterpoint").
     *
     * --  Implementing the direct product of the four additive groups
     *     OPTI x T x I x V (octavewise revoicings within a given range). This
     *     provides a set of 4 independent "knobs" for the composer to control prime
     *     form or set class, transposition, inversion, and voicing (octavewise
     *     permutation of voices within a specified range).
     *
     * DEFINITIONS
     *
     * Pitch is the perception of a distinct sound frequency. It is a logarithmic
     * perception; octaves, which sound 'equivalent' in a basic sense, represent
     * doublings or halvings of frequency.
     *
     * Pitches and intervals are represented as real numbers. Middle C is 60 and the
     * octave is 12. Our usual system of 12-tone equal temperament, as well as MIDI
     * key numbers, are completely represented by the whole numbers; any and all
     * other pitches can be represented simply by using fractions.
     *
     * A voice is a distinct sound that is heard as having a pitch.
     *
     * A chord is simply a set of voiceN heard at the same time, represented here
     * as a point in a chord space having one dimension of pitch for each voice
     * in the chord.
     *
     * For the purposes of algorithmic composition in Silencio, a score is considered
     * to be a sequence of more or less fleeting chords.
     *
     * EQUIVALENCE CLASSES
     *
     * An equivalence class identifies elements of a set. Operations that send one
     * equivalent point to another induce quotient spaces or orbifolds, where the
     * equivalence operation identifies points on one face of the orbifold with
     * points on an opposing face. The fundamental domain of the equivalence class
     * is the space "within" the orbifold.
     *
     * Plain chord space has no equivalence classes. Ordered chords are represented
     * as vectors in parentheses (p1, ..., pN). Unordered chords are represented as
     * sorted vectors in braces {p1, ..., pN}. Unordering is itself an equivalence
     * class (P).
     *
     * The following equivalence classes apply to pitches and chords, and exist in
     * different orbifolds. Equivalence classes can be combined (Callendar, Quinn,
     * and Tymoczko, "Generalized Voice-Leading Spaces," _Science_ 320, 2008), and
     * the more equivalence classes are combined, the more abstract is the resulting
     * orbifold compared to the parent space.
     *
     * In most cases, a chord space can be divided into a number, possibly
     * infinite, of geometrically equivalent fundamental domains for the same
     * equivalence class. Therefore, here we use the notion of 'representative'
     * fundamental domain. For example, the representative fundamental domain of
     * unordered sequences, out of all possible orderings, consists of all sequences
     * in their ordinary sorted order. It is important, in the following, to identify
     * representative fundamental domains that combine properly, e.g. such that the
     * representative fundamental domain of OP / the representative fundamental
     * domain of PI equals the representative fundamental domain of OPI. And this in
     * turn may require accounting for duplicate elements of the representative
     * fundamental domain caused by reflections or singularities in the orbifold.
     *
     * C       Cardinality equivalence, e.g. {1, 1, 2} == {1, 2}. _Not_ assuming
     *         cardinality equivalence ensures that there is a proto-metric in plain
     *         chord space that is inherited by all child chord spaces. Cardinality
     *         equivalence is never assumed here, because we are working in chord
     *         spaces of fixed dimensionality; e.g. we represent the note middle C
     *         not as {60}, but as {60, 60, ..., 60}.
     *
     * O       Octave equivalence. The fundamental domain is defined by the pitches
     *         in a chord spanning the range of an octave or less, and summing to
     *         an octave or less.
     *
     * P       Permutational equivalence. The fundamental domain is defined by a
     *         "wedge" of plain chord space in which the voiceN of a chord are always
     *         sorted by pitch.
     *
     * T       Transpositional equivalence, e.g. {1, 2} == {7, 8}. The fundamental
     *         domain is defined as a plane in chord space at right angles to the
     *         diagonal of unison chords. Represented by the chord always having a
     *         sum of pitches equal to 0.
     *
     * I       Inversional equivalence. Care is needed to distinguish the
     *         mathematician's sense of 'invert', which means 'pitch-space inversion'
     *         or 'reflect in a point', from the musician's sense of 'invert', which
     *         varies according to context but in practice often means 'registral
     *         inversion' or 'revoice by adding an octave to the lowest tone of a
     *         chord.' Here, we use 'invert' and 'inversion' in the mathematician's
     *         sense, and we use the terms 'revoice' and 'voicing' for the musician's
     *         'invert' and 'inversion'. The inversion point for any inversion lies
     *         on the unison diagonal. A fundamental domain is defined as any half of
     *         chord space that is bounded by a plane containing the inversion point.
     *         Represented as the chord having the first interval between voiceN be
     *         smaller than or equal to the final interval (recursing for chords of
     *         more than 3 voiceN).
     *
     * PI      Inversional equivalence with permutational equivalence. The
     *         'inversion flat' of unordered chord space is a hyperplane consisting
     *         of all those unordered chords that are invariant under inversion. A
     *         fundamental domain is defined by any half space bounded by a
     *         hyperplane containing the inversion flat. It is represented as that
     *         half of the space on or lower than the hyperplane defined by the
     *         inversion flat and the unison diagonal.
     *
     * OP      Octave equivalence with permutational equivalence. Tymoczko's orbifold
     *         for chords; i.e. chords with a fixed number of voiceN in a harmonic
     *         context. The fundamental domain is defined as a hyperprism one octave
     *         long with as many sides as voiceN and the ends identified by octave
     *         equivalence and one cyclical permutation of voiceN, modulo the
     *         unordering. In OP for trichords in 12TET, the augmented triads run up
     *         the middle of the prism, the major and minor triads are in 6
     *         alternating columns around the augmented triads, the two-pitch chords
     *         form the 3 sides, and the one-pitch chords form the 3 edges that join
     *         the sides.
     *
     * OPT     The layer of the OP prism as close as possible to the origin, modulo
     *         the number of voiceN. Chord type. Note that CM and Cm are different
     *         OPT. Because the OP prism is canted down from the origin, at least one
     *         pitch in each OPT chord (excepting the origin itself) is negative.
     *
     * OPI     The OP prism modulo inversion, i.e. 1/2 of the OP prism. The
     *         representative fundamental consists of those chords less than or equal
     *         to their inversions modulo OP.
     *
     * OPTI    The OPT layer modulo inversion, i.e. 1/2 of the OPT layer.
     *         Set-class. Note that CM and Cm are the same OPTI.
     *
     * TRANSFORMATIONS
     *
     * Each of the above equivalence classes is, of course, a transformation that
     * sends chords outside the fundamental domain to chords inside the fundamental
     * domain. And we define the following additional transformations:
     *
     * T(p, x)         Translate p by x.
     *
     * I(p [, x])      Reflect p in x, by default the origin.
     *
     * P               Send a major triad to the minor triad with the same root,
     *                 or vice versa (Riemann's parallel transformation).
     *
     * L               Send a major triad to the minor triad one major third higher,
     *                 or vice versa (Riemann's Leittonwechsel or leading-tone
     *                 exchange transformation).
     *
     * R               Send a major triad to the minor triad one minor third lower,
     *                 or vice versa (Riemann's relative transformation).
     *
     * D               Send a triad to the next triad a perfect fifth lower
     *                 (dominant transformation).
     *
     * P, L, and R have been extended as follows, see Fiore and Satyendra,
     * "Generalized Contextual Groups", _Music Theory Online_ 11, August 2008:
     *
     * K(c)            Interchange by inversion;
     *                 K(c) := I(c, c[1] + c[2]).
     *                 This is a generalized form of P; for major and minor triads,
     *                 it is exactly the same as P, but it also works with other
     *                 chord types.
     *
     * Q(c, n, m)      Contexual transposition;
     *                 Q(c, n, m) := T(c, n) if c is a T-form of m,
     *                 or T(c, -n) if c is an I-form of M. Not a generalized form
     *                 of L or R; but, like them, K and Q generate the T-I group.
     *
     * J is from Jessica Rudman, _Common-Tone Preserving Contextual Inversions
     * in the Music of Ellen Taaffe Zwilich_, City University of New York, 2015.
     * J(c, n [, g [, i]])  Contextual inversion;
     *                 J(c, n [, g [, i]]) returns all (or, optionally, the ith)
     *                 inversion(s) of chord c that preserve the pitch-classes of n
     *                 voices of c. The remaining voices of c invert "around" the
     *                 invariant pitch-classes. The inversions need  not preserve any
     *                 equivalence classes with respect to the inverted pitches. If
     *                 there is no such inversion, an empty list is returned. If
     *                 there is more than one such inversion, an ordered list of them
     *                 is returned. Algorithm:
     *                 (1) Create an empty list of inverted chords.
     *                 (2) For each pitch-class from 0 to 12 step g (g is the
     *                     generator of transposition, e.g. 1 in TET):
     *                     (a) Invert c in the pitch-class.
     *                     (b) Test if n pitch-classes of pc are preserved.
     *                     (c) If so, add the inverted chord to the list of
     *                         inversions.
     *                     (d) Sort the list of inversions.
     *                 (3) Return the list of inversions (or, optionally, the ith
     *                     inversion in the list).
     */

    ChordSpace.factorial = function(n) {
        if (n === 0) {
            return 1;
        } else {
            return n * ChordSpace.factorial(n - 1);
        }
    };

    ChordSpace.eq_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        if (Math.abs(a - b) < (ChordSpace.EPSILON * factor)) {
            return true;
        }
        return false;
    };

    ChordSpace.gt_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        var eq = ChordSpace.eq_epsilon(a, b, factor);
        if (eq) {
            return false;
        }
        if (a > b) {
            return true;
        }
        return false;
    };

    ChordSpace.lt_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        var eq = ChordSpace.eq_epsilon(a, b, factor);
        if (eq) {
            return false;
        }
        if (a < b) {
            return true;
        }
        return false;
    };

    ChordSpace.ge_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        var eq = ChordSpace.eq_epsilon(a, b, factor);
        if (eq) {
            return true;
        }
        if (a > b) {
            return true;
        }
        return false;
    };

    ChordSpace.le_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        var eq = ChordSpace.eq_epsilon(a, b, factor);
        if (eq) {
            return true;
        }
        if (a < b) {
            return true;
        }
        return false;
    };

    ChordSpace.compare_epsilon = function(a, b) {
        if (ChordSpace.lt_epsilon(a, b)) {
            return -1;
        }
        if (ChordSpace.gt_epsilon(a, b)) {
            return 1;
        }
        return 0;
    };

    // The size of the octave, defined to be consistent with
    // 12 tone equal temperament and MIDI.
    ChordSpace.OCTAVE = 12;

    // Middle C.
    ChordSpace.MIDDLE_C = 60;

    // Returns the pitch transposed by semitones, which may be any scalar.
    // NOTE: Does NOT return the result under any equivalence class.
    ChordSpace.T = function(pitch, semitones) {
        return pitch + semitones;
    };

    // Returns the pitch reflected in the center, which may be any pitch.
    // NOTE: Does NOT return the result under any equivalence class.
    ChordSpace.I = function(pitch, center) {
        center = typeof center !== 'undefined' ? center : 0;
        return center - pitch;
    };

    // Returns the Euclidean distance between chords a and b,
    // which must have the same number of voiceN.
    ChordSpace.euclidean = function(a, b) {
        var sumOfSquaredDifferences = 0;
        for (var voice = 0; voice < a.voices.length; voice++) {
            sumOfSquaredDifferences = sumOfSquaredDifferences + Math.pow((a.voices[voice] - b.voices[voice]), 2);
        }
        return Math.sqrt(sumOfSquaredDifferences);
    };

    // A chord is one point in a space with one dimension per voice.
    // Pitches are represented as semitones with 0 at the origin
    // and middle C as 60.
    var Chord = function() {
        this.voices = [];
        this.duration = [];
        this.channel = [];
        this.velocity = [];
        this.pan = [];
    };
    ChordSpace.Chord = Chord;

    Chord.prototype.size = function() {
        return this.voices.length;
    };

    // Resizes a chord to the specified number of voiceN.
    // Existing voiceN are not changed. Extra voiceN are removed.
    // New voiceN are initialized to 0.
    Chord.prototype.resize = function(voiceN) {
        var original_length = this.voices.length;
        this.voices.length = voiceN;
        this.duration.length = voiceN;
        this.channel.length = voiceN;
        this.velocity.length = voiceN;
        this.pan.length = voiceN;
        for (var voice = original_length; voice < voiceN; voice++) {
            this.voices[voice] = 0;
            this.duration[voice] = 0;
            this.channel[voice] = 0;
            this.velocity[voice] = 0;
            this.pan[voice] = 0;
        }
    };

    // Resizes the chord to the length of the array, and sets
    // the pitches from the values of the array.
    Chord.prototype.set = function(array) {
        this.resize(array.length);
        for (var i = 0; i < this.size(); i++) {
            this.voices[i] = array[i];
        }
    };

    Chord.prototype.add = function(pitch) {
        this.resize(this.size() + 1);
        this.voices[this.size() - 1] = pitch;
        return this;
    };

    Chord.prototype.setPitch = function(voice, value) {
        this.voices[voice] = value;
    };

    Chord.prototype.getPitch = function(voice) {
        return this.voices[voice];
    };

    Chord.prototype.setDuration = function(value) {
        for (var voice = 0; voice < this.voices.length; voice++) {
            this.voices[voice] = value;
        }
    };

    Chord.prototype.getDuration = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.duration[voice];
    };

    Chord.prototype.setChannel = function(value) {
        for (var voice = 0; voice < this.voices.length; voice++) {
            this.channel[voice] = value;
        }
    };

    Chord.prototype.getChannel = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.channel[voice];
    };

    Chord.prototype.setVelocity = function(value) {
        for (var voice = 0; voice < this.voices.length; voice++) {
            this.velocity[voice] = value;
        }
    };

    Chord.prototype.getVelocity = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.velocity[voice];
    };

    Chord.prototype.setPan = function(value) {
        for (var voice = 0; voice < this.voices.length; voice++) {
            this.pan[voice] = value;
        }
    };

    Chord.prototype.getPan = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.pan[voice];
    };

    Chord.prototype.count = function(pitch) {
        var n = 0;
        for (var voice = 0; voice < this.voices.length; voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], pitch)) {
                n++;
            }
        }
        return n;
    };

    // Returns a string representation of the chord.
    // Quadratic complexity, but short enough not to matter.
    Chord.prototype.toString = function() {
        var buffer = '[';
        for (var voice = 0; voice < this.voices.length; voice++) {
            buffer = buffer + sprintf('%12.7f ', this.voices[voice]);
        }
        buffer = buffer + ']';
        return buffer;
    };

    // Implements value semantics for ==, for the pitches in this only.
    Chord.prototype.eq_epsilon = function(other) {
        if (this.voices.length !== other.voices.length) {
            return false;
        }
        for (var voice = 0; voice < this.voices.length; voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], other.voices[voice]) === false) {
                return false;
            }
        }
        return true;
    };

    Chord.prototype.lt_epsilon = function(other) {
        var voiceN = Math.min(this.voices.length, other.voices.length);
        for (var voice = 0; voice < voiceN; voice++) {
            if (ChordSpace.lt_epsilon(this.voices[voice], other.voices[voice])) {
                return true;
            }
            if (ChordSpace.gt_epsilon(this.voices[voice], other.voices[voice])) {
                return false;
            }
        }
        if (this.voices.length < other.voices.length) {
            return true;
        }
        return true;
    };

    Chord.prototype.gt_epsilon = function(other) {
        var voiceN = Math.min(this.voices.length, other.voices.length);
        for (var voice = 0; voice < voiceN; voice++) {
            if (ChordSpace.gt_epsilon(this.voices[voice], other.voices[voice])) {
                return true;
            }
            if (ChordSpace.lt_epsilon(this.voices[voice], other.voices[voice])) {
                return false;
            }
        }
        if (this.voices.length < other.voices.length) {
            return false;
        }
        return true;
    };

    Chord.prototype.le_epsilon = function(other) {
        if (this.eq_epsilon(other)) {
            return true;
        }
        return this.lt_epsilon(other);
    };

    // Returns whether or not the chord contains the pitch.
    Chord.prototype.contains = function(pitch) {
        for (var voice = 0; voice < this.voices.length; voice++) {
            if (this.voices[voice] === pitch) {
                return true;
            }
        }
        return false;
    };

    ChordSpace.chord_compare_epsilon = function(a, b) {
        if (a.lt_epsilon(b)) {
            return -1;
        }
        if (a.gt_epsilon(b)) {
            return 1;
        }
        return 0;
    };

    // This hash function is used e.g. to give chords value semantics for sets.
    Chord.prototype.hash = function() {
        var buffer = '';
        for (var voice = 0; voice < this.voices.length; voice++) {
            var value = this.voices[voice].toFixed(6).trim();
            if (voice === 0) {
                buffer = buffer.concat(value);
            } else {
                buffer = buffer.concat(',', value);
            }
        }
        return buffer;
    };

    // Returns the lowest pitch in the chord,
    // and also its voice index.
    Chord.prototype.min = function() {
        var lowestVoice = 0;
        var lowestPitch = this.voices[lowestVoice];
        for (var voice = 1; voice < this.voices.length; voice++) {
            if (ChordSpace.lt_epsilon(this.voices[voice], lowestPitch) === true) {
                lowestPitch = this.voices[voice];
                lowestVoice = voice;
            }
        }
        return [lowestPitch, lowestVoice];
    };

    // Returns the minimum interval in the chord.
    Chord.prototype.minimumInterval = function() {
        var minimumInterval_ = Math.abs(this.voices[1] - this.voices[2]);
        for (var v1 = 1; v1 < this.voices.length; v1++) {
            for (var v2 = 1; v2 < this.voices.length; v2++) {
                if (v1 == v2) {
                    var interval = Math.abs(this.voices[v1] - this.voices[v2]);
                    if (interval < minimumInterval_) {
                        minimumInterval_ = interval;
                    }
                }
            }
        }
        return minimumInterval_;
    };

    // Returns the highest pitch in the chord,
    // and also its voice index.
    Chord.prototype.max = function() {
        var highestVoice = 0;
        var highestPitch = this.voices[highestVoice];
        for (var voice = 1; voice < this.voices.length; voice++) {
            if (this.voices[voice] > highestPitch) {
                highestPitch = this.voices[voice];
                highestVoice = voice;
            }
        }
        return [highestPitch, highestVoice];
    };

    // Returns the maximum interval in the chord.
    Chord.prototype.maximumInterval = function() {
        var maximumInterval_ = Math.abs(this.voices[1] - this.voices[2]);
        for (var v1 = 0; v1 < this.voices.length; v1++) {
            for (var v2 = 0; v2 < this.voices.length; v2++) {
                if (v1 != v2) {
                    var interval = Math.abs(this.voices[v1] - this.voices[v2]);
                    if (interval > maximumInterval_) {
                        maximumInterval_ = interval;
                    }
                }
            }
        }
        return maximumInterval_;
    };

    // Returns a value copy of the chord.
    Chord.prototype.clone = function() {
        var clone_ = new Chord();
        clone_.resize(this.size());
        for (var voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = this.voices[voice];
            clone_.duration[voice] = this.duration[voice];
            clone_.channel[voice] = this.channel[voice];
            clone_.velocity[voice] = this.velocity[voice];
            clone_.pan[voice] = this.pan[voice];
        }
        return clone_;
    };

    // Returns a new chord whose pitches are the floors of this chord's pitches.
    Chord.prototype.floor = function() {
        var chord = this.clone();
        for (var voice = 0; voice < this.voices.length; voice++) {
            chord.voices[voice] = Math.floor(this.voices[voice]);
        }
        return chord;
    };

    // Returns a new chord whose pitches are the ceilings of this chord's pitches.
    Chord.prototype.ceil = function() {
        var chord = this.clone();
        for (var voice = 0; voice < this.voices.length; voice++) {
            chord.voices[voice] = Math.ceil(this.voices[voice]);
        }
        return chord;
    };

    // Returns the origin of the chord's space.
    Chord.prototype.origin = function() {
        var clone_ = this.clone();
        for (var voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = 0;
        }
        return clone_;
    };

    Chord.prototype.distanceToOrigin = function() {
        var origin = this.origin();
        return ChordSpace.euclidean(this, origin);
    };

    // Returns the sum of the pitches in the chord.
    Chord.prototype.layer = function() {
        var s = 0;
        for (var voice = 0; voice < this.size(); voice++) {
            s = s + this.voices[voice];
        }
        return s;
    };

    // Returns the Euclidean distance from this chord
    // to the unison diagonal of its chord space.
    Chord.prototype.distanceToUnisonDiagonal = function() {
        var unison = this.origin();
        var pitch = this.layer() / this.size();
        for (var voice = 0; voice < this.size(); voice++) {
            unison.voices[voice] = pitch;
        }
        return ChordSpace.euclidean(this, unison);
    };

    // Returns the maximally even chord in the chord's space,
    // e.g. the augmented triad for 3 dimensions.
    Chord.prototype.maximallyEven = function() {
        var clone_ = this.clone();
        var g = ChordSpace.OCTAVE / clone_.size();
        for (var i = 0; i < clone_.size(); i++) {
            clone_.voices[i] = i * g;
        }
        return clone_;
    };

    // Transposes the chord by the indicated interval (may be a fraction).
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.T = function(interval) {
        var clone_ = this.clone();
        for (var voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = ChordSpace.T(this.voices[voice], interval);
        }
        return clone_;
    };

    // Inverts the chord by another chord that is on the unison diagonal, by
    // default the origin.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.I = function(center) {
        center = typeof center !== 'undefined' ? center : 0;
        var inverse = this.clone();
        for (var voice = 0; voice < this.size(); voice++) {
            inverse.voices[voice] = ChordSpace.I(this.voices[voice], center);
        }
        return inverse;
    };

    // Returns the remainder of the dividend divided by the divisor,
    // according to the Euclidean definition.
    ChordSpace.modulo = function(dividend, divisor) {
        var quotient = 0.0;
        if (divisor < 0.0) {
            quotient = Math.ceil(dividend / divisor);
        }
        if (divisor > 0.0) {
            quotient = Math.floor(dividend / divisor);
        }
        var remainder = dividend - (quotient * divisor);
        return remainder;
    };

    // Returns the equivalent of the pitch under pitch-class equivalence, i.e.
    // the pitch is in the interval [0, OCTAVE).
    ChordSpace.epc = function(pitch) {
        var pc = ChordSpace.modulo(pitch, ChordSpace.OCTAVE);
        return pc;
    };

    // Returns whether the chord is within the fundamental domain of
    // pitch-class equivalence, i.e. is a pitch-class set.
    Chord.prototype.isepcs = function() {
        for (var voice = 0; voice < this.size(); voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], ChordSpace.epc(chord.voices[voice])) === false) {
                return false;
            }
        }
        return true;
    };

    Chord.prototype.er = function(range) {
        var chord = this.clone();
        for (var voice = 0; voice < this.size(); voice++) {
            chord.voices[voice] = ChordSpace.modulo(chord.voices[voice], range);
        }
        return chord;
    };

    // Returns the equivalent of the chord under pitch-class equivalence,
    // i.e. the pitch-class set of the chord.
    Chord.prototype.epcs = function() {
        return this.er(ChordSpace.OCTAVE);
    };

    Chord.prototype.eopcs = function() {
        return this.er(ChordSpace.OCTAVE).eP();
    };

    // Returns the equivalent of the chord within the fundamental domain of
    // transposition to 0.
    Chord.prototype.et = function() {
        var min_ = this.min();
        return this.T(-min_[0]);
    };

    // Returns whether the chord is within the fundamental domain of
    // transposition to 0.
    Chord.prototype.iset = function() {
        var et = this.et();
        if (et.eq_epsilon(this) === false) {
            return false;
        }
        return true;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of the indicated range equivalence.
    Chord.prototype.iseR = function(range) {
        var max_ = this.max()[0];
        var min_ = this.min()[0];
        if (ChordSpace.le_epsilon(max_, (min_ + range)) === false) {
            return false;
        }
        var layer_ = this.layer();
        if (ChordSpace.le_epsilon(0, layer_) === false) {
            return false;
        }
        if (ChordSpace.le_epsilon(layer_, range) === false) {
            return false;
        }
        return true;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of octave equivalence.
    Chord.prototype.iseO = function() {
        return this.iseR(ChordSpace.OCTAVE);
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of a range equivalence.
    Chord.prototype.eR = function(range) {
        // The clue here is that at least one voice must be >= 0,
        // but no voice can be > range.
        // First, move all pitches inside the interval [0,  range),
        // which is not the same as the fundamental domain.
        var normal = this.er(range);
        // Then, reflect voices that are outside of the fundamental domain
        // back into it, which will revoice the chord, i.e.
        // the sum of pitches will then be in [0,  range].
        while (ChordSpace.lt_epsilon(normal.layer(), range) === false) {
            var max_ = normal.max();
            var maximumPitch = max_[0];
            var maximumVoice = max_[1];
            // Because no voice is above the range,
            // any voices that need to be revoiced will now be negative.
            normal.voices[maximumVoice] = maximumPitch - range;
        }
        return normal;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of octave equivalence.
    Chord.prototype.eO = function() {
        return this.eR(ChordSpace.OCTAVE);
    };

    // Returns whether the chord is within the representative fundamental domain
    // of permutational equivalence.
    Chord.prototype.iseP = function() {
        for (var voice = 1; voice < this.size(); voice++) {
            if (ChordSpace.le_epsilon(this.voices[voice - 1], this.voices[voice]) === false) {
                return false;
            }
        }
        return true;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of permutational equivalence.
    // NB: Order is correct!
    Chord.prototype.eP = function() {
        clone_ = this.clone();
        clone_.voices.sort(ChordSpace.compare_epsilon);
        return clone_;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of transpositional equivalence.
    Chord.prototype.iseT = function() {
        var layer_ = this.layer();
        if (ChordSpace.eq_epsilon(layer_, 0) === false) {
            return false;
        }
        return true;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of transpositonal equivalence.
    Chord.prototype.eT = function() {
        var layer_ = this.layer();
        var sumPerVoice = layer_ / this.size();
        return this.T(-sumPerVoice);
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of transpositonal equivalence and the equal temperament generated
    // by g. I.e., returns the chord transposed such that its layer is 0 or, under
    // transposition, the positive integral layer closest to 0. NOTE: Does NOT
    // return the result under any other equivalence class.
    Chord.prototype.eTT = function(g) {
        g = typeof g !== 'undefined' ? g : 1;
        var normal = this.eT();
        var indg = Math.ceil(normal.voices[0] / g);
        var transposition = (indg * g) - normal.voices[0];
        normal = normal.T(transposition);
        return normal;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of translational equivalence and the equal temperament generated by g.
    Chord.prototype.iseTT = function(g) {
        g = typeof g !== 'undefined' ? g : 1;
        var ep = this.eP();
        if (ep.eq_epsilon(ep.eTT(g)) === false) {
            return false;
        }
        return true;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of inversional equivalence.
    Chord.prototype.iseI = function(inverse) {
        var lowerVoice = 1;
        var upperVoice = this.size();
        while (lowerVoice < upperVoice) {
            var lowerInterval = this.voices[lowerVoice] - this.voices[lowerVoice - 1];
            var upperInterval = this.voices[upperVoice] - this.voices[upperVoice - 1];
            if (ChordSpace.lt_epsilon(lowerInterval, upperInterval)) {
                return true;
            }
            if (ChordSpace.gt_epsilon(lowerInterval, upperInterval)) {
                return false;
            }
            lowerVoice = lowerVoice + 1;
            upperVoice = upperVoice - 1;
        }
        return true;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of inversional equivalence.
    // FIXME: Do I need the "inverse" argument and would that work correctly?
    Chord.prototype.eI = function() {
        if (this.iseI()) {
            return this.clone();
        }
        return this.I();
    };

    // Returns whether the chord is within the representative fundamental domain
    // of range and permutational equivalence.
    Chord.prototype.iseRP = function(range) {
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
            return false;
        }
        return true;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of octave and permutational equivalence.
    Chord.prototype.iseOP = function() {
        return this.iseRP(ChordSpace.OCTAVE);
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of range and permutational equivalence.
    Chord.prototype.eRP = function(range) {
        return this.eR(range).eP();
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of octave and permutational equivalence.
    Chord.prototype.eOP = function() {
        return this.eRP(ChordSpace.OCTAVE);
    };

    // Returns a copy of the chord cyclically permuted by a stride, by default 1.
    // The direction of rotation is the same as musicians' first inversion, second
    // inversion, and so on.
    Chord.prototype.cycle = function(stride) {
        stride = typeof stride !== 'undefined' ? stride : 1;
        var permuted = this.clone();
        var i = 0;
        if (stride < 0) {
            for (i = 0; i < Math.abs(stride); i++) {
                var tail = permuted.voices.pop();
                permuted.voices.unshift(tail);
            }
            return permuted;
        }
        if (stride > 0) {
            for (i = 0; i < stride; i++) {
                var head = permuted.voices.shift();
                permuted.voices.push(head);
            }
        }
        return permuted;
    };

    // Returns the permutations of the pitches in a chord. The permutations from
    // any particular permutation are always returned in the same order.
    // FIXME: This is not thought through. It does what I said to do, but that may
    // not be what I meant.
    Chord.prototype.permutations = function() {
        var permutation = this.clone();
        var permutations_ = [];
        permutations_.push(permutation);
        for (var i = 1; i < this.size(); i++) {
            permutation = permutation.cycle(1);
            permutations_.push(permutation);
        }
        permutations_.sort(ChordSpace.chord_compare_epsilon);
        return permutations_;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of voicing equivalence.
    Chord.prototype.iseV = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        var outer = this.voices[0] + range - this.voices[this.size() - 1];
        var inner;
        var voice;
        for (voice = 0; voice < this.size() - 2; voice++) {
            inner = this.voices[voice + 1] - this.voices[voice];
            if (ChordSpace.ge_epsilon(outer, inner) === false) {
                return false;
            }
        }
        return true;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of voicing equivalence.
    Chord.prototype.eV = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        var permutations = this.permutations();
        for (var i = 0; i < this.size(); i++) {
            var permutation = permutations[i];
            if (permutation.iseV(range)) {
                return permutation;
            }
        }
    };

    // Returns whether the chord is within the representative fundamental domain
    // of range, permutational, and transpositional equivalence.
    Chord.prototype.iseRPT = function(range) {
        if (this.iseR(range) === false) {
            return false;
        }
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseT() === false) {
            return false;
        }
        if (this.iseV() === false) {
            return false;
        }
        return true;
    };

    Chord.prototype.iseRPTT = function(range) {
        if (this.iseR(range) === false) {
            return false;
        }
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseTT() === false) {
            return false;
        }
        if (this.iseV() === false) {
            return false;
        }
        return true;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of octave, permutational, and transpositional equivalence.
    Chord.prototype.iseOPT = function() {
        return this.iseRPT(ChordSpace.OCTAVE);
    };

    Chord.prototype.iseOPTT = function() {
        return this.iseRPTT(ChordSpace.OCTAVE);
    };

    // Returns a copy of the chord 'inverted' in the musician's sense,
    // i.e. revoiced by cyclically permuting the chord and
    // adding (or subtracting) an octave to the highest (or lowest) voice.
    // The revoicing will move the chord up or down in pitch.
    // A positive direction is the same as a musician's first inversion,
    // second inversion, etc.
    // FIXME: Original is probably not correct.
    Chord.prototype.v = function(direction) {
        direction = typeof direction !== 'undefined' ? direction : 1;
        var chord = this.clone();
        while (direction > 0) {
            chord.voices[0] = chord.voices[0] + ChordSpace.OCTAVE;
            chord = chord.cycle(1);
            direction = direction - 1;
        }
        var n = chord.size() - 1;
        while (direction < 0) {
            chord.voices[n] = chord.voices[n] - ChordSpace.OCTAVE;
            chord = chord.cycle(-1);
            direction = direction + 1;
        }
        return chord;
    };

    // Returns all the 'inversions' (in the musician's sense)
    // or octavewise revoicings of the chord.
    Chord.prototype.voicings = function() {
        var chord = this.clone();
        var voicings = [];
        voicings.push(chord);
        for (var i = 1; i < chord.size(); i++) {
            chord = chord.v();
            voicings.push(chord);
        }
        return voicings;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of range, permutational, and transpositional equivalence; the same
    // as set-class type, or chord type.
    // FIXME: Take g into account?
    Chord.prototype.eRPT = function(range) {
        var erp = this.eRP(range);
        var voicings_ = erp.voicings();
        for (var i = 0; i < voicings_.length; i++) {
            var voicing = voicings_[i];
            if (voicing.iseV()) {
                return voicing.eT();
            }
        }
        console.log('ERROR: chord.eRPT() should not come here: ' + this);
    };

    Chord.prototype.eRPTT = function(range) {
        var erp = this.eRP(range);
        var voicings_ = erp.voicings();
        for (var i = 0; i < voicings_.length; i++) {
            var voicing = voicings_[i].eTT();
            if (voicing.iseV()) {
                return voicing;
            }
        }
        console.log('ERROR: chord.eRPTT() should not come here: ' + this);
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of octave, permutational, and transpositional equivalence.
    Chord.prototype.eOPT = function() {
        return this.eRPT(ChordSpace.OCTAVE);
    };

    Chord.prototype.eOPTT = function() {
        return this.eRPTT(ChordSpace.OCTAVE);
    };

    // Returns whether the chord is within the representative fundamental domain
    // of range, permutational, and inversional equivalence.
    Chord.prototype.iseRPI = function(range) {
        if (this.iseRP(range) === false) {
            return false;
        }
        var inverse = this.I();
        var inverseRP = inverse.eRP(range);
        //assert(inverse, 'Inverse is nil.');
        if (this.le_epsilon(inverseRP) === true) {
            return true;
        }
        return false;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of octave, permutational, and inversional equivalence.
    Chord.prototype.iseOPI = function() {
        return this.iseRPI(ChordSpace.OCTAVE);
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of range, permutational, and inversional equivalence.
    Chord.prototype.eRPI = function(range) {
        if (this.iseRPI(range) === true) {
            return this.clone();
        }
        var normalRP = this.eRP(range);
        var normalRPInverse = normalRP.I();
        var normalRPInverseRP = normalRPInverse.eRP(range);
        if (normalRP.le_epsilon(normalRPInverseRP) === true) {
            return normalRP;
        } else {
            return normalRPInverseRP;
        }
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of octave, permutational, and inversional equivalence.
    Chord.prototype.eOPI = function() {
        return this.eRPI(ChordSpace.OCTAVE);
    };

    // Returns whether the chord is within the representative fundamental domain
    // of range, permutational, transpositional, and inversional equivalence.
    Chord.prototype.iseRPTI = function(range) {
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
            return false;
        }
        if (this.iseT() === false) {
            return false;
        }
        if (this.iseV(range) === false) {
            return false;
        }
        return true;
    };

    Chord.prototype.iseRPTTI = function(range) {
        if (this.iseRPTT(range) === false) {
            return false;
        }
        var inverse = this.I();
        var normalRPTT = inverse.eRPTT(range);
        if (this.le_epsilon(normalRPTT) === true) {
            return true;
        }
        return false;
    };

    // Returns whether the chord is within the representative fundamental domain
    // of octave, permutational, transpositional, and inversional equivalence.
    Chord.prototype.iseOPTI = function() {
        return this.iseRPTI(ChordSpace.OCTAVE);
    };
    Chord.prototype.iseOPTTI = function() {
        var result = this.iseRPTTI(ChordSpace.OCTAVE);
        if (result === true) {
            console.log('Chord.prototype.iseOPTTI: ' + this + ' ' + result);
        }
        return result;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of range, permutational, transpositional, and inversional
    // equivalence.
    Chord.prototype.eRPTI = function(range) {
        var normalRPT = this.eRPT(range);
        if (normalRPT.iseI() === true) {
            return normalRPT;
        } else {
            var normalI = normalRPT.eRPI(range);
            var normalRPT_ = normalI.eRPT(range);
            return normalRPT_;
        }
    };

    Chord.prototype.eRPTTI = function(range) {
        var normalRPTT = this.eRPTT(range);
        var inverse = normalRPTT.I();
        var inverseNormalRPTT = inverse.eRPTT(range);
        if (normalRPTT.le_epsilon(inverseNormalRPTT) === true) {
            return normalRPTT;
        }
        return inverseNormalRPTT;
    };

    // Returns the equivalent of the chord within the representative fundamental
    // domain of range, permutational, transpositional, and inversional
    // equivalence.
    Chord.prototype.eOPTI = function() {
        return this.eRPTI(ChordSpace.OCTAVE);
    };
    Chord.prototype.eOPTTI = function() {
        return this.eRPTTI(ChordSpace.OCTAVE);
    };

    var pitchClassesForNames = {};

    pitchClassesForNames["C"] = 0;
    pitchClassesForNames["C#"] = 1;
    pitchClassesForNames["Db"] = 1;
    pitchClassesForNames["D"] = 2;
    pitchClassesForNames["D#"] = 3;
    pitchClassesForNames["Eb"] = 3;
    pitchClassesForNames["E"] = 4;
    pitchClassesForNames["F"] = 5;
    pitchClassesForNames["F#"] = 6;
    pitchClassesForNames["Gb"] = 6;
    pitchClassesForNames["G"] = 7;
    pitchClassesForNames["G#"] = 8;
    pitchClassesForNames["Ab"] = 8;
    pitchClassesForNames["A"] = 9;
    pitchClassesForNames["A#"] = 10;
    pitchClassesForNames["Bb"] = 10;
    pitchClassesForNames["B"] = 11;
    ChordSpace.pitchClassesForNames = pitchClassesForNames;

    var chordsForNames = {};
    var namesForChords = {};

    var fill = function(rootName, rootPitch, typeName, typePitches) {
        typePitches = typePitches.trim();
        var chordName = rootName + typeName;
        var chord = new ChordSpace.Chord();
        var splitPitches = typePitches.split(/\s+/g);
        if (typeof splitPitches !== 'undefined') {
            chord.resize(splitPitches.length);
            for (var voice = 0; voice < splitPitches.length; voice++) {
                var pitchName = splitPitches[voice];
                if (pitchName.length > 0) {
                    var pitch = ChordSpace.pitchClassesForNames[pitchName];
                    chord.voices[voice] = rootPitch + pitch;
                }
            }
            var eop = chord.eOP();
            chordsForNames[chordName] = eop;
            namesForChords[eop.hash()] = chordName;
        }
    };

    for (var rootName in pitchClassesForNames) {
        if (pitchClassesForNames.hasOwnProperty(rootName)) {
            var rootPitch = pitchClassesForNames[rootName];
            fill(rootName, rootPitch, " minor second", "C  C#                             ");
            fill(rootName, rootPitch, " major second", "C     D                           ");
            fill(rootName, rootPitch, " minor third", "C        Eb                       ");
            fill(rootName, rootPitch, " major third", "C           E                     ");
            fill(rootName, rootPitch, " perfect fourth", "C              F                  ");
            fill(rootName, rootPitch, " tritone", "C                 F#              ");
            fill(rootName, rootPitch, " perfect fifth", "C                    G            ");
            fill(rootName, rootPitch, " augmented fifth", "C                       G#        ");
            fill(rootName, rootPitch, " sixth", "C                          A      ");
            fill(rootName, rootPitch, " minor seventh  ", "C                             Bb  ");
            fill(rootName, rootPitch, " major seventh", "C                                B");
            // Scales.
            fill(rootName, rootPitch, " major", "C     D     E  F     G     A     B");
            fill(rootName, rootPitch, " minor", "C     D  Eb    F     G  Ab    Bb  ");
            fill(rootName, rootPitch, " natural minor", "C     D  Eb    F     G  Ab    Bb  ");
            fill(rootName, rootPitch, " harmonic minor", "C     D  Eb    F     G  Ab       B");
            fill(rootName, rootPitch, " chromatic", "C  C# D  D# E  F  F# G  G# A  A# B");
            fill(rootName, rootPitch, " whole tone", "C     D     E     F#    G#    A#  ");
            fill(rootName, rootPitch, " diminished", "C     D  D#    F  F#    G# A     B");
            fill(rootName, rootPitch, " pentatonic", "C     D     E        G     A      ");
            fill(rootName, rootPitch, " pentatonic major", "C     D     E        G     A      ");
            fill(rootName, rootPitch, " pentatonic minor", "C        Eb    F     G        Bb  ");
            fill(rootName, rootPitch, " augmented", "C        Eb E        G  Ab    Bb  ");
            fill(rootName, rootPitch, " Lydian dominant", "C     D     E     Gb G     A  Bb  ");
            fill(rootName, rootPitch, " 3 semitone", "C        D#       F#       A      ");
            fill(rootName, rootPitch, " 4 semitone", "C           E           G#        ");
            fill(rootName, rootPitch, " blues", "C     D  Eb    F  Gb G        Bb  ");
            fill(rootName, rootPitch, " bebop", "C     D     E  F     G     A  Bb B");
            // Major chords.
            fill(rootName, rootPitch, "M", "C           E        G            ");
            fill(rootName, rootPitch, "6", "C           E        G     A      ");
            fill(rootName, rootPitch, "69", "C     D     E        G     A      ");
            fill(rootName, rootPitch, "69b5", "C     D     E     Gb       A      ");
            fill(rootName, rootPitch, "M7", "C           E        G           B");
            fill(rootName, rootPitch, "M9", "C     D     E        G           B");
            fill(rootName, rootPitch, "M11", "C     D     E  F     G           B");
            fill(rootName, rootPitch, "M#11", "C     D     E  F#    G           B");
            fill(rootName, rootPitch, "M13", "C     D     E  F     G     A     B");
            // Minor chords.
            fill(rootName, rootPitch, "m", "C        Eb          G            ");
            fill(rootName, rootPitch, "m6", "C        Eb          G     A      ");
            fill(rootName, rootPitch, "m69", "C     D  Eb          G     A      ");
            fill(rootName, rootPitch, "m7", "C        Eb          G        Bb  ");
            fill(rootName, rootPitch, "m#7", "C        Eb          G           B");
            fill(rootName, rootPitch, "m7b5", "C        Eb       Gb          Bb  ");
            fill(rootName, rootPitch, "m9", "C     D  Eb          G        Bb  ");
            fill(rootName, rootPitch, "m9#7", "C     D  Eb          G           B");
            fill(rootName, rootPitch, "m11", "C     D  Eb    F     G        Bb  ");
            fill(rootName, rootPitch, "m13", "C     D  Eb    F     G     A  Bb  ");
            // Augmented chords.
            fill(rootName, rootPitch, "+", "C            E         G#         ");
            fill(rootName, rootPitch, "7#5", "C            E         G#     Bb  ");
            fill(rootName, rootPitch, "7b9#5", "C  Db        E         G#     Bb  ");
            fill(rootName, rootPitch, "9#5", "C     D      E         G#     Bb  ");
            // Diminished chords.
            fill(rootName, rootPitch, "o", "C        Eb       Gb              ");
            fill(rootName, rootPitch, "o7", "C        Eb       Gb       A      ");
            // Suspended chords.
            fill(rootName, rootPitch, "6sus", "C              F     G     A      ");
            fill(rootName, rootPitch, "69sus", "C     D        F     G     A      ");
            fill(rootName, rootPitch, "7sus", "C              F     G        Bb  ");
            fill(rootName, rootPitch, "9sus", "C     D        F     G        Bb  ");
            fill(rootName, rootPitch, "M7sus", "C              F     G           B");
            fill(rootName, rootPitch, "M9sus", "C     D        F     G           B");
            // Dominant chords.
            fill(rootName, rootPitch, "7", "C            E       G        Bb  ");
            fill(rootName, rootPitch, "7b5", "C            E    Gb          Bb  ");
            fill(rootName, rootPitch, "7b9", "C  Db        E       G        Bb  ");
            fill(rootName, rootPitch, "7b9b5", "C  Db        E    Gb          Bb  ");
            fill(rootName, rootPitch, "9", "C     D      E       G        Bb  ");
            fill(rootName, rootPitch, "9#11", "C     D      E F#    G        Bb  ");
            fill(rootName, rootPitch, "13", "C     D      E F     G     A  Bb  ");
            fill(rootName, rootPitch, "13#11", "C     D      E F#    G     A  Bb  ");
        }
    }

    ChordSpace.namesForChords = namesForChords;
    ChordSpace.chordsForNames = chordsForNames;

    Chord.prototype.name = function() {
        var chordName = ChordSpace.namesForChords[this.eOP().hash()];
        if (typeof chordName === 'undefined') {
            chordName = '';
        }
        return chordName;
    };

    ChordSpace.nameForChord = function(chord) {
        return chord.name();
    };

    ChordSpace.chordForName = function(name) {
        return ChordSpace.chordsForNames[name];
    };

    // Returns a formatted string with information about the chord.

    Chord.prototype.information = function() {
        var et = this.eT().et();
        var evt = this.eV().et();
        var eopt = this.eOPT().et();
        var epcs = this.eopcs();
        var eopti = this.eOPTI().et();
        var eOP = this.eOP();
        var chordName = this.name();
        return sprintf("Pitches:  %s  %s\nI:        %s\neO:       %s  iseO:    %s\neP:       %s  iseP:    %s\neT:       %s  iseT:    %s\n          %s\neI:       %s  iseI:    %s\neV:       %s  iseV:    %s\n          %s\neOP:      %s  iseOP:   %s\npcs:      %s\neOPT:     %s  iseOPT:  %s\neOPTT:    %s\n          %s\neOPI:     %s  iseOPI:  %s\neOPTI:    %s  iseOPTI: %s\neOPTTI:   %s\n          %s\nlayer:      %6.2f",
            this, chordName,
            this.I(),
            this.eO(), this.iseO(),
            this.eP(), this.iseP(),
            this.eT(), this.iseT(),
            et,
            this.eI(), this.iseI(),
            this.eV(), this.iseV(),
            evt,
            this.eOP(), this.iseOP(),
            epcs,
            this.eOPT(), this.iseOPT(),
            this.eOPTT(),
            eopt,
            this.eOPI(), this.iseOPI(),
            this.eOPTI(), this.iseOPTI(),
            this.eOPTTI(),
            eopti,
            this.layer());
    };

    // Move 1 voice of the chord.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.move = function(voice, interval) {
        var chord = this.clone();
        chord.voices[voice] = ChordSpace.T(chord.voices[voice], interval);
        return chord;
    };

    // Performs the neo-Riemannian parallel transformation.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.nrP = function() {
        var cv = this.eV();
        var cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[1] = cv.voices[1] - 1;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[1] = cv.voices[1] + 1;
        }
        return cv;
    };

    // Performs the neo-Riemannian relative transformation.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.nrR = function() {
        var cv = this.eV();
        var cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[2] = cv.voices[2] + 2;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[0] = cv.voices[0] - 2;
        }
        return cv;
    };

    // Performs the neo-Riemannian Lettonwechsel transformation.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.nrL = function() {
        var cv = this.eV();
        var cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[0] = cv.voices[0] - 1;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[2] = cv.voices[2] + 1;
        }
        return cv;
    };

    // Performs the neo-Riemannian dominant transformation.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.nrD = function() {
        return this.T(-7);
    };

    // Returns the chord inverted by the sum of its first two voices.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.K = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        var chord = this.clone();
        if (chord.size() < 2) {
            return chord;
        }
        var ep = chord.eP();
        var x = ep.voices[0] + ep.voices[1];
        return this.I(x);
    };

    // Returns whether the chord is a transpositional form of Y with interval size g.
    // Only works in equal temperament.
    // FIXME: Check this in Lua and C++.
    Chord.prototype.Tform = function(Y_, g) {
        var eopx = this.eOP();
        var i = 0;
        while (i < ChordSpace.OCTAVE) {
            var ty = Y_.T(i);
            var eopty = ty.eOP();
            if (eopx.eq_epsilon(eopty) === true) {
                return true;
            }
            i = i + g;
        }
        return false;
    };

    // Returns whether the chord is an inversional form of Y with interval size g.
    // Only works in equal temperament.
    // FIXME: Check this in Lua and C++.
    Chord.prototype.Iform = function(Y, g) {
        var eopx = this.eOP();
        var i = 0;
        while (i < ChordSpace.OCTAVE) {
            var iy = Y.I(i);
            var eopiy = iy.eOP();
            if (eopx.eq_epsilon(eopiy) === true) {
                return true;
            }
            i = i + g;
        }
        return false;
    };

    // Returns the contextual transposition of the chord by x with respect to m
    // with minimum interval size g.
    // NOTE: Does NOT return the result under any equivalence class.
    Chord.prototype.Q = function(x, m, g) {
        g = typeof g !== 'undefined' ? g : 1;
        if (this.Tform(m, g) === true) {
            return this.T(x);
        }
        if (this.Iform(m, g) === true) {
            return this.T(-x);
        }
        return this.clone();
    };

    // Returns the number of invariant voiceN, under pitch-class equivalence, in
    // the chord. The two chords must have the same number of voiceN.
    ChordSpace.invariantvoiceN = function(a_, b_) {
        var a = a_.eOP();
        var b = b_.eOP();
        var count = 0;
        for (var voice = 0; voice < a_.size(); voice++) {
            var p = a.voices[voice];
            if (a.count(p) == b.count(p)) {
                count = count + 1;
            }
        }
        return count;
    };

    // Returns the contextual inversion(s) of the chord that preserves n
    // invariant voiceN under pitch-class equivalence, in a sorted list. If there
    // are no such inversions, an empty list is returned. The inversions are
    // returned in equivalence class OP. g is the generator of transposition, by
    // default 1.
    Chord.prototype.J = function(n, g) {
        g = typeof g !== 'undefined' ? g : 1;
        var inversions = {};
        for (var I = 0; ChordSpace.le_epsilon(I, ChordSpace.OCTAVE); I = I + g) {
            var inversion = this.I(I);
            if (ChordSpace.invariantvoiceN(this, inversion) === n) {
                var eopi = inversion.eOP();
                inversions[eopi.hash()] = eopi;
            }
        }
        result = [];
        for (var key in inversions) {
            if (inversions.hasOwnProperty(key)) {
                result.push(inversions[key]);
            }
        }
        result.sort(ChordSpace.chord_compare_epsilon);
        return result;
    };

    // Returns the voice-leading between chords a and b,
    // i.e. what you have to add to a to get b, as a
    // chord of directed intervals.
    ChordSpace.voiceleading = function(a, b) {
        var voiceleading = a.clone();
        for (var voice = 0; voice < a.size(); voice++) {
            voiceleading.voices[voice] = b.voices[voice] - a.voices[voice];
        }
        return voiceleading;
    };

    // Returns whether the voiceleading
    // between chords a and b contains a parallel fifth.
    // FIXME: Fix in Lua and C++.
    ChordSpace.parallelFifth = function(a, b) {
        var v = ChordSpace.voiceleading(a, b);
        if (v.count(7) > 1 || v.count(-7) > 1) {
            return true;
        } else {
            return false;
        }
    };

    // Returns the smoothness of the voiceleading between
    // chords a and b by L1 norm.
    ChordSpace.voiceleadingSmoothness = function(a, b) {
        var L1 = 0;
        for (var voice = 0; voice < a.size(); voice++) {
            L1 = L1 + Math.abs(b.voices[voice] - a.voices[voice]);
        }
        return L1;
    };

    // Returns which of the voiceleadings (source to d1, source to d2)
    // is the smoother (shortest moves), optionally avoiding parallel fifths.
    ChordSpace.voiceleadingSmoother = function(source, d1, d2, avoidParallels, range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        if (avoidParallels === true) {
            if (ChordSpace.parallelFifth(source, d1) === true) {
                return d2;
            }
            if (ChordSpace.parallelFifth(source, d2) === true) {
                return d1;
            }
        }
        var s1 = ChordSpace.voiceleadingSmoothness(source, d1);
        var s2 = ChordSpace.voiceleadingSmoothness(source, d2);
        if (s1.le_epsilon(s2) === true) {
            return d1;
        } else {
            return d2;
        }
    };

    // Returns which of the voiceleadings (source to d1, source to d2)
    // is the simpler (fewest moves), optionally avoiding parallel fifths.
    ChordSpace.voiceleadingSimpler = function(source, d1, d2, avoidParallels) {
        avoidParallels = typeof avoidParallels !== 'undefined' ? avoidParallels : false;
        if (avoidParallels === true) {
            if (ChordSpace.parallelFifth(source, d1) === true) {
                return d2;
            }
            if (ChordSpace.parallelFifth(source, d2) === true) {
                return d1;
            }
        }
        var v1 = ChordSpace.voiceleading(source, d1).eP();
        var v2 = ChordSpace.voiceleading(source, d2).eP();
        //for voice = #v1, 1, -1 do
        for (var voice = v1.size() - 1; voice >= 0; voice--) {
            if (ChordSpace.lt_epsilon(v1[voice], v2[voice]) === true) {
                return d1;
            }
            if (ChordSpace.lt_epsilon(v2[voice], v1[voice]) === true) {
                return d2;
            }
        }
        return d1;
    };

    // Returns which of the voiceleadings (source to d1, source to d2)
    // is the closer (first smoother, then simpler), optionally avoiding parallel fifths.
    ChordSpace.voiceleadingCloser = function(source, d1, d2, avoidParallels) {
        avoidParallels = typeof avoidParallels !== 'undefined' ? avoidParallels : false;
        if (avoidParallels === true) {
            if (ChordSpace.parallelFifth(source, d1) === true) {
                return d2;
            }
            if (ChordSpace.parallelFifth(source, d2) === true) {
                return d1;
            }
        }
        var s1 = ChordSpace.voiceleadingSmoothness(source, d1);
        var s2 = ChordSpace.voiceleadingSmoothness(source, d2);
        if (ChordSpace.lt_epsilon(s1, s2) === true) {
            return d1;
        }
        if (ChordSpace.gt_epsilon(s1, s2) === true) {
            return d2;
        }
        return ChordSpace.voiceleadingSimpler(source, d1, d2, avoidParallels);
    };

    //FIXME: low is Chord in C++ and scalar here and in Lua.
    ChordSpace.next = function(odometer, low, high, g) {
        var voiceN = odometer.size();
        // "Tick."
        odometer.voices[voiceN - 1] = odometer.voices[voiceN - 1] + g;
        // "Carry."
        for (var voice = voiceN - 1; voice >= 1; voice--) {
            if (odometer.voices[voice] > high) {
                odometer.voices[voice] = low;
                odometer.voices[voice - 1] = odometer.voices[voice - 1] + g;
            }
        }
        if (odometer.voices[0] > high) {
            return false;
        }
        return true;
    };

    ChordSpace.next = function(iterator_, origin, range, g) {
        var leastSignificantVoice = iterator_.size() - 1;
        var mostSignificantVoice = 0;
        // Increment, as in an odometer.
        iterator_.setPitch(leastSignificantVoice, iterator_.getPitch(leastSignificantVoice) + g);
        // If necessary, carry the increment to the next most significant voice.
        var voice;
        for (voice = leastSignificantVoice; voice > mostSignificantVoice; --voice) {
            if (Silencio.gt_epsilon(iterator_.voices[voice], (origin.voices[voice] + range))) {
                iterator_.voices[voice] = origin.voices[voice];
                iterator_.voices[voice - 1] = (iterator_.voices[voice - 1] + g);
            }
        }
        if (Silencio.gt_epsilon(iterator_.voices[mostSignificantVoice], (origin.voices[mostSignificantVoice] + range)) === true) {
            return false;
        }
        return true;
    };

    // Returns the voicing of the destination which has the closest voice-leading
    // from the source within the range, optionally avoiding parallel fifths.
    ChordSpace.voiceleadingClosestRange = function(source, destination, range, avoidParallels) {
        var destinationeOP = destination.eOP();
        var d = destinationeOP.clone();
        var odometer = source.origin();
        while (ChordSpace.next(odometer, 0, range, ChordSpace.OCTAVE) === true) {
            var revoicing = odometer.clone();
            for (var voice = 0; voice < revoicing.size(); voice++) {
                revoicing.voices[voice] = revoicing.voices[voice] + destinationeOP.voices[voice];
            }
            d = ChordSpace.voiceleadingCloser(source, d, revoicing, avoidParallels);
        }
        return d;
    };

    // Creates a complete Silencio "note on" event for the
    // indicated voice of the chord. The other parameters are used
    // if the internal duration, channel, velocity, and pan of the
    // chord are undefined.
    Chord.prototype.note = function(voice_, time_, duration_, channel_, velocity_, pan_) {
        time_ = typeof time_ !== 'undefined' ? time_ : 0;
        duration_ = typeof this.duration[voice_] !== 'undefined' ? this.duration[voice_] : duration_;
        channel_ = typeof this.channel[voice_] !== 'undefined' ? this.channel[voice_] : channel_;
        var key_ = this.voices[voice_];
        velocity_ = typeof this.velocity[voice_] !== 'undefined' ? this.velocity[voice_] : velocity_;
        pan_ = typeof this.pan[voice_] !== 'undefined' ? this.pan[voice_] : pan_;
        var note_ = new Silencio.Event();
        note_.data[Silencio.Event.TIME] = time_;
        note_.data[Silencio.Event.DURATION] = duration_;
        note_.data[Silencio.Event.CHANNEL] = channel_;
        note_.data[Silencio.Event.KEY] = key_;
        note_.data[Silencio.Event.VELOCITY] = velocity_;
        note_.data[Silencio.Event.PAN] = pan_;
        return note_;
    };

    // Returns an individual note for each voice of the chord.
    // The chord's duration, instrument, and loudness are used if present,
    // if not the specified values are used.
    Chord.prototype.notes = function(time_, duration_, channel_, velocity_, pan_) {
        var notes_ = new Silencio.Score();
        for (var voice = 0; voice < this.size(); voice++) {
            notes_.append(this.note(voice, time_, duration_, channel_, velocity_, pan_));
        }
        return notes_;
    };

    Chord.prototype.toScore = function(score, time_, duration_, channel_, velocity_, pan_) {
        for (var voice = 0; voice < this.size(); voice++) {
            score.append(this.note(voice, time_, duration_, channel_, velocity_, pan_));
        }
        return score;
    };

    // Move the pitch to the closest pitch-class of the chord.
    // FIXME: Correct Lua and C++.
    ChordSpace.conformPitchToChord = function(pitch, chord, octaveEquivalence) {
        octaveEquivalence = typeof octaveEquivalence !== 'undefined' ? octaveEquivalence : true;
        var pitchClass = ChordSpace.modulo(pitch, ChordSpace.OCTAVE);
        var octave = pitch - pitchClass;
        var chordPitchClass = ChordSpace.modulo(chord.voices[0], ChordSpace.OCTAVE);
        var distance = Math.abs(chordPitchClass - pitchClass);
        var closestPitchClass = chordPitchClass;
        var minimumDistance = distance;
        for (var voice = 1; voice < chord.size(); voice++) {
            chordPitchClass = ChordSpace.modulo(chord.voices[voice], ChordSpace.OCTAVE);
            distance = Math.abs(chordPitchClass - pitchClass);
            if (ChordSpace.lt_epsilon(distance, minimumDistance) === true) {
                minimumDistance = distance;
                closestPitchClass = chordPitchClass;
            }
        }
        if (octaveEquivalence === true) {
            return closestPitchClass;
        } else {
            return octave + closestPitchClass;
        }
    };

    // If the event is a note, moves its pitch
    // to the closest pitch of the chord.
    // If octaveEquivalence is true (the default),
    // the pitch-class of the note is moved to the closest pitch-class
    // of the chord; otherwise, the pitch of the note is moved to the closest
    // absolute pitch of the chord.
    // FIXME: Correct Lua and C++.
    ChordSpace.conformToChord = function(event, chord, octaveEquivalence) {
        octaveEquivalence = typeof octaveEquivalence !== 'undefined' ? octaveEquivalence : true;
        if (event.status === 144) {
            event.key = ChordSpace.conformPitchToChord(event.key, chord, octaveEquivalence);
        }
        return event;
    };

    ChordSpace.conformScoreToChord = function(score, chord, octaveEquivalence) {
        octaveEquivalence = typeof octaveEquivalence !== 'undefined' ? octaveEquivalence : true;
        var event = null;
        for (var i = 0; i < score.size(); i++) {
            event = score.data[i];
            if (event.status === 144) {
                event.key = ChordSpace.conformPitchToChord(event.key, chord, octaveEquivalence);
            }
        }
        return event;
    };

    // Inserts the notes of the chord into the score at the specified time.
    // The internal duration, instrument, and loudness are used.
    ChordSpace.insert = function(score, chord, time_) {
        // console.log(score, chord, time_, duration, channel, velocity, pan)
        for (var voice = 0; voice < chord.size(); voice++) {
            var event = chord.note(voice, time_, chord.getDuration(voice), chord.getChannel(voice), chord.getVelocity(voice), chord.getPan(voice));
            score.append(event);
        }
        return score;
    };

    // For all the notes in the score beginning at or later than the start time,
    // and up to but not including the end time, moves the pitch of the note to
    // belong to the chord, using the conformToChord function.
    ChordSpace.apply = function(score, chord, start, end_, octaveEquivalence) {
        octaveEquivalence = typeof octaveEquivalence !== 'undefined' ? octaveEquivalence : true;
        var s = score.slice(start, end_, true);
        for (var index = 0; index < s.size(); index++) {
            var event = s.data[index];
            ChordSpace.conformToChord(event, chord, octaveEquivalence);
        }
        console.log('Conform to: ' + chord.name() + ' from: ' + start + ' to: ' + end_ + ' notes: ' + s.data.length + '.');
        return s;
    };

    // Returns a chord containing all the pitches of the score
    // beginning at or later than the start time,
    // and up to but not including the end time.
    ChordSpace.gather = function(score, start, end_) {
        var chord = new ChordSpace.Chord();
        var slice = score.slice(start, end);
        for (var index = 0; index < slice.size(); index++) {
            var event = slice.get(index);
            var pitch = event.key;
            if (chord.contains(pitch) === false) {
                chord.add(pitch);
            }
        }
        return chord;
    };

    /**
     * New LSys with chord operations.
     */
    ChordSpace.LSys = function() {
        Silencio.LSys.call(this);
        this.chordsForTimes = {};
        return this;
    };

    ChordSpace.LSys.prototype = new Silencio.LSys();

    ChordSpace.LSys.prototype.generate = function(n) {
        csound.message('ChordSpace.LSys.prototype.generate\n');
        this.chordsForTimes = {};
        this.sentence = this.axiom.split(' ');
        for (var g = 0; g < n; g++) {
            var next = [];
            for (var i = 0; this.sentence.length > i; i++) {
                var c = this.sentence[i];
                var r = this.rules[c];
                if (r) {
                    next = next.concat(r.split(' '));
                } else {
                    next = next.concat(c.split(' '));
                }
            }
            this.sentence = next; //.join("");
        }
    };

    ChordSpace.LSys.prototype.interpret = function(c, t, context, size) {
        // This was too goopy to call the super.
        if (c === 'F') {
            if (typeof size === 'undefined') {
                t.startNote();
                t.go(context);
            } else {
                t.move();
            }
        } else if (c === 'f') t.move();
        else if (c === '+') t.turnRight();
        else if (c === '-') t.turnLeft();
        else if (c === '[') t.push();
        else if (c === ']') t.pop();
        else if (c === 'I') t.upInstrument();
        else if (c === 'i') t.downInstrument();
        else if (c === 'V') t.upVelocity();
        else if (c === 'v') t.downVelocity();
        else if (c === 'T') t.upTempo();
        else if (c === 't') t.downTempo();
        else if (c === 'C') {
            this.chordsForTimes[t.event.start] = t.chord.clone();
        } else if (c === 'K') {
            t.K();
            this.chordsForTimes[t.event.start] = t.chord.clone();
        } else {
            var parts = c.split(',');
            var cc = parts[0];
            if (cc === 'T') {
                t.T(Number(parts[1]));
                this.chordsForTimes[t.event.start] = t.chord.clone();
            } else if (cc === 'I') {
                t.I(Number(parts[1]));
                this.chordsForTimes[t.event.start] = t.chord.clone();
            } else if (cc === 'Q') {
                t.Q(Number(parts[1]));
                this.chordsForTimes[t.event.start] = t.chord.clone();
            } else if (cc === 'J') {
                t.J(Number(parts[1], Number(parts[2])));
                this.chordsForTimes[t.event.start] = t.chord.clone();
            } else if (cc === 't') {
                var operation = parts[1];
                var operand = Number(parts[2]);
                if (operation === '=') {
                    t.tempo = operand;
                } else if (operation === '+') {
                    t.tempo += operand;
                } else if (operation === '-') {
                    t.tempo -= operand;
                } else if (operation === '*') {
                    t.tempo *= operand;
                } else if (operation === '/') {
                    t.tempo /= operand;
                    csound.message('tempo:' + t.tempo + '\n');
                }
            }
        }
        if (typeof size === 'undefined') {
            if (c === 'F') {
                t.endNote(this.score);
            }
            this.prior = c;
        } else {
            this.findSize(t, size);
        }
    };

    /**
     * Conforms the pitch of each event in this,
     * to the closest pitch-class in the chord that applies to the event's time.
     */
    ChordSpace.LSys.prototype.conformToChords = function() {
        var times = [];
        for (var tyme in this.chordsForTimes) {
            if (this.chordsForTimes.hasOwnProperty(tyme)) {
                times.push(tyme);
            }
        }
        var end = this.score.duration();
        for (var i = 0; i < times.length; i++) {
            var begin = times[i];
            var chord = this.chordsForTimes[tyme];
            ChordSpace.apply(this.score, begin, end, chord, false);
            end = begin;
        }
    };

    /**
     * New Turtle with chord operations T,x, I,x, K, Q,x, J,n,i
     * for transposition, inversion, switch of modality, contextual
     * transposition, and contextual inversion.
     */
    ChordSpace.Turtle = function(len, theta, chord, modality) {
        Silencio.Turtle.call(this, len, theta);
        this.chord = chord.clone();
        this.modality = modality.clone();
        this.tempo = 1;
        return this;
    };

    ChordSpace.Turtle.prototype = new Silencio.Turtle();

    ChordSpace.Turtle.prototype.endNote = function(score) {
        this.event.end = this.p.x;
        if (this.event.duration > 0) {
            var event = this.event.clone();
            event.chord = this.chord;
            score.data.push(event);
        }
    };

    ChordSpace.Turtle.prototype.T = function(n) {
        this.chord = this.chord.T(n);
    };

    ChordSpace.Turtle.prototype.I = function(c) {
        this.chord = this.chord.I(c);
    };

    ChordSpace.Turtle.prototype.K = function() {
        this.chord = this.chord.K();
    };

    ChordSpace.Turtle.prototype.Q = function(n) {
        this.chord = this.chord.Q(n, this.modality);
    };

    ChordSpace.Turtle.prototype.J = function(n, i) {
        var inversions = this.chord.J(n);
        if (inversions.length > i) {
            this.chord = inversions[i];
        }
    };

    /**
     * Returns the ith arpeggiation, current voice, and corresponding revoicing
     * of the chord. Positive arpeggiations start with the lowest voice of the
     * chord and revoice up; negative arpeggiations start with the highest voice
     * of the chord and revoice down.
     */
    Chord.prototype.a = function(arpeggiation) {
        var chord = this.v(arpeggiation);
        if (arpeggiation < 0) {
            return {
                'pitch': chord.voices[chord.length - 1],
                'voice': chord.length - 1,
                'chord': chord
            };
        }
        return {
            'pitch': chord.voices[0],
            'voice': 0,
            'chord': chord
        };
    };

    /**
     * Orthogonal additive groups for unordered chords of given arity under range
     * equivalence (RP): prime form or P, inversion or I, transposition or T, and
     * voicing or V. P x I x T = OP, P x I x T x V = RP. Therefore, an
     * operation on P, I, T, or V may be used to independently transform the
     * respective symmetry of any chord. Some of these operations will reflect
     * in RP.
     */
    var ChordSpaceGroup = function() {
        this.optisForIndexes = [];
        this.indexesForOptis = {};
        this.voicingsForIndexes = [];
        this.indexesForVoicings = {};
    };
    ChordSpace.ChordSpaceGroup = ChordSpaceGroup;

    ChordSpace.octavewiseRevoicings = function(chord, range) {
        range = range !== 'undefined' ? range : ChordSpace.OCTAVE;
        var voices = chord.size();
        var origin = chord.eOP();
        var odometer = origin.clone();
        // Enumerate the permutations.
        // iterator[0] is the most significant voice, and
        // iterator[N-1] is the least significant voice.
        var voicings = 0;
        while (ChordSpace.next(odometer, origin, range, ChordSpace.OCTAVE) === true) {
            voicings = voicings + 1;
        }
        return voicings;
    };

    ///FIXME
    ChordSpace.octavewiseRevoicing = function(chord, index, range) {
        ///var voices = chord.size();
        var origin = chord.eOP();
        var odometer = origin.clone();
        // Enumerate the permutations.
        // iterator[0] is the most significant voice, and
        // iterator[N-1] is the least significant voice.
        var voicings = 0;
        var v;
        for (v = 0; v < index; v++) {
            ChordSpace.next(odometer, origin, range, ChordSpace.OCTAVE);
            // Wrap around?
            if (odometer.voices[0] > range) {
                odometer = chord.origin();
            }
            voicings = voicings + 1;
        }
        ///for (v = 0; v < odometer.size(); v++) {
        ///    odometer.voices[v] = odometer.voices[v] + origin.voices[v];
        ///}
        return odometer;
    };

    ChordSpace.allOfEquivalenceClass = function(voices, equivalence, g) {
        g = typeof g !== 'undefined' ? g : 1;
        var equivalenceMapper = null;
        if (equivalence == 'OP') {
            equivalenceMapper = Chord.prototype.iseOP;
        }
        if (equivalence == 'OPT') {
            equivalenceMapper = Chord.prototype.iseOPT;
        }
        if (equivalence == 'OPTT') {
            equivalenceMapper = Chord.prototype.iseOPTT;
        }
        if (equivalence == 'OPI') {
            equivalenceMapper = Chord.prototype.iseOPI;
        }
        if (equivalence == 'OPTI') {
            equivalenceMapper = Chord.prototype.iseOPTI;
        }
        if (equivalence == 'OPTTI') {
            equivalenceMapper = Chord.prototype.iseOPTTI;
        }
        var equivalentChords = new Set();
        // Enumerate all chords in [-O, O].
        var iterator = ChordSpace.iterator(voices, -13);
        var origin = iterator.clone();
        console.log('iterator:' + iterator);
        console.log('equivalenceMapper:' + equivalenceMapper);
        while (ChordSpace.next(iterator, origin, 26, g) === true) {
            if (iterator.iseP() === true) {
                var eP = iterator.clone();
                if (equivalenceMapper.apply(eP)) {
                    equivalentChords.add(eP);
                }
            }
        }
        var zeroBasedChords = Array.from(equivalentChords);
        zeroBasedChords = zeroBasedChords.sort(ChordSpace.chord_compare_epsilon);
        equivalentChords = new Set(zeroBasedChords);
        return {
            'array': zeroBasedChords,
            'set': equivalentChords
        };
    };

    /**
     * Returns a chord with the specified number of voices all set to a first
     * pitch, useful as an iterator.
     */
    ChordSpace.iterator = function(voices, first) {
        var odometer = new Chord();
        odometer.resize(voices);
        for (var voice = 0; voice < voices; voice++) {
            odometer.voices[voice] = first;
        }
        return odometer;
    };

    /**
     * Creates a JSON filename encoding the structure of a chord space group.
     * NOTE: Serialization and deserialization of ChordSpaceGroup is not complete
     * and may or may not prove necessary.
     */
    ChordSpace.createFilename = function(voices, range, g) {
        var gstring = sprintf('g%.6f', g);
        gstring = gstring.replace('.', '_');
        var filename = sprintf('ChordSpaceGroup_V%d_R%d_%s.json', voices, range, gstring);
        return filename;
    };

    /**
     * Loads the group if found, creates and saves it otherwise.
     * NOTE: Serialization and deserialization of ChordSpaceGroup is not complete
     * and may or may not prove necessary.
    */
    ChordSpace.createChordSpaceGroup = function(voices, range, g) {
        var chordSpaceGroup = new ChordSpaceGroup();
        var filename = ChordSpace.createFilename(voices, range, g);
        var restored_ = Silencio.restoreFromLocalFile(true, filename);
        if (restored_ !== null) {
            Object.assign(chordSpaceGroup, restored_);
            console.log(sprintf('Loaded ChordSpaceGroup from file "%s"...', filename));
        } else {
            console.log(sprintf('File "%s" not found, creating...', filename));
            chordSpaceGroup.initialize(voices, range, g);
            Silencio.saveToLocalFile(true, chordSpaceGroup, filename);
        }
        return chordSpaceGroup;
    };

    /**
     * Returns the chord for the indices of prime form, inversion,
     * transposition, and voicing. The chord is not in RP; rather, each voice
     * of the chord's OP may have zero or more octaves added to it.
     * Please note, because some set classes e.g. diminished chords are
     * invariant under some T, there may be more than one PITV to get the
     * same chord.
     */
    ChordSpaceGroup.prototype.toChord = function(P, I, T, V, printme) {
        printme = typeof printme !== 'undefined' ? printme : false;
        P = P % this.countP;
        I = I % 2;
        T = T % ChordSpace.OCTAVE;
        V = V % this.countV;
        if (printme) {
            console.log(sprintf('toChord:             %s %s %s %s', P, I, T, V));
        }
        var optti = this.optisForIndexes[P];
        if (printme) {
            console.log('toChord:   optti:    ' + optti);
        }
        var optt;
        if (I === 0) {
            optt = optti;
        } else {
            optt = optti.I().eOPTT();
        }
        if (console.logme) {
            console.log('toChord:   optt:      ' + optt);
        }
        var optt_t = optt.T(T);
        if (printme) {
            console.log('toChord:   optt_t:    ' + optt_t);
        }
        var op = optt_t.eOP();
        if (printme) {
            console.log('toChord:   op:        ' + op);
        }
        V = V % this.countV;
        var revoicing = ChordSpace.octavewiseRevoicing(op, V, this.range);
        if (printme) {
            console.log('toChord:   revoicing: ' + revoicing);
        }
        return {'revoicing': revoicing, 'opti': optti, 'op': op};
    };

    /**
     * Returns the indices of prime form, inversion, transposition,
     * and voicing for a chord in the group.
     */
    ChordSpaceGroup.prototype.fromChord = function(chord, printme) {
        printme = typeof(printme) !== 'undefined' ? printme : false;
        if (printme) {
            console.log('fromChord: chord:    ' + chord + ' ' + chord.iseOP());
        }
        var op;
        if (chord.iseOP()) {
            op = chord.clone();
        } else {
            op = chord.eOP();
        }
        if (printme) {
            console.log('fromChord: op:       ' + op);
        }
        var optt = chord.eOPTT();
        if (printme) {
            console.log('fromChord: optt:     ' + optt);
        }
        var T = 0;
        for (t = 0; t < ChordSpace.OCTAVE - 1; t = t + this.g) {
            var optt_t = optt.T(t).eOP();
            if (printme) {
                console.log('fromChord: optt_t:   ' + optt_t + ' T: ' + t);
            }
            if (optt_t.eq_epsilon(op) === true) {
                if (printme) {
                    console.log('equals');
                }
                T = t;
                break;
            }
        }
        var optti = chord.eOPTTI();
        if (printme) {
            console.log('fromChord: optti:    ' + optti);
        }
        var P = this.indexesForOptis[optti.toString()];
        var I = 0;
        var optt_i_optt;
        if (optti.eq_epsilon(optt) === false) {
            I = 1;
            optt_i_optt = optt.I().eOPTT();
            if (optt_i_optt.eq_epsilon(optti) === false) {
                console.log("Error: OPTT(I(OPTT)) must equal OPTTI.");
                console.log('optt_i_optt:' + optt_i_optt.information());
                console.log('optti:      ' + optti.information());
                process.exit();
            }
        }
        var voicing = ChordSpace.voiceleading(op, chord);
        // Possible alternative implementation: V = this.indexesForVoicings[voicing.toString()];
        var V = ChordSpace.indexForOctavewiseRevoicing(chord, range, printme);
        if (V === -1) {
            V = 0;
        }
        if (printme) {
            console.log('fromChord: voicing:  ' + voicing + ' ' + V);
            console.log('fromChord:           ' + P + ' ' + I + ' ' + T + ' ' + V);
        }
        return {'P': P, 'I': I, 'T': T, 'V': V};
    };

    ChordSpaceGroup.prototype.printChords = function() {
        for (var index = 0; index < this.optisForIndexes.length; index++) {
            var opti = this.optisForIndexes[index];
            var name = opti.name();
            console.log(sprintf('index: %5d  opti: %s %s', index, opti.toString(), name));
        }
    };

    ChordSpaceGroup.prototype.printNamedChords = function() {
        for (var index = 0; index < this.optisForIndexes.length; index++) {
            var opti = this.optisForIndexes[index];
            var name = opti.name();
            if (name !== '') {
                console.log(sprintf('index: %5d  opti: %s %s', index, opti.toString(), name));
            }
        }
    };

    /**
     * Returns the index of the octavewise revoicing that this chord is,
     * relative to its OP equivalent, within the indicated range. Returns
     * -1 if there is no such chord within the range.
     */
    ChordSpace.indexForOctavewiseRevoicing = function (chord, range, debug) {
        var revoicingN = ChordSpace.octavewiseRevoicings(chord, range);
        var origin = chord.eOP();
        var revoicing = origin.clone();
        var revoicingI = 0;
        while (true) {
            if (debug) {
                console.log(sprintf("indexForOctavewiseRevoicing of %s in range %7.3f: %5d of %5d: %s",
                    chord,
                    range,
                    revoicingI,
                    revoicingN,
                    revoicing));
            }
            if (revoicing.eq_epsilon(chord) === true) {
                return revoicingI;
            }
            ChordSpace.next(revoicing, origin, range, ChordSpace.OCTAVE);
            revoicingI++;
            if (revoicingI > revoicingN) {
                return -1;
            }
        }
    };

    ChordSpaceGroup.prototype.list = function(listheader, listopttis, listvoicings) {
        listheader = typeof listheader !== 'undefined' ? listheader : false;
        listopttis = typeof listopttis !== 'undefined' ? listopttis : false;
        listvoicings = typeof listvoicings !== 'undefined' ? listvoicings : false;
        if (listheader) {
            console.log(sprintf('ChordSpaceGroup.voices: %8d', this.voices));
            console.log(sprintf('ChordSpaceGroup.range : %8d', this.range));
            console.log(sprintf('ChordSpaceGroup.g     : %13.4f', this.g));
            console.log(sprintf('ChordSpaceGroup.countP: %8d', this.countP));
            console.log(sprintf('ChordSpaceGroup.countI: %8d', this.countI));
            console.log(sprintf('ChordSpaceGroup.countT: %8d', this.countT));
            console.log(sprintf('ChordSpaceGroup.countV: %8d', this.countV));
        }
        var index;
        var voicing_index;
        var opti;
        var voicingFromIndex;
        var indexFromVoicing;
        if (listopttis) {
            for (index = 0; index < this.optisForIndexes.length; index++) {
                opti = this.optisForIndexes[index];
                console.log(sprintf('index: %5d  opti: %s  index from opti: %s', index, opti.toString(), this.indexesForOptis[opti.toString()]));
                if (listvoicings) {
                    for (voicing_index = 0; voicing_index < this.countV; voicing_index++) {
                        voicingFromIndex = ChordSpace.octavewiseRevoicing(opti, voicing_index, this.range);
                        indexFromVoicing = ChordSpace.indexForOctavewiseRevoicing(voicingFromIndex, this.range);
                        console.log(sprintf('  voicing index: %5d  voicing: %s  index from voicing: %5d', voicing_index, voicingFromIndex, indexFromVoicing));
                    }
                }
            }
        }
    };

    /**
     * N is the number of voices in the chord space, g is the generator of
     * transposition, and range is the size of chord space.
     */
    ChordSpaceGroup.prototype.initialize = function(voices, range, g) {
        this.voices = typeof voices !== 'undefined' ? voices : 3;
        this.range = typeof range !== 'undefined' ? range : 60;
        this.g = typeof g !== 'undefined' ? g : 1;
        this.countP = 0;
        this.countI = 2;
        this.countT = ChordSpace.OCTAVE / this.g;
        var chord = new ChordSpace.Chord();
        chord.resize(voices);
        this.countV = ChordSpace.octavewiseRevoicings(chord, this.range);
        this.indexesForOptis = {};
        var result = ChordSpace.allOfEquivalenceClass(voices, 'OPTTI');
        this.optisForIndexes = result.array;
        this.indexesForOptis = new Map();
        for (var index = 0; index < this.optisForIndexes.length; index++) {
            var opti = this.optisForIndexes[index];
            this.indexesForOptis[opti.toString()] = index;
            this.countP = this.countP + 1;
        }
    };

    //////////////////////////////////////////////////////////////////////////////
    // EXPORTS
    //////////////////////////////////////////////////////////////////////////////

    // Node: Export function
    if (typeof module !== "undefined" && module.exports) {
        module.exports = ChordSpace;
    }
    // AMD/requirejs: Define the module
    else if (typeof define === 'function' && define.amd) {
        define(function() {
            return ChordSpace;
        });
    }
    // Browser: Expose to window
    else {
        window.ChordSpace = ChordSpace;
    }

})();

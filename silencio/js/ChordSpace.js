/**
C H O R D S P A C E

Copyright (C) 2014 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an algorithmic music composition library for Csound.

PLEASE NOTE

This module is deprecated. Please use the code in the WebAssembly build of 
CsoundAC instead of this code.

A number of errors in the equivalence classes and symmetries of chord space 
exist in this module, but have been fixed in CsoundAC.

There are some differences in the APIs between this module and CsoundAC, but 
all functionality is available in CsoundAC with the exception of 
ParametricLindenmayer.js.

REGARDING BLUE

Steven Yi's Java program blue, for composing with Csound, uses the Nashorn
JavaScript runtime and does not support the DOM or other objects found in a
Web browser's JavaScript context. To use Silencio in blue:

--  Load sprintf.js and tinycolor.js first.
--  The following polyfill is enough to run some things.

*/
if (typeof console === 'undefined') {
    let global = this;
    let window = this;
    let process = {env: {}};
    let console = {};
    console.debug = print;
    console.warn = print;
    console.info = print;
}

(function() {

    // All JavaScript dependencies of ChordSpace.js:
    // let numeric = require("numeric.js");
    // let svd = require("svd.js");
    // let Silencio = require("Silencio");

    let ChordSpace = {};

    ChordSpace.EPSILON = 1;
    ChordSpace.epsilonFactor = 1000;
    ChordSpace.debug = true;

    while (true) {
        ChordSpace.EPSILON = ChordSpace.EPSILON / 2;
        let nextEpsilon = ChordSpace.EPSILON / 2;
        let onePlusNextEpsilon = 1 + nextEpsilon;
        if (onePlusNextEpsilon === 1) {
            console.info('ChordSpace EPSILON: ' + ChordSpace.EPSILON);
            break;
        }
    }

    /**
     * C H O R D S P A C E
     *
     * Copyright 2010, 2011, 2015, 2016, 2020 by Michael Gogins.
     *
     * This software is licensed under the terms of the GNU Lesser General 
     * Public License.
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
     *     OPTI x T x I x V (V is octavewise revoicings within a given range). This
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
     * A chord is simply a set of voices heard at the same time, represented here
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
     * points on an opposing face. The the space "within" the orbifold is a 
     * fundamental domain of the equivalence class.
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
     *         "wedge" of plain chord space in which the voices of a chord are always
     *         sorted by pitch.
     *
     * T       Transpositional equivalence, e.g. {1, 2} == {7, 8}. The fundamental
     *         domain is defined as a plane in chord space at right angles to the
     *         diagonal of unison chords. Represented by the chord always having a
     *         sum of pitches equal to 0.
     *
     * TT`     A varient of T that preserves equal temperament in chords. The sum 
     *         of the pitches in a chord is the least possible for an equally 
     *         tempered chord.
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
     *         chord space that is bounded by a plane at right angles to the unison 
     *         diagonal. Represented as the chord having the first interval 
     *         between voices be smaller than or equal to the final interval 
     *         (recursing for chords of more than 3 voices).
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
     *         for chords; i.e. chords with a fixed number of voices in a harmonic
     *         context. The fundamental domain is defined as a hyperprism one octave
     *         long with as many sides as voices and the ends identified by octave
     *         equivalence and one cyclical permutation of voices, modulo the
     *         unordering. In OP for trichords in 12TET, the augmented triads run up
     *         the middle of the prism, the major and minor triads are in 6
     *         alternating columns around the augmented triads, the two-pitch chords
     *         form the 3 sides, and the one-pitch chords form the 3 edges that join
     *         the sides.
     *
     * OPT     The sum of the OP prism as close as possible to the origin, modulo
     *         the number of voices. Chord type. Note that CM and Cm are different
     *         OPT. Because the OP prism is canted down from the origin, at least one
     *         pitch in each OPT chord (excepting the origin itself) is negative.
     *
     * OPI     The OP prism modulo inversion, i.e. 1/2 of the OP prism. The
     *         representative fundamental consists of those chords less than or equal
     *         to their inversions modulo OP.
     *
     * OPTI    The OPT sum modulo inversion, i.e. 1/2 of the OPT sum.
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
    
    /**
     * Returns whether a is equal to b within acceptable bounds of numerical 
     * error in the floating-point computations. Used to construct all other 
     * numerical comparisons.
     */ 
    ChordSpace.eq_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        if (Math.abs(a - b) < (ChordSpace.EPSILON * factor)) {
            return true;
        }
        return false;
    };

    ChordSpace.gt_epsilon = function(a, b, factor) {
        factor = typeof factor !== 'undefined' ? factor : ChordSpace.epsilonFactor;
        let eq = ChordSpace.eq_epsilon(a, b, factor);
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
        let eq = ChordSpace.eq_epsilon(a, b, factor);
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
        let eq = ChordSpace.eq_epsilon(a, b, factor);
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
        let eq = ChordSpace.eq_epsilon(a, b, factor);
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
    
    ChordSpace.floor = function(x, g) {
        g = typeof g !== 'undefined' ? g : 1;
        // TODO: Make this work for g <> 1.
        let floor_ = Math.floor(x);
        return floor_;
        
    }
    
    ChordSpace.ceiling = function(x, g) {
        g = typeof g !== 'undefined' ? g : 1;
        // TODO: Make this work for g <> 1.
        let ceiling_ = Math.ceil(x);
        return ceiling_;
    }

    /** 
     * The size of the octave, defined to be consistent with 12 tone equal 
     * temperament and MIDI.
     */
    ChordSpace.OCTAVE = 12;

    /**
     * Middle C, defined to be consistent with MIDI.
     */
    ChordSpace.MIDDLE_C = 60;

    /**
     * Returns the pitch transposed by semitones, which may be any scalar.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    ChordSpace.T = function(pitch, semitones) {
        return pitch + semitones;
    };

    /**
     * Returns the pitch reflected in the center, which may be any pitch.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    ChordSpace.I = function(pitch, center) {
        center = typeof center !== 'undefined' ? center : 0;
        return center - pitch;
    };

    /**
     * Returns the Euclidean distance between chords a and b,
     * which must have the same number of voices.
     */
    ChordSpace.euclidean = function(a, b) {
        let sum_of_squared_differences = 0;
        for (let voice = 0; voice < a.voices.length; voice++) {
            sum_of_squared_differences = sum_of_squared_differences + Math.pow((a.voices[voice] - b.voices[voice]), 2);
        }
        return Math.sqrt(sum_of_squared_differences);
    };

    /**
     * A chord is one point in a space with one dimension of linear pitch per 
     * voice. Pitches are represented as semitones with 0 at the origin
     * and middle C as 60.
     */
    let Chord = function(array_) {
        this.voices = [];
        this.duration = [];
        this.channel = [];
        this.velocity = [];
        this.pan = [];
        if (typeof array_ != 'undefined') {
            this.set(array_);
        }
    };
    ChordSpace.Chord = Chord;
    
    /** 
     * Global of chords defining inversion flat hyperplanes for chords from 3 
     * through 7 voices, from the _Science_ material.
     */
    ChordSpace.inversion_flats = new Map();
        
    /** 
     * Global of unit normal vectors for reflecting chords in the inversion 
     * flat that folds OPT to OPTI, for chords from 3 through 7 voices.
     */
    ChordSpace.inversion_flat_normals = new Map();
    
    /**
     * Returns the number of voices in the chord.
     */
    Chord.prototype.size = function() {
        return this.voices.length;
    };

    /**
     * Resizes a chord to the specified number of voices.
     * Existing voices are not changed. Extra voices are removed.
     * New voices are initialized to 0.
     */
    Chord.prototype.resize = function(voiceN) {
        let original_length = this.voices.length;
        this.voices.length = voiceN;
        this.duration.length = voiceN;
        this.channel.length = voiceN;
        this.velocity.length = voiceN;
        this.pan.length = voiceN;
        for (let voice = original_length; voice < voiceN; voice++) {
            this.voices[voice] = 0;
            this.duration[voice] = 0;
            this.channel[voice] = 0;
            this.velocity[voice] = 0;
            this.pan[voice] = 0;
        }
    };

    /**
     * Resizes the chord to the length of the array, and sets
     * the pitches from the values of the array.
     */
    Chord.prototype.set = function(array) {
        this.resize(array.length);
        for (let i = 0; i < this.size(); i++) {
            this.voices[i] = array[i];
        }
    };

    Chord.prototype.addVoice = function(pitch) {
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
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.duration[voice] = value;
        }
    };

    Chord.prototype.getDuration = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.duration[voice];
    };

    Chord.prototype.setChannel = function(value) {
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.channel[voice] = value;
        }
    };

    Chord.prototype.setChannelsToVoices = function(base_voice) {
        if (typeof base_voice === 'undefined') {
            base_voice = 1;
        }
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.channel[voice] = base_voice + voice;
        }
    };

    Chord.prototype.setPansToVoices = function() {
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.pan[voice] = (voice + 1) / (this.voices.length + 1);
        }
    };

    Chord.prototype.getChannel = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.channel[voice];
    };

    Chord.prototype.setVelocity = function(value) {
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.velocity[voice] = value;
        }
    };

    Chord.prototype.getVelocity = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.velocity[voice];
    };

    Chord.prototype.setPan = function(value) {
        for (let voice = 0; voice < this.voices.length; voice++) {
            this.pan[voice] = value;
        }
    };

    Chord.prototype.getPan = function(voice) {
        voice = typeof voice !== 'undefined' ? voice : 0;
        return this.pan[voice];
    };

    Chord.prototype.count = function(pitch) {
        let n = 0;
        for (let voice = 0; voice < this.voices.length; voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], pitch)) {
                n++;
            }
        }
        return n;
    };

    /**
     * Returns a string representation of the chord.
     * Quadratic complexity, but short enough not to matter.
     */
    Chord.prototype.toString = function() {
        let buffer = '[';
        for (let voice = 0; voice < this.voices.length; voice++) {
            buffer = buffer + sprintf('%12.7f ', this.voices[voice]);
        }
        buffer = buffer + ']';
        return buffer;
    };

    /**
     * Returns a musician-friendly string representation of the chord.
     */
    Chord.prototype.toPitches = function() {
        let text = ''
        for (let voice = 0; voice < this.voices.length; voice++) {
            if (voice > 0) {
                text = text + " ";
            }
            text = text + ChordSpace.noteName(this.voices[voice]);
        }
        return text;
    };

    /**
     * Implements value semantics for ==, for the pitches in this only.
     */
    Chord.prototype.eq_epsilon = function(other) {
        if (this.voices.length !== other.voices.length) {
            return false;
        }
        for (let voice = 0; voice < this.voices.length; voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], other.voices[voice]) === false) {
                return false;
            }
        }
        return true;
    };

    Chord.prototype.lt_epsilon = function(other) {
        let voiceN = Math.min(this.voices.length, other.voices.length);
        for (let voice = 0; voice < voiceN; voice++) {
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
        let voiceN = Math.min(this.voices.length, other.voices.length);
        for (let voice = 0; voice < voiceN; voice++) {
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

    /**
     * Returns whether or not the chord contains the pitch.
     */
    Chord.prototype.contains = function(pitch) {
        for (let voice = 0; voice < this.voices.length; voice++) {
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

    /**
     * This hash function is used e.g. to give chords value semantics for sets.
     */
    Chord.prototype.hash = function() {
        var text = '';
        for (var voice = 0; voice < this.voices.length; voice++) {
            var value = this.voices[voice].toFixed(6).trim();
            if (voice === 0) {
                text = text.concat(value);
            } else {
                text = text.concat(',', value);
            }
        }
        return text;
    };

    /**
     * Returns the lowest pitch in the chord,
     * and also its voice index.
     */
    Chord.prototype.min = function() {
        let lowest_voice = 0;
        let lowest_pitch = this.voices[lowest_voice];
        for (let voice = 1; voice < this.voices.length; voice++) {
            if (ChordSpace.lt_epsilon(this.voices[voice], lowest_pitch) === true) {
                lowest_pitch = this.voices[voice];
                lowest_voice = voice;
            }
        }
        return [lowest_pitch, lowest_voice];
    };

    /**
     * Returns the minimum interval in the chord.
     */
    Chord.prototype.minimumInterval = function() {
        let minimum_interval = Math.abs(this.voices[1] - this.voices[2]);
        for (let v1 = 1; v1 < this.voices.length; v1++) {
            for (let v2 = 1; v2 < this.voices.length; v2++) {
                if (v1 === v2) {
                    let interval = Math.abs(this.voices[v1] - this.voices[v2]);
                    if (interval < minimum_interval) {
                        minimum_interval = interval;
                    }
                }
            }
        }
        return minimum_interval;
    };

    /**
     * Returns the highest pitch in the chord,
     * and also its voice index.
     */
    Chord.prototype.max = function() {
        let highest_voice = 0;
        let highest_pitch = this.voices[highest_voice];
        for (let voice = 1; voice < this.voices.length; voice++) {
            if (ChordSpace.gt_epsilon(this.voices[voice], highest_pitch) === true) {
                highest_pitch = this.voices[voice];
                highest_voice = voice;
            }
        }
        return [highest_pitch, highest_voice];
    };

    /**
     * Returns the maximum interval in the chord.
     */
    Chord.prototype.maximumInterval = function() {
        let maximum_interval = Math.abs(this.voices[1] - this.voices[2]);
        for (let v1 = 0; v1 < this.voices.length; v1++) {
            for (let v2 = 0; v2 < this.voices.length; v2++) {
                if (v1 != v2) {
                    let interval = Math.abs(this.voices[v1] - this.voices[v2]);
                    if (interval > maximum_interval) {
                        maximum_interval = interval;
                    }
                }
            }
        }
        return maximum_interval;
    };
    
    Chord.prototype.span = function() {
        let top = this.max()[0];
        let bottom = this.min()[0];
        return top - bottom;
    }

    /**
     * Returns a value copy of the chord.
     */
    Chord.prototype.clone = function() {
        let clone_ = new Chord();
        clone_.resize(this.size());
        for (let voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = this.voices[voice];
            clone_.duration[voice] = this.duration[voice];
            clone_.channel[voice] = this.channel[voice];
            clone_.velocity[voice] = this.velocity[voice];
            clone_.pan[voice] = this.pan[voice];
        }
        return clone_;
    };

    /**
     * Returns the origin of the chord's space.
     */
    Chord.prototype.origin = function() {
        let clone_ = this.clone();
        for (let voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = 0;
        }
        return clone_;
    };
    
    /**
     * Returns the center of OPT, that is, the lowest chord with sum 0 on the 
     * axis of equally spaced chords, e.g. (-4, 0, 4) for trichords.
     */
    Chord.prototype.center = function() {
        let n = this.size();
        let interval = ChordSpace.OCTAVE / n;
        let even = this.clone();
        for (let i = 0; i < n; i++) {
            even.voices[i] = i * interval;
        }
        let center_ = even.eT();
        return center_;
    };
    
    /**
     * Returns a new chord whose pitches are the ceilings with respect to g of 
     * the pitches in this, where g is the generator of transposition.
     */    
    Chord.prototype.ceiling = function(g) {
        g = typeof g !== 'undefined' ? g : 1;
        ceiling_ = this.clone();
        for (let i = 0; i < this.size(); i++) {
            ceiling_.voices[i] = ChordSpace.ceiling(this.voices[i], g);
        }
        ceiling_.clamp(g);        
        return ceiling_;
    }

    /**
     * Returns a new chord whose pitches are the floors with respect to g of 
     * the pitches in this, where g is the generator of transposition.
     */    
    Chord.prototype.floor = function(g) {
        g = typeof g !== 'undefined' ? g : 1;
        floor_ = this.clone();
        for (let i = 0; i < this.size(); i++) {
            floor_.voices[i] = ChordSpace.floor(this.voices[i], g);
        }
        floor_.clamp(g);        
        return floor_;
    };

    Chord.prototype.distanceToOrigin = function() {
        let origin_ = this.origin();
        return ChordSpace.euclidean(this, origin_);
    };

    /**
     * Returns the sum of the pitches in the chord.
     */
    Chord.prototype.sum = function() {
        let sum_ = 0;
        for (let voice = 0; voice < this.size(); voice++) {
            sum_ = sum_ + this.voices[voice];
        }
        return sum_;
    };

    Chord.prototype.unisonAtSum = function() {
        let unison_ = this.origin();
        let pitch = this.sum() / this.size();
        for (let voice = 0; voice < this.size(); voice++) {
            unison_.voices[voice] = pitch;
        }
        return unison_;
    };

    /**
     * Returns the Euclidean distance from this chord
     * to the unison diagonal of its chord space.
     */
    Chord.prototype.distanceToUnisonDiagonal = function() {
        let unison_ = this.unisonAtSum();
        return ChordSpace.euclidean(this, unison_);
    };

    /**
     * Transposes the chord by the indicated interval (may be a fraction).
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.T = function(interval) {
        let clone_ = this.clone();
        for (let voice = 0; voice < this.size(); voice++) {
            clone_.voices[voice] = ChordSpace.T(this.voices[voice], interval);
        }
        return clone_;
    };

    /**
     * Inverts the chord by another chord that is on the unison diagonal, by
     * default the origin.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.I = function(center) {
        center = typeof center !== 'undefined' ? center : 0;
        let inverse = this.clone();
        for (let voice = 0; voice < this.size(); voice++) {
            inverse.voices[voice] = ChordSpace.I(this.voices[voice], center);
        }
        return inverse;
    };

    /**
     * Returns the reflection of this chord specifically in the inversion flat 
     * within RPT. Preserves the fundamental domain of RP and RPT.
     * TODO: Fix for all cardinalities.
     */
    Chord.prototype.reflect = function() {
        let unit_normal_vector;
        if (this.size() === 3) {
            // Find a unit vector that is normal to the inversion flat...
            let origin_ = this.origin();
            let low_normal_endpoint = origin_.clone();
            low_normal_endpoint.voices[low_normal_endpoint.size() - 1] = ChordSpace.OCTAVE;
            let high_normal_endpoint = origin_.clone();
            for (let i = 1; i < high_normal_endpoint.size(); i++ ) {
                high_normal_endpoint.voices[i] = ChordSpace.OCTAVE;
            }
            let normal_vector = high_normal_endpoint.subtract(low_normal_endpoint);
            let magnitude = numeric.norm2(normal_vector.voices);
            // This is a _unit_ vector normal to the inversion flat.
            unit_normal_vector = numeric.div(normal_vector.voices, magnitude);
        } else {
            unit_normal_vector = ChordSpace.inversion_flat_normals.get(this.size());
        }
        // H = I_n - 2 * ( u x u), x is outer product.
        // For an affine hyperplane, the reflection is:
        // Ref(v) = v - 2 {[(v . u) - c] / (u . u)} . u, where c is the distance of the
        // hyperplane from the origin.
        let translate_to_origin = numeric.sub(origin, unit_normal_vector);
        let tensor_ = numeric.tensor(unit_normal_vector, unit_normal_vector);
        let product_ = numeric.mul(tensor_, 2);
        let identity_ = numeric.identity(this.size());
        let householder = numeric.sub(identity_, product_);
        let translated_voices = numeric.add(this.voices, translate_to_origin);
        let reflected_translated_voices = numeric.dot(householder, translated_voices); 
        let reflected_voices = numeric.sub(reflected_translated_voices, translate_to_origin);
        reflection = new ChordSpace.Chord(reflected_voices);
        return reflection;
    };
    
    /**
     * Moves this chord by adding to it the pitches of another.
     * NOTE: Does NOT necessarily return the result under any equivalence 
     * class.
     */
    Chord.prototype.add = function(other) {
        let translation = this.clone();
        for (let i = 0; i < this.size(); i++) {
            translation.voices[i] = this.voices[i] + other.voices[i];
        }
        return translation;
    };

    /**
     * Moves this chord by subtracting from  it the pitches of another.
     * NOTE: Does NOT necessarily return the result under any equivalence 
     * class.
     */
    Chord.prototype.subtract = function(other) {
        let translation = this.clone();
        for (let i = 0; i < this.size(); i++) {
            translation.voices[i] = this.voices[i] - other.voices[i];
        }
        return translation;
    };
    
    /**
     * Returns the remainder of the dividend divided by the divisor,
     * according to the Euclidean definition.
     */
    ChordSpace.modulo = function(dividend, divisor) {
        let quotient = 0.0;
        if (divisor < 0.0) {
            quotient = Math.ceil(dividend / divisor);
        }
        if (divisor > 0.0) {
            quotient = Math.floor(dividend / divisor);
        }
        let remainder = dividend - (quotient * divisor);
        return remainder;
    };

    /**
     * Returns the equivalent of the pitch under pitch-class equivalence, i.e.
     * the pitch is in the interval [0, OCTAVE).
     */
    ChordSpace.epc = function(pitch) {
        let pc = ChordSpace.modulo(pitch, ChordSpace.OCTAVE);
        return pc;
    };

    /**
     * Returns whether the chord is within the fundamental domain of
     * pitch-class equivalence, i.e. is a pitch-class set.
     */
    Chord.prototype.isepcs = function() {
        for (let voice = 0; voice < this.size(); voice++) {
            if (ChordSpace.eq_epsilon(this.voices[voice], ChordSpace.epc(chord.voices[voice])) === false) {
                return false;
            }
        }
        return true;
    };

    Chord.prototype.er = function(range) {
        let chord = this.clone();
        for (let voice = 0; voice < this.size(); voice++) {
            chord.voices[voice] = ChordSpace.modulo(chord.voices[voice], range);
        }
        return chord;
    };

    /**
     * Returns the equivalent of the chord under pitch-class equivalence,
     * i.e. the pitch-class set of the chord.
     */
    Chord.prototype.epcs = function() {
        return this.er(ChordSpace.OCTAVE);
    };

    Chord.prototype.eopcs = function() {
        return this.er(ChordSpace.OCTAVE).eP();
    };

    /**
     * Returns the equivalent of the chord within the fundamental domain of
     * transposition to 0.
     */
    Chord.prototype.et = function() {
        let min_ = this.min()[0];
        return this.T(-min_);
    };
    
    /**
     * Returns the standard notation for "chord type," i.e., root position
     * pitch-classes in ascending order.
     */
    Chord.prototype.chord_type = function() {
        let ev = this.eV();
        // Rotate "voicing equivalent" voicing back to "root position."
        ev = ev.v(-1);
        return ev.et();
    };

    /**
     * Returns whether the chord is within the fundamental domain of
     * transposition to 0.
     */
    Chord.prototype.iset = function() {
        let et = this.et();
        if (et.eq_epsilon(this) === false) {
            return false;
        }
        return true;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of the indicated range equivalence.
     */
    Chord.prototype.iseR = function(range) {
        let max_ = this.max()[0];
        let min_ = this.min()[0];
        if (ChordSpace.le_epsilon(max_, (min_ + range)) === false) {
            return false;
        }
        let sum_ = this.sum();
        if (ChordSpace.le_epsilon(0., sum_) === false) {
            return false;
        }
        if (ChordSpace.le_epsilon(sum_, range) === false) {
            return false;
        }
        return true;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave equivalence.
     */
    Chord.prototype.iseO = function() {
        return this.iseR(ChordSpace.OCTAVE);
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of a range equivalence.
     */
    Chord.prototype.eR = function(range) {
        // The clue here is that at least one voice must be >= 0,
        // but no voice can be > range.
        // First, move all pitches inside the interval [0,  range).
        let er = this.er(range);
        let most_compact_er = er;
        // Then, reflect voices that are outside of the fundamental domain
        // back into it, which will revoice the chord, i.e.
        // the sum of pitches will then be in [0,  range].
        // There may actually be more than one chord in the fundamental 
        // domain that meet this criterion.
        while (ChordSpace.le_epsilon(er.sum(), range) === false) {
            let max_ = er.max();
            let maximum_pitch = max_[0];
            let maximum_voice = max_[1];
            // Because no voice is above the range,
            // any voices that need to be revoiced will now be negative.
            er.voices[maximum_voice] = maximum_pitch - range;
            if (ChordSpace.le_epsilon(er.span(), most_compact_er.span()) == true) {
                most_compact_er = er;
            }
        }
        return most_compact_er;
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave equivalence.
     */
    Chord.prototype.eO = function() {
        return this.eR(ChordSpace.OCTAVE);
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of permutational equivalence.
     */
    Chord.prototype.iseP = function() {
        for (let voice = 1; voice < this.size(); voice++) {
            if (ChordSpace.le_epsilon(this.voices[voice - 1], this.voices[voice]) === false) {
                return false;
            }
        }
        return true;
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of permutational equivalence.
     */
    Chord.prototype.eP = function() {
        clone_ = this.clone();
        clone_.voices.sort(ChordSpace.compare_epsilon);
        return clone_;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of transpositional equivalence.
     */
    Chord.prototype.iseT = function() {
        let sum_ = this.sum();
        if (ChordSpace.eq_epsilon(sum_, 0) === false) {
            return false;
        } else {
            return true;
        }
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of transpositonal equivalence.
     */
    Chord.prototype.eT = function() {
        let sum_ = this.sum();
        let transposition = sum_ / this.size();
        return this.T(-transposition);
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of transpositonal equivalence and the equal temperament generated
     * by g. I.e., returns the chord transposed such that its sum is the least 
     * that divides evenly by g. NOTE: Does NOT
     * return the result under any other equivalence class.
     */
    Chord.prototype.eTT = function(g) {
        g = typeof g !== 'undefined' ? g : 1;
        let t_ = this.eT();
        let t_ceiling = t_.ceiling(g);
        while (ChordSpace.lt_epsilon(t_ceiling.sum(), 0.) === true) {
            t_ceiling == t_ceiling.T(g);
        }
        return t_ceiling;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of translational equivalence and the equal temperament generated by g.
     */
    Chord.prototype.iseTT = function(g) {
        let sum_ = this.sum();
        if (ChordSpace.eq_epsilon(sum_, 0) === true) {
            return true;
        }
        let t_ = this.eT();
        let tt = t_.eTT(g);
        if (this.eq_epsilon(tt) == true) {
            return true;
        }
        return false;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of inversional equivalence.
     */
    Chord.prototype.iseI = function() {
        let lower_voice = 0;
        let upper_voice = this.size() - 2;
        while (lower_voice < upper_voice) {
            let lower_interval = this.voices[lower_voice + 1] - this.voices[lower_voice];
            let upper_interval = this.voices[upper_voice + 1] - this.voices[upper_voice];
            if (ChordSpace.le_epsilon(lower_interval, upper_interval) === false) {
                return false;
            }
            lower_voice = lower_voice + 1;
            upper_voice = upper_voice - 1;
        }
        return true;
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of inversional equivalence; in this context, reflection through the 
     * point at the center of chord space.
     */
    Chord.prototype.eI = function() {
        if (this.iseI()) {
            return this.clone();
        }
        return this.eI();
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range and permutational equivalence.
     */
    Chord.prototype.iseRP = function(range) {
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
            return false;
        }
        return true;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave and permutational equivalence.
     */
    Chord.prototype.iseOP = function() {
        return this.iseRP(ChordSpace.OCTAVE);
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range and permutational equivalence.
     */
    Chord.prototype.eRP = function(range) {
        return this.eR(range).eP();
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave and permutational equivalence.
     */
    Chord.prototype.eOP = function() {
        return this.eRP(ChordSpace.OCTAVE);
    };

    /**
     * Returns a copy of the chord cyclically permuted by a stride, by default 1.
     * The direction of rotation is the same as musicians' first inversion, second
     * inversion, and so on.
     */
    Chord.prototype.cycle = function(stride) {
        stride = typeof stride !== 'undefined' ? stride : 1;
        let permuted = this.clone();
        let i = 0;
        if (stride < 0) {
            for (i = 0; i < Math.abs(stride); i++) {
                let tail = permuted.voices.pop();
                permuted.voices.unshift(tail);
            }
            return permuted;
        }
        if (stride > 0) {
            for (i = 0; i < stride; i++) {
                let head = permuted.voices.shift();
                permuted.voices.push(head);
            }
        }
        return permuted;
    };

    /**
     * Returns the permutations of the pitches in a chord. The permutations from
     * any particular permutation are always returned in the same order.
     */
    Chord.prototype.permutations = function() {
        let permutation = this.clone();
        let permutations_ = [];
        permutations_.push(permutation);
        for (let i = 1; i < this.size(); i++) {
            permutation = permutation.cycle(1);
            permutations_.push(permutation);
        }
        permutations_.sort(ChordSpace.chord_compare_epsilon);
        return permutations_;
    };

    /**
     * Returns whether the chord is within the representative fundamental 
     * domain of voicing equivalence. In Tymoczko's 1-based notation:
     * x[1] + 12 - x[N] <= x[i + 1] - x[i], 1 <= i < N - 1
     * In 0-based notation:
     * x[0] + 12 - x[N-1] <= x[i + 1] - x[i], 0 <= i < N - 2
     */
    Chord.prototype.iseV = function(range) {
        if (this.size() < 3) {
            return true;
        }
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        let comparisons = this.size() - 1;
        let top_voice = comparisons;
        let bottom_voice = 0;
        let wraparound_interval = (this.voices[bottom_voice] + range) - this.voices[top_voice];
        let other_interval;
        for ( ; bottom_voice < comparisons; bottom_voice++) {
            other_interval = this.voices[bottom_voice + 1] - this.voices[bottom_voice];
            if (ChordSpace.le_epsilon(wraparound_interval, other_interval) === false) {
                return false;
            }
        }
        return true;
    };
        
    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of voicing equivalence.
     */
    Chord.prototype.eV = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        let voicings_ = this.voicings();
        for (let i = 0; i < this.size(); i++) {
            let voicing = voicings_[i];
            if (voicing.iseV(range)) {
                return voicing;
            }
        }
        console.error("Chord.eV: no voicing equivalent found.");
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range, permutational, and transpositional equivalence.
     */
    Chord.prototype.iseRPT = function(range) {
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
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

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, and transpositional equivalence.
     */
    Chord.prototype.iseOPT = function() {
        return this.iseRPT(ChordSpace.OCTAVE);
    };

    Chord.prototype.iseOPTT = function() {
        return this.iseRPTT(ChordSpace.OCTAVE);
    };

    /**
     * Returns a copy of the chord 'inverted' in the musician's sense,
     * i.e. revoiced by cyclically permuting the chord and
     * adding (or subtracting) an octave to the highest (or lowest) voice.
     * The revoicing will move the chord up or down in pitch.
     * A positive direction is the same as a musician's first inversion,
     * second inversion, etc.
     */
    Chord.prototype.v = function(direction) {
        direction = typeof direction !== 'undefined' ? direction : 1;
        let chord = this.clone();
        while (direction > 0) {
            chord.voices[0] = chord.voices[0] + ChordSpace.OCTAVE;
            chord = chord.cycle(1);
            direction = direction - 1;
        }
        let n = chord.size() - 1;
        while (direction < 0) {
            chord.voices[n] = chord.voices[n] - ChordSpace.OCTAVE;
            chord = chord.cycle(-1);
            direction = direction + 1;
        }
        return chord;
    };

    /**
     * Returns all the 'inversions' (in the musician's sense)
     * or octavewise revoicings of the chord.
     */
    Chord.prototype.voicings = function() {
        let chord = this.clone();
        let voicings = [];
        voicings.push(chord);
        for (let i = 1; i < chord.size(); i++) {
            chord = chord.v();
            voicings.push(chord);
        }
        return voicings;
    };

    /**
     * Returns the equivalent of the chord within the representative 
     * fundamental domain of range, permutational, and transpositional 
     * equivalence; similar to chord type.
     * FIXME: Take g into account?
     */
    Chord.prototype.eRPT = function(range) {
        let rp = this.eRP(range);
        let rp_voicings = rp.voicings();
        let rp_voicing_t;
        for (rp_voicing of rp_voicings) {
            if (rp_voicing.iseV() === true) {
                break;
            }
        }
        rp_voicing_t = rp_voicing.eT();
        return rp_voicing_t;
    };

    Chord.prototype.eRPTT = function(range) {
        let rp = this.eRP(range);
        let rp_voicings = rp.voicings();
        let rp_voicing_tt;
        for (rp_voicing of rp_voicings) {
            if (rp_voicing.iseV() === true) {
                break;
            }
        }
        rp_voicing_tt = rp_voicing.eTT();
        return rp_voicing_tt;
     };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave, permutational, and transpositional equivalence.
     */
    Chord.prototype.eOPT = function() {
        return this.eRPT(ChordSpace.OCTAVE);
    };

    Chord.prototype.eOPTT = function() {
        return this.eRPTT(ChordSpace.OCTAVE);
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range, permutational, and inversional equivalence.
     */
    Chord.prototype.iseRPI = function(range) {
        if (this.eP(range) == false) {
            return false;
        }
        if (this.eR(range) == false) {
            return false;
        }
        if (this.eI() == false) {
            return false;
        }
        return true;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, and inversional equivalence.
     */
    Chord.prototype.iseOPI = function() {
        return this.iseRPI(ChordSpace.OCTAVE);
    };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, and inversional equivalence.
     * TODO: Verify.
     */
    Chord.prototype.eRPI = function(range) {
        if (this.iseRPI(range) === true) {
            return this.clone();
        }
        return this.eI().eRP(range);
     };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of octave, permutational, and inversional equivalence.
     */
    Chord.prototype.eOPI = function() {
        return this.eRPI(ChordSpace.OCTAVE);
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of range, permutational, transpositional, and inversional equivalence.
     */
    Chord.prototype.iseRPTI = function(range) {      
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
            return false;
        }
        // Testing for voicing equivalence first makes it possible to test 
        // only one of the possible transpositional equivalents.
        if (this.iseV() === false) {
            return false;
        }
        if (this.iseT(range) === false) {
            return false;
        }
        if (this.iseI() === false) {
            return false;
        }
        return true;
    };

    Chord.prototype.iseRPTTI = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        if (this.iseP() === false) {
            return false;
        }
        if (this.iseR(range) === false) {
            return false;
        }
        if (this.iseV() === false) {
            return false;
        }
        // Testing for voicing equivalence first makes it possible to test 
        // only one of the possible transpositional equivalents.
        if (this.iseTT() === false) {
            return false;
        }
        if (this.iseI() === false) {
            return false;
        }
        return true;
    };

    /**
     * Returns whether the chord is within the representative fundamental domain
     * of octave, permutational, transpositional, and inversional equivalence.
     */
    Chord.prototype.iseOPTI = function() {
        return this.iseRPTI(ChordSpace.OCTAVE);
    };
    Chord.prototype.iseOPTTI = function() {
        let result = this.iseRPTTI(ChordSpace.OCTAVE);
        return result;
    };

    /**
     * Returns the equivalent of the chord within the representative 
     * fundamental domain of range, permutational, transpositional, and 
     * inversional equivalence.
     */
    Chord.prototype.eRPTI = function(range) {
        let rpt = this.eRPT(range);
        if (rpt.iseI() === true) {
            return rpt;
        } else {
            // If this is not eI, then reflect this in the center, which preserves
            // the fundamental domain of OPT.
            let center_ = rpt.center();
            let rpt_i = center_.subtract(rpt);
            // Should spin this back into V to preserve T.
            let rpt_i_t = rpt_i.eRP(range);
            return rpt_i_t;
        }
    };
    
    /**
     * Returns the equivalent of the chord within the representative 
     * fundamental domain of range, permutational, transpositional, and 
     * inversional equivalence, quantized within equal temperament.
     */
    Chord.prototype.eRPTTI = function(range) {
        let rpt = this.eRPT(range);
        if (rpt.iseI() === true) {
            let rpt_tt = rpt.eTT();
            return rpt_tt;
        } else {
            let rpt_i = rpt.reflect();
            let rpt_i_tt = rpt_i.eTT();
            return rpt_i_tt;            
        }
     };

    /**
     * Returns the equivalent of the chord within the representative fundamental
     * domain of range, permutational, transpositional, and inversional
     * equivalence.
     */
    Chord.prototype.eOPTI = function() {
        return this.eRPTI(ChordSpace.OCTAVE);
    };
    Chord.prototype.eOPTTI = function() {
        return this.eRPTTI(ChordSpace.OCTAVE);
    };
    
    let pitchClassesForNames = {};

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

    let namesForPitchClasses = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];

    let chordsForNames = {};
    let namesForChords = {};

    /**
     * Returns the standard name for the pitch (middle C is MIDI key 60 is
     * C4). There are of course no enharmonic names here, so all are sharps.
     * Works only for 12-tone equal temperament.
     */
    ChordSpace.noteName = function (midi_key) {
        midi_key = Math.round(midi_key);
        let pc = Math.round(ChordSpace.epc(midi_key))
        let note_name = namesForPitchClasses[pc]
        let octave = Math.floor(midi_key / 12) - 1;
        let text = sprintf("%s%d", note_name, octave);
        return text;
    }

    let fill = function(rootName, rootPitch, typeName, typePitches) {
        typePitches = typePitches.trim();
        let chordName = rootName + typeName;
        let chord = new ChordSpace.Chord();
        let splitPitches = typePitches.split(/\s+/g);
        if (typeof splitPitches !== 'undefined') {
            chord.resize(splitPitches.length);
            for (let voice = 0; voice < splitPitches.length; voice++) {
                let pitchName = splitPitches[voice];
                if (pitchName.length > 0) {
                    let pitch = ChordSpace.pitchClassesForNames[pitchName];
                    chord.voices[voice] = rootPitch + pitch;
                }
            }
            let eop = chord.eOP();
            chordsForNames[chordName] = eop;
            namesForChords[eop.hash()] = chordName;
        }
    };

    for (let rootName in pitchClassesForNames) {
        if (pitchClassesForNames.hasOwnProperty(rootName)) {
            let rootPitch = pitchClassesForNames[rootName];
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
            fill(rootName, rootPitch, "7#9", "C  D#        E       G        Bb  ");
            fill(rootName, rootPitch, "9", "C     D      E       G        Bb  ");
            fill(rootName, rootPitch, "9#11", "C     D      E F#    G        Bb  ");
            fill(rootName, rootPitch, "13", "C     D      E F     G     A  Bb  ");
            fill(rootName, rootPitch, "13#11", "C     D      E F#    G     A  Bb  ");
        }
    }

    ChordSpace.namesForChords = namesForChords;
    ChordSpace.chordsForNames = chordsForNames;

    Chord.prototype.name = function() {
        let name_ = ChordSpace.namesForChords[this.eOP().hash()];
        if (typeof name_ === 'undefined') {
            name_ = this.eOP().toPitches();
        }
        return name_;
    };

    ChordSpace.nameForChord = function(chord) {
        return chord.name();
    };

    ChordSpace.chordForName = function(name) {
        return ChordSpace.chordsForNames[name];
    };

    /**
     * Returns a formatted string with much information about the chord.
     */
    Chord.prototype.information = function() {
        let template = `
        Chord:   %s  name:     %s 
        pcs:     %s
        pitches: %s
        type:    %s
        eO:      %s  iseO:     %s
        eP:      %s  iseP:     %s
        eT:      %s  iseT:     %s
        eTT:     %s  iseTT:    %s
        eI:      %s  iseI:     %s
        eV:      %s  iseV:     %s
        eOP:     %s  iseOP:    %s
        eOPT:    %s  iseOPT:   %s
        eOPTI:   %s  iseOPTI:  %s
        eOPTT:   %s  iseOPTT:  %s
        eOPTTI:  %s  iseOPTTI: %s
        sum:     %s
        `;
        let text = sprintf(template, 
            this, this.name(),
            this.eopcs(), 
            this.toPitches(),
            this.chord_type(),
            this.eO(), this.iseO(),
            this.eP(), this.iseP(),
            this.eT(), this.iseT(),
            this.eTT(), this.iseTT(),
            this.eI(), this.iseI(),
            this.eV(), this.iseV(),
            this.eOP(), this.iseOP(),
            this.eOPT(), this.iseOPT(),
            this.eOPTI(), this.iseOPTI(),
            this.eOPTT(), this.iseOPTT(),
            this.eOPTTI(), this.iseOPTTI(),
            this.sum());
        return text;
    };

    /**
     * Move 1 voice of the chord.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.move = function(voice, interval) {
        let chord = this.clone();
        chord.voices[voice] = ChordSpace.T(chord.voices[voice], interval);
        return chord;
    };

    /**
     * Performs the neo-Riemannian parallel transformation.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.nrP = function() {
        let cv = this.eV();
        let cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[1] = cv.voices[1] - 1;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[1] = cv.voices[1] + 1;
        }
        return cv;
    };

    /**
     * Performs the neo-Riemannian relative transformation.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.nrR = function() {
        let cv = this.eV();
        let cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[2] = cv.voices[2] + 2;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[0] = cv.voices[0] - 2;
        }
        return cv;
    };

    /**
     * Performs the neo-Riemannian Lettonwechsel transformation.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.nrL = function() {
        let cv = this.eV();
        let cvt = this.eV().et();
        if (ChordSpace.eq_epsilon(cvt.voices[1], 4) === true) {
            cv.voices[0] = cv.voices[0] - 1;
        } else if (ChordSpace.eq_epsilon(cvt.voices[1], 3) === true) {
            cv.voices[2] = cv.voices[2] + 1;
        }
        return cv;
    };

    /**
     * Performs the neo-Riemannian dominant transformation.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.nrD = function() {
        return this.T(-7);
    };

    /**
     * Returns the chord inverted by the sum of its first two voices.
     * NOTE: Does NOT return the result under any equivalence class.
     */
    Chord.prototype.K = function(range) {
        range = typeof range !== 'undefined' ? range : ChordSpace.OCTAVE;
        let chord = this.clone();
        if (chord.size() < 2) {
            return chord;
        }
        // Unordered! In [0, 12)!
        let epc = chord.epcs();
        let x = epc.voices[0] + epc.voices[1];
        return this.I(x);
    };

    /**
     * Returns whether the chord is a transpositional form of Y with interval size g.
     * Only works in equal temperament.
      */
    Chord.prototype.Tform = function(Y_, g) {
        let eopx = this.eOP();
        let i = 0;
        while (i < ChordSpace.OCTAVE) {
            let ty = Y_.T(i);
            let eopty = ty.eOP();
            if (eopx.eq_epsilon(eopty) === true) {
                return true;
            }
            i = i + g;
        }
        return false;
    };

    /**
     * Returns whether the chord is an inversional form of Y with interval size g.
     * Only works in equal temperament.
     */
    Chord.prototype.Iform = function(Y, g) {
        let eopx = this.eOP();
        let i = 0;
        while (i < ChordSpace.OCTAVE) {
            let iy = Y.I(i);
            let eopiy = iy.eOP();
            if (eopx.eq_epsilon(eopiy) === true) {
                return true;
            }
            i = i + g;
        }
        return false;
    };

    /**
     * Returns the contextual transposition of the chord by x with respect to m
     * with minimum interval size g.
     * NOTE: Does NOT return the result under any equivalence class.
     */
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

    /**
     * Returns the number of invariant voices, under pitch-class equivalence, in
     * the chord. The two chords must have the same number of voices.
     */
    ChordSpace.invariantvoiceN = function(a_, b_) {
        let a = a_.eOP();
        let b = b_.eOP();
        let count = 0;
        for (let voice = 0; voice < a_.size(); voice++) {
            let p = a.voices[voice];
            if (a.count(p) === b.count(p)) {
                count = count + 1;
            }
        }
        return count;
    };

    /**
     * Returns the contextual inversion(s) of the chord that preserves n
     * invariant voices under pitch-class equivalence, in a sorted list. If there
     * are no such inversions, an empty list is returned. The inversions are
     * returned in equivalence class OP. g is the generator of transposition, by
     * default 1.
     */
    Chord.prototype.J = function(n, g) {
        g = typeof g !== 'undefined' ? g : 1;
        let inversions = {};
        for (let I = 0; ChordSpace.le_epsilon(I, ChordSpace.OCTAVE); I = I + g) {
            let inversion = this.I(I);
            if (ChordSpace.invariantvoiceN(this, inversion) === n) {
                let eopi = inversion.eOP();
                inversions[eopi.hash()] = eopi;
            }
        }
        result = [];
        for (let key in inversions) {
            if (inversions.hasOwnProperty(key)) {
                result.push(inversions[key]);
            }
        }
        result.sort(ChordSpace.chord_compare_epsilon);
        return result;
    };

    /**
     * Returns the voice-leading between chords a and b,
     * i.e. what you have to add to a to get b, as a
     * chord of directed intervals.
     */
    ChordSpace.voiceleading = function(a, b) {
        let voiceleading = a.clone();
        for (let voice = 0; voice < a.size(); voice++) {
            voiceleading.voices[voice] = b.voices[voice] - a.voices[voice];
        }
        return voiceleading;
    };

    /**
     * Returns whether the voiceleading
     * between chords a and b contains a parallel fifth.
     */
    ChordSpace.parallelFifth = function(a, b) {
        let v = ChordSpace.voiceleading(a, b);
        if (v.count(7) > 1 || v.count(-7) > 1) {
            return true;
        } else {
            return false;
        }
    };

    /**
     * Returns the smoothness of the voiceleading between
     * chords a and b by L1 norm.
     */
    ChordSpace.voiceleadingSmoothness = function(a, b) {
        let L1 = 0;
        for (let voice = 0; voice < a.size(); voice++) {
            L1 = L1 + Math.abs(b.voices[voice] - a.voices[voice]);
        }
        return L1;
    };

    /**
     * Returns which of the voiceleadings (source to d1, source to d2)
     * is the smoother (shortest moves), optionally avoiding parallel fifths.
     */
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
        let s1 = ChordSpace.voiceleadingSmoothness(source, d1);
        let s2 = ChordSpace.voiceleadingSmoothness(source, d2);
        if (s1.le_epsilon(s2) === true) {
            return d1;
        } else {
            return d2;
        }
    };

    /**
     * Returns which of the voiceleadings (source to d1, source to d2)
     * is the simpler (fewest moves), optionally avoiding parallel fifths.
     */
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
        let v1 = ChordSpace.voiceleading(source, d1).eP();
        let v2 = ChordSpace.voiceleading(source, d2).eP();
        //for voice = #v1, 1, -1 do
        for (let voice = v1.size() - 1; voice >= 0; voice--) {
            if (ChordSpace.lt_epsilon(v1[voice], v2[voice]) === true) {
                return d1;
            }
            if (ChordSpace.lt_epsilon(v2[voice], v1[voice]) === true) {
                return d2;
            }
        }
        return d1;
    };

    /**
     * Returns which of the voiceleadings (source to d1, source to d2)
     * is the closer (first smoother, then simpler), optionally avoiding parallel fifths.
     */
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
        let s1 = ChordSpace.voiceleadingSmoothness(source, d1);
        let s2 = ChordSpace.voiceleadingSmoothness(source, d2);
        if (ChordSpace.lt_epsilon(s1, s2) === true) {
            return d1;
        }
        if (ChordSpace.gt_epsilon(s1, s2) === true) {
            return d2;
        }
        return ChordSpace.voiceleadingSimpler(source, d1, d2, avoidParallels);
    };

    ChordSpace.next = function(iterator_, origin, range, g) {
        let least_significant_voice = iterator_.size() - 1;
        let most_significant_voice = 0;
        // Increment, as in an odometer.
        iterator_.setPitch(least_significant_voice, iterator_.getPitch(least_significant_voice) + g);
        // If necessary, carry the increment to the next most significant voice.
        let voice;
        for (voice = least_significant_voice; voice > most_significant_voice; --voice) {
            if (Silencio.gt_epsilon(iterator_.voices[voice], (origin.voices[voice] + range))) {
                iterator_.voices[voice] = origin.voices[voice];
                iterator_.voices[voice - 1] = (iterator_.voices[voice - 1] + g);
            }
        }
        if (Silencio.gt_epsilon(iterator_.voices[most_significant_voice], (origin.voices[most_significant_voice] + range)) === true) {
            return false;
        }
        return true;
    };

    /**
     * FIXME: Looks like untested bad logic.
     * Returns the voicing of the destination that has the closest voice-leading
     * from the source within the range, optionally avoiding parallel fifths.
     */
    ChordSpace.voiceleadingClosestRange = function(prior_chord, destination, range, avoidParallels) {
      //` return d;
        let voices = destination.size();
        let origin = destination.eOP();
        let odometer = origin.clone();
        // Enumerate the permutations.
        // iterator[0] is the most significant voice, and
        // iterator[N-1] is the least significant voice.
        let voicings = 0;
        let d = origin.clone();
        while (ChordSpace.next(odometer, origin, range, ChordSpace.OCTAVE) === true) {
            let revoicing = odometer.clone();
            d = ChordSpace.voiceleadingCloser(prior_chord, d, revoicing, avoidParallels);
            voicings = voicings + 1;
        }
        return d;
    };

    /**
     * Creates a complete Silencio "note on" event for the
     * indicated voice of the chord.
     */
    Chord.prototype.note = function(voice_, time_, duration_, channel_, velocity_, pan_) {
        duration_ = typeof this.duration[voice_] !== 'undefined' ? this.duration[voice_] : duration_;
        channel_ = typeof this.channel[voice_] !== 'undefined' ? this.channel[voice_] : channel_;
        let key_ = this.voices[voice_];
        velocity_ = typeof this.velocity[voice_] !== 'undefined' ? this.velocity[voice_] : velocity_;
        pan_ = typeof this.pan[voice_] !== 'undefined' ? this.pan[voice_] : pan_;
        if (typeof time_ === 'undefined') {
            time_ = 0;
        }
        if (typeof duration_ === 'undefined') {
            duration_ = this.duration[voice_];
        }
        let note_ = new Silencio.Event();
        note_.data[Silencio.Event.TIME] = time_;
        note_.data[Silencio.Event.DURATION] = duration_;
        note_.data[Silencio.Event.CHANNEL] = channel_;
        note_.data[Silencio.Event.KEY] = key_;
        note_.data[Silencio.Event.VELOCITY] = velocity_;
        note_.data[Silencio.Event.PAN] = pan_;
        return note_;
    };

    /**
     * Returns an individual note for each voice of the chord.
     * The chord's duration, instrument, and loudness are used if present,
     * if not the specified values are used.
     */
    Chord.prototype.notes = function(time_, duration_, channel_, velocity_, pan_) {
        let notes_ = new Silencio.Score();
        for (let voice = 0; voice < this.size(); voice++) {
            notes_.append(this.note(voice, time_, duration_, channel_, velocity_, pan_));
        }
        return notes_;
    };

    Chord.prototype.toScore = function(score, time_, duration_, channel_, velocity_, pan_) {
        for (let voice = 0; voice < this.size(); voice++) {
            score.append(this.note(voice, time_, duration_, channel_, velocity_, pan_));
        }
        return score;
    };

    /**
     * Move the pitch to the closest pitch-class of the chord.
     */
    ChordSpace.conformPitchToChord = function(pitch, chord, octave_equivalence) {
        octave_equivalence = typeof octave_equivalence !== 'undefined' ? octave_equivalence : true;
        let pitch_class = ChordSpace.modulo(pitch, ChordSpace.OCTAVE);
        let octave = pitch - pitch_class;
        let chord_pitch_class = ChordSpace.modulo(chord.voices[0], ChordSpace.OCTAVE);
        let distance = Math.abs(chord_pitch_class - pitch_class);
        let closest_pitch_class = chord_pitch_class;
        let minimum_distance = distance;
        for (let voice = 1; voice < chord.size(); voice++) {
            chord_pitch_class = ChordSpace.modulo(chord.voices[voice], ChordSpace.OCTAVE);
            distance = Math.abs(chord_pitch_class - pitch_class);
            if (ChordSpace.lt_epsilon(distance, minimum_distance) === true) {
                minimum_distance = distance;
                closest_pitch_class = chord_pitch_class;
            }
        }
        let new_pitch = closest_pitch_class;
        if (octave_equivalence !== true) {
            new_pitch = octave + new_pitch;
        }
        return new_pitch;
    };

    /**
     * If the event is a note, moves its pitch
     * to the closest pitch of the chord.
     * If octave_equivalence is true (the default),
     * the pitch-class of the note is moved to the closest pitch-class
     * of the chord; otherwise, the pitch of the note is moved to the closest
     * absolute pitch of the chord.
     */
    ChordSpace.conformToChord = function(event, chord, octave_equivalence) {
        octave_equivalence = typeof octave_equivalence !== 'undefined' ? octave_equivalence : true;
        if (event.status === 144) {
            event.key = ChordSpace.conformPitchToChord(event.key, chord, octave_equivalence);
        }
        return event;
    };

    ChordSpace.conformScoreToChord = function(score, chord, octave_equivalence) {
        octave_equivalence = typeof octave_equivalence !== 'undefined' ? octave_equivalence : true;
        let event = null;
        for (let i = 0; i < score.size(); i++) {
            event = score.data[i];
            if (event.status === 144) {
                event.key = ChordSpace.conformPitchToChord(event.key, chord, octave_equivalence);
            }
        }
        return event;
    };

    /**
     * Inserts the notes of the chord into the score at the specified time.
     * The internal duration, instrument, and loudness are used.
     */
    ChordSpace.insert = function(score, chord, time_) {
        for (let voice = 0; voice < chord.size(); voice++) {
            let event = chord.note(voice, time_, chord.getDuration(voice), chord.getChannel(voice), chord.getVelocity(voice), chord.getPan(voice));
            score.append(event);
        }
        return score;
    };

    /**
     * For all the notes in the score beginning at or later than the start time,
     * and up to but not including the end time, moves the pitch of the note to
     * belong to the chord, using the conformToChord function.
     */
    ChordSpace.apply = function(score, chord, start, end_, octave_equivalence) {
        if (typeof chord === 'undefined') {
            return;
        }
        octave_equivalence = typeof octave_equivalence !== 'undefined' ? octave_equivalence : true;
        let s = score.slice(start, end_, true);
        for (let index = 0; index < s.size(); index++) {
            let event = s.data[index];
            ChordSpace.conformToChord(event, chord, octave_equivalence);
        }
        let message = sprintf('Conform to: %-20.20s from: %12.4f to: %12.4f notes: %5d.', chord.name(), parseFloat(start), parseFloat(end_), parseInt(s.data.length));
        if (typeof csound !== 'undefined') {
            console.info(message + '\n');
        } else {
            console.info(message);
        }
        return s;
    };

    /**
     * Returns a chord containing all the pitches of the score
     * beginning at or later than the start time,
     * and up to but not including the end time.
     */
    ChordSpace.gather = function(score, start, end_) {
        let chord = new ChordSpace.Chord();
        let slice = score.slice(start, end_);
        for (let index = 0; index < slice.size(); index++) {
            let event = slice.get(index);
            let pitch = event.key;
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
        console.info('ChordSpace.LSys.prototype.generate\n');
        this.chordsForTimes = {};
        this.sentence = this.axiom.split(' ');
        for (let g = 0; g < n; g++) {
            let next = [];
            for (let i = 0; this.sentence.length > i; i++) {
                let c = this.sentence[i];
                let r = this.rules[c];
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
            this.chordsForTimes[t.event.time] = t.chord.clone();
        } else if (c === 'K') {
            t.K();
            this.chordsForTimes[t.event.time] = t.chord.clone();
        } else {
            let parts = c.split(',');
            let cc = parts[0];
            if (cc === 'T') {
                t.T(Number(parts[1]));
                this.chordsForTimes[t.event.time] = t.chord.clone();
            } else if (cc === 'I') {
                t.I(Number(parts[1]));
                this.chordsForTimes[t.event.time] = t.chord.clone();
            } else if (cc === 'Q') {
                t.Q(Number(parts[1]));
                this.chordsForTimes[t.event.time] = t.chord.clone();
            } else if (cc === 'J') {
                t.J(Number(parts[1], Number(parts[2])));
                this.chordsForTimes[t.event.start] = t.chord.clone();
            } else if (cc === 't') {
                let operation = parts[1];
                let operand = Number(parts[2]);
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
                    //console.info('tempo:' + t.tempo + '\n');
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
     * NOTE: Must do this AFTER rescaling pitches in the score, but BEFORE
     * rescaling times in the score.
     */
    ChordSpace.LSys.prototype.conformToChords = function() {
        let times = [];
        for (let tyme in this.chordsForTimes) {
            if (this.chordsForTimes.hasOwnProperty(tyme)) {
                times.push(tyme);
            }
        }
        times.push(this.score.getDuration());
        times.sort(function(a, b) { return a - b; });
        for (let i = 0; i < times.length - 1; i++) {
            let begin = times[i];
            let end = times[i + 1];
            let chord = this.chordsForTimes[begin];
            ChordSpace.apply(this.score, chord, begin, end, false);
        }
        console.info(sprintf("Applied %5d chords to this score.\n", Object.keys(this.chordsForTimes).length));
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
            let event = this.event.clone();
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
        let inversions = this.chord.J(n);
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
        let chord = this.v(arpeggiation);
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
     * Forces the voices to the most precise possible representation of
     * equal temperament, where g is the generator of transposition.
     * Probably works in all cases only when g is an integer.
     */
    Chord.prototype.clamp = function(g) {
        for (let voice = 0; voice < this.size(); voice++) {
            let pitch = this.voices[voice];
            pitch /= g;
            pitch = Math.round(pitch);
            pitch *= g;
            this.voices[voice] = pitch;
        }
    };
    
        // GENERIC FUNCTIONS

    // unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    var unfoldr = function (mf, v) {
        var elements = [];
        return [until(function (m) {
                return !m.valid;
            }, function (m) {
                var m2 = mf(m);
                return m2.valid && (elements = [m2.value].concat(elements)), m2;
            }, {
                valid: true,
                value: v,
                new: v
            })
            .value
        ].concat(elements);
    };

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    var until = function (p, f, x) {
        var v = x;
        while (!p(v)) {
            v = f(v);
        }
        return v;
    };

    // replicate :: Int -> a -> [a]
    var replicate = function (n, a) {
        var v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // show :: a -> String
    var show = function (x) {
        return JSON.stringify(x);
    }; //, null, 2);

    // curry :: Function -> Function
    var curry = function (f) {
        for (var lng = arguments.length,
                args = Array(lng > 1 ? lng - 1 : 0),
                iArg = 1; iArg < lng; iArg++) {
            args[iArg - 1] = arguments[iArg];
        }

        var intArgs = f.length,
            go = function (elements) {
                return elements.length >= intArgs ? f.apply(null, elements) : function () {
                    return go(elements.concat([].slice.apply(arguments)));
                };
            };
        return go([].slice.call(args, 1));
    };

    // range :: Int -> Int -> [Int]
    var range = function (m, n) {
        return Array.from({
            length: Math.floor(n - m) + 1
        }, function (_, i) {
            return m + i;
        });
    };

    /**
     * Orthogonal additive groups for unordered chords of given arity under range
     * equivalence (RP): prime form or P, inversion or I, transposition or T,
     * voicing or V, and arrangement or A. P x I x T = OP, P x I x T x V = RP.
     * Therefore, an operation on P, I, T, V, or A may be used to independently
     * transform the respective symmetry of any chord. Some of these operations
     * will reflect in RP.
     */
    let ChordSpaceGroup = function() {
        this.opttis_for_p = new Map();
        this.p_for_opttis = new Map();
    };
    ChordSpace.ChordSpaceGroup = ChordSpaceGroup;

    ChordSpace.octavewiseRevoicings = function(chord, range) {
        range = range !== 'undefined' ? range : ChordSpace.OCTAVE;
        let voices = chord.size();
        let origin = chord.eOP();
        let odometer = origin.clone();
        // Enumerate the permutations of voicings.
        // iterator[0] is the most significant voice, and
        // iterator[N-1] is the least significant voice.
        let voicings = 0;
        while (ChordSpace.next(odometer, origin, range, ChordSpace.OCTAVE) === true) {
            voicings = voicings + 1;
        }
        return voicings;
    };

    ChordSpace.octavewiseRevoicing = function(chord, index, range) {
        ///let voices = chord.size();
        let origin = chord.eOP();
        let odometer = origin.clone();
        // Enumerate the permutations.
        // iterator[0] is the most significant voice, and
        // iterator[N-1] is the least significant voice.
        let voicings = 0;
        let v;
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
        let is_isex = null;
        let make_ex = null;
        if (equivalence === 'OP') {
            is_isex = Chord.prototype.iseOP;
            make_ex = Chord.prototype.eOP;
        }
        if (equivalence === 'OPT') {
            is_isex = Chord.prototype.iseOPT;
            make_ex = Chord.prototype.eOPT;
        }
        if (equivalence === 'OPTT') {
            is_isex = Chord.prototype.iseOPTT;
            make_ex = Chord.prototype.eOPTT;
        }
        if (equivalence === 'OPI') {
            is_isex = Chord.prototype.iseOPI;
            make_ex = Chord.prototype.eOPI;
        }
        if (equivalence === 'OPTI') {
            is_isex = Chord.prototype.iseOPTI;
            make_ex = Chord.prototype.eOPTI;
        }
        if (equivalence === 'OPTTI') {
            is_isex = Chord.prototype.iseOPTTI;
            make_ex = Chord.prototype.eOPTTI;
        }
        let upper_pitch = 2 * (ChordSpace.OCTAVE + 1)    ;
        let lower_pitch = - (ChordSpace.OCTAVE + 1);
        let iterator = ChordSpace.iterator(voices, lower_pitch);
        let origin = iterator.clone();
        // Construct maps to correctly map back and forth between chords 
        // in the fundamental domain and their indexes, taking duplicates 
        // into account:
        // (1) From the index ->  the _representative_ chord (eX) in the 
        //     fundamental domain (one-to-one). 
        // (2) From _any_ equivalent chord in the fundamental domain (iseX) -> 
        //     the index of the corresponding _representative_ chord (eX) 
        //     (many-to-one). This must use a _value key_.
        // Thus, for any equivalent chord (iseX) in the fundamental domain, it 
        // must be possible to look up the index of the corresponding 
        // representative chord (eX).
        // 
        // The purpose of this is to enable the use of the index of the 
        // representative chord (eX) in OPTTI as "P" for the ChordSpaceGroup.
        //
        // To get P from iseX, use indexes_for_isexs.get(iseX).
        // To get eX from P, use exs_for_indexes.get(P).
        let exs_for_indexes = new Map();
        let indexes_for_exs = new Map();
        let indexes_for_isexs = new Map();
        let unique_isexs = new Map();
        let unique_exs = new Map();
        while (ChordSpace.next(iterator, origin, upper_pitch, g) === true) {
            let ex = make_ex.apply(iterator);
            let ex_key = ex.toString();
            if (unique_exs.has(ex_key) === false) {
                unique_exs.set(ex_key, ex);
                // Every eX must be iseX!
                if (is_isex.apply(ex) === false) {
                    // Redo the calls to make them easier to debug.
                    let ex_ = make_ex.apply(iterator.clone());
                    let isex_ = is_isex.apply(ex_);
                    console.error(sprintf("chord: %s  e%s(%s) => %s ise%s(%s): %s\n", iterator.toString(),  equivalence, iterator.toString(), ex_.toString(), equivalence, ex_.toString(), isex_));
                }
            }
            if (is_isex.apply(iterator) === true) {
                let isex = iterator.clone();
                let isex_key = isex.toString();
                if (unique_isexs.has(isex_key) === false) {
                    unique_isexs.set(isex_key, isex);
                    console.info(sprintf("%s: %4d %s sum: %9.4f span: %9.4f chord type: %s iseI: %s\n", equivalence, unique_isexs.size + 1, isex.toString(), isex.sum(), isex.span(), isex.chord_type().toString(), isex.iseI()));
                }
            }
        }
        // There must not be more eX than there are iseX.
        if (unique_exs.size > unique_isexs.size) {
            console.error(sprintf("unique_exes.size (%d) is greater than unique_isexs.size (%d)!", unique_exs.size, unique_isexs.size));
        }
        // Construct the actual maps.
        for (unique_ex of unique_exs) {
            let index = exs_for_indexes.size;
            let ex_key = unique_ex[0];
            let ex = unique_ex[1];
            exs_for_indexes.set(index, ex);
            indexes_for_exs.set(ex_key, index);
        }
        for (unique_isex of unique_isexs) {
            let isex_key = unique_isex[0];
            let isex = unique_ex[1];
            let ex = make_ex.apply(isex);
            let ex_key = ex.toString();
            let index = indexes_for_exs.get(ex_key);
            indexes_for_isexs.set(isex_key, index);
        }
        console.info(sprintf("%s: exs_for_indexes:    %6d", equivalence, exs_for_indexes.size));
        console.info(sprintf("%s: indexes_for_isexs:  %6d", equivalence, indexes_for_isexs.size));
        console.info(sprintf("%s: unique_exs:         %6d", equivalence, unique_exs.size));
        console.info(sprintf("%s: unique_isexs:       %6d", equivalence, unique_isexs.size));        
        return [exs_for_indexes, indexes_for_isexs, unique_exs, unique_isexs];
    };

    /**
     * Returns a chord with the specified number of voices all set to a first
     * pitch, useful as an iterator.
     */
    ChordSpace.iterator = function(voices, first) {
        let odometer = new Chord();
        odometer.resize(voices);
        for (let voice = 0; voice < voices; voice++) {
            odometer.voices[voice] = first;
        }
        return odometer;
    };

    ChordSpace.createChordSpaceGroup = function(voices, range, instruments, dynamics, durations, g) {
        let chordSpaceGroup = new ChordSpaceGroup();
        chordSpaceGroup.initialize(voices, range, instruments, dynamics, durations, g);
        return chordSpaceGroup;
    };

    let kpermutationForIndex = function(elements, voice_count, index) {
        let intBase = elements.length,
            intSetSize = Math.pow(intBase, voice_count),
            lastIndex = intSetSize - 1; // zero-based
        if (intBase < 1 || index > lastIndex) return undefined;
        let baseElements = unfoldr(function (m) {
                let v = m.new,
                    d = Math.floor(v / intBase);
                return {
                    valid: d > 0,
                    value: elements[v % intBase],
                    new: d
                };
            }, index),
            intZeros = voice_count - baseElements.length;
        return intZeros > 0 ? replicate(intZeros, elements[0])
            .concat(baseElements) : baseElements;
    };
    
    // TEST
    // Just items 30 to 35 in the (zero-indexed) series:
    console.info(range(30, 35)
        .map(curry(kpermutationForIndex)([1, 2, 3, 4, 5], 4)));    

    /**
     * A is the index of k-permutations with repetition of instruments for
     * voices, instruments is an array of instrument numbers.
     */
    ChordSpace.arrange_instruments = function(chord, A, instruments) {
        let arrangement = kpermutationForIndex(instruments, chord.size(), A);
        for (let voice = 0; voice < chord.size(); voice++) {
            chord.channel[voice] = arrangement[voice];
        }
    }

    /**
     * A is the index of k-permutations with repetition of instruments for
     * voices, instruments is an array of instrument numbers.
     */
    ChordSpace.arrange_dynamics = function(chord, L, dynamics) {
        let arrangement = kpermutationForIndex(dynamics, chord.size(), L);
        for (let voice = 0; voice < chord.size(); voice++) {
            chord.velocity[voice] = arrangement[voice];
        }
    }

    /**
     * A is the index of k-permutations with repetition of note durations for
     * voices, instruments is an array of instrument numbers.
     */
    ChordSpace.arrange_durations = function(chord, D, durations) {
        let arrangement = kpermutationForIndex(durations, chord.size(), D);
        for (let voice = 0; voice < chord.size(); voice++) {
            chord.duration[voice] = arrangement[voice];
        }
    }

    /**
     * Returns the chord for the indices of prime form, inversion,
     * transposition, voicing, and arrangement. The chord is not in RP; rather,
     * each voice of the chord's OP may have zero or more octaves added to it.
     * Please note, because some set classes e.g. diminished chords are
     * invariant under some T, there may be more than one PITV to get the same
     * chord. Also, every P will return one chord from the representative 
     * fundamental domain, even if there is a singularity that identifies several 
     * chords at that point.
     */
    ChordSpaceGroup.prototype.toChord = function(P, I, T, V, A, L, D, printme) {
        try {
            printme = typeof printme !== 'undefined' ? printme : false;
            P = Silencio.modulo(P, this.countP);
            I = Silencio.modulo(I, 2);
            T = Silencio.modulo(T, ChordSpace.OCTAVE);
            V = V % this.countV;
            if (printme) {
                console.info(sprintf('toChord:             %s %s %s %s', P, I, T, V));
            }
            let optti = this.optisForIndexes[P];
            if (printme) {
                console.info('toChord:   optti:    ' + optti);
            }
            let optt;
            if (I === 0) {
                optt = optti;
            } else {
                optt = optti.I()    ;
            }
            if (console.logme) {
                console.info('toChord:   optt:      ' + optt);
            }
            let optt_t = optt.T(T);
            if (printme) {
                console.info('toChord:   optt_t:    ' + optt_t);
            }
            let op = optt_t.eOP();
            if (printme) {
                console.info('toChord:   op:        ' + op);
            }
            V = Silencio.modulo(V, this.countV);
            let revoicing = ChordSpace.octavewiseRevoicing(op, V, this.range);
            if (printme) {
                console.info('toChord:   revoicing: ' + revoicing);
            }
            A = Silencio.modulo(A, this.countA);
            if (typeof this.instruments != 'undefined') {
                ChordSpace.arrange_instruments(revoicing, A, this.instruments);
            }
            L = Silencio.modulo(L, this.countL);
            if (typeof this.dynamics != 'undefined') {
                ChordSpace.arrange_dynamics(revoicing, L, this.dynamics);
            }
            D = Silencio.modulo(D, this.countD);
            if (typeof this.durations != 'undefined') {
                ChordSpace.arrange_durations(revoicing, D, this.durations);
            }
            return {'revoicing': revoicing, 'opti': optti, 'op': op};
        } catch (ex) {
            console.info(ex);
            throw ex;
        }
    };

    /**
     * Returns the indices of prime form, inversion, transposition,
     * and voicing for a chord in the group. Any of several chords 
     * that are identified in a singualar point of the fundamental domain will 
     * return the same P.
     */
    ChordSpaceGroup.prototype.fromChord = function(chord, printme) {
        printme = typeof(printme) !== 'undefined' ? printme : false;
        if (printme) {
            console.info('fromChord: chord:    ' + chord + ' ' + chord.iseOP());
        }
        let op;
        if (chord.iseOP()) {
            op = chord.clone();
        } else {
            op = chord.eOP();
        }
        if (printme) {
            console.info('fromChord: op:       ' + op);
        }
        let optt = chord.eOPTT();
        if (printme) {
            console.info('fromChord: optt:     ' + optt);
        }
        let T = 0;
        for (t = 0; t < ChordSpace.OCTAVE - 1; t = t + this.g) {
            let optt_t = optt.T(t).eOP();
            if (printme) {
                console.info('fromChord: optt_t:   ' + optt_t + ' T: ' + t);
            }
            if (optt_t.eq_epsilon(op) === true) {
                if (printme) {
                    console.info('equals');
                }
                T = t;
                break;
            }
        }
        let optti = chord.eOPTTI();
        optti.clamp(this.g);
        if (printme) {
            console.info('fromChord: optti:    ' + optti);
        }
        let P = this.indexesForOptis.get(optti.toString());
        if (typeof P === 'undefined') {
            throw "Undefined optti:" + optti;
        }
        let I = 0;
        let optt_i_optt;
        if (optti.eq_epsilon(optt) === false) {
            I = 1;
            optt_i_optt = optt.I().eOPTT();
            if (optt_i_optt.eq_epsilon(optti) === false) {
                console.error("Error: OPTT(I(OPTT)) must equal OPTTI.");
                console.error('optt_i_optt:' + optt_i_optt.information());
                console.error('optti:      ' + optti.information());
                process.exit();
            }
        }
        let voicing = ChordSpace.voiceleading(op, chord);
        // Possible alternative implementation: V = this.indexesForVoicings[voicing.toString()];
        let V = ChordSpace.indexForOctavewiseRevoicing(chord, this.range, printme);
        if (V === -1) {
            V = 0;
        }
        if (printme) {
            console.info('fromChord: voicing:  ' + voicing + ' ' + V);
            console.info('fromChord:           ' + P + ' ' + I + ' ' + T + ' ' + V);
        }
        // TODO: return {'P': P, 'I': I, 'T': T, 'V': V, 'A': A, 'L': L, 'D': D};
        return {'P': P, 'I': I, 'T': T, 'V': V};
    };

    ChordSpaceGroup.prototype.printChords = function() {
        for (let index = 0; index < this.optisForIndexes.length; index++) {
            let opti = this.optisForIndexes[index];
            let name = opti.name();
            console.info(sprintf('index: %5d  opti: %s %s', index, opti.toString(), name));
        }
    };

    ChordSpaceGroup.prototype.printNamedChords = function() {
        for (let index = 0; index < this.optisForIndexes.length; index++) {
            let opti = this.optisForIndexes[index];
            let name = opti.name();
            if (name !== '') {
                console.info(sprintf('index: %5d  opti: %s %s', index, opti.toString(), name));
            }
        }
    };

    /**
     * Returns the index of the octavewise revoicing that this chord is,
     * relative to its OP equivalent, within the indicated range. Returns
     * -1 if there is no such chord within the range.
     */
    ChordSpace.indexForOctavewiseRevoicing = function (chord, range, debug) {
        let revoicing_count = ChordSpace.octavewiseRevoicings(chord, range);
        let origin = chord.eOP();
        let revoicing = origin.clone();
        let revoicing_index = 0;
        while (true) {
            if (debug) {
                console.info(sprintf("indexForOctavewiseRevoicing of %s in range %7.3f: %5d of %5d: %s",
                    chord,
                    range,
                    revoicing_index,
                    revoicing_count,
                    revoicing));
            }
            if (revoicing.eq_epsilon(chord) === true) {
                return revoicing_index;
            }
            ChordSpace.next(revoicing, origin, range, ChordSpace.OCTAVE);
            revoicing_index++;
            if (revoicing_index > revoicing_count) {
                return -1;
            }
        }
    };

    ChordSpaceGroup.prototype.list = function(listheader, listopttis, listvoicings) {
        listheader = typeof listheader !== 'undefined' ? listheader : false;
        listopttis = typeof listopttis !== 'undefined' ? listopttis : false;
        listvoicings = typeof listvoicings !== 'undefined' ? listvoicings : false;
        if (listheader) {
            console.info(sprintf('ChordSpaceGroup.voices: %8d\n', this.voices));
            console.info(sprintf('ChordSpaceGroup.range : %8d\n', this.range));
            console.info(sprintf('ChordSpaceGroup.g     : %13.4f\n', this.g));
            console.info(sprintf('ChordSpaceGroup.countP: %8d\n', this.countP));
            console.info(sprintf('ChordSpaceGroup.countI: %8d\n', this.countI));
            console.info(sprintf('ChordSpaceGroup.countT: %8d\n', this.countT));
            console.info(sprintf('ChordSpaceGroup.countV: %8d\n', this.countV));
            console.info(sprintf('ChordSpaceGroup.countA: %8d\n', this.countA));
            console.info(sprintf('ChordSpaceGroup.countL: %8d\n', this.countL));
            console.info(sprintf('ChordSpaceGroup.countD: %8d\n', this.countD));
        }
        let index;
        let voicing_index;
        let opti;
        let voicingFromIndex;
        let indexFromVoicing;
        if (listopttis) {
            const iterator = this.indexesForOptis.entries();
            let result = iterator.next();
            while (!result.done) {
                key = result.value[0];
                index = result.value[1];
                opti = this.optisForIndexes[index];
                console.info(sprintf('index: %5d  opti: %s\n', index, opti.toString()));
                if (listvoicings) {
                    for (voicing_index = 0; voicing_index < this.countV; voicing_index++) {
                        voicingFromIndex = ChordSpace.octavewiseRevoicing(opti, voicing_index, this.range);
                        indexFromVoicing = ChordSpace.indexForOctavewiseRevoicing(voicingFromIndex, this.range);
                        console.info(sprintf('  voicing index: %5d  voicing: %s  index from voicing: %5d\n', voicing_index, voicingFromIndex, indexFromVoicing));
                    }
                }
                result = iterator.next();
            }
        }
    };

    /**
     * N is the number of voices in the chord space, g is the generator of
     * transposition, instruments is an array of instrument numbers from which
     * to select an arrangement, and range is the size of chord space.
     */
    ChordSpaceGroup.prototype.initialize = function(voices, range, instruments, dynamics, durations, g) {
        let began = performance.now();
        console.info("ChordSpaceGroup.prototype.initialize...");
        this.voices = typeof voices !== 'undefined' ? voices : 3;
        this.range = typeof range !== 'undefined' ? range : 60;
        this.instruments = typeof instruments !== 'undefined' ? instruments : [1];
        this.g = typeof g !== 'undefined' ? g : 1;
        if (typeof dynamics !== 'undefined') {
            this.dynamics = dynamics;
        } else {
            this.dynamics = [0, 1, 2, 3];
        };
        if (typeof durations !== 'undefined') {
            this.durations = durations;
        } else {
            // These are sixteenth notes (subdivisions of the nominal beat).
            this.durations = [0, 1/4, 2/4, 3/4, 1];
        };
        this.countP = 0;
        this.countI = 2;
        this.countT = ChordSpace.OCTAVE / this.g;
        let chord = new ChordSpace.Chord();
        chord.resize(voices);
        this.countV = ChordSpace.octavewiseRevoicings(chord, this.range);
        this.countA = Math.pow(this.instruments.length, this.voices);
        this.countL = Math.pow(this.dynamics.length, this.voices);
        this.countD = Math.pow(this.durations.length, this.voices);
        let result = ChordSpace.allOfEquivalenceClass(voices, 'OPTTI');
        this.optisForIndexes = result[0];
        // Remove duplicates from optisForIndexes and correct both collections accordingly.
        this.indexesForOptis = result[1];
        this.countP = this.optisForIndexes.length;
        let ended = performance.now();
        let elapsed = (ended - began) / 1000.;
        console.info("ChordSpaceGroup.prototype.initialize: " + elapsed);
    };
    
    /** 
     * Following https://madoshakalaka.github.io/2019/03/02/generalized-cross-product-for-high-dimensions.html, 
     * returns the unit normal vector to a hyperplane defined by n linearly 
     * indepenbdent points.
def generalized_cross_product(vectors):
    dim = len(vectors[0])
    product = []
    for j in range(dim):
        basis_vector = [0] * dim
        basis_vector[j] = 1
        print("basis_vector:", basis_vector)
        matrix = scipy.vstack([vectors, basis_vector])
        print("Matrix:", matrix)
        product.append(scipy.linalg.det(matrix))
    return product
    
     */
    ChordSpace.unit_normal_vector = function(independent_points) {
        // Subtract one of the points from each of the others to define 
        // n - 1 vectors.
        console.info("points:");
        for (point of independent_points) {
            console.info(point);
        }
        let subtrahend = independent_points[independent_points.length - 1];
        let n = subtrahend.length;
        let vectors = new Array(n);
        for (let i = 0; i < n - 1; i++) {
            let vector = numeric.sub(independent_points[i], subtrahend);
            vectors[i] = vector;
        }
        let cross_product = new Array();
        for (let j = 0; j < n; j++) {
            let basis_vector = new Array(n);
            basis_vector = basis_vector.fill(0);
            basis_vector[j] = 1;
            vectors[n - 1] = basis_vector;
            console.info("vectors:");
            console.info(vectors);
            let d = numeric.det(vectors);
            console.info("determinant:", d);
            cross_product.push(d);
        }
        magnitude = numeric.norm2(cross_product);
        let unit_normal = numeric.div(cross_product, magnitude);
        return cross_product;
    };

    //~ /**
     //~ * Precompute inversion flats for OPTI (as unit normal vectors) for 
     //~ * different chord spaces.
     //~ */
    //~ for (let n = 3; n <= 4; n++) {
        //~ console.info(sprintf("Computing inversion flat for %d voices...", n));
        //~ // First store the pitch-class sets for n linearly independent points 
        //~ // in each inversion flat, normalized to T. Then use the generalized 
        //~ // outer product to compute a normal vector to the flat.
        //~ let chords;
        //~ let chord;
        //~ let magnitude;
        //~ let opt;
        //~ if        (n === 3) {
            //~ chords = new Array()
            //~ chord = new Chord([0,  0,  6]);
            //~ chords.push(chord.eT());
            //~ chord = new Chord([0, 12, 12]);
            //~ chords.push(chord.eT());
            //~ chord = new Chord([0,  4,  8]);
            //~ chords.push(chord.eT());
            //~ ChordSpace.inversion_flats.set(n, chords);
        //~ } else if (n === 4) {
            //~ opt = ChordSpace.allOfEquivalenceClass(4, "OPT");
            //~ let count = 0;
            //~ for (const opt_chord of opt[3].entries()) {
                //~ count = count + 1;
                //~ console.info(sprintf("%3d: key: %s value: %s sum: %f isI: %s", count, opt_chord[0], opt_chord[1], opt_chord[1].sum(), opt_chord[1].iseI()))
            //~ }
            //~ chords = new Array()
            //~ chord = new Chord([ 0,   0,   0,   6  ]);
            //~ chords.push(chord.eT());
            //~ chord = new Chord([ 0,   0,   6,   6  ]);
            //~ chords.push(chord.eT());
            //~ chord = new Chord([ 0,   6,   6,  12  ]);
            //~ chords.push(chord.eT());
            //~ chord = new Chord([ 0,   1.5, 3,   1.5]);
            //~ chords.push(chord.eT());
            //~ ChordSpace.inversion_flats.set(n, chords);
        //~ } else if (n === 5) {
        //~ } else if (n === 6) {
        //~ } else if (n === 7) {
        //~ }
        //~ let points = new Array();
        //~ for (let i = 0; i < n; i++) {
            //~ points.push(chords[i].voices);
        //~ }
        //~ unit_normal = ChordSpace.unit_normal_vector(points);
        //~ console.info(sprintf("Found unit normal for inversion flat with %d voices:", n))
        //~ console.info(unit_normal);
        //~ ChordSpace.inversion_flat_normals.set(n, unit_normal);
    //~ }    

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
    else if (typeof window !== 'undefined') {
        window.ChordSpace = ChordSpace;
    } else {
        return ChordSpace;
    }

})();

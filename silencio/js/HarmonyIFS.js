/**
H A R M O N Y   I F S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License version 2.1.

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores as the attractors of iterated
function systems in a score space that has a harmony subspace in which time is
subdivided such that harmony is a linear progression of time.

Requires Silencio.js, ChordSpace.js, and tf.js (TensorFlow.js).

Dimensions used herein are: {t time, P set-class, I inversion in the origin, T
pitch-class transposition, k MIDI key, v MIDI velocity, i MIDI channel}.
*/

(function() {

var HarmonyIFS = {};

/**
 * Represents a point in a time line. The point consists of a homogeneous vector
 * with dimensions {t, P, I, T, k, v, i, 1}. At rendering time, the point will be
 * translated to that pitch which most closely matches the chord defined by P,
 * I, and T.
 */
HarmonyIFS.Point = function() {
    this.data = numeric.transpose([0, 0, 0, 0, 0, 0, 0, 1]);
};

HarmonyIFS.Point.prototype.clone = function() {
    let other = new HarmonyIFS.Point();
    other.data = this.data.clone();
    return other;
};

HarmonyIFS.Point.prototype.to_string = function() {
    let text = sprintf("Point:\n%s\n", this.data.toString(true));
    return text;
}

HarmonyIFS.Point.prototype.to_note(chord_space) {
    // ChordSpaceGroup.prototype.toChord = function(P, I, T, V, A, L, D, printme) {
    let chord = chord_space.toChord(this.P, this.I, this.T, 0, 0, 0, 0);
    let note = new Silencio.Event();
    note.status = 144;
    note.time = this.t;
    note.key = this.k;
    note.velocity = this.v;
    note.channel = this.i;
    note = chord_space.conformToChord(note, chord, false);
    return note;
}

/**
 * Represents an interpolation point for a fractal interpolation function in the
 * time-harmony subspace of the score space, with dimensions t, P, I, T, s_P,
 * s_I, and s_T.
 */
HarmonyIFS.InterpolationPoint = function(t, P, I, T, s_P, s_I, s_T) {
    this.data = [t, P, I, T, s_P, s_I, s_T];
    /**
     * Time.
     */
    Object.defineProperty(this, "t", {
        get: function() {
            return this.data[0];
        },
        set: function(value) {
            this.data[0] = value;
        }
    });
    /**
     * Set-class.
     */
    Object.defineProperty(this, "P", {
        get: function() {
            return this.data[1];
        },
        set: function(value) {
            this.data[1] = value;
        }
    });
    /**
     * Inversion in the origin.
     */
    Object.defineProperty(this, "I", {
        get: function() {
            return this.data[2];
        },
        set: function(value) {
            this.data[2] = value;
        }
    });
    /**
     * Transposition by semitone.
     */
    Object.defineProperty(this, "T", {
        get: function() {
            return this.data[3];
        },
        set: function(value) {
            this.data[3] = value;
        }
    });
    /**
     * Scaling factor in the range.
     */
    Object.defineProperty(this, "s_P", {
        get: function() {
            return this.data[4];
        },
        set: function(value) {
            this.data[4] = value;
        }
    });
    /**
     * Scaling factor in the range.
     */
    Object.defineProperty(this, "s_I", {
        get: function() {
            return this.data[5];
        },
        set: function(value) {
            this.data[5] = value;
        }
    });
    /**
     * Scaling factor in the range.
     */
    Object.defineProperty(this, "s_T", {
        get: function() {
            return this.data[6];
        },
        set: function(value) {
            this.data[6] = value;
        }
    });
};

HarmonyIFS.InterpolationPoint.prototype.clone = function() {
    let other = new HarmonyIFS.InterpolationPoint();
    other.data = this.slice(0);
    return other;
};

HarmonyIFS.InterpolationPoint.prototype.to_string = function() {
  let text = sprintf("InterpolationPoint:\nt: %9.4f P: %9.4f I: %9.4f T: %9.4f s_P: %9.4f
s_I: %9.4f s_T: %9.4f\n", this.data[0], this.data[1], this.data[2],
this.data[3], this.data[4], this.data[5], this.data[6]);
  return text;
}

/**
 * Initialize the ScoreAttractor for N voices in a range of MIDI keys for a
 * vector of I instrument numbers and a note duration in seconds. g is the
 * generator of transposition.
 */
HarmonyIFS.ScoreAttractor = function(voices_, range_, bass_, instruments_, dynamics_, durations_, tie_overlaps_, rescale_, g_) {
    if (typeof tie_overlaps_ == "undefined") {
        this.tie_overlaps = true;
    } else {
        this.tie_overlaps = tie_overlaps_;
    }
    if (typeof rescale_ == "undefined") {
        this.rescale = false;
    } else {
        this.rescale = rescale_;
    }
    if (typeof g_ == "undefined") {
        this.g = 1;
    } else {
        this.g = g_;
    }
    this.voices = voices_;
    this.range = range_;
    this.bass = bass_;
    this.instruments = instruments_;
    this.dynamics = dynamics_;
    this.durations = durations_;
    this.chord_space_group = ChordSpace.createChordSpaceGroup(this.voices, this.range, this.instruments, this.dynamics, this.durations);
    this.interpolation_points = [];
    this.hutchinson_operator = [];
    this.score_graph = [];
    this.score = new Silencio.Score();
};

/**
 * Adds an interpolation point to the Hutchinson operator. Once all of the
 * interpolation points have been added, the elements on dimensions t, P, I, T,
 * and s will be used to derive shear transformation matrices for a Hutchinson
 * operator.
 */
HarmonyIFS.ScoreAttractor.prototype.add_interpolation_point = function(t, P, I, T, s, segment) {
    let interolation_point = new HarmonyIFS.InterpolationPoint(t, P, I, T s);
    this.interpolation_points.push(interpolation_point)
    return interpolation_point;
};

/**
 * Interpolation points are sorted by time and the corresponding shear
 * transformations are computed, according to Polychronis Manousopoulos,
 * Vasileios Drakopoulos, and Theoharis Theoharis. “Curve Fitting by Fractal
 * Interpolation”. In: Transactions on Computational Science 1 (Jan. 2008), pp.
 * 85–103. doi: 10.1007/978-3-540-79299-4_4. Once this function has been called,
 * the non-shear elements of the transformation matrices may be modified in any
 * way as long as the Hutchinson operator remains contractive.
 */
HarmonyIFS.ScoreAttractor.prototype.initialize_hutchinson_operator = function() {
    this.interpolation.points.sort(function(a, b) { return a.data[0] < b.data[0];});
    let transformation = numeric.identity(8)
    let point_0 = this.interpolation_points[0];
    let point_N = this.interpolation_points[this.interpolation_points.length - 1];
    for (let i = 1; i < this.interpolaion_points.length; i++) {
        let point_i_1 = this.interpolations_points[i - 1];
        let point_i = this.interpolation_points[i];
        // t or time dimension.
        transformation[0][0] = (point_i.t - point_i_1.t) / (point_N.t - point_0.t);
        transformation[0][7] = (point_N.t * point_i_1.t - point_0.t * point_i.t) / (point_N.t - point_0.t);
        // P or set-class dimension.
        transformation[1][0] = (point_i.I - point_i_1.P) - point_i.s_P * (point_N.P - point_0.P)) / (point_N.t - point_0.t);
        transformation[1][1] = point_i.s_P;
        transformation[1][7] = ((point_N.t * point_i_1.P - point_0.t * point_i.P) - point_i.s_P * (point_i.t * point_0.P - point_0.t * point_N.P)) / (point_N.t - point_0.t);
        // I or inversion dimension.
        transformation[2][2] = (point_i.I - point_i_1.I) - point_i.s_I * (point_N.I - point_0.I)) / (point_N.t - point_0.t);
        transformation[2][3] = point_i.s_I;
        transformation[2][7] = ((point_N.t * point_i_1.I - point_0.t * point_i.I) - point_i.s_I * (point_i.t * point_0.I - point_0.t * point_N.I)) / (point_N.t - point_0.t);
        // T or transposition dimension.
        transformation[3][2] = (point_i.T - point_i_1.T) - point_i.s_T * (point_N.T - point_0.T)) / (point_N.t - point_0.t);
        transformation[3][4] = point_i.s_T;
        transformation[3][7] = ((point_N.t * point_i_1.T - point_0.t * point_i.T) - point_i.s_T * (point_i.t * point_0.T - point_0.t * point_N.T)) / (point_N.t - point_0.t);
    }
    this.hutchinson_operator.push(transformation);
}

HarmonyIFS.ScoreAttractor.prototype.remove_duplicate_notes = function() {
    csound.message(sprintf("Before removing duplicate notes: %6d\n", this.score.size()));
    let note_set = new Silencio.ValueSet(function(a) {return a.toString()});
    for (let i = 0; i < this.score.size(); i++) {
        note_set.add(this.score.data[i]);
    }
    this.score.clear();
    this.score.data = [...note_set.values()];
    csound.message(sprintf("After removing duplicate notes:  %6d\n", this.score.size()));
}

/**
 * Recursively computes the score graph, translates the points to notes, adds
 * them to the score, ties overlapping notes in the score, and rescales the
 * score.
 */
HarmonyIFS.ScoreAttractor.prototype.generate = function(depth) {
    csound.message("Generated " + this.score_graph.length + " points.\n");
    let iteration = 0;
    // This point should be within the bounds of the attractor.
    let initial_point = new HarmonyIFS.Point();
    this.iterate(this.depth, iteration, initial_point);
    if (this.rescale == true) {
        this.rescale_score_graph();
    }
    this.translate_score_attractor_to_score();
};

/**
 * Compute the score attractor.
 */
 HarmonyIFS.ScoreAttractor.prototype.iterate = function(depth, iteration, point) {
    iteration = iteration + 1;
    if (iteration >= depth) {
        let note = point.to_note();
        this.score.add(note);
        return;
    }
    for (let i = 0; i < hutchinson_operator.length; i++) {
        let transformation = this.hutchinson_operator[i];
        let new_point = numeric.dot(transformation, point);
        this.iterate(depth, iteration, new_point);
    }
};

/**
 * Process the initial score attractor to the final score.
 */
HarmonyIFS.ScoreAttractor

// Node: Export function
if (typeof module !== "undefined" && module.exports) {
    module.exports = HarmonyIFS;
}
// AMD/requirejs: Define the module
else if (typeof define === 'function' && define.amd) {
    define(function () {return HarmonyIFS;});
}
// Browser: Expose to window
else {
    window.HarmonyIFS = HarmonyIFS;
}

})();

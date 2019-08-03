/**
H A R M O N Y   I F S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License version 2.1.

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores by means of iterated function
systems in a score space that has a harmony subspace in which time is subdivided
such that harmony is a linear progression of time.

Requires Silencio.js, ChordSpace.js, and tf.js (TensorFlow.js).

Dimensions used herein are: {t time, P set-class, I inversion in the origin, T
pitch-class transposition, MIDI key, MIDI velocity, MIDI channel}.
*/

(function() {

var HarmonyIFS = {};

/**
 * Represents a point in a time line. The point consists of a homogeneous vector
 * with dimensions {t, P, I, T, k, v, i}. The point will be translated to a
 * note whose pitch matches the chord defined by P, I, and T.
 */
HarmonyIFS.Point = function() {
    this.data = tf.tensor1d([0, 0, 0, 0, 0, 0, 0, 1 ], 'float32');
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

/**
 * Represents an interpolation point for a fractal interpolation function,
 * with elements t, P, I, T, and s.
 */
HarmonyIFS.InterpolationPoint = function(t, P, I, T, s) {
    this.t = t;
    this.P = P;
    this.I = I;
    this.T = T;
    this.s = s;
};

HarmonyIFS.InterpolationPoint.prototype.clone = function() {
    let other = new HarmonyIFS.InterpolationPoint();
    other.t = this.t;
    other.P = this.P;
    other.I = this.I;
    other.T = this.T;
    other.s = this.s;
    return other;
};

HarmonyIFS.Point.InterpolationPoint.to_string = function() {
    let text = sprintf("InterpolationPoint:\nt: %9.4f P: %9.4f I: %9.4f T: %9.4f s: %9.4f\n", this.t, this.P, this.I, this.T, this.s);
    return text;
}

/**
 * Initialize the ScoreAttractor for N voices in a range of MIDI keys for a
 * vector of I instrument numbers and a note duration in seconds. g is the
 * generator of transposition.
 */
HarmonyIFS.ScoreAttractor = function(voices_, range_, bass_, instruments_, note_duration_, tie_overlaps_, rescale_, g_) {
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
    this.note_duration = note_duration_;
    this.interpolation_points = new Map();
    this.hutchinson_operator = new Map();
    this.score_graph = [];
    this.score = new Silencio.Score();
};

/**
 * Adds an interpolation point at a depth and segment to the Hutchinson
 * operator. Once all of the interpolation points have been added, The
 * parameters t, P, I, T, and s will be used to derive shear transformation
 * matrices for a Hutchinson operator. The transformation matrices may be
 * further modified if the shear elements are not changed.
 */
HarmonyIFS.ScoreAttractor.prototype.add_interpolation_point = function(t, P, I, T, s, depth, segment) {
    let points = this.interpolation_points.get(depth);
    if (typeof transformations === 'undefined') {
        points = [];
        this.interpolation_points[depth] = transformations;
    }
    return transformation;
};

/**
 * For each interpolation point, computes a corresponding shear transformation,
 * according to Polychronis Manousopoulos, Vasileios Drakopoulos, and Theoharis
 * Theoharis. “Curve Fitting by Fractal Interpolation”. In: Transactions on
 * Computational Science 1 (Jan. 2008), pp. 85–103. doi:
 * 10.1007/978-3-540-79299-4_4.
 */
HarmonyIFS.ScoreAttractor.prototype.initialize_hutchinson_operator = function() {

}

HarmonyIFS.ScoreAttractor.prototype.get_transformation = function(depth, segment) {
    let transformations = this.hutchinson_operator.get(depth);
    if (typeof transformations === 'undefined') {
        transformations = [];
        this.interpolation_points[depth] = transformations;
    }
    return transformation;

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
HarmonyIFS.ScoreAttractor.prototype.generate = function(depth, time_steps, duration_scale_factor) {
    // Sort the transformations in the operator by time.
    if (typeof duration_scale_factor == 'undefined') {
        this.duration_scale_factor = 1;
    } else {
        this.duration_scale_factor = duration_scale_factor;
    }
    let n = this.hutchinson_operator.size();
    for (let d = 0; d < n; d++) {
        let transformations = this.hutchinson_operator[d];
        transformations.sort(function(a, b){ return a.X - b.X});
    }
    this.time_0 = this.hutchinson_operator[0].X;
    this.time_N = this.hutchinson_operator[this.hutchinson_operator.length - 1].X;
    let interval = this.time_N - this.time_0;
    this.time_step = interval / time_steps;
    this.score_graph.length = 0;
    this.score.clear();
    // Map the operator over the time interval. This is similar to computing the
    // attractor of an IFS with the deterministic algorithm.
    for (let i = 0; i < time_steps; i++) {
        iteration = 0;
        let point = new ScoreGraphs.Point();
        point.time = this.time_0 + i * this.time_step;
        this.iterate(depth, iteration, point);
    }
    csound.message("Generated " + this.score_graph.length + " points.\n");
    if (this.rescale == true) {
        this.rescale_score_graph();
    }
    this.translate_score_graph_to_score();
};

/**
 * Compute the score graph. At each iteration, the domain is re-partitioned
 * and the ranges of the sub-domains are computed.
 *
 * TODO: Add characteristic
 * function to implement **local** iterated function systems.
 */
 HarmonyIFS.ScoreAttractor.prototype.iterate = function(depth, iteration, point_) {
    iteration = iteration + 1;
    if (iteration >= depth) {
        this.score_graph.push(point_.clone());
        return;
    }
    // Look up transformations by depth and by slot. If there are no transformations at a depth, use the prior depth.
    for (let data_point_index = 1; data_point_index < this.hutchinson_operator.length; data_point_index++) {
        let transformation = this.hutchinson_operator[data_point_index];
        let point = transformation.apply(this.hutchinson_operator, data_point_index, point_);
        this.iterate(depth, iteration, point);
    }
};

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

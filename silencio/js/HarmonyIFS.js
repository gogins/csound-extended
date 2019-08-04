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
        transformation[1][0] = (point_i.P - point_i_1.P) - point_i.s_P * (point_N.P - point_0.P) / (point_N.t - point_0.t);
        transformation[1][1] = point_i.s_P;
        transformation[1][7] = (point_N.t * point_i_1.P - point_0.t * point_i.P) - point_i.s_P * (point_i.t * point_0.P 
        // I or inversion dimension.
        // T or transposition dimension.



    }

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

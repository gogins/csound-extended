/**
S C O R E   G R A P H S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores by means of score graphs, which
graph a sequence of points in a chord symmetry space as a function of time,
using a fractal approximation that acts like a Read-Bajraktarevic operator. Any
callable with the correct interface can be used as a transformation. Time is
always subdivided, never folded.

Requires Silencio.js and ChordSpace2.js.

*/

(function() {

var ScoreGraphs = {};

/**
 * Represents a point in a discrete orbifold of chord symmetries. The dimensions
 * are P, I, T, V, A, or, set-class (OPTIC for N voices, clock group), inversion
 * in the origin (I, clock group), transposition within a range (T, clock
 * group), permutation of octavewise revoicings within a range (V, clock group),
 * and k-permutation with repetition of instrument numbers for N voices (A,
 * clock group).
 */
ScoreGraphs.Point = function() {
    this.time = 0;
    this.data = [0, 0, 0, 0, 0];
    Object.defineProperty(this, "P", {
        get: function() {
            return this.data[0];
        },
        set: function(value) {
            this.data[0] = value;
        }
    });
    Object.defineProperty(this, "I", {
        get: function() {
            return this.data[1];
        },
        set: function(value) {
            this.data[1] = value;
        }
    });
    Object.defineProperty(this, "T", {
        get: function() {
            return this.data[2];
        },
        set: function(value) {
            this.data[2] = value;
        }
    });
    Object.defineProperty(this, "V", {
        get: function() {
            return this.data[3];
        },
        set: function(value) {
            this.data[3] = value;
        }
    });
    Object.defineProperty(this, "A", {
        get: function() {
            return this.data[4];
        },
        set: function(value) {
            this.data[4] = value;
        }
    });
};

ScoreGraphs.Point.prototype.clone = function() {
    let other = new ScoreGraphs.Point();
    other.time = this.time;
    other.data = [...this.data];
    return other;
};

/**
 * A simple standard transformation. Note that X is a scalar for time and both Y
 * and the scaling factors s are vector values with one element for each of {P,
 * I, T, V, A}. All transformations must have the X and Y properties.
 */
ScoreGraphs.BilinearTransformation = function(X, Y, s) {
    this.X = X;
    this.Y = Y;
    this.s = s;
};

/**
 * Implements Equation 6.5 of "Bilinear Fractal Interpolation and Box Dimension"
 * by Barnsley and Massopust (2013). Note that x is time and y is location in
 * the range. The IFS transformations are calculated from the interpolation
 * points of the score graph. In theory, these points could be the chords of an
 * existing piece of music.
 */
ScoreGraphs.BilinearTransformation.prototype.apply = function(hutchinson_operator, n, point) {
    let x = point.time;
    let y = point.data;
    let N = hutchinson_operator.length;
    let X_0 = hutchinson_operator[0].X;
    let Y_0 = hutchinson_operator[0].Y;
    let s_0 = hutchinson_operator[0].s;
    let X_N = hutchinson_operator[N - 1].X;
    let Y_N = hutchinson_operator[N - 1].Y;
    let s_N = hutchinson_operator[N - 1].s;
    let X_n = hutchinson_operator[n].X;
    let Y_n = hutchinson_operator[n].Y;
    let s_n = hutchinson_operator[n].s;
    if (n == 0) {
        var X_n_prior = 0.;
        var Y_n_prior = [0., 0., 0., 0., 0.];
        var s_n_prior = [0., 0., 0., 0., 0.];
    } else {
        var X_n_prior = hutchinson_operator[n - 1].X;
        var Y_n_prior = hutchinson_operator[n - 1].Y;
        var s_n_prior = hutchinson_operator[n - 1].s;
    }
    let new_point = point.clone();
    new_point.time = X_n_prior + (((X_n - X_n_prior) / (X_N - X_0)) * (x - X_0));
    csound.message("  new_point.time: " + new_point.time + "\n")
    for (let i = 0; i < 5; i++) {
        let y_i = Y_n_prior[i] + ((Y_n[i] - Y_n_prior[i]) / (X_N - X_0)) * (x - X_0) + (s_n_prior[i] + ((s_n[i] - s_n_prior[i]) / (X_N - X_0))) * (y[i] - Y_0[i] - ((Y_N[i] - Y_0[i]) / (X_N - X_0)) * (x - X_0));
        new_point.data[i] = y_i;
    }
    return new_point;
};

/**
 * Initialize the ScoreGroup for N voices in a range of MIDI keys for a vector
 * of I instrument numbers and a total duration of the score in seconds. g is
 * the generator of transposition.
 */
ScoreGraphs.ScoreGraph = function(voices_, range_, bass_, instruments_, duration_, g_) {
    if (typeof g_ == "undefined") {
        this.g = 1;
    } else {
        this.g = g_;
    }
    this.voices = voices_;
    this.range = range_;
    this.bass = bass_;
    this.instruments = instruments_;
    this.duration = duration_;
    this.chord_space = new ChordSpace.ChordSpaceGroup();
    this.chord_space.initialize(this.voices, this.range, this.g);
    this.hutchinson_operator = [];
    this.score_graph = [];
    this.score = new Silencio.Score();
};

/**
 * Adds a transformation (the callable) to the Hutchinson operator for this
 * score graph. The callable signature takes a point and returns a new point and
 * has the following signature:
 * point = callable(hutchinson_operator, transformation_index, time, point)
 * Note also that the callable must have a scalar X property for time and a
 * vector Y property for values for {P, I, T, V, A}.
 */
ScoreGraphs.ScoreGraph.prototype.add_transformation = function(callable) {
    this.hutchinson_operator.push(callable);
};

/**
 * Rescales the computed score graph to fit just within the hypercube defined by
 * the minima and maxima of {P, I, T, V, A}. Time is not affected.
 */
ScoreGraphs.ScoreGraph.prototype.rescale_score_graph = function() {
    let minima = [...this.score_graph[0].data];
    let maxima = [...minima];
    let actual_ranges = [0,0,0,0,0];
    let target_ranges = [0,0,0,0,0];
    let scale_factors = [000,0,0,0];
    target_ranges[0] = this.chord_space.countP;
    target_ranges[1] = this.chord_space.countI;
    target_ranges[2] = this.chord_space.countT;
    target_ranges[3] = this.chord_space.countV;
    target_ranges[4] = this.chord_space.countA;
    // Find the existing minima and maxima.
    for (let i = 0; i < this.score_graph.length; i++) {
        let point = this.score_graph[i];
        for (let j = 0; j < 5; j++) {
            if (minima[j] > point.data[j]) {
                minima[j] = point.data[j];
            }
            if (maxima[j] < point.data[j]) {
                maxima[j] = point.data[j];
            }
        }
    }
    for (i = 0; i < 5; i++) {
        actual_ranges[i] = maxima[i] - minima[i];
        if(actual_ranges[i] == 0.0) {
            scale_factors[i] = 1;
        } else {
            scale_factors[i] = target_ranges[i] / actual_ranges[i];
        }
    }
    // Rescale the score graph to the to minima and maxima of {P, I, T, V, A}.
    for (let i = 0; i < this.score_graph.length; i++) {
        let point = this.score_graph[i];
        for (let j = 0; j < 5; j++) {
            // Move to origin (here, the target minimum is always 0):
            point.data[j] = point.data[j] - minima[j];
            // Rescale: value = value * target_range / actual_range.
            // In order to generate chords from the chord space group, the
            // values must be integers.
            point.data[j] = Math.round(point.data[j] * scale_factors[j]);
        }
    }
};

ScoreGraphs.ScoreGraph.prototype.translate_score_graph_to_score = function() {
    for (let i = 0; i < this.score_graph.length; i++) {
        let point = this.score_graph[i];
        let chord = this.chord_space.toChord(point.P, point.I, point.T, point.V, point.A).revoicing;
        chord.setDuration(this.time_step);
        // A nominal velocity so that tieing overlaps will work.
        chord.setVelocity(80);
        ChordSpace.insert(this.score, chord, point.time);
    }
    this.score.tieOverlaps();
    for (let i = 0; i < this.score.size(); i++) {
        let note = this.score.data[i];
        let key = note.key;
        note.key = this.bass + key;
        let channel = note.channel;
        note.channel = 1 + channel;
    }
    this.score.setDuration(this.duration);
    if (true) {
        console.log(this.score.toCsoundScore());
    }
}

/**
 * Recursively computes the score graph, translates the points to chords,
 * translates the chords to notes, adds them to the score, ties overlapping
 * notes in the score, and rescales the score.
 */
ScoreGraphs.ScoreGraph.prototype.generate = function(depth, time_steps) {
    // Sort the transformations in the operator by time.
    this.hutchinson_operator.sort(function(a, b){ return a.X - b.X});
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
        csound.message("point.time: " + point.time + "\n");
        this.iterate(depth, iteration, point);
    }
    // Rescale the score graph to the bounds of {P, I, T, V, A}.
    this.rescale_score_graph();
    // Translate each Point in the score graph to a Chord and insert it into the
    this.translate_score_graph_to_score();
};

/**
 * Compute the score graph as a fractal approximation of the graph of {P, I, T,
 * V, A} as a function of time. At each iteration, the domain is re-partitioned
 * and the ranges of the sub-domains are contracted.
 *
 * TODO: Add characteristic function to implement **local** iterated function
 * systems.
 */
 ScoreGraphs.ScoreGraph.prototype.iterate = function(depth, iteration, point_) {
    iteration = iteration + 1;
    if (iteration >= depth) {
        this.score_graph.push(point);
    } else {
        for (let transformation_i = 0; transformation_i < this.hutchinson_operator.length; transformation_i++) {
            let transformation = this.hutchinson_operator[transformation_i];
            point = transformation.apply(this.hutchinson_operator, transformation_i, point_);
            this.iterate(depth, iteration, point);
        }
    }
};

// Node: Export function
if (typeof module !== "undefined" && module.exports) {
    module.exports = ScoreGraphs;
}
// AMD/requirejs: Define the module
else if (typeof define === 'function' && define.amd) {
    define(function () {return ScoreGraphs;});
}
// Browser: Expose to window
else {
    window.ScoreGraphs = ScoreGraphs;
}

})();

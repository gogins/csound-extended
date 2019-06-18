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
    other.data = [...this.data];
    return other;
};

/**
 * A simple standard transformation. Note that a, b, c, and d are vector-valued
 * with one element for each of P, I, T, V, and A.
 */
ScoreGraphs.BilinearTransformation = function(a, b, c, d) {
    this.a = a;
    this.b = b;
    this.c = c;
    this.d = d;
};

ScoreGraphs.BilinearTransformation.prototype.apply = function(transformations, time, point) {
    let new_point = point.clone();
    for (let i = 0; i < 5; i++) {
        let y = this.a[i] + this.b[i] * time + this.c[i] * point.data[i] + this.d[i] * time * point.data[i];
        new_point.data[i] = y;
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
    this.hutchinson_operator = new Map();
    this.score_graph = new Map();
    this.score = new Silencio.Score();
};

/**
 * Adds a transformation (the callable) to the Hutchinson operator for this
 * score graph. The callable takes a Point and returns a Point. The
 * transformation should be contractive. There are no other restrictions.
 */
ScoreGraphs.ScoreGraph.prototype.add_transformation = function(time, callable) {
    this.hutchinson_operator.set(time, callable);
};

ScoreGraphs.ScoreGraph.prototype.rescale_score_graph = function() {

};

ScoreGraphs.ScoreGraph.prototype.translate_score_graph_to_score = function() {
    for (let [time, point] of this.score_graph) {
        let chord = this.chord_space.toChord(point.P, point.I, point.T, point.V, point.A).revoicing;
        chord.setDuration(this.time_step);
        ChordSpace.insert(this.score, chord, time);
    }
    this.score.tieOverlaps();
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
    this.hutchinson_operator = new Map([...this.hutchinson_operator.entries()].sort());
    this.transformations = Array.from(this.hutchinson_operator.entries());
    this.time_0 = this.transformations[0][0];
    this.time_N = this.transformations[this.hutchinson_operator.size - 1][0];
    let interval = this.time_N - this.time_0;
    this.time_step = interval / time_steps;
    this.score_graph.clear();
    this.score.clear();
    // Map the operator over the time interval. This is similar to computing the
    // attractor of an IFS with the deterministic algorithm.
    for (let i = 0; i < time_steps; i++) {
        iteration = 0;
        let time = this.time_0 + i * this.time_step;
        let point = new ScoreGraphs.Point();
        this.iterate(depth, iteration, time, point);
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
 ScoreGraphs.ScoreGraph.prototype.iterate = function(depth, iteration, time, point) {
    iteration = iteration + 1;
    if (iteration >= depth) {
        this.score_graph.set(time, point);
    } else {
        // Subdivide the domain (time) and contract the range (the chord
        // symmetry space) of each sub-domain.
        let time_prior_i = this.time_0;
        for (let [time_i, transformation] of this.hutchinson_operator) {
            time = time_prior_i + ((time_i - time_prior_i) / (this.time_N - this.time_0)) * (time - this.time_0);
            time_prior_i = time_i;
            // The entire Hutchinson operator is passed into the invocation of
            // each transformation that belongs to the operator. This is
            // necessary for properly subdividing and rescaling the domain and
            // range for each transformation.
            point = transformation.apply(this.transformations, time, point);
            // Recurse...
            this.iterate(depth, iteration, time, point);
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

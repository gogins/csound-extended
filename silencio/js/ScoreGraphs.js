/**
S C O R E   G R A P H S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores by means of score graphs, which
graph a sequence of points in a chord symmetry space as a function of time,
using a fractal approximation. Any callable with the correct interface can be
used as a transformation. Time is always subdivided, never folded.

Requires Silencio.js and ChordSpace.js.

*/

(function() {

var ScoreGraphs = {};

/**
 * Represents a point in a discrete orbifold of chord symmetries. The dimensions
 * are P, I, T, V, A.
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
    other = new Point();
    other.data = this.data.slice(0);
}

// A simple standard transformation. Note that a, b, c, and d are vector-valued
// with one element for each of P, I, T, V, and A.

ScoreGraphs.BilinearTransformation = function(a, b, c, d) {
    this.a = a;
    this.b = b;
    this.c = c;
    this.d = d;
};

ScoreGraphs.BilinearTransformation.prototype.apply(time, point) {
    new_point = point.clone();
    for (var i = 0; i < 5; i++) {
        var y = this.a[i] + this.b[i] * t + this.c[i] * point.data[i] + this.d[i] * t * point.data[i];
        new_point.data[i] = y;
    }
    return new_point;
};

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
    this.chord_space = ChordSpace.ChordSpaceGroup();
    this.chord_space.initialize(this.voices, this.range, this.g);
    this.score = Silencio.Score();
    this.hutchinson_operator = new Map();
}

/**
 * Adds a transformation (the callable) to the Hutchinson operator for this
 * score graph. The callable takes a Point and returns a Point. The
 * transformation should be contractive. There are no other restrictions.
 */
ScoreGraphs.ScoreGraph.prototype.add_transformation = function(time, callable) {
    this.hutchinson_operator.set(time, callable);
}

/**
 * Recursively computes the score graph, translates the points to chords,
 * translates the chords to notes, adds them to the score, ties overlapping
 * notes in the score, and rescales the score.
 */
ScoreGraphs.ScoreGraph.prototype.generate = function(depth, time_steps) {
    // Sort the transformations in the operator by time.
    this.hutchinson_operator = new Map([...this.hutchinson_operator.entries()].sort());
    this.time_0 = this.hutchinson_operator.values()[0];
    this.time_N = this.hutchinson_operator.values()[this.hutchinson_operator.size() - 1];
    this.interval = time_N - time_0;
    var time_step = interval / this.time_steps;
    // Map the operator over the time interval.
    for (var i = 0; i < this.time_steps; i++) {
        iteration = 0;
        var time = time_1 + i * time_step;
        var point = new Point();
        this.iterate(depth, iteration, time, point);
    }
    this.score.tieOverlaps();
}

// TODO: Add characteristic function to implement local iterated function
// systems.

ScoreGraphs.ScoreGraph.prototype.iterate = function(depth, iteration, time, point) {
    iteration++;
    if (iteration >== depth) {
        // Translate the particular value of chord symmetry to an actual chord,
        // which includes octavewise revoicing and arrangement.
        var chord = this.chord_space.toChord(point.P, point.I, point.T, point.V, point.A);
        // Insert the notes from the chord into the score at this particular time.
        chord.setDuration(this.time_step);
        ChordSpace.insert(this.score, time, chord);
    } else {
        // Subdivide the domain (time) and contract the range (the chord
        // symmetry space).
        var time_prior_i = time_0;
        for (var [time_i, transformation] of this.hutchinson_operator) {
            time = time_prior_i + ((time_i - time_prior_i) / (time_N - time_0)) * (time - time_0);
            time_prior_i = time_i;
            point = transformation.apply(time, point);
            // Recurse...
            this.iterate(depth, iteration, time, point);
        }
    }
}

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

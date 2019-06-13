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

}

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
    var time_1 = this.hutchinson_operator.values()[0];
    var time_N = this.hutchinson_operator.values()[this.hutchinson_operator.size() - 1];
    var interval = time_N - time_1;
    var time_step = interval / this.time_steps;
    // Map the operator over the time interval.
    for (var i = 0; i < this.time_steps; i++) {
        iteration = 0;
        var time = time_1 + i * time_step;
        var point = new Point();
        this.iterate(depth, iteration, time, point, this.hutchinson_operator, this.chord_space, this.score);
    }
    this.score.tieOverlaps();
}

ScoreGraphs.ScoreGraph.prototype.iterate = function(depth, iteration, time, point, hutchinson_operator, chord_space, score) {
    iteration++;
    if (iteration >== depth) {
        var chord = chord_space.toChord(point.P, point.I, point.T, point.V);
        ChordSpace.insert(score, time, chord);
    } else {
        for (var [time, transformation] of hutchinson_operator) {

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

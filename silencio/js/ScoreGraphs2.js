/**
S C O R E   G R A P H S  2

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores by means of score graphs, which
graph a sequence of points as a function of time, using a fractal approximation
that acts like a Read-Bajraktarevic operator. Any callable with the correct
interface can be used as a transformation. Time is always subdivided, never
folded. The points actually consist of a note with an  associated chord.

Requires Silencio.js and ChordSpace.js.

*/

(function() {

var ScoreGraphs2 = {};

/**
 * Represents a point in a time line. The point consists of a note with an
 * associated chord.
 */
ScoreGraphs2.Point = function() {
    this.note = new Silencio.Event();
    this.chord = new ChordSpace.Chord();
};

ScoreGraphs2.Point.prototype.clone = function() {
    let other = new ScoreGraphs2.Point();
    other.note = this.note.clone();
    other.chord = this.chord.clone();
    return other;
};

ScoreGraphs.Point2.prototype.to_string = function() {
    let text = sprintf("Point: note: %s\nchord: %s\n\n", this.event.toString(), this.chord.information());
    return text;
}

/**
 * Initialize the ScoreGraph for N voices in a range of MIDI keys for a vector
 * of I instrument numbers and a total duration of the score in seconds. g is
 * the generator of transposition.
 */
ScoreGraphs2.ScoreGraph = function(voices_, range_, bass_, instruments_, duration_, tie_overlaps_, rescale_, g_) {
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
    this.duration = duration_;
    this.hutchinson_operator = new Map();
    this.score_graph = [];
    this.score = new Silencio.Score();
};

/**
 * Adds a transformation at a depth and segment to the Hutchinson operator. The
 * transformation has an apply function that takes a Point and returns a new
 * Point and has the following signature: point = apply(hutchinson_operator,
 * depth, iteration, begin_time, end_time, point). In addition, the
 * transformation has a scalar X property that represents time.
 */
ScoreGraphs.ScoreGraph.prototype.add_transformation = function(transformation, depth, segment) {
    let transformations = this.hutchinson_operator.get(depth);
    if (typeof transformations === 'undefined') {
        transformations = [];
        this.hutchinson_operator[depth] = transformations;
    }
    this.hutchinson_operator[iteration][segment] = transformation;
};

ScoreGraphs.ScoreGraph.prototype.remove_duplicate_notes = function() {
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
ScoreGraphs.ScoreGraph.prototype.generate = function(depth, time_steps, duration_scale_factor) {
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
 ScoreGraphs.ScoreGraph.prototype.iterate = function(depth, iteration, point_) {
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

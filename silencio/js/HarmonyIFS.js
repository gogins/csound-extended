/**
H A R M O N Y   I F S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License version 2.1.

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements the generation of scores as the attractors of iterated
function systems in a score space that has a harmony subspace in which time is
subdivided such that harmony is a linear progression of time.

Requires Silencio.js, ChordSpace.js, and numeric.js.

Dimensions used herein are: {t time, P set-class, I inversion in the origin, T
pitch-class transposition, k MIDI key, v MIDI velocity, i MIDI channel}.
*/

(function() {

var HarmonyIFS = {};

/**
 * Represents a point in a time line. The point consists of a homogeneous column
 * vector with dimensions {t, P, I, T, k, v, i, 1}. At rendering time, the point
 * will be translated to that pitch which most closely matches the chord defined
 * by P, I, and T.
 */
HarmonyIFS.Point = function() {
    // It seems a vector in numeric.js is treated as a row vector.
    this.data = [0, 0, 0, 0, 0, 0, 0, 1];
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
     * Index of set-class.
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
     * Index of inversion in the origin.
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
     * Index of transposition by semitone.
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
     * Pitch as MIDI key number.
     */
    Object.defineProperty(this, "k", {
        get: function() {
            return this.data[4];
        },
        set: function(value) {
            this.data[4] = value;
        }
    });
    /**
     * Loudness as MIDI velocity.
     */
    Object.defineProperty(this, "v", {
        get: function() {
            return this.data[5];
        },
        set: function(value) {
            this.data[5] = value;
        }
    });
    /**
     * Instrument as MIDI channel.
     */
    Object.defineProperty(this, "i", {
        get: function() {
            return this.data[6];
        },
        set: function(value) {
            this.data[6] = value;
        }
    });
};

HarmonyIFS.Point.prototype.clone = function() {
    let other = new HarmonyIFS.Point();
    other.data = this.data.clone();
    return other;
};

HarmonyIFS.Point.prototype.to_string = function() {
    let text = sprintf("Point:\n%s\n", this.data.toString(true));
    return text;
};

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
  let text = sprintf("InterpolationPoint:\nt: %9.4f P: %9.4f I: %9.4f T: %9.4f s_P: %9.4f s_I: %9.4f s_T: %9.4f\n", this.data[0], this.data[1], this.data[2], this.data[3], this.data[4], this.data[5], this.data[6]);
  return text;
};

/**
 * Initialize the ScoreAttractor for N voices in a range of MIDI keys for a
 * vector of I instrument numbers and a note duration in seconds. g is the
 * generator of transposition.
 */
HarmonyIFS.ScoreAttractor = function(voices_, range_, bass_, note_duration_, tie_overlaps_, rescale_, g_) {
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
    this.chord_space_group = ChordSpace.createChordSpaceGroup(this.voices, this.range);
    this.interpolation_points = [];
    this.hutchinson_operator = [];
    this.note_duration = note_duration_;
    this.score = new Silencio.Score();
};

/**
 * Adds an interpolation point to the Hutchinson operator. Once all of the
 * interpolation points have been added, the elements on dimensions t, P, I, T,
 * and s will be used to derive shear transformation matrices for a Hutchinson
 * operator.
 */
HarmonyIFS.ScoreAttractor.prototype.add_interpolation_point = function(t, P, I, T, s_P, s_I, s_T) {
    let interpolation_point = new HarmonyIFS.InterpolationPoint(t, P, I, T, s_P, s_I, s_T);
    this.interpolation_points.push(interpolation_point);
    return interpolation_point;
};

/**
 * Interpolation points are sorted by time and the corresponding shear
 * transformations for a Hutchinson operator are computed, according to
 * Polychronis Manousopoulos, Vasileios Drakopoulos, and Theoharis Theoharis.
 * “Curve Fitting by Fractal Interpolation”. In: Transactions on Computational
 * Science 1 (Jan. 2008), pp. 85–103. doi: 10.1007/978-3-540-79299-4_4. Once
 * this function has been called, the non-shear elements of the transformation
 * matrices may be modified in any way as long as the Hutchinson operator
 * remains contractive.
 */
HarmonyIFS.ScoreAttractor.prototype.initialize_hutchinson_operator = function() {
    this.interpolation_points.sort(function(a, b) { return a.data[0] - b.data[0];});
    let p_0 = this.interpolation_points[0];
    let p_N = this.interpolation_points[this.interpolation_points.length - 1];
    this.hutchinson_operator.length = 0;
    for (let i = 1; i < this.interpolation_points.length; i++) {
        let p_i_1 = this.interpolation_points[i - 1];
        let p_i = this.interpolation_points[i];
        let transformation = numeric.identity(8)
        // t or time dimension.
        transformation[0][0] = (p_i.t - p_i_1.t) / (p_N.t - p_0.t);
        transformation[0][7] = ((p_N.t * p_i_1.t) - (p_0.t * p_i.t)) / (p_N.t - p_0.t);
        // P or set-class dimension.
        transformation[1][0] = ((p_i.P - p_i_1.P) / (p_N.t - p_0.t)) - (p_i.s_P * ((p_N.P - p_0.P) / (p_N.t - p_0.t)));
        transformation[1][1] = p_i.s_P;
        transformation[1][7] = (((p_N.t * p_i_1.P) - (p_0.t * p_i.P)) / (p_N.t - p_0.t)) - (p_i.s_P * (((p_i.t * p_0.P) - (p_0.t * p_N.P)) / (p_N.t - p_0.t)));
        // I or inversion dimension.
        transformation[2][0] = ((p_i.I - p_i_1.I) / (p_N.t - p_0.t)) - (p_i.s_I * ((p_N.I - p_0.I) / (p_N.t - p_0.t)));
        transformation[2][2] = p_i.s_I;
        transformation[2][7] = (((p_N.t * p_i_1.I) - (p_0.t * p_i.I)) / (p_N.t - p_0.t)) - (p_i.s_I * (((p_i.t * p_0.I) - (p_0.t * p_N.I)) / (p_N.t - p_0.t)));
        // T or transposition dimension.
        transformation[3][0] = ((p_i.T - p_i_1.T) / (p_N.t - p_0.t)) - (p_i.s_T * ((p_N.T - p_0.T) / (p_N.t - p_0.t)));
        transformation[3][3] = p_i.s_T;
        transformation[3][7] = (((p_N.t * p_i_1.T) - (p_0.t * p_i.T)) / (p_N.t - p_0.t)) - (p_i.s_T * (((p_i.t * p_0.T) - (p_0.t * p_N.T)) / (p_N.t - p_0.t)));
        this.hutchinson_operator.push(transformation);
    }
}

HarmonyIFS.ScoreAttractor.prototype.point_to_note = function(point) {
    let note = new Silencio.Event();
    note.status = 144;
    note.time = point[0];
    note.duration = this.note_duration;
    let P = Math.round(point[1]);
    let I = Math.round(point[2]);
    let T = Math.round(point[3]);
    note.chord = this.chord_space_group.toChord(P, I, T, 0, 0, 0, 0).op;
    note.key = point[4];
    note.velocity = point[5];
    note.channel = point[6];
    return note;
}

HarmonyIFS.ScoreAttractor.prototype.remove_duplicate_notes = function() {
    let make_key = function(a) {
        return a.toString();
    };
    console.info(sprintf("Before removing duplicate notes: %6d\n", this.score.size()));
    let note_set = new Silencio.ValueSet(make_key);
    for (let i = 0; i < this.score.size(); i++) {
        note_set.add(this.score.data[i]);
    }
    this.score.clear();
    this.score.data = [...note_set.values()];
    console.info(sprintf("After removing duplicate notes:  %6d\n", this.score.size()));
}

/**
 * Recursively computes the score graph, translates the points to notes, adds
 * them to the score, ties overlapping notes in the score, and rescales the
 * score.
 */
HarmonyIFS.ScoreAttractor.prototype.generate = function(depth) {
    console.log("Generating ScoreAttractor...");
    this.score.clear();
    let iteration = 0;
    // This point should be within the bounds of the attractor.
    let initial_point = new HarmonyIFS.Point();
    initial_point.k = 60;
    initial_point.v = 60;
    initial_point.i = 1;
    for (let i = 0; i < this.hutchinson_operator.length; i++) {
        let transformation = this.hutchinson_operator[i];
        console.log("transformation " + i + ":\n" + numeric.prettyPrint(transformation));
    }
    this.iterate(depth, iteration, initial_point.data);
    console.info(sprintf("Generated %d points.\n", this.score.size()));
    //console.log(this.score.toString());
    ///if (this.rescale == true) {
    ///    this.rescale_score_graph();
    ///}
    this.translate_score_attractor_to_score();
    console.log("Finished generating ScoreAttractor.");
};

/**
 * Compute the score attractor.
 */
 HarmonyIFS.ScoreAttractor.prototype.iterate = function(depth, iteration, point) {
    iteration = iteration + 1;
    if (iteration >= depth) {
        let note = this.point_to_note(point);
        //let text = sprintf("depth: %3d iteration: %3d point: %s note: %s\n", depth, iteration, point.toString(), note.toString());
        //console.info(text);
        this.score.append(note);
        return;
    }
    for (let i = 0; i < this.hutchinson_operator.length; i++) {
        let transposed_transformation = this.hutchinson_operator[i];
        // Remember, we are using row vectors.
        ///let new_point = numeric.dotMV(point, transposed_transformation);
        let new_point = numeric.dotMV(transposed_transformation, point);
        this.iterate(depth, iteration, new_point);
    }
};
/**
 * Processes the score attractor (the raw notes in the score) to quantize and
 * rescale certain dimensions, and to conform pitches to chords.
 */
HarmonyIFS.ScoreAttractor.prototype.translate_score_attractor_to_score = function() {
    console.log("Translating ScoreAttractor to final score...");
    this.score.setScale(Silencio.Event.TIME, 0);
    this.score.setScale(Silencio.Event.KEY, this.bass, this.range);
    this.score.temper();
    for (let i = 0; i < this.score.size(); i++) {
        let pitch = this.score.data[i].key;
        let new_pitch = ChordSpace.conformPitchToChord(pitch, this.score.data[i].chord, false);
        this.score.data[i].key = new_pitch;
    }
    console.log(this.score.toString());
    console.log("Removing duplicate notes...");
    this.remove_duplicate_notes();
    this.score.tieOverlaps(true);
    console.log(this.score.toString());
    console.log("Finished translating ScoreAttractor to final score.")
}

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

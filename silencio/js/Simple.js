/**
S I M P L E

Copyright (C) 2019 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License.

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

I'm tired of complexity.

Requires Silencio.js and ChordSpace.js.

*/

(function() {

/**
 * Generates a score as a sequence of points in chord symmetry space as a
 * function of time. Because chord symmetry space is a discrete orbifold, the
 * transformations do not need to be contractive. However, as in a fractal
 * approximation function, the time of the point never folds, but always
 * divides. Any callable with the correct interface can be used as a
 * transformation. Note that any number of transformations may be applied at the
 * **same** time. 
 */
var generate = function(depth, iteration, time, chord, transformations, score) {
    iteration++;
    if (iteration >== depth) {
        var chord = to_chord(point);
        score.insert(point.t, chord);
    } else {
        for (var i = 0; i < transformations.length; ++i) {
            var transformation = transformation[i];
            chord = transformation(chord);
        }
    }
}

var Simple = {
    Generator: Generator
};

// Node: Export function
if (typeof module !== "undefined" && module.exports) {
    module.exports = Simple;
}
// AMD/requirejs: Define the module
else if (typeof define === 'function' && define.amd) {
    define(function () {return Simple;});
}
// Browser: Expose to window
else {
    window.Simple = Simple;
}

})();

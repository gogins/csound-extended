/**
T E N D E N C Y   M A S K S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements Koenig tendency masks with some extensions. A tendency 
mask is an envelope for the boundaries of a random variable, which is sampled 
to generate parameters for musical events.

The user must specify a random number generator (or indeed any function that 
can be sampled), the top and bottom envelopes within which the samples are 
rescaled, and the sample count, stride, and time step.

The mask may be used to generate a series of samples over an interval, 
normally time, or to modify one or another field of the events in a 
Silencio score.

*/

(function() {
    
var EnvelopeGenerator = function() {
}

var TendencyMask = function() {
}

var TendencyMasks = {
};

// Node: Export function
if (typeof module !== "undefined" && module.exports) {
    module.exports = TendencyMasks;
}
// AMD/requirejs: Define the module
else if (typeof define === 'function' && define.amd) {
    define(function () {return TendencyMasks;});
}
// Browser: Expose to window
else {
    window.TendencyMasks = TendencyMasks;
}

})();

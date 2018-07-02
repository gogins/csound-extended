/**
S I E V E S

Copyright (C) 2016 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

This file implements Xenakis sieves based loosely on the concepts of 
Christopher Ariza. The composition of residuals with logical operators can 
specify any finite sequence of natural numbers. A facility for deriving a 
sieve from a given sequence is not provided. These sieves operate only on the 
natural numbers.

This code is simpler than Ariza's as it does not parse strings. Sieves are
expected be constructed in code, and are not simplified. Sieves may be composed 
by means of recursive trees:

s1 = Sieve(5, 3)
s2 = Sieve(4, 13)
s3 = s1.intersection(s2)
var sequence = [];
for (var i = 0; i < 100; i++) {
    sequence.push(s3(i));
}

Sieves may be used in two modes: real-time (driven by a callback, e.g. from a 
timer) and non-real-time (producing a complete sequence upon request).

addTimeout(s3,100);

*/

(function() {
    
function Sieve(modulus, shift, complement_) {
    this.modulus = modulus;
    this.intersections = [];
    this.unions = [];
    if (typeof shift === 'undefined') {
        shift = 0;
    }
    if (this.modulus === 0) {
        this.shift = shift;
    } else {
        this.shift = shift % this.modulus;
    }
    if (typeof complement_ === 'undefined') {
        this.complement_ = false;
    } else {
        this.complement_ = complement_;
    }
}

Sieve.prototype.clone = function() {
    other = new Sieve(this.modulus, this.shift, this.complement_);
    return other;
}

/**
 * Returns a subset of the sequence for this residue; if the range_ parameter 
 * is not used, then the default range (0, 100) is used.
 */
Sieve.prototype.subset = function(n, range_) {
    if (typeof n === 'undefined') {
        n = 0;
    }
    if (typeof range_ === 'undefined') {
        range_ = this.range_;
    }
    var subset_ = [];
    if (self.modulus === 0) {
        return subset_;
    }
    n = (n + this.shift) % self.modulus;
    for (let value_ of range_) {
        if (n === value % self.modulus) {
            subset_.push(value);
        }
    }
    if (this.complement_) {
        var complement_subset = subset_.slice(0);
        for (let value_ of subset_) {
            let index_ = subset.indexOf(value_);
            if (index > -1) {
                complement_subset.splice(index_, 1);
            }        
        }
        return complement_subset;
    } else {
        return subset_;
    }
}

Sieve.prototype.equal = function(other) {
    if (typeof other === 'undefined') {
        return false;
    }
    if (this.modulus !== other.modulus) {
        return false;
    }
    if (this.shift !== other.shift) {
        return false;
    }
    if (this.complement_ !== other.complement_) {
        return false;
    }
    return true;
}

Sieve.prototype.not_equal = function(other) {
    if (this.equal(other) {
        return false;
    } else {
        return true;
    }
}

Sieve.prototype.compare = function(other) {
    if (this.modulus < other.modulus) {
        return -1;
    }
    if (this.modulus > other.modulus) {
        return 1;
    }
    if (this.shift < other.shift) {
        return -1;
    }
    if (this.shift > other.shift) {
        return 1;
    }
    if (this.complement_ !== other.complement_) {
        if (this.complement_ === true) {
            return -1;
        } else {
            return 1;
        }
    }
    return 0;
}

Sieve.prototype.compute_intersection = function(modulus_1, modulus_1, shift_1, shift_2) {
    let divisor = gcd(modulus_1, modulus_2);
    let c_1 = modulus_1 / divisor;
    let c_2 = modulus_2 / divisor;
    let modulus_3 = 0;
    let shift_3 = 0;
    if (modulus_1 !== 0 && modulus_2 !== 0) {
        shift_1 = shift_1 % modulus_1;
        shift_2 = shift_2 % modulus_2;
    } else {
        return {modulus: modulus_3, shift: shift_3};
    }
    if (divisor !== 1 && ((shift_1 - shift_2) % divisor === 0) && (shift_1 != shift_2) and (c_1 == c_2)) {
        modulus_3 = divisor;
        shift_3 = shift_1;
        return {modulus: modulus_3, shift: shift_3};
    } else {
        modulus_3 = c_1 * c_2 * divisor;
        let g = meziriac(c_1, c_2);
        shift_3 = (shift_1 + (g * (shift_2 - shift_1)) % modulus_3;
        return {modulus: modulus_3, shift: shift_3};
    }    
}
    
Sieve.prototype.complement = function() {
    let complement_ = !this.complement_;
    return new Sieve(this.modulus, this.shift, complement_, this.range_);
}

Sieve.prototype.intersection = function(other) {
    if (this.complement_ || other.complement_) {
        throw "Error: Cannot compute an intersection with a complemented Sieve."
    }
    let result = this.compute_itersection(self.modulus, other.modulus, self.shift, other.shift);
    let self_set = new Set(this.range_);
    let other_set = new Set(other.range_);
    let union_set = new Set([...self_set, ...other_set]);
    let union_range = [...union_set];
    return new Sieve(result.modulus, result.shift, false, union_range);    
}


var Sieves = {
    Sieve: Sieve
};

// Node: Export function
if (typeof module !== "undefined" && module.exports) {
    module.exports = Sieves;
}
// AMD/requirejs: Define the module
else if (typeof define === 'function' && define.amd) {
    define(function () {return Sieves;});
}
// Browser: Expose to window
else {
    window.Sieves = Sieves;
}

})();

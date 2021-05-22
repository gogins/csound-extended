/**
S I L E N C I O

Copyright (C) 2014 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an HTML5 algorithmic music composition library for Csound.

TO DO

--  Implement various scales found in 20th and 21st century harmony
    along with 'splitting' and 'merging' operations.

--  Implement tendency masks.

--  Implement Xenakis sieves.

DEPENDENCIES

fs
three.js
TrackballControls.js
sprintf.js
tinycolor.js

REGARDING BLUE

Steven Yi's Java program blue, for composing with Csound, uses the Nashorn
JavaScript runtime and does not support the DOM or other objects found in a
Web browser's JavaScript context. To use Silencio in blue:

--  Load sprintf.js and tinycolor.js first.
--  The following polyfill is enough to run some things.

/**
 * /page Silencio
 *
 * Silencio is a port to JavaScript of parts of CsoundAC, and has taken on a
 * life of its own, particularly for the production of interfactive pieces
 * with user interfaces, and visual music.
 *
 */
if (typeof console === 'undefined') {
    var global = this;
    var window = this;
    var process = {env: {}};
    var console = {};
    console.debug = print;
    console.warn = print;
    console.info = print;
}

(function() {
    /**
    A Score is a matrix in which the rows are Events.

    An Event is a homogeneous vector with the following dimensions:

     1 Time in seconds from start of performance.
     2 Duration in seconds, -1 is "indefinite."
     3 MIDI status (only the most significant nybble, e.g. 144 for 'NOTE ON').
     4 MIDI channel (any real number, fractional part ties events,
       negative is 'NOTE OFF').
     5 MIDI key number from 0 to 127, 60 is middle C (a real number).
     6 MIDI velocity from 0 to 127, 80 is mezzo-forte (a real number).
     7 x or depth, 0 is the origin.
     8 y or pan, 0 is the origin.
     9 z or height, 0 is the origin.
    10 Phase, in radians.
    11 Homogeneity, normally always 1.

    NOTE: ECMASCRIPT 5 doesn't support inheritance from Array
    in a clean and complete way, so we don't even try.
    */

    /**
     * Deserialize the file, which must contain JSON, and return either
     * the JSON or the parsed object.
     * If the pathname is undefined, then use the location + ".json".
     * Returns the object for success, or null for failure.
     */
    var restoreFromLocalFile = function(toObject, filepath) {
        try {
            if (typeof filepath === 'undefined') {
                filepath = window.location.pathname.slice(1);
                filepath = fs.realpathSync(filepath);
                filepath = filepath + '.json';
            }
            console.info('loading from filepath: ' + filepath);
            var json = fs.readFileSync(filepath);
            console.info('json: ' + json);
            if (toObject === true) {
                var parsed_object = JSON.parse(json);
                console.info('parsed object: ' + parsed_object);
                return parsed_object;
            }
            return json;
        } catch (err) {
            console.info(err.message);
            return null;
        }
    };

    /**
     * Translate the object to JSON and save it on the local filesystem.
     * If the object is already a JSON string, do not translate it.
     * If the filepath is undefined, then use the location + ".json";
     * in this case, obviously, the location must be indeed be a local
     * filepath. Returns true for success, and false for failure.
     */
    var saveToLocalFile = function(fromObject, object, filepath) {
        try {
            var json = null;
            if (fromObject) {
                json = JSON.stringify(object);
            } else {
                json = object;
            }
            if (typeof json === 'undefined') {
                throw "saveToLocalFile: json is undefined.";
            }
            if (json === null || json === 'null') {
                throw "saveToLocalFile: json is null.";
            }
            if (typeof filepath === 'undefined') {
                filepath = window.location.pathname.slice(1);
                filepath = fs.realpathSync(filepath);
                filepath = filepath + '.json';
            }
            console.info('json: ' + json);
            console.info('saving to filepath: ' + filepath);
            fs.writeFileSync(filepath, json);
            return true;
        } catch (err) {
            console.info(err.message);
            return false;
        }
    };

    /**
     * Restore dat.gui parameters as JSON from:
     * Local storage, if it exists (this happens in dat.gui itself); otherwise,
     * from the local file system, if the file exists; otherwise,
     * using the default parameters in JSON form.
     * NOTE: 'load' element for dat.gui constructor is a JSON _object_,
     * not a string.
     */
    var restoreDatGuiJson = function(default_parameters_json) {
        var parameters_filesystem_json = Silencio.restoreFromLocalFile(false);
        if (parameters_filesystem_json !== null && parameters_filesystem_json !== 'null') {
            console.info('Restored dat.gui parameters from local filesystem: ' + parameters_filesystem_json);
            return JSON.parse(parameters_filesystem_json);
        } else {
            console.info('Restored dat.gui parameters from default: ' + default_parameters_json);
            return default_parameters_json;
        }
    };

    /**
     * Save the parameters object for dat.gui as JSON to the local file system.
     * Returns true for success, and false for failure.
     */
    var saveDatGuiJson = function(gui) {
        try {
            var json = gui.getSaveObject();
            console.info('typeof json:' + (typeof json));
            saveToLocalFile(true, json);
            return true;
        } catch (err) {
            console.info(err.message);
            return false;
        }
    };

    /**
     * Parse the Csound orchestra for hints to create a user interface using
     * nw.gui sliders; create that user interface in the HTML window; and create
     * and compile a "Controls" instrument ro receive values from that user
     * interface.
     *
     * The hints are in the form of special comments following global variable
     * declarations associated with an instrument definition:
     * g<x>_<instrname>_<variablename> init <default_value> ;|Instrument Name|Control Name|Minimum Valuie|Maximum Value|Increment|
     */
    var createNwSlider = function(line, window, nwfolder) {

    };

    var createNwUi = function(orc, csound, window) {
        var channels = [];
    };

    function eq_epsilon(a, b) {
        var epsilon_factor = 100 * Number.EPSILON;
        if (Math.abs(a - b) > epsilon_factor) {
            return false;
        }
        return true;
    }

    function lt_epsilon(a, b) {
        if (eq_epsilon(a, b)) {
            return false;
        }
        if (a < b) {
            return true;
        }
        return false;
    }

    function le_epsilon(a, b) {
        if (eq_epsilon(a, b)) {
            return true;
        }
        if (a < b) {
            return true;
        }
        return false;
    }

    function gt_epsilon(a, b) {
        if (eq_epsilon(a, b)) {
            return false;
        }
        if (a > b) {
            return true;
        }
        return false;
    }

    function ge_epsilon(a, b) {
        if (eq_epsilon(a, b)) {
            return true;
        }
        if (a > b) {
            return true;
        }
        return false;
    }

    function modulo(a, n) {
        return ((a % n) + n) % n;
    }

    function Event() {
        // ID collisions should be rare. IDs will be used e.g.
        // in tieing notes or in working around Csound quirks
        // with indefinite notes.
        this.id = Math.random() / 2.0;
        this.data = [0, 0, 144, 0, 0, 0, 0, 0, 0, 0, 1];
        this.chord = null;
        Object.defineProperty(this, "time", {
            get: function() {
                return this.data[0];
            },
            set: function(value) {
                this.data[0] = value;
            }
        });
        Object.defineProperty(this, "duration", {
            get: function() {
                return this.data[1];
            },
            set: function(value) {
                this.data[1] = value;
            }
        });
        Object.defineProperty(this, "end", {
            get: function() {
                return this.data[0] + this.data[1];
            },
            set: function(end_) {
                var duration_ = end_ - this.data[0];
                if (duration_ > 0) {
                    this.data[1] = duration_;
                } else {
                    this.data[0] = this.data[0] + duration_;
                    this.data[1] = -1 * duration_;
                }
            }
        });
        Object.defineProperty(this, "status", {
            get: function() {
                return this.data[2];
            },
            set: function(value) {
                this.data[2] = value;
            }
        });
        Object.defineProperty(this, "channel", {
            get: function() {
                return this.data[3];
            },
            set: function(value) {
                this.data[3] = value;
            }
        });
        Object.defineProperty(this, "key", {
            get: function() {
                return this.data[4];
            },
            set: function(value) {
                this.data[4] = value;
            }
        });
        Object.defineProperty(this, "velocity", {
            get: function() {
                return this.data[5];
            },
            set: function(value) {
                this.data[5] = value;
            }
        });
        Object.defineProperty(this, "depth", {
            get: function() {
                return this.data[6];
            },
            set: function(value) {
                this.data[6] = value;
            }
        });
        // Ambisonic left to right is the Y dimension!
        Object.defineProperty(this, "pan", {
            get: function() {
                return this.data[7];
            },
            set: function(value) {
                this.data[7] = value;
            }
        });
        Object.defineProperty(this, "heigth", {
            get: function() {
                return this.data[8];
            },
            set: function(value) {
                this.data[8] = value;
            }
        });
        Object.defineProperty(this, "phase", {
            get: function() {
                return this.data[9];
            },
            set: function(value) {
                this.data[9] = value;
            }
        });
        Object.defineProperty(this, "homogeneity", {
            get: function() {
                return this.data[10];
            },
            set: function(value) {
                this.data[10] = value;
            }
        });
    }
    Event.TIME = 0;
    Event.DURATION = 1;
    Event.STATUS = 2;
    Event.CHANNEL = 3;
    Event.KEY = 4;
    Event.VELOCITY = 5;
    Event.X = 6;
    Event.Y = 7;
    Event.Z = 8;
    Event.DEPTH = 6;
    Event.PAN = 7;
    Event.HEIGHT = 8;
    Event.PHASE = 9;
    Event.HOMOGENEITY = 10;
    Event.COUNT = 11;

    Event.prototype.toString = function() {
        var text = '';
        for (var i = 0; i < this.data.length; i++) {
            text = text.concat(' ', this.data[i].toFixed(6));
        }
        if (this.chord !== null && typeof this.chord !== 'undefined') {
            text = text.concat(' ', this.chord.name());
        }
        text = text.concat('\n');
        return text;
    };

    /**
     * Csound pfields are:
     * p1 Instrument number
     * p2 Time (seconds)
     * p3 Duration (seconds)
     * p4 MIDI key
     * p5 MIDI velocity (roughly dBSPL)
     * p6 Depth (spatial front to back)
     * p7 Pan (spatial left to right)
     * p8 Height (spatial bottom to top)
     * p9 Phase (radians)
     */
    Event.prototype.toIStatement = function() {
        var text = 'i';
        let insno = this.data[3];// + this.id;
        text = text.concat(' ', insno.toFixed(6)); // p1
        text = text.concat(' ', this.data[0].toFixed(6)); // p2
        text = text.concat(' ', this.data[1].toFixed(6)); // p3
        text = text.concat(' ', this.data[4].toFixed(6)); // p4
        text = text.concat(' ', this.data[5].toFixed(6)); // p5
        text = text.concat(' ', this.data[6].toFixed(6)); // p6
        text = text.concat(' ', this.data[7].toFixed(6)); // p7
        text = text.concat(' ', this.data[8].toFixed(6)); // p8
        text = text.concat(' ', this.data[9].toFixed(6)); // p9
        text = text.concat('\n');
        return text;
    };

    /**
     * For turning an event off; the default is with immediate effect,
     * otherwise at the scheduled time.
     */
    Event.prototype.toDStatement = function(scheduled) {
        var text = 'd';
        text = text.concat(' ', this.data[3].toFixed(6)); // p1
        if (typeof scheduled === 'undefined') {
            text = text.concat(' ', '0'); // p2
        } else {
            text = text.concat(' ', scheduled.toFixed(6)); // p2
        }
        let insno = this.data[3];// + this.id;
        text = text.concat(' ', Math.floor(insno.toFixed(6))); // p3
        text = text.concat(' ', this.data[4].toFixed(6)); // p4
        text = text.concat(' ', this.data[5].toFixed(6)); // p5
        text = text.concat(' ', this.data[6].toFixed(6)); // p6
        text = text.concat(' ', this.data[7].toFixed(6)); // p7
        text = text.concat(' ', this.data[8].toFixed(6)); // p8
        text = text.concat(' ', this.data[9].toFixed(6)); // p9
        text = text.concat('\n');
        return text;
    }

    Event.prototype.toFomus = function() {
        return 'note part ' + Math.floor(this.channel) + ' time ' + this.time * 2 + ' duration ' + this.duration * 2 + ' pitch ' + this.key + ' dynamic ' + this.velocity + ';';
    };

    Event.prototype.temper = function(tonesPerOctave) {
        if (typeof tonesPerOctave === 'undefined') {
            tonesPerOctave = 12;
        }
        var octave = this.key / 12;
        var tone = Math.floor((octave * tonesPerOctave) + 0.5);
        octave = tone / tonesPerOctave;
        this.key = octave * 12;
    };

    Event.prototype.clone = function(clone_chord) {
        if (typeof clone_chord === 'undefined') {
            clone_chord = false;
        }
        other = new Event();
        other.data = this.data.slice(0);
        other.id = this.id;
        if (clone_chord === true) {
            other.chord = this.chord.clone();
        } else {
            other.chord = this.chord;
        }
        return other;
    };

    function Score() {
        this.data = [];
        this.minima = new Event();
        this.maxima = new Event();
        this.ranges = new Event();
        this.context = null;
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.controls = null;
        this.score_cursor = null;
        this.title = '';
        this.composer = '';
        this.names_for_instrument_numbers = {};
    }

    Score.prototype.add = function(p0_time, p1_duration, p2_status, p3_channel, p4_key, p5_velocity, p6_x, p7_y, p8_z, p9_phase, p10_homogeneity) {
        var event = new Event();
        for (var i = 0; i < event.data.length; i++) {
            if (typeof arguments[i] !== 'undefined') {
                event.data[i] = arguments[i];
            }
        }
        this.data.push(event);
    };

    Score.prototype.append = function(event) {
        this.data.push(event);
    };

    Score.prototype.append_score = function(score) {
        for (var i = 0; i < score.data.length; i++) {
            this.data.push(score.data[i]);
        }
    };

    Score.prototype.clear = function() {
        while (this.data.length > 0) {
            this.data.pop();
        }
    };

    Score.prototype.getDuration = function() {
        this.sort();
        this.findScale(0);
        var duration = 0;
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            if (i === 0) {
                duration = event.end; //data[0] + event.data[1];
            } else {
                var currentDuration = event.end; //data[0] + event.data[1];
                if (currentDuration > duration) {
                    duration = currentDuration;
                }
            }
        }
        return duration;
    };

    Score.prototype.log = function(what) {
        if (typeof what === 'undefined') {
            what = '';
        } else {
            what = what + ': ';
        }
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            console.info(what + event.toString());
        }
    };

    Score.prototype.getEnd = function() {
        var end = null;
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            if (i === 0) {
                end = event.end;
            } else {
                if (end < event.end) {
                    end = event.end;
                }
            }
        }
        return end;
    };

    Score.prototype.setDuration = function(duration) {
        this.sort();
        var start = this.data[0].time;
        var i;
        var event;
        for (i = 0; i < this.data.length; i++) {
            event = this.data[i];
            event.data[0] = event.data[0] - start;
        }
        var currentDuration = this.data[0].end;
        for (i = 0; i < this.data.length; i++) {
            event = this.data[i];
            if (event.end > currentDuration) {
                currentDuration = event.end;
            }
        }
        var factor = Math.abs(duration / currentDuration);
        for (i = 0; i < this.data.length; i++) {
            event = this.data[i];
            event.data[0] = event.data[0] * factor;
            event.data[1] = event.data[1] * factor;
        }
    };

    Score.prototype.quantize = function(dimension, quantum) {
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            var value = Math.floor(event.data[dimension] / quantum);
            event.data[dimension] = (value * quantum);
        }
    };

    Score.prototype.quantizeTime = function(quantum) {
        this.quantize(0, quantum);
        this.quantize(1, quantum);
    };

    Score.prototype.sendToCsound = function(csound, extra) {
        this.sort();
        if (typeof extra === 'undefined') {
            jscore = '';
        } else {
            extra = 5.0;
            var duration = this.getDuration() + extra;
            jscore = 'f 0 ' + duration + ' 0\n';
        }
        for (var i = 0; i < this.data.length; i++) {
            jscore += this.data[i].toIStatement();
        }
        csound.ReadScore(jscore);
    };

    Score.prototype.turnoffInCsound = function(csound, extra) {
        this.sort();
        if (typeof extra === 'undefined') {
            jscore = '';
        } else {
            extra = 5.0;
            var duration = this.getDuration() + extra;
            jscore = 'f 0 ' + duration + ' 0\n';
        }
        for (var i = 0; i < this.data.length; i++) {
            jscore += this.data[i].toDStatement();
        }
        //console.info(jscore);
        csound.ReadScore(jscore);
    };

    Score.prototype.findScales = function() {
        for (var i = 0; i < this.minima.data.length; i++) {
            this.findScale(i);
        }
    };

    Score.prototype.findScale = function(dimension) {
        var min = Number.NaN;
        var max = Number.NaN;
        for (var i = 0; i < this.data.length; i++) {
            var value = this.data[i].data[dimension];
            if (i === 0) {
                min = value;
                max = value;
            } else {
                if (value < min) {
                    min = value;
                }
                if (value > max) {
                    max = value;
                }
            }
        }
        this.minima.data[dimension] = min;
        this.maxima.data[dimension] = max;
        this.ranges.data[dimension] = max - min;
    };

    Score.prototype.setScale = function(dimension, minimum, range) {
        this.findScale(dimension);
        var toOrigin = this.minima.data[dimension];
        var currentRange = this.ranges.data[dimension];
        if (currentRange === 0) {
            currentRange = 1;
        }
        if (typeof range === 'undefined') {
            range = 1;
        }
        var rescale = range / currentRange;
        var translate = minimum;
        for (var i = 0; i < this.data.length; i++) {
            var value = this.data[i].data[dimension];
            value -= toOrigin;
            value *= rescale;
            value += translate;
            this.data[i].data[dimension] = value;
        }
    };

    Score.prototype.temper = function(tonesPerOctave) {
        for (var i = 0; i < this.data.length; i++) {
            this.data[i].temper(tonesPerOctave);
        }
    };

    Score.prototype.sort = function() {
        this.data.sort(eventComparator);
    };

    Score.prototype.tieOverlaps = function(tieExact) {
        console.info("Before tieing: " + this.data.length + "\n");
        if (typeof tieExact === 'undefined') {
            tieExact = false;
        }
        this.sort();
        var laterI;
        var laterEvent;
        var earlierI;
        var earlierEvent;
        let temporary_score = [];
        // Get rid of notes that will not sound.
        /*
        0 Time in seconds from start of performance.
        1 Duration in seconds, -1 is "indefinite."
        2 MIDI status (only the most significant nybble, e.g. 144 for 'NOTE ON').
        3 MIDI channel (any real number, fractional part ties events,
          negative is 'NOTE OFF').
        4 MIDI key number from 0 to 127, 60 is middle C (a real number).
        5 MIDI velocity from 0 to 127, 80 is mezzo-forte (a real number).
       */
        for (let event of this.data) {
            if (event.data[1] > 0 && event.data[5] > 0) {
                temporary_score.push(event)
            }
        }
        for (laterI = temporary_score.length - 1; laterI >= 0; laterI--) {
            laterEvent = temporary_score[laterI];
            if (laterEvent.data[2] === 144) {
                for (earlierI = laterI - 1; earlierI >= 0; earlierI--) {
                    earlierEvent = temporary_score[earlierI];
                    if (earlierEvent.data[2] === 144) {
                        var overlaps = false;
                        let earlier_event_end = earlierEvent.data[0] + earlierEvent.data[1];
                        let later_event_end = laterEvent.data[0] + laterEvent.data[1];
                        if (tieExact) {
                            overlaps = ge_epsilon(earlier_event_end, laterEvent.data[0]);
                        } else {
                            overlaps = gt_epsilon(earlier_event_end, laterEvent.data[0]);
                        }
                        if (overlaps === true) {
                            if ((Math.floor(earlierEvent.data[3]) === Math.floor(laterEvent.data[3])) &&
                                (Math.round(earlierEvent.data[4]) === Math.round(laterEvent.data[4]))) {
                                //console.info('Tieing: ' + earlierI + ' ' + earlierEvent.toString());
                                //console.info('    to: ' + laterI + ' ' + laterEvent.toString());
                                earlierEvent.data[1] = later_event_end - earlierEvent.data[0];
                                laterEvent.data[1] = 0;
                                laterEvent.data[5] = 0;
                                //console.info('Result: ' + earlierI + ' ' +  earlierEvent.toString() + '\n');
                                break;
                            }
                        }
                    }
                }
            }
        }
        // Get rid of notes that will not sound (again).
        this.clear();
        for (let event of temporary_score) {
            if (event.data[1] > 0 && event.data[5] > 0) {
                this.append(event)
            }
        }
        console.info("After tieing: " + this.data.length + "\n");
    };

    Score.prototype.progress = function(score_time) {
        if (context !== null) {
            context.fillStyle = "LawnGreen";
            context.fillRect(0, 60, score_time, 0.01);
        }
    };

    /**
     * Displays the score cursor at the current time.
     */
    Score.prototype.progress3D = function(score_time) {
        if (this.scene !== null) {
            this.score_cursor.position.x = score_time;
            this.score_cursor.position.y = 60;
            this.score_cursor.position.z = 0.5;
            this.controls.update();
            this.camera.updateProjectionMatrix();
            this.renderer.render(this.scene, this.camera);
        }
    };

    /**
     * Sets up a scene, camera, and renderer with controls to view
     * either a fixed or a real-time score.
     */
    Score.prototype.prepareScene3D = function(canvas) {
        this.canvas = canvas;
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        this.findScales();
        this.scene = new THREE.Scene();
        var scene = this.scene;
        this.renderer = new THREE.WebGLRenderer({
            canvas: canvas,
            antialias: true
        });
        var renderer = this.renderer;
        renderer.setClearColor(0);
        renderer.sortObjects = false;
        renderer.setViewport(0, 0, canvas.clientWidth, canvas.clientHeight);
        renderer.setPixelRatio(canvas.devicePixelRatio);
        // Wire up the view controls to the camera.
        this.camera = new THREE.PerspectiveCamera(45, canvas.clientWidth / canvas.clientHeight, 1, 10000);
        var camera = this.camera;
        this.controls = new THREE.TrackballControls(camera, canvas);
        var controls = this.controls;
        controls.rotateSpeed = 1.0;
        controls.zoomSpeed = 1;
        controls.panSpeed = 1;
        controls.noZoom = false;
        controls.noPan = false;
        controls.staticMoving = true;
        controls.dynamicDampingFactor = 0.3;
        // Ensure that all sides are lighted.
        var light = new THREE.DirectionalLight(0xffffff, 1);
        light.position.set(1, 1, 1).normalize();
        this.scene.add(light);
        var light2 = new THREE.AmbientLight(0x404040, 0.5);
        this.scene.add(light2);
        var onResize = function() {
            canvas.width = canvas.clientWidth;
            canvas.height = canvas.clientHeight;
            this.renderer.setViewport(0, 0, canvas.clientWidth, canvas.clientHeight);
            this.camera.aspect = canvas.clientWidth / canvas.clientHeight;
            this.controls.handleResize();
            this.camera.updateProjectionMatrix();
            this.renderer.render(this.scene, this.camera);
        };
        window.addEventListener('resize', onResize, false);
    };

    /**
     * Adds the note to the 3D scene. Can be used with a fixed or a real-time score.
     */
    Score.prototype.plotNote3D = function(note, channel_minimum, channel_range, velocity_minimum, velocity_range) {
        var begin = note.time;
        var end = note.end;
        var duration = end - begin;
        var key = note.key;
        var channel = note.channel - channel_minimum;
        var geometry = new THREE.BoxBufferGeometry(duration, 1, 1);
        if (channel_range === 0) {
            channel_range = 1;
        }
        if (velocity_range === 0) {
            velocity_range = 1;
        }
        hue = channel / channel_range;
        var value = note.velocity - velocity_minimum;
        value = value / velocity_range;
        value = 0.5 + value / 2;
        var material = new THREE.MeshLambertMaterial();
        material.color.setHSL(hue, 1, value);
        material.opacity = 0.5;
        material.reflectivity = 0.5;
        material.transparent = true;
        material.emissive = material.color;
        material.emissiveIntensity = 2 / 3;
        var note_mesh = new THREE.Mesh(geometry, material);
        note_mesh.position.x = begin + duration / 2; // + note.scale.x;
        note_mesh.position.y = key;
        note_mesh.position.z = channel;
        this.scene.add(note_mesh);
    };

    /**
     * Plots a grid for a fixed score.
     */
    Score.prototype.plotGrid3D = function() {
        // Generate the grid. Its origin for time is 0 and for pitch its origin is the
        // first C lower than or equal to the lowest pitch in the score.
        var time_minimum = this.minima.time;
        var time_maximum = this.getDuration();
        var key_minimum = this.minima.key;
        var key_maximum = this.maxima.key;
        var channel_minimum = this.minima.channel;
        var channel_maximum = this.maxima.channel;
        var line_material = new THREE.LineBasicMaterial();
        time_minimum = 0;
        instrument_minimum = 0;
        if (key_minimum % 12 !== 0) {
            key_minimum -= (key_minimum % 12);
        }
        var grid_geometry = new THREE.BoxBufferGeometry(10, 12, 1);
        for (var t = time_minimum; t <= time_maximum + 10; t = t + 10) {
            for (var k = key_minimum; k <= key_maximum; k = k + 12) {
                var box = new THREE.LineSegments(new THREE.EdgesGeometry(grid_geometry), line_material);
                ///box = new THREE.EdgesGeometry(box);
                box.material.color.setRGB(0, 0.25, 0);
                box.material.opacity = 0.25;
                box.material.transparent = true;
                box.position.x = t + 5;
                box.position.y = k + 6;
                box.position.z = 0;
                box.scale.z = 0;
                this.scene.add(box);
            }
        }
        // Put a ball at the origin, to indicate the orientation of the score.
        var origin_geometry = new THREE.SphereGeometry(1, 10, 10);
        var origin_material = new THREE.MeshLambertMaterial();
        origin_material.color.setRGB(0, 255, 0);
        var origin = new THREE.Mesh(origin_geometry, origin_material);
        origin.position.x = time_minimum;
        origin.position.y = key_minimum;
        origin.position.z = 0;
        this.scene.add(origin);
        // Put a ball at the start of middle C, to indicate the current Csound
        // score time.
        var cursor_geometry = new THREE.SphereGeometry(1, 10, 10);
        var cursor_material = new THREE.MeshLambertMaterial();
        cursor_material.color.setRGB(255, 0, 0);
        this.score_cursor = new THREE.Mesh(cursor_geometry, cursor_material);
        this.score_cursor.position.x = time_minimum;
        this.score_cursor.position.y = 60;
        this.score_cursor.position.z = 0;
        this.scene.add(this.score_cursor);
    };

    /**
     * Looks at a full fixed score.
     */
    Score.prototype.lookAtFullScore3D = function() {
        var bounding_box = new THREE.Box3().setFromObject(this.scene);
        this.camera.lookAt(bounding_box.getCenter());
        this.camera.fov = 2 * Math.atan((bounding_box.getSize().x / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().y)) * (180 / Math.PI);
        this.camera.position.copy(bounding_box.getCenter());
        this.camera.position.z = 1.125 * Math.min(bounding_box.getSize().x, bounding_box.getSize().y);
        this.controls.target.copy(bounding_box.getCenter());
        this.controls.update();
        this.camera.updateProjectionMatrix();
        this.renderer.render(this.scene, this.camera);
    };

    /**
     * Looks at the front (current notes) of a real-time score.
     */
    Score.prototype.lookAtFront3D = function() {
        var bounding_box = new THREE.Box3().setFromObject(this.scene);
        this.camera.lookAt(bounding_box.getCenter());
        this.camera.fov = 2 * Math.atan((bounding_box.getSize().y / (this.canvas.width / this.canvas.height)) / (2 * bounding_box.getSize().z)) * (180 / Math.PI);
        this.camera.position.copy(bounding_box.getCenter());
        this.camera.position.x = 1.125 * Math.max(bounding_box.getSize().x, bounding_box.getSize().y);
        this.controls.target.copy(bounding_box.getCenter());
        this.controls.update();
        this.camera.updateProjectionMatrix();
        this.renderer.render(this.scene, this.camera);
    };

    /**
     * Redraws the scene using the camera updated from the controls.
     */
    Score.prototype.render3D = function() {
        this.controls.update();
        this.camera.updateProjectionMatrix();
        this.renderer.render(this.scene, this.camera);
    };

    /**
     * Draws the notes in a fixed score as a 3-dimensional piano roll. The score is
     * fitted into the viewport to start with, but the user can use the mouse or
     * trackball to move around the score and to zoom in and out. The dimensions
     * are: time = x, MIDI key = y, MIDI channel = z and hue, and loudness =
     * value; a grid shows tens of seconds and octaves.
     */
    Score.prototype.draw3D = function(canvas) {
        this.prepareScene3D(canvas);
        // Plot the notes.
        for (var i = 0; i < this.data.length; i++) {
            this.plotNote3D(this.data[i], this.minima.channel, this.ranges.channel, this.minima.velocity, this.ranges.velocity);
        }
        this.plotGrid3D();
        this.lookAtFullScore3D();
        return canvas;
    };

    Score.prototype.toString = function() {
        var result = '';
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            result = result.concat(event.toString());
        }
        return result;
    };

    Score.prototype.toCsoundScore = function(extra) {
        var result = '';
        for (var i = 0; i < this.data.length; i++) {
            var event = this.data[i];
            result = result.concat(event.toIStatement());
        }
        if (typeof extra !== 'undefined') {
            result.concat('e ' + extra);
        }
        return result;
    };

    Score.prototype.size = function() {
        return this.data.length;
    };

    Score.prototype.get = function(index) {
        return this.data[index];
    };

    // Returns the sub-score containing events
    // starting at or later than the begin time,
    // and up to but not including the end time.
    // The events in the slice are values unless
    // by_reference is true.
    Score.prototype.slice = function(begin, end_, by_reference) {
        if (typeof by_reference === 'undefined') {
            by_reference = false;
        }
        this.sort();
        var s = new Silencio.Score();
        for (var index = 0; index < this.size(); index++) {
            var event = this.data[index];
            var time_ = event.time;
            if (time_ >= begin && time_ < end_) {
                if (by_reference === true) {
                    s.append(event);
                } else {
                    s.append(event.clone());
                }
            }
        }
        return s;
    };

    /**
     * Writes a FOMUS score in "basename.fms". The tempo is 120 BPM and notes are
     * written strictly by time in beats, i.e. bar lines are not calculated. Any
     * FOMUS commands in "fomus_overrides" take precedence. The FOMUS file directs
     * FOMUS to produce output files for LilyPond, MusicXML, and MIDI.
     */
    Score.prototype.engrave = function(fomus_overrides) {
        try {
            var filepath = window.location.pathname.slice(1);
            filepath = fs.realpathSync(filepath);
            filepath = filepath + '.fms';
            console.info('saving to filepath: ' + filepath);
            this.sort();
            this.findScales();
            lines = [];
            lines.push('title "' + this.title + '"');
            lines.push('author "' + this.composer + '"');
            lines.push('output (ly mid xml)');
            lines.push('timesig (4 4)');
            // 32nd notes are the shortest.
            lines.push('beatdiv 8');
            // No tuplets -- they make this kind of thing ureadable.
            lines.push('tuplets ()');
            lines.push('quartertones yes');
            lines.push('dyns yes');
            lines.push('prune-type steal');
            lines.push('untie yes');
            lines.push('untie-dur-range (1/64 4)');
            lines.push('dyn-range (' + Math.round(this.minima.velocity) + ' ' + Math.round(this.maxima.velocity) + ')');
            lines.push('init-tempo 120');
            lines.push('init-tempo-text "* = #"');
            lines.push('lily-papersize "11x17"');
            if (typeof fomus_overrides !== 'undefined') {
                lines.push(fomus_overrides + '\n');
            }
            first_part = null;
            for (var number in this.names_for_instrument_numbers) {
                if (this.names_for_instrument_numbers.hasOwnProperty(number)) {
                    if (first_part === null) {
                        first_part = number;
                    }
                    var name = this.names_for_instrument_numbers[number];
                    lines.push('part <id: ' + number + ' name: "' + name + '" abbr: "' + name.substring(0, 3) + '">');
                }
            }
            var i;
            for (i = 0; i < this.data.length; i++) {
                var note = this.data[i].toFomus();
                lines.push(note);
            }
            var fd = fs.openSync(filepath, 'w');
            for (i = 0; i < lines.length; i++) {
                var line = lines[i] + '\n';
                fs.writeSync(fd, line);
            }
            fs.close(fd);
            return true;
        } catch (err) {
            console.info(err.message);
            return false;
        }
    };

    function eventComparator(a, b) {
        for (var i = 0; i < a.data.length; i++) {
            var avalue = a.data[i];
            var bvalue = b.data[i];
            var difference = avalue - bvalue;
            if (difference !== 0) {
                return difference;
            }
        }
        return 0;
    }

    function Turtle(len, theta) {
        this.len = len;
        this.theta = theta;
        this.reset();
        return this;
    }
    Turtle.prototype.reset = function() {
        this.angle = Math.PI / 2;
        this.p = {
            'x': 0,
            'y': 0
        };
        this.stack = [];
        this.instrument = 'red';
        this.tempo = 1;
        this.event = new Silencio.Event();
    };
    Turtle.prototype.next = function() {
        return {
            'x': this.p.x + this.len * this.tempo * Math.cos(this.angle),
            'y': this.p.y - this.len * Math.sin(this.angle)
        };
    };
    Turtle.prototype.go = function(context) {
        var nextP = this.next();
        if (context !== null) {
            context.strokeStyle = tinycolor(this.instrument).toString();
            context.beginPath();
            context.moveTo(this.p.x, this.p.y);
            context.lineTo(nextP.x, nextP.y);
            context.stroke();
        }
        this.p = nextP;
    };
    Turtle.prototype.move = function() {
        this.p = this.next();
    };
    Turtle.prototype.turnLeft = function() {
        this.angle += this.theta;
    };
    Turtle.prototype.turnRight = function() {
        this.angle -= this.theta;
    };
    Turtle.prototype.upInstrument = function() {
        this.instrument = tinycolor(this.instrument).spin(10);
    };
    Turtle.prototype.downInstrument = function() {
        this.instrument = tinycolor(this.instrument).spin(-10);
    };
    Turtle.prototype.upVelocity = function() {
        this.instrument = tinycolor(this.instrument).darken(-1);
    };
    Turtle.prototype.downVelocity = function() {
        this.instrument = tinycolor(this.instrument).darken(1);
    };
    Turtle.prototype.upTempo = function() {
        this.tempo = this.tempo / 1.25;
    };
    Turtle.prototype.downTempo = function() {
        this.tempo = this.tempo * 1.25;
    };
    Turtle.prototype.push = function() {
        this.stack.push({
            'p': this.p,
            'angle': this.angle,
            'instrument': this.instrument,
            'tempo': this.tempo,
            'event': this.event.clone()
        });
    };
    Turtle.prototype.pop = function() {
        var s = this.stack.pop();
        this.p = s.p;
        this.angle = s.angle;
        this.instrument = s.instrument;
        this.tempo = s.tempo;
        this.event = s.event;
    };

    function LSys() {
        this.axiom = '';
        this.rules = {};
        this.prior = '';
        this.score = new Silencio.Score();
        return this;
    }
    LSys.prototype.addRule = function(c, replacement) {
        this.rules[c] = replacement;
    };
    LSys.prototype.generate = function(n) {
        this.sentence = this.axiom;
        for (var g = 0; g < n; g++) {
            var next = [];
            for (var i = 0; this.sentence.length > i; i++) {
                var c = this.sentence[i];
                var r = this.rules[c];
                if (r) {
                    next.push(r);
                } else {
                    next.push(c);
                }
            }
            this.sentence = next.join("");
        }
    };
    LSys.prototype.write_score = function(t) {
        t.reset();
        for (i = 0; this.sentence.length > i; i++) {
            c = this.sentence[i];
            this.interpret(c, t, null);
        }
    }
    LSys.prototype.draw = function(t, context, W, H) {
        context.fillStyle = 'black';
        context.fillRect(0, 0, W, H);
        // Draw for size.
        t.reset();
        var size = [t.p.x, t.p.y, t.p.x, t.p.y];
        var i;
        var c;
        for (i = 0; this.sentence.length > i; i++) {
            c = this.sentence[i];
            this.interpret(c, t, context, size);
        }
        // Draw to show.
        var xsize = size[2] - size[0];
        var ysize = size[3] - size[1];
        var xscale = Math.abs(W / xsize);
        var yscale = Math.abs(H / ysize);
        var xmove = -size[0];
        var ymove = -size[1];
        context.scale(xscale, yscale);
        context.translate(xmove, ymove);
        t.reset();
        for (i = 0; this.sentence.length > i; i++) {
            c = this.sentence[i];
            this.interpret(c, t, context);
        }
    };
    LSys.prototype.findSize = function(t, size) {
        if (t.p.x < size[0]) {
            size[0] = t.p.x;
        }
        if (t.p.y < size[1]) {
            size[1] = t.p.y;
        }
        if (t.p.x > size[2]) {
            size[2] = t.p.x;
        }
        if (t.p.y > size[3]) {
            size[3] = t.p.y;
        }
    };
    Turtle.prototype.startNote = function() {
        var hsv = tinycolor(this.instrument).toHsv();
        this.event = new Silencio.Event();
        this.event.channel = hsv.h;
        this.event.time = this.p.x;
        this.event.key = -this.p.y;
        this.event.velocity = hsv.v;
        this.event.pan = Math.random();
    };
    Turtle.prototype.endNote = function(score) {
        this.event.end = this.p.x;
        if (this.event.duration > 0) {
            var event = this.event.clone();
            score.data.push(event);
        }
    };
    LSys.prototype.interpret = function(c, t, context, size) {
        //console.info('c:' + c + '\n');
        if (c === 'F') {
            if (typeof size === 'undefined') {
                t.startNote();
                t.go(context);
            } else {
                t.move();
            }
        } else if (c === 'f') t.move();
        else if (c === '+') t.turnRight();
        else if (c === '-') t.turnLeft();
        else if (c === '[') t.push();
        else if (c === ']') t.pop();
        else if (c === 'I') t.upInstrument();
        else if (c === 'i') t.downInstrument();
        else if (c === 'V') t.upVelocity();
        else if (c === 'v') t.downVelocity();
        else if (c === 'T') t.upTempo();
        else if (c === 't') t.downTempo();
        if (typeof size === 'undefined') {
            if (c === 'F') {
                t.endNote(this.score);
            }
            this.prior = c;
        } else {
            this.findSize(t, size);
        }
    };

    /**
    Generates scores by recursively applying a set of generating
    functions to a single initial musical event.
    This event can be considered to represent a cursor within a score.
    The generating functions may move this cursor around
    within the score, as if moving a pen, and may at any time write the
    current state of the cursor into the score, or write other events
    based, or not based, upon the cursor into the score.

    The generating functions may be lambdas. Generated notes must not
    be the same object as the cursor, but may be clones of the cusor,
    or entirely new objects.

    cursor          A Silencio.Event object that represents a position in a
                    musical score. This could be a note, a grain of sound, or
                    a control event.

    depth           The current depth of recursion. This must begin > 1.
                    For each recursion, the depth is decremented. Recursion
                    ends when the depth reaches 0.

    Returns the next position of the cursor, and optionally, a table of
    generated events.

    generator = function(cursor, depth);
    {cursor, events} = generator(cursor, depth);

    The algorithm is similar to the deterministic algorithm for computing the
    attractor of a recurrent iterated function systems. Instead of using
    affine transformation matrixes as the RIFS algorithm does, the current
    algorithm uses generating functions; but if each generating function
    applies a single affine transformation to the cursor, the current
    algorithm will in fact compute a RIFS.

    generators      A list of generating functions with the above signature.
                    Unlike RIFS, the functions need not be contractive.

    transitions     An N x N transition matrix for the N generating functions.
                    If entry [i][j] is 1, then if the current generator is the
                    ith, then the jth generator will be applied to the current
                    cursor after the ith; if the entry is 0, the jth generator
                    will not be applied to the current cursor after the ith.
                    In addition, each generator in the matrix must be reached
                    at some point during recursion.

    depth           The current depth of recursion. This must begin > 1.
                    For each recursion, the depth is decremented. Recursion
                    ends when the depth reaches 0.

    index           Indicates the current generating function, i.e. the
                    index-th row of the transition matrix.

    cursor          A Silencio.Event object that represents a position in a
                    musical score. This could be a note, a grain of sound, or
                    a control event.

    score           A Silencio.Score object that collects generated events.
    */

    function RecurrentResult(c) {
        this.cursor = c;
        this.events = [];
        return this;
    }

    function Recurrent(generators, transitions, depth, index, cursor, score) {
        depth = depth - 1;
        //console.info(string.format('Recurrent(depth: %d  index: %d  cursor: %s)', depth, index, cursor:__tostring()))
        if (depth === 0) {
            return;
        }
        var transitionsForThisIndex = transitions[index];
        for (var j = 0; j < transitionsForThisIndex.length; j++) {
            if (transitionsForThisIndex[j] == 1) {
                var result = generators[j](cursor.clone(), depth);
                for (var i = 0; i < result.events.length; i++) {
                    score.append(result.events[i].clone());
                }
                Recurrent(generators, transitions, depth, j, result.cursor.clone(), score);
            }
        }
    }

    /**
     * Like ES6 Map, but with value keys, which are represented by stringified
     * keys.
     */
    function ValueMap(make_key_) {
        this.map = new Map();
        this.make_key = this.make_key_;
        /*     Object.defineProperty(this,"size",{
         *         configurable: true,
         *         get: function() { return this.map.size; },
         *         set: function(value) { this.map.size = value; }
         *     });
         */
    }

    ValueMap.prototype.get_size = function() {
        return this.map.size;
    };

    ValueMap.prototype.set_size = function(count) {
        this.map.size = count;
    };

    ValueMap.prototype.clear = function() {
        this.map.clear();
    };

    ValueMap.prototype.delete = function(key) {
        return this.map.delete(this.make_key(key));
    };

    ValueMap.prototype.entries = function() {
        return this.map.entries();
    };

    ValueMap.prototype.forEach = function(callback, thisarg) {
        this.map.forEach(callback, thisarg);
    };

    ValueMap.prototype.get = function(key) {
        return this.map.get(this.make_key(key));
    };

    ValueMap.prototype.has = function(key) {
        return this.map.has(this.make_key(key));
    };

    ValueMap.prototype.keys = function() {
        return this.map.keys();
    };

    ValueMap.prototype.set = function(key, value) {
        this.map.set(this.make_key(key), value);
        return this;
    };

    ValueMap.prototype.values = function() {
        return this.map.values();
    };

    /**
     * Like ES6 Set, but treats elements as values, which are represented as
     * stringified values.
     */
    function ValueSet(make_key_) {
        this.map = new Map();
        this.make_key = make_key_;
        /*     Object.defineProperty(this,"size",{
         *         configurable: true,
         *         get: function() { return this.map.size; },
         *         set: function(value) { this.map.size = value; }
         *     });
         */
    }

    ValueSet.prototype.get_size = function() {
        return this.map.size;
    };

    ValueSet.prototype.set_size = function(count) {
        this.map.size = count;
    };

    ValueSet.prototype.add = function(value_) {
        let key_ = this.make_key(value_);
        this.map.set(key_, value_);
        return this;
    };

    ValueSet.prototype.clear = function() {
        this.map.clear();
    };

    ValueSet.prototype.delete = function(value) {
        return this.map.delete(this.make_key(value));
    };

    ValueSet.prototype.entries = function() {
        return this.map.entries();
    };

    ValueSet.prototype.forEach = function(callback, thisarg) {
        this.map.forEach(callback, thisarg);
    };

    ValueSet.prototype.has = function(value) {
        return this.map.has(this.make_key(value));
    };

    ValueSet.prototype.values = function() {
        return this.map.values();
    };

    if (typeof navigator !== 'undefined') {
        console.info('browser:  ' + navigator.appName);
        console.info('platform: ' + navigator.platform);
    }

    var Silencio = {
        eq_epsilon: eq_epsilon,
        gt_epsilon: gt_epsilon,
        lt_epsilon: lt_epsilon,
        ge_epsilon: ge_epsilon,
        le_epsilon: le_epsilon,
        modulo: modulo,
        Event: Event,
        Score: Score,
        Turtle: Turtle,
        LSys: LSys,
        RecurrentResult: RecurrentResult,
        Recurrent: Recurrent,
        saveToLocalFile: saveToLocalFile,
        restoreFromLocalFile: restoreFromLocalFile,
        saveDatGuiJson: saveDatGuiJson,
        restoreDatGuiJson: restoreDatGuiJson,
        ValueMap: ValueMap,
        ValueSet: ValueSet
    };
    // Node: Export function
    if (typeof module !== "undefined" && module.exports) {
        module.exports = Silencio;
    }
    // AMD/requirejs: Define the module
    else if (typeof define === 'function' && define.amd) {
        define(function() {
            return Silencio;
        });
    }
    // Browser: Expose to window
    else if (typeof window !== 'undefined') {
        window.Silencio = Silencio;
    } else {
        return Silencio;
    }

})();

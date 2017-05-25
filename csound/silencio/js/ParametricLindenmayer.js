/**
P A R A M E T R I C   L I N D E N M A Y E R   S Y S T E M

Copyright (C) 2014 by Michael Gogins

This software is licensed under the terms of the
GNU Lesser General Public License

Part of Silencio, an algorithmic music composition library for Csound.

This parametric Lindenmayer system for generating musical scores is defined
as follows. See http://hardlikesoftware.com/projects/lsystem/lsystem.html.
For the original definition of this type of system, see Przemyslaw
Prusinkiewicz and Aristid Lindenmayer, _The Algorithmic Beauty of Plants_
(New York: Springer Verlag, 1996 [1990]), pp. 40-50.

Name: JavaScript identifier.

Word: Text for a JavaScript expression consisting of a name, or a JavaScript
function call with either formal or actual parameters, terminated with a
semicolon, associated with a Command.

Production: A sequence of Words.

Command: A function that modifies the state of a Turtle; may be built-in or
user-defined. A Word that is not assigned a Command is associated with a default
identity Command.

Turtle: An abstract pen that writes a musical score by performing the Commands
in a Production.

Axiom: The initial Production of a Lindenmayer system, in which any parameters
are actual.

Rule: A triple [Word, Condition, Production] in which any parameters may be
actual or formal, or indeed any JavaScript expression.

Lindenmayer system: A set of Words, a set of associated Commands, an Axiom,
one or more Rules, and a finite number N of Iterations. For each Word in the
Axiom, the Axiom Word is replaced from the Rules; if the Axiom Word Name
matches the Rule Word Name, and the Axiom Word parameters number the same as
the Rule Word parameters, then if the Condition evaluates as true, the Rule
Production replaces the Axiom Word after evaluating each Production Word's
actual parameter expressions after substituting the Axiom Word's actual
parameter values for any formal parameter names in the Production Word's actual
parameter expressions; if as false, there is no Production; otherwise, the
Axiom Word replaces itself. The resulting Production is taken as the Axiom for
the next iteration. This is repeated N times. Then the final Production,
consisting of a possibly long string of Words with only actual parameters, is
evaluated.

Evaluation: The Command of each Word in the final Production is evaluated
using the Turtle state and the Command with actual parameters, possibly
causing the Turtle to write a musical score.

Note: The formal parameter names of the Word must be the same as the formal
parameter names (after 'lsystem' and 'turtle') of the Word's Command (which is
not a class member of the Word). The actual parameters of the Word may be
values or unevaluated expressions; when the Command is called, the actual
parameter expressions are evaluated using the actual parameter values of the
parent Word as the values of the unevaluated parameters in the actual
parameter expressions.

Example: Note(i,t,d,k,v,p) is replaced by Note(i*2,t^1.1,d-1,k+3,v*.9,p=Math.random().

*/
(function() {

    // All JavaScript dependencies of ParametricLindenmayer.js:
    // var Silencio = require("Silencio");
    // var ChordSpace = require("ChordSpace");
    // var numeric = require("numeric");
    // var sprintf = require("sprintf");

    var ParametricLindenmayer = {};

    ParametricLindenmayer.Turtle = function(note_, chord_, modality_) {
        this.step = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1];
        this.scale = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
        if (typeof note_ === 'undefined') {
            this.note = new Silencio.Event();
        } else {
            this.note = note_;
        }
        if (typeof chord_ === 'undefined') {
            this.chord = new ChordSpace.Chord();
        } else {
            this.chord = chord_.clone();
        }
        if (typeof modality_ === 'undefined') {
            this.modlity_ = new ChordSpace.Chord();
        } else {
            this.modality = modality_.clone();
        }
    };

    ParametricLindenmayer.Turtle.prototype.clone = function() {
        clone_ = new ParametricLindenmayer.Turtle();
        clone_.step = this.step.slice();
        clone_.scale = this.scale.slice();
        clone_.note = this.note.clone();
        clone_.chord = this.chord.clone();
        clone_.modality = this.modality.clone();
        return clone_;
    };

    // Creates a Word with a name, a list of actual parameter expressions,
    // an empty list of actual parameter values, and a Production-matching key
    // from the text of the Word.

    ParametricLindenmayer.Word = function(text) {
        this.text = text;
        this.name = /s*([^(]*)/.exec(text)[1].trim();
        this.actual_parameter_expressions = [];
        var opening_parenthesis = text.indexOf('(');
        var ending_parenthesis = text.lastIndexOf(')');
        if (opening_parenthesis != -1 && ending_parenthesis != -1) {
            this.actual_parameter_expressions = text.substring(opening_parenthesis + 1, ending_parenthesis).split(',');
        }
        this.key = this.name + '(' + this.actual_parameter_expressions.length + ')';
        this.actual_parameter_values = [];
        for (var i = 0; i < this.actual_parameter_expressions.length; i++) {
            this.actual_parameter_values.push(null);
        }
    };

    ParametricLindenmayer.Word.prototype.clone = function() {
        clone_ = new ParametricLindenmayer.Word('');
        clone_.text = this.text;
        clone_.key = this.key;
        clone_.actual_parameter_expressions = this.actual_parameter_expressions.slice();
        clone_.actual_parameter_values = this.actual_parameter_values.slice();
        return clone_;
    };

    ParametricLindenmayer.Word.prototype.rewrite = function(lsystem, current_production) {
        var rule = lsystem.rule_for_word(this);
        if (typeof rule === 'undefined') {
            var rule_less = this.clone();
            lsystem.evaluate_actual_parameter_expressions(null, rule_less);
            current_production.push(rule_less);
        } else {
            var productions_for_conditions = rule.productions_for_conditions;
            for (var condition in productions_for_conditions) {
                if (productions_for_conditions.hasOwnProperty(condition)) {
                    var production = productions_for_conditions[condition];
                    if (lsystem.evaluate_condition_expression(this, condition) === true) {
                        for (var i = 0; i < production.length; i++) {
                            var child = production[i].clone();
                            lsystem.evaluate_actual_parameter_expressions(this, child);
                            current_production.push(child);
                        }
                    }
                } else {
                    console.log('Condition "false", skipping rewriting of ' + this.text + '.');
                }
            }
        }
    };

    ParametricLindenmayer.Rule = function(word_, condition_, production_) {
        if (typeof word_ === typeof '') {
            this.word = new ParametricLindenmayer.Word(word_);
        } else {
            this.word = word_.clone();
        }
        this.productions_for_conditions = {};
        this.add_condition(condition_, production_);
    };

    ParametricLindenmayer.Rule.prototype.add_condition = function(condition_, production_) {
        var production = [];
        var words = production_.split(';');
        for (var i = 0; i < words.length; i++) {
            var word = words[i];
            if (typeof word !== 'undefined' && word !== null) {
                if (word.length > 0) {
                    production.push(new ParametricLindenmayer.Word(word));
                }
            }
        }
        this.productions_for_conditions[condition_] = production;
    };

    ParametricLindenmayer.PLSystem = function() {
        this.commands_for_words = {};
        this.formal_parameters_for_commands = {};
        this.axiom = [];
        this.rules_for_words = {};
        this.turtle = new ParametricLindenmayer.Turtle();
        this.identity_command = function(lsystem, turtle_) {
            return turtle_;
        };
        // Default Commands.
        this.add_command('Assign(dimension, value)', function(lsystem, turtle, dimension, value) {
            turtle.note.data[dimension] = value;
            return turtle;
        });
        this.add_command('Scale(dimension, value)', function(lsystem, turtle, dimension, value) {
            turtle.scale[dimension] = value;
            return turtle;
        });
        this.add_command('Move(dimension, value)', function(lsystem, turtle, dimension, value) {
            turtle.note.data[dimension] += value;
            return turtle;
        });
        this.add_command('Steps(s)', function(lsystem, turtle, s) {
            var step_ = numeric.mul(turtle.step, s);
            step_ = numeric.mul(step_, turtle.scale);
            turtle.note.data = numeric.add(turtle.note.data, step_);
            return turtle;
        });
        this.add_command('Step()', function(lsystem, turtle) {
            var scaled_step = numeric.mul(turtle.step, turtle.scale);
            turtle.note.data = numeric.add(turtle.note.data, scaled_step);
            return turtle;
        });
        // http://wscg.zcu.cz/wscg2004/Papers_2004_Short/N29.pdf: main rotations.
        this.add_command('Turn(from_axis, to_axis, angle)', function(lsystem, turtle, from_axis, to_axis, angle) {
            var rotation = numeric.identity(turtle.step.length);
            rotation[from_axis][from_axis] = Math.cos(angle);
            rotation[from_axis][to_axis] = -Math.sin(angle);
            rotation[to_axis][from_axis] = Math.sin(angle);
            rotation[to_axis][to_axis] = Math.cos(angle);
            // The step is a row vector, not a column vector.
            turtle.step = numeric.dotVM(turtle.step, rotation);
            return turtle;
        });
        this.add_command('Assign(i,t,d,k,v,p)', function(lsystem, turtle, i, t, d, k, v, p) {
            turtle.note.channel = i;
            turtle.note.time = t;
            turtle.note.duration = d;
            turtle.note.key = k;
            turtle.note.velocity = v;
            turtle.note.pan = p;
            return turtle;
        });
        this.add_command('Move(i,t,d,k,v,p)', function(lsystem, turtle, i, t, d, k, v, p) {
            turtle.note.channel += (i * turtle.scale[3]);
            turtle.note.time = (turtle.note.time + (t * turtle.scale[0]));
            turtle.note.duration += (d * turtle.scale[1]);
            turtle.note.key += (k * turtle.scale[4]);
            turtle.note.velocity += (v * turtle.scale[5]);
            turtle.note.pan += (p * turtle.scale[6]);
            return turtle;
        });
        this.add_command('Note(i,t,d,k,v,p)', function(lsystem, turtle, i, t, d, k, v, p) {
            turtle.note.channel = i;
            turtle.note.time = t;
            turtle.note.duration = d;
            turtle.note.key = k;
            turtle.note.velocity = v;
            turtle.note.pan = p;
            lsystem.score.append(turtle.note.clone());
            return turtle;
        });
        this.add_command('Note()', function(lsystem, turtle) {
            lsystem.score.append(turtle.note.clone());
            return turtle;
        });
        this.add_command('Push()', function(lsystem, turtle) {
            lsystem.turtle_stack.push(turtle.clone());
            return turtle;
        });
        this.add_command('Pop()', function(lsystem, turtle) {
            turtle = lsystem.turtle_stack.pop();
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.add_command('T(n)', function(lsystem, turtle, n) {
            turtle.chord = turtle.chord.T(n);
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.add_command('I(c)', function(lsystem, turtle, c) {
            turtle.chord = turtle.chord.I(c);
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.add_command('K()', function(lsystem, turtle) {
            turtle.chord = turtle.chord.K();
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.add_command('Q(n)', function(lsystem, turtle, n) {
            turtle.chord = turtle.chord.Q(n, turtle.modality);
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.add_command('J(n,m)', function(lsystem, turtle, n, m) {
            var inversions = turtle.chord.J(n);
            if (inversions.length > m) {
                turtle.chord = inversions[m];
            }
            lsystem.chords_for_times[turtle.note.time] = turtle.chord.clone();
            return turtle;
        });
        this.reset();
    };

    ParametricLindenmayer.PLSystem.prototype.reset = function(text) {
        this.iteration = 0;
        this.turtle_stack = [];
        this.score = new Silencio.Score();
        this.chords_for_times = {};
    };

    ParametricLindenmayer.evaluate_with_minimal_scope = function(code) {
        var result = eval(code);
        return result;
    };

    ParametricLindenmayer.PLSystem.prototype.evaluate_actual_parameter_expressions = function(parent_word, child_word) {
        try {
            var prologue = 'var iteration = ' + this.iteration + ';';
            if (parent_word !== null) {
                var formal_parameters = this.formal_parameters_for_commands[child_word.key];
                if (typeof formal_parameters !== 'undefined') {
                    for (var i = 0; i < formal_parameters.length; i++) {
                        var formal_parameter_name = formal_parameters[i];
                        var parent_actual_parameter_value = parent_word.actual_parameter_values[i];
                        if (parent_actual_parameter_value === null) {
                            var parent_word_parameter_expression = parent_word.actual_parameter_expressions[i];
                            parent_actual_parameter_value = ParametricLindenmayer.evaluate_with_minimal_scope(parent_word_parameter_expression);
                        }
                        var value_assignment = 'var ' + formal_parameter_name + ' = ' + parent_actual_parameter_value + ';';
                        prologue += value_assignment;
                    }
                }
            }
            for (var parameterIndex = 0; parameterIndex < child_word.actual_parameter_expressions.length; parameterIndex++) {
                var child_word_actual_parameter_expression = child_word.actual_parameter_expressions[parameterIndex];
                child_word.actual_parameter_values[parameterIndex] = ParametricLindenmayer.evaluate_with_minimal_scope(prologue + child_word_actual_parameter_expression);
            }
        } catch (err) {
            console.log(err.stack);
            throw err;
        }
    };

    ParametricLindenmayer.PLSystem.prototype.evaluate_condition_expression = function(parent_word, condition) {
        try {
            var prologue = 'var iteration = ' + this.iteration + ';';
            var formal_parameters = this.formal_parameters_for_commands[parent_word.key];
            if (typeof formal_parameters !== 'undefined') {
                for (var i = 0; i < formal_parameters.length; i++) {
                    var formal_parameter_name = formal_parameters[i];
                    var parent_actual_parameter_value = parent_word.actual_parameter_values[i];
                    if (parent_actual_parameter_value === null) {
                        var parent_word_actual_parameter_expression = parent_word.actual_parameter_expressions[i];
                        parent_actual_parameter_value = ParametricLindenmayer.evaluate_with_minimal_scope(parent_word_actual_parameter_expression);
                    }
                    var value_assignment = 'var ' + formal_parameter_name + ' = ' + parent_actual_parameter_value + ';';
                    prologue += value_assignment;
                }
            }
            return ParametricLindenmayer.evaluate_with_minimal_scope(prologue + condition);
        } catch (err) {
            console.log(err.stack);
            throw err;
        }
    };

    ParametricLindenmayer.PLSystem.prototype.set_axiom = function(text) {
        this.axiom.length = 0;
        var words = text.split(';');
        for (var i = 0; i < words.length; i++) {
            var word = words[i];
            if (word.length > 0) {
                this.axiom.push(new ParametricLindenmayer.Word(word));
            }
        }
    };

    ParametricLindenmayer.PLSystem.prototype.set_turtle = function(turtle_) {
        this.turtle = turtle_;
    };

    ParametricLindenmayer.PLSystem.prototype.add_command = function(word_text, command) {
        var word = new ParametricLindenmayer.Word(word_text);
        this.commands_for_words[word.key] = command;
        var formal_parameters = this.parameters_from_function_declaration(word_text);
        this.formal_parameters_for_commands[word.key] = formal_parameters;
    };

    ParametricLindenmayer.PLSystem.prototype.add_rule = function(word_, condition, production) {
        var word = new ParametricLindenmayer.Word(word_);
        var rule = this.rule_for_word(word);
        if (typeof rule === 'undefined') {
            rule = new ParametricLindenmayer.Rule(word, condition, production);
            this.rules_for_words[rule.word.key] = rule;
        } else {
            rule.add_condition(condition, production);
        }
    };

    ParametricLindenmayer.PLSystem.prototype.command_for_word = function(word) {
        var command = this.commands_for_words[word.key];
        if (typeof command === 'undefined') {
            command = this.identity_command;
        }
        return command;
    };

    ParametricLindenmayer.PLSystem.prototype.invoke_command = function(word, turtle) {
        var actual_parameter_values = word.actual_parameter_values.slice();
        var command = this.command_for_word(word);
        actual_parameter_values.splice(0, 0, this, turtle);
        return command.apply(word, actual_parameter_values);
    };

    ParametricLindenmayer.PLSystem.prototype.generate = function(iterations) {
        if (typeof iterations !== 'undefined') {
            this.iterations = iterations;
        }
        var initial_production = this.axiom;
        var current_production = [];
        var wordIndex;
        for (this.iteration = 0; this.iteration < this.iterations; this.iteration++) {
            current_production.length = 0;
            for (wordIndex = 0; wordIndex < initial_production.length; wordIndex++) {
                var parent = initial_production[wordIndex].clone();
                parent.rewrite(this, current_production);
            }
            initial_production = current_production.slice();
        }
        var working_turtle = this.turtle.clone();
        for (wordIndex = 0; wordIndex < current_production.length; wordIndex++) {
            var word = current_production[wordIndex];
            working_turtle = this.invoke_command(word, working_turtle);
        }
    };

    ParametricLindenmayer.PLSystem.prototype.rule_for_word = function(word) {
        return this.rules_for_words[word.key];
    };

    ParametricLindenmayer.PLSystem.prototype.parameters_from_function_declaration = function(str) {
        var args = /\(\s*([^)]+?)\s*\)/.exec(str);
        if (args === null) {
            return [];
        }
        if (args[1]) {
            args = args[1].split(/\s*,\s*/);
        }
        return args;
    };

    ParametricLindenmayer.PLSystem.prototype.function_name_from_word = function(word) {
        var function_name = /function ([^(]*)/.exec(word)[1];
        return function_name;
    };

    ParametricLindenmayer.PLSystem.prototype.words_from_production = function(production) {
        var words = production.split(';');
        return words;
    };

    /**
     * Conforms the pitch of each event in this,
     * to the closest pitch-class in the chord that applies to the event's time.
     */
    ParametricLindenmayer.PLSystem.prototype.conformToChords = function() {
        var times = [];
        for (var tyme in this.chords_for_times) {
            if (this.chords_for_times.hasOwnProperty(tyme)) {
                times.push(parseFloat(tyme));
            }
        }
        times = times.sort(function(a, b) {
            return a - b;
        });
        var length_ = times.length;
        times.push(this.score.getEnd());
        for (var i = 0; i < length_; i++) {
            var begin = times[i];
            var end = times[i + 1];
            var chord = this.chords_for_times[begin];
            ChordSpace.apply(this.score, chord, begin, end, false);
        }
    };

    //////////////////////////////////////////////////////////////////////////////
    // EXPORTS
    //////////////////////////////////////////////////////////////////////////////

    // Node: Export function
    if (typeof module !== "undefined" && module.exports) {
        module.exports = ParametricLindenmayer;
    }
    // AMD/requirejs: Define the module
    else if (typeof define === 'function' && define.amd) {
        define(function() {
            return ParametricLindenmayer;
        });
    }
    // Browser: Expose to window
    else {
        window.ParametricLindenmayer = ParametricLindenmayer;
    }

})();
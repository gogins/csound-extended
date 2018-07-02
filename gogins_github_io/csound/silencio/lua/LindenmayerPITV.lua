local Silencio = require("Silencio")
local ChordSpace = require("ChordSpace")

LindenmayerPITV = {}

function LindenmayerPITV.help()
print [[

L I N D E N M A Y E R   P I T V

Copyright (C) 2010 by Michael Gogins
This software is licensed under the terms
of the GNU Lesser General Public License.

The LindenmayerPITV class implements a context-free Lindenmayer system that
performs operations on four additive cyclical groups P, I, T, and V which
represent respectively set-class, inversion, transposition, and octavewise
revoicing of chords. Chord duration (d) and dynamic level or MIDI velocity
(v) also are represented.

An earlier version of this class was found too complex for use. This version
is intended to expose the basic symmetries of chord space to programmatic
manipulation as clearly and simply as possible.

The turtle state consists of six numbers P, I, T, V, d, and v. These are the
targets of operations. Time is defined implicity; when the Lindenmayer
system is evaluated, score time is accmulated from the sequence of chords
and their durations.

All turtle commands are of the form target [operation [operand] ].

Operations are arithmetical +, -, and assignment =.

Operands are signed real numbers. The results of operations on P, I, T, and V
wrap around their equivalence class. The results of operations on d and v
do not wrap around and may need to be rescaled to be musically meaningful.

The syntax of turtle commands (target:operation:operand) is:

P o n       Operate on the zero-based index of set-class (prime form).
P = name    Assign the set-class index from a Jazz-style chord name. This may
            involve replacing the chord with its equivalent under
            transpositional and/or inversional equivalence.
I o n       Operate on the zero-based index of inversional equivalence, where
            inversion is reflection in chord {n,..n} on the unison diagonal
            of chord space.
T o n       Operate on the zero-based index of transposition within octave
            equivalence.
V o n       Operate on the zero-based index of octavewise revoicings within
            the permitted range of pitches.
d o n       Operate on the duration of the chord.
v o n       Operate on the loudness of the chord (represented as MIDI velocity
            in the interval [0, 127].
C           Write the current turtle state into the score as a chord.
L           Write the current turtle state into the score as a chord; or
            rather, use a geometric voice-leading algorithm to find the PITV
            that has the PIT of the current chord which is as close as possible
            to the PITV of the prior chord.
[           Push the current state of the turtle onto the top of a stack.
]           Pop the current state of the turtle off the top of the stack.
]]
end

Turtle = {}

function Turtle:new(o)
    o = o or {t = 0, d = 0, v = 0, P = 0, I = 0, T = 0, V = 0}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Turtle:__tostring()
    return string.format('t: %9.4f  d: %9.4f  v: %9.4f  P: %9.4f  I: %9.4f  T: %9.4f  V: %9.4f', self.t, self.d, self.v, self.P, self.I, self.T, self.V)
end

function LindenmayerPITV:new(o)
    if not o then
        o = {}
        o.score = Score:new()
        o.axiom = ''
        o.rules = {}
        o.turtle = Turtle:new()
        o.priorTurtle = self.turtle
        o.stack = {}
        o.iterations = 3
        o.tieOverlaps = true
        o.avoidParallelFifths = false
        o.currentProduction = ''
        o.priorProduction = ''
        o.octaves = 5
        o.targets = {}
        o.targets['t'] = self.target_t
        o.targets['d'] = self.target_d
        o.targets['v'] = self.target_v
        o.targets['P'] = self.target_P
        o.targets['I'] = self.target_I
        o.targets['T'] = self.target_T
        o.targets['V'] = self.target_V
        o.targets['K'] = self.target_K
        o.targets['C'] = self.target_C
        o.targets['L'] = self.target_L
        o.targets['['] = self.target_push
        o.targets[']'] = self.target_pop
        o.operations = {}
        o.operations['='] = self.operation_assign
        o.operations['+'] = self.operation_add
        o.operations['-'] = self.operation_subtract
        o.chordSpaceGroup = ChordSpaceGroup:new()
        o.duration = 120
        o.bass = 36
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

function LindenmayerPITV:initialize(voices, octaves, g)
    print('LindenmayerPITV:initialize...')
    self.chordSpaceGroup:initialize(voices, octaves, g)
end

function LindenmayerPITV:operation_assign(y, x)
    y = x
    return y
end

function LindenmayerPITV:operation_add(y, x)
    y = y + x
    return y
end

function LindenmayerPITV:operation_subtract(y, x)
    y = y - x
    return y
end

function LindenmayerPITV:target_t(target, operation, operand)
    local function_ = self.operations[operation]
    self.turtle.t = function_(self, self.turtle.t, operand)
end

function LindenmayerPITV:target_d(target, operation, operand)
    local function_ = self.operations[operation]
    self.turtle.d = function_(self, self.turtle.d, operand)
end

function LindenmayerPITV:target_v(target, operation, operand)
    local function_ = self.operations[operation]
    self.turtle.v = function_(self, self.turtle.v, operand)
end

function LindenmayerPITV:target_P(target, operation, operand)
    local chord_ = nil
    if type(operand) == 'string' then
        print(operand)
        chord_ = ChordSpace.chordsForNames[operand]
        print(chord_, chord_:iseOP())
        local p, i, t, v = self.chordSpaceGroup:fromChord(chord_)
        self.turtle.P = p
    else
        local function_ = self.operations[operation]
        self.turtle.P = function_(self, self.turtle.P, operand) % self.chordSpaceGroup.countP
    end
end

function LindenmayerPITV:target_I(target, operation, operand)
    if type(operand) == 'string' then
        local chord = ChordSpace.chordsForNames[operand]
        local p, i, t, v = chordSpaceGroup.fromChord(chord)
        self.turtle.I = i
    else
        local function_ = self.operations[operation]
        self.turtle.I = function_(self, self.turtle.I, operand) % self.chordSpaceGroup.countI
    end
end

function LindenmayerPITV:target_T(target, operation, operand)
    if type(operand) == 'string' then
        local chord = ChordSpace.chordsForNames[operand]
        local p, i, t, v = chordSpaceGroup.fromChord(chord)
        self.turtle.T = t
    else
        local function_ = self.operations[operation]
        self.turtle.T = function_(self, self.turtle.T, operand) % self.chordSpaceGroup.countT
    end
end

function LindenmayerPITV:target_V(target, operation, operand)
    if type(operand) == 'string' then
        local chord = ChordSpace.chordsForNames[operand]
        local p, i, t, v = chordSpaceGroup.fromChord(chord)
        self.turtle.V = v
    else
        local function_ = self.operations[operation]
        self.turtle.V = function_(self, self.turtle.V, operand) % self.chordSpaceGroup.countV
    end
end

function LindenmayerPITV:target_K(target, operation, operand)
    local p = math.floor(self.turtle.P + 0.5)
    local i = math.floor(self.turtle.I + 0.5)
    local t = math.floor(self.turtle.T + 0.5)
    local v = math.floor(self.turtle.V + 0.5)
    local chord = self.chordSpaceGroup:toChord(p, i, t, v)
    local p, i, t, v = self.chordSpaceGroup:fromChord(chord:K())
    self.turtle.P = p
    self.turtle.I = i
    self.turtle.T = t
end

function LindenmayerPITV:target_C(target, operation, operand)
    if self.chord == nil then
        self.onset = 0
    else
        self.priorChord = self.chord
    end
    local p = math.floor(self.turtle.P + 0.5)
    local i = math.floor(self.turtle.I + 0.5)
    local t = math.floor(self.turtle.T + 0.5)
    local v = math.floor(self.turtle.V + 0.5)
    self.chord = self.chordSpaceGroup:toChord(p, i, t, v)
    for i = 1, #self.chord do
        self.chord.channel[i] = i - 1
    end
    self.chord:setDuration(self.turtle.d)
    self.chord:setVelocity(self.turtle.v)
    ChordSpace.insert(self.score, self.chord, self.onset)
    print(string.format('C: %s %s', tostring(self.chord), self.chord:eOP():name()))
    self.onset = self.onset + self.turtle.d
end

function LindenmayerPITV:target_L(target, operation, operand)
    if self.chord == nil then
        self.onset = 0
    else
        self.priorChord = self.chord
    end
    local p = math.floor(self.turtle.P + 0.5)
    local i = math.floor(self.turtle.I + 0.5)
    local t = math.floor(self.turtle.T + 0.5)
    local v = math.floor(self.turtle.V + 0.5)
    self.chord = self.chordSpaceGroup:toChord(p, i, t, v)
    if self.priorChord ~= nil then
        self.chord = ChordSpace.voiceleadingClosestRange(self.priorChord, self.chord, self.octaves * ChordSpace.OCTAVE, true)
    end
    for i = 1, #self.chord do
        self.chord.channel[i] = i - 1
    end
    self.chord:setDuration(self.turtle.d)
    self.chord:setVelocity(self.turtle.v)
    ChordSpace.insert(self.score, self.chord, self.onset)
    print(string.format('L: %s %s', tostring(self.chord), self.chord:eOP():name()))
    self.onset = self.onset + self.turtle.d
end

function LindenmayerPITV:target_push(target, operation, operand)
    table.insert(self.stack, Silencio.clone(self.turtle))
end

function LindenmayerPITV:target_pop(target, operation, operand)
    self.turtle = table.remove(self.stack)
end

-- Beginning with the axiom,
-- the current production is created by replacing each word
-- in the prior production either with itself, or with its replacement
-- from the dictionary of rules.

function LindenmayerPITV:produce()
    print('LindenmayerPITV:produce...')
    print('axiom:', self.axiom)
    for iteration = 1, self.iterations do
        print(string.format('Iteration: %4d', iteration))
        if iteration == 1 then
            self.priorProduction = self.axiom
        else
            self.priorProduction = self.currentProduction
        end
        self.currentProduction = {}
        local words = Silencio.split(self.priorProduction, ' ')
        for index, word in pairs(words) do
            local replacement = self.rules[word]
            if replacement == nil then
                table.insert(self.currentProduction, word)
            else
                table.insert(self.currentProduction, replacement)
            end
        end
        self.currentProduction = table.concat(self.currentProduction, ' ')
    end
end

function LindenmayerPITV:parseCommand(command)
    local target = string.sub(command, 1, 1)
    if target == '[' or target == ']' or target == 'C' or target == 'L' then
        return target, nil, nil
    end
    return target, string.sub(command, 2, 2), string.sub(command, 3, -1)
end

function LindenmayerPITV:interpret()
    print('LindenmayerPITV:interpret...')
    local commands = Silencio.split(self.currentProduction, ' ')
    for index, command in ipairs(commands) do
        target, opcode, operand = self:parseCommand(command)
        local function_ = self.targets[target]
        if function_ ~= nil then
            local value = nil
            if operand ~= nil then
                value = tonumber(operand)
                if value == nil then
                    value = operand
                end
            end
            function_(self, target, opcode, value)
            print(self.turtle)
        end
    end
end

function LindenmayerPITV:generate()
    print('LindenmayerPITV:generate...')
    self:produce()
    self:interpret()
    if self.tieOverlaps == true then
        print('LindenmayerPITV:tieing notes...')
        self.score:tieOverlaps(true)
    end
    self.score:setDuration(self.duration)
    local bass, range = self.score:findScale(KEY)
    self.score:setScale(KEY, self.bass, range)
end

return LindenmayerPITV

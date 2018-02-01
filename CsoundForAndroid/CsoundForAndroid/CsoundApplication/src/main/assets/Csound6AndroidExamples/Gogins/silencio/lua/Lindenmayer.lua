local Silencio = require("Silencio")
local ChordSpace = require("ChordSpace")
--local Som = require("Som")

Lindenmayer = {}

function Lindenmayer.help()
print [[
L I N D E N M A Y E R

Copyright (C) 2010 by Michael Gogins
This software is licensed under the terms 
of the GNU Lesser General Public License.

The Lindenmayer class implements a context-free Lindenmayer system that 
performs operations on chords, voicings of chords, arpeggiations of chords,
and individual voices of chords. 

TODO: If not already possible, a legato command, to increase or decrease
the overlap of voicings of successive chords.

The turtle commands have the following effective combinations of parts
(a colon without a part is permitted if the part is not used,
but the colons are always required except for trailing colons;
equivalence classes may be combined, but only in the order given):

STMCVAGdckvp

Target   :Operation :Equivalence :Operand :Index
S        :[]
MC       :PLRDK     :~ORPTI
MC       :=TIQ      :~ORPTI      :x
CA       :WL        :~ORPTI:     :      
I        :WL        :~ORPTI:     :        :i
dckvp    :=*/+-     :            :x       :i
VAGtdcvp :=*/+-     :            :x

Targets:
    S   Entire turtle state.
    M   Modality.
    C   Chord.
    V   Voicing of chord.
    A   Arpeggiation of chord.
    G   Interval size (1 = 12TET).
    t   Time.
    d   Duration.
    c   Channel.
    k   Pitch of individual voice (MIDI key).
    v   Loudness (MIDI velocity).
    p   Pan.
Operations:
    [   Push state onto stack.
    ]   Pop state from stack.
    P   Parallel Riemannian operation on chord (only guaranteed for major/minor triads).
    L   Lettonwechsel or leading-tone exchange Riemannian operation (only guaranteed for major/minor triads).
    R   Relative Riemannian operation (only guaranteed for major/minor triads).
    D   Dominant Riemannian operation.
    W   Write target C or I to score.
    V   Write target C or I that is the closest voice-leading from the prior state of the target.
    T   Transpose by operand semitones.
    I   Invert around operand semitones.
    K   Invert in context of modality.
    Q   Transpose in context of modality.
    =   Assign operand.
    *   Multiply by operand.
    /   Divide by operand.
    +   Add operand.
    -   Subtract operand.
Equivalence classes:
    ~   No equivalence class (same as blank).
    O   Order.
    R   Range.
    P   Octave (pitch-class).
    T   Transposition.
    I   Inversion.
]]
end

Turtle = {}

function Turtle:new(o)
    o = o or {modality = Chord:new(), chord = Chord:new(), voicing = 0, arpeggiation = 0, onset = 1, channel = 1, pan = 1, octaves = 3, intervalSize = 1}
    o.range = o.octaves * ChordSpace.OCTAVE
    setmetatable(o, self)
    self.__index = self
    return o
end

function Turtle:__tostring()
    local text = string.format('C: %s  M: %s  voicing: %f  arpeggiation: %f  onset: %f  channel: %f  loudness: %f  pan: %f', 
    tostring(self.chord), 
    tostring(self.modality), 
    self.voicing, 
    self.arpeggiation, 
    self.onset, 
    self.chord:getChannel() or -1,
    self.chord:getVelocity() or -1,
    self.chord:getPan() or -1)
    return text
end

function Lindenmayer:new(o)
    if not o then
        o = {}
        o.score = Score:new()
        o.axiom = ''
        o.rules = {}
        o.turtle = Turtle:new()
        o.priorTurtle = self.turtle
        o.stack = {}
        o.iterations = 3
        o.merge = true
        o.tieOverlaps = true
        o.avoidParallelFifths = false
        o.rescaleTimes = false
        o.currentProduction = ''
        o.priorProduction = ''
        o.octaves = 3
        o.actions = {}
        o.actions['['] = self.actionPush
        o.actions[']'] = self.actionPop
        o.actions['P'] = self.actionParallel
        o.actions['L'] = self.actionLettonwechsel
        o.actions['R'] = self.actionRelative
        o.actions['D'] = self.actionDominant
        o.actions['W'] = self.actionWrite
        o.actions['V'] = self.actionWriteVoiceleading
        o.actions['T'] = self.actionTranspose
        o.actions['I'] = self.actionInvert
        o.actions['K'] = self.actionContexualInversion
        o.actions['Q'] = self.actionContextualTransposition
        o.actions['='] = self.actionAssign
        o.actions['*'] = self.actionMultiply
        o.actions['/'] = self.actionDivide
        o.actions['+'] = self.actionAdd
        o.actions['-'] = self.actionSubtract
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

function Lindenmayer:equivalenceClass(chord, equivalence)
    if equivalence == 'R' then
        return chord:eR(self.octaves * OCTAVE)
    end
    if equivalence == 'O' then
        return chord:eO()
    end
    if equivalence == 'P' then
        return chord:eP()
    end
    if equivalence == 'T' then
        return chord:eT()
    end
    if equivalence == 'I' then
        return chord:eI()
    end
    if equivalence == 'RP' then
        return chord:eRP(self.octaves * ChordSpace.OCTAVE)
    end
    if equivalence == 'OP' then
        return chord:eOP()
    end
    if equivalence == 'OT' then
        return chord:eOT()
    end
    if equivalence == 'OI' then
        return chord:eOI()
    end
    if equivalence == 'OPT' then
        return chord:eOPTT()
    end
    if equivalence == 'OPI' then
        return chord:eOPI()
    end
    if equivalence == 'OPTI' then
        return chord:eOPTTI()
    end
end

function Lindenmayer:E(target, opcode, equivalence, operand, index)
    if target == 'C' or target == 'I' then
        self.turtle.chord = self:equivalenceClass(self.turtle.chord, equivalence)
    end
    if target == 'M' then
        self.turtle.modality = self:equivalenceClass(self.turtle.modality, equivalence)
    end
end

function Lindenmayer:actionPush(target, opcode, equivalence, operand, index)
    table.insert(self.stack, Silencio.clone(self.turtle))
end

function Lindenmayer:actionPop(target, opcode, equivalence, operand, index)
    self.turtle = table.remove(self.stack)
end

function Lindenmayer:actionParallel(target, opcode, equivalence, operand, index)
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:nrP()
    end
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:nrP()
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionLettonwechsel(target, opcode, equivalence, operand, index)
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:nrL()
    end
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:nrL()
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionRelative(target, opcode, equivalence, operand, index)
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:nrR()
    end
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:nrR()
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionDominant(target, opcode, equivalence, operand, index)
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:nrD()
    end
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:nrD()
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionWrite(target, opcode, equivalence, operand, index)
    local chord = self.turtle.chord:clone()    
    print('C Pre: ', chord, self.turtle.voicing)
    chord = chord:v(self.turtle.voicing)
    if target == 'C' then
        chord = self:equivalenceClass(chord, 'RP')
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration()
        ChordSpace.insert(self.score, chord, self.turtle.onset, self.turtle.chord:getDuration() + 0.001, self.turtle.channel, self.turtle.pan)
    end
    if target == 'I' then
        chord = self:equivalenceClass(chord, 'RPI')
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration()
        ChordSpace.insert(self.score, chord, self.turtle.onset, self.turtle.chord:getDuration() + 0.001, self.turtle.channel, self.turtle.pan)
    end
    if target == 'A' then
        local p, v
        p, v, chord = chord:a(self.turtle.arpeggiation)
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration(v)
        chord = self:equivalenceClass(chord, 'RP')
        local note = chord:note(v, self.turtle.onset, self.turtle.duration, self.turtle.channel, self.turtle.pan)
        self.score[#self.score + 1] = note
    end
    print('C Post:', chord)
    print()
    self.priorChord = chord
end

function Lindenmayer:actionWriteVoiceleading(target, opcode, equivalence, operand, index)
    if self.priorChord == nil then
        return self:actionWrite(target, opcode, equivalence, operand, index)
    end
    local chord = self.turtle.chord:clone()    
    print('V Pre: ', chord, self.turtle.voicing)
    chord = ChordSpace.voiceleadingClosestRange(self.priorChord, chord, self.octaves * ChordSpace.OCTAVE, true)
    if target == 'C' then
        chord = self:equivalenceClass(chord, 'RP')
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration()
        ChordSpace.insert(self.score, chord, self.turtle.onset, self.turtle.chord:getDuration() + 0.001, self.turtle.channel, self.turtle.pan)
    end
    if target == 'I' then
        chord = self:equivalenceClass(chord, 'RPI')
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration()
        ChordSpace.insert(self.score, chord, self.turtle.onset, self.turtle.chord:getDuration() + 0.001, self.turtle.channel, self.turtle.pan)
    end
    if target == 'A' then
        local p, v
        p, v, chord = chord:a(self.turtle.arpeggiation)
        self.turtle.onset = self.turtle.onset + self.turtle.chord:getDuration(v)
        chord = self:equivalenceClass(chord, 'RP')
        local note = chord:note(v, self.turtle.onset, self.turtle.duration, self.turtle.channel, self.turtle.pan)
        self.score[#self.score + 1] = note
    end
    print('V Post:', chord)
    print()
    self.priorChord = chord
end

function Lindenmayer:actionTranspose(target, opcode, equivalence, operand, index)
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:T(operand)
    end
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:T(operand)
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionInvert(target, opcode, equivalence, operand, index)
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:I(operand)
    end
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:I(operand)
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionContexualInversion(target, opcode, equivalence, operand, index)
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:K()
    end
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:K()
    end
    self:E(target, opcode, equivalence, operand, index)
end

function Lindenmayer:actionContextualTransposition(target, opcode, equivalence, operand, index)
    if target == 'M' then
        self.turtle.modality = self.turtle.modality:Q(operand, self.turtle.modality, self.intervalSize)
    end
    if target == 'C' then
        self.turtle.chord = self.turtle.chord:Q(operand, self.turtle.modality, self.intervalSize)
    end
    self:E(target, opcode, equivalence, operand, index)
end

-- dckvp    :=*/+-     :            :x       :i
-- VAGtdcvp :=*/+-     :            :x

function Lindenmayer:actionAssign(target, opcode, equivalence, operand, index, stringOperand)
    if index ~= nil then
        if target == 'd' then
            self.turtle.chord.duration[index] = operand
        end
        if target == 'c' then
            self.turtle.chord.channel[index] = operand
        end
        if target == 'k' then
            self.turtle.chord[index] = operand
        end
        if target == 'v' then
            self.turtle.chord.velocity[index] = operand
        end
        if target == 'p' then
            self.turtle.chord.pan[index] = operand
        end
    else        
        if target == 'C' then
            self.turtle.chord = ChordSpace.chordsForNames[stringOperand]
        end
        if target == 'M' then
            self.turtle.modality = ChordSpace.chordsForNames[stringOperand]
        end
        if target == 'V' then
            self.turtle.voicing = operand
        end
        if target == 'A' then
            self.turtle.arpeggiation = operand
        end
        if target == 'G' then
            self.turtle.intervalSize = operand
        end
        if target == 't' then
            self.turtle.onset = operand
        end
        if target == 'd' then
            self.turtle.chord:setDuration(operand)
        end
        if target == 'c' then
            self.turtle.chord:setChannel(operand)
        end
        if target == 'v' then
            self.turtle.chord:setVelocity(operand)
        end
        if target == 'p' then
            self.turtle.chord:setPan(operand)
        end
    end
end

-- dckvp    :=*/+-     :            :x       :i
-- VAGtdcvp :=*/+-     :            :x

function Lindenmayer:actionMultiply(target, opcode, equivalence, operand, index)
    if index ~= nil then
        if target == 'd' then
            self.turtle.chord.duration[index] = self.turtle.chord.duration[index] * operand
        end
        if target == 'c' then
            self.turtle.chord.channel[index] = self.turtle.chord.channel[index] * operand
        end
        if target == 'k' then
            self.turtle.chord[index] = self.turtle.chord[index] * operand
        end
        if target == 'v' then
            self.turtle.chord.velocity[index] = self.turtle.chord.velocity[index] * operand
        end
        if target == 'p' then
            self.turtle.chord.pan[index] = self.turtle.chord.pan[index] * operand
        end
    else        
        if target == 'V' then
            self.turtle.voicing = self.turtle.voicing * operand
        end
        if target == 'A' then
            self.turtle.arpeggiation = self.turtle.arpeggiation * operand
        end
        if target == 'T' then
            self.turtle.onset = self.turtle.onset * operand
        end
        if target == 'G' then
            self.turtle.intervalSize = self.turtle.intervalSize * operand
        end
        if target == 'd' then
            self.turtle.chord:setDuration(self.turtle.chord:getDuration() * operand)
        end
        if target == 'c' then
            self.turtle.chord:setChannel(self.turtle.chord:getChannel() * operand)
        end
        if target == 'v' then
            self.turtle.chord:setVelocity(self.turtle.chord:getVelocity() * operand)
        end
        if target == 'p' then
            self.turtle.chord:setPan(self.turtle.chord:getPan() * operand)
        end
    end
end

function Lindenmayer:actionDivide(target, opcode, equivalence, operand, index)
    if index ~= nil then
        if target == 'd' then
            self.turtle.chord.duration[index] = self.turtle.chord.duration[index] / operand
        end
        if target == 'c' then
            self.turtle.chord.channel[index] = self.turtle.chord.channel[index] / operand
        end
        if target == 'k' then
            self.turtle.chord[index] = self.turtle.chord[index] / operand
        end
        if target == 'v' then
            self.turtle.chord.velocity[index] = self.turtle.chord.velocity[index] / operand
        end
        if target == 'p' then
            self.turtle.chord.pan[index] = self.turtle.chord.pan[index] / operand
        end
    else        
        if target == 'V' then
            self.turtle.voicing = self.turtle.voicing / operand
        end
        if target == 'A' then
            self.turtle.arpeggiation = self.turtle.arpeggiation / operand
        end
        if target == 'T' then
            self.turtle.onset = self.turtle.onset / operand
        end
        if target == 'G' then
            self.turtle.intervalSize = self.turtle.intervalSize / operand
        end
        if target == 'd' then
            self.turtle.chord:setDuration(self.turtle.chord:getDuration() / operand)
        end
        if target == 'c' then
            self.turtle.chord:setChannel(self.turtle.chord:getChannel() / operand)
        end
        if target == 'v' then
            self.turtle.chord:setVelocity(self.turtle.chord:getVelocity() / operand)
        end
        if target == 'p' then
            self.turtle.chord:setPan(self.turtle.chord:getPan() / operand)
        end
    end
end

function Lindenmayer:actionAdd(target, opcode, equivalence, operand, index)
    if index ~= nil then
        if target == 'd' then
            self.turtle.chord.duration[index] = self.turtle.chord.duration[index] + operand
        end
        if target == 'c' then
            self.turtle.chord.channel[index] = self.turtle.chord.channel[index] + operand
        end
        if target == 'k' then
            self.turtle.chord[index] = self.turtle.chord[index] + operand
        end
        if target == 'v' then
            self.turtle.chord.velocity[index] = self.turtle.chord.velocity[index] + operand
        end
        if target == 'p' then
            self.turtle.chord.pan[index] = self.turtle.chord.pan[index] + operand
        end
    else        
        if target == 'V' then
            self.turtle.voicing = self.turtle.voicing + operand
        end
        if target == 'A' then
            self.turtle.arpeggiation = self.turtle.arpeggiation + operand
        end
        if target == 'T' then
            self.turtle.onset = self.turtle.onset + operand
        end
        if target == 'G' then
            self.turtle.intervalSize = self.turtle.intervalSize + operand
        end
        if target == 'd' then
            self.turtle.chord:setDuration(self.turtle.chord:getDuration() + operand)
        end
        if target == 'c' then
            self.turtle.chord:setChannel(self.turtle.chord:getChannel() + operand)
        end
        if target == 'v' then
            self.turtle.chord:setVelocity(self.turtle.chord:getVelocity() + operand)
        end
        if target == 'p' then
            self.turtle.chord:setPan(self.turtle.chord:getPan() + operand)
        end
    end
end

function Lindenmayer:actionSubtract(target, opcode, equivalence, operand, index)
    if index ~= nil then
        if target == 'd' then
            self.turtle.chord.duration[index] = self.turtle.chord.duration[index] - operand
        end
        if target == 'c' then
            self.turtle.chord.channel[index] = self.turtle.chord.channel[index] - operand
        end
        if target == 'k' then
            self.turtle.chord[index] = self.turtle.chord[index] - operand
        end
        if target == 'v' then
            self.turtle.chord.velocity[index] = self.turtle.chord.velocity[index] - operand
        end
        if target == 'p' then
            self.turtle.chord.pan[index] = self.turtle.chord.pan[index] - operand
        end
    else        
        if target == 'V' then
            self.turtle.voicing = self.turtle.voicing - operand
        end
        if target == 'A' then
            self.turtle.arpeggiation = self.turtle.arpeggiation - operand
        end
        if target == 'T' then
            self.turtle.onset = self.turtle.onset - operand
        end
        if target == 'G' then
            self.turtle.intervalSize = self.turtle.intervalSize - operand
        end
        if target == 'd' then
            self.turtle.chord:setDuration(self.turtle.chord:getDuration() - operand)
        end
        if target == 'c' then
            self.turtle.chord:setChannel(self.turtle.chord:getChannel() - operand)
        end
        if target == 'v' then
            self.turtle.chord:setVelocity(self.turtle.chord:getVelocity() - operand)
        end
        if target == 'p' then
            self.turtle.chord:setPan(self.turtle.chord:getPan() - operand)
        end
    end
end

-- Beginning with the axiom,
-- the current production is created by replacing each word
-- in the prior production either with itself, or with its replacement
-- from the dictionary of rules.

function Lindenmayer:produce()
    print('Lindenmayer:produce...')
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
        -- print(self.currentProduction)
    end
end

function Lindenmayer:parseCommand(command)
    local parts = Silencio.split(command, ':')
    local target = parts[1]
    local opcode = parts[2]
    local equivalence = parts[3]
    local operand = tonumber(parts[4])
    local stringOperand = parts[4]
    local index = tonumber(parts[5])
    return target, opcode, equivalence, operand, index, stringOperand
end


function Lindenmayer:interpret()
    print('Lindenmayer:interpret...')
    local commands = Silencio.split(self.currentProduction, ' ')
    for index, command in ipairs(commands) do
        target, opcode, equivalence, operand, index, stringOperand = self:parseCommand(command)
        local action = self.actions[opcode]
        if action ~= nil then
            -- print(target, opcode, equivalence, operand, index, stringOperand)
            action(self, target, opcode, equivalence, operand, index, stringOperand)
        end
    end
end

function Lindenmayer:generate()
    print('Lindenmayer:generate...')
    self:produce()
    self:interpret()
    if self.tieOverlaps == true then
        print('Lindenmayer: tieing notes...')    
        self.score:tieOverlaps(true)
    end
end

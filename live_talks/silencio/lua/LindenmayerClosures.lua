local Silencio = require("Silencio")
local ChordSpace = require("ChordSpace")
--local Som = require("Som")

Lindenmayer = {}

function Lindenmayer.help()
print [[
L I N D E N M A Y E R   W I T H   C L O S U R E S

Copyright (C) 2010 by Michael Gogins
This software is licensed under the terms
of the GNU Lesser General Public License.

The Lindenmayer class implements a Lindenmayer system in which the turtle
state consists of both a note and a chord, and in which the actions associated
with rules are defined by Lua closures.

Operations:
    [   Push state onto stack.
    ]   Pop state from stack.
    W   Write turtle state into score as a note.

Other operations may be added as closures to the actions table. These
functions will take the Lindenmayer system as their first parameter, and
any other parts of the turtle command as their second parameter, as a string.
]]
end

Turtle = {}

function Turtle:new(o)
    o = o or {modality = Chord:new(), chord = Chord:new(), note = Event:new()}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Turtle:__tostring()
    local text = string.format('C: %s  M: %s  note: %s',
    tostring(self.chord),
    tostring(self.modality),
    tostring(self.note))
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
        o.actions['[' ] = self.actionPush
        o.actions[']' ] = self.actionPop
        o.actions['W' ] = self.actionWrite
        o.actions['WC'] = self.actionWriteInChord
    end
    setmetatable(o, self)
    self.__index = self
    return o
end


function Lindenmayer:actionPush(arguments)
    table.insert(self.stack, Silencio.clone(self.turtle))
end

function Lindenmayer:actionPop(arguments)
    self.turtle = table.remove(self.stack)
end

function Lindenmayer:actionWrite(arguments)
    self.score[#self.score + 1] = Silencio.clone(self.turtle.note)
end

function Lindenmayer:actionWriteInChord(arguments)
    local note = Silencio.clone(self.turtle.note)
    ChordSpace.conformToChord(note, self.turtle.chord)
    self.score[#self.score + 1] = note
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
    local opcode = parts[1]
    local arguments = parts[2]
    return opcode, arguments
end


function Lindenmayer:interpret()
    print('Lindenmayer:interpret...')
    local commands = Silencio.split(self.currentProduction, ' ')
    for index, command in ipairs(commands) do
        opcode, arguments = self:parseCommand(command)
        local action = self.actions[opcode]
        if action ~= nil then
            print(action, opcode, arguments)
            action(self, arguments)
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

return Lindenmayer


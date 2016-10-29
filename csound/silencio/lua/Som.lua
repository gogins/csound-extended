Som = {}

function Som.help()
print [[
S O M

Copyright (C) 2010 by Michael Gogins
This software is licensed under the terms 
of the GNU Lesser General Public License.

Som is a user-programmable and user-extensible software sound synthesizer 
written for LuaJIT + FFI. Som is part of Silencio, a system for making music 
by programming in Lua.

Thanks to Mike Pall for his Lua Just-in-Time compiler (LuaJIT) and
native Foreign Function Interface (FFI) for LuaJIT, which make the
thought of doing high-performance computing in a dynamic language
not only possible, but attractive (http://luajit.org).

Basic idioms:

Cache C namespaces in local variables.

Cache C objects in local variables.

For Lua classes, use C structs instead of tables where possible; 
then in class methods, retrieve the struct into a local variable 
'this' (cf. 'self'): local this = self.cstruct. The FFI semantics 
may change to permit true metatables with ctypes, however, which 
would make this unnecessary.

Keep constants (any non-variable expression) inline.
]]
end

local ffi = require("ffi")
local C = ffi.C
local Silencio = require("Silencio")
local sndfile = ffi.load('libsndfile.so')

-- Forward declarations of all core synthesizer classes.

-- Signals carry audio or control events for one time step.

Signal = {}
-- ControlSignal = Signal:new()
-- AudioSignal = Signal:new()

-- Nodes are what are run by the SignalFlowGraph,
-- and implement a contract to process signals,
-- time step by time step.

Node = {}
-- Outlet = Node:new()
-- Inlet = Outlet:new()
-- ControlOutlet = Outlet:new()
-- AudioOutlet = Outlet:new()
-- ControlInlet = Inlet:new()
-- AudioInlet = Inlet:new()
-- ScoreSource = Node:new()
-- MidiFileSource = Node:new()
-- MidiSource = Node:new()
-- AudioFileSource = Node:new()
-- AudioSource = Node:new()
-- ScoreSink = Node:new()
-- MidiFileSink = Node:new()
-- MidiSink = Node:new()
-- AudioFileSink = Node:new()
-- AudioSink = Node:new()
-- Processor = Node:new()
-- Panner = Processor:new()
-- Mixer = Processor:new()
-- Instrument = Node:new()
-- Polyphonic = Instrument:new()
-- SignalFlowGraph = Node:new()

-- Unit generators process signals for one time step,
-- but do so in the context of the Node:tick() function.
-- They use standard object-oriented procedural code.

Oscillator = {}
PhaseOscillator = {}
QuadrupoleFilter = {}
StateVariableFilter = {}
LinearEnvelopeGenerator = {}
ExponentialEnvelopeGenerator = {}
DelayLine = {}
InterpolatingDelayLine = {}
RandomVariable = {}

-- A Signal carries one time step's worth of some 
-- type of value from one node to another in a SignalFlowGraph.
-- Examples: audio samples, control events, time/frequency frames.

function Signal:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

-- Make the value of the Signal empty or zero.

function Signal:clear()
    
end

-- Add the value of another Signal of the same type to this Signal.

function Signal:sum(other)
end

-- Replace the value of this Signal with a copy of the value 
-- of the other Signal.

function Signal:assign(other)
end

-- A control signal represents the state of a MIDI-like control channel 
-- during one tick. This includes event triggering, key and velocity, 
-- and MIDI controller numbers.

ControlSignal = Signal:new()

ffi.cdef([[
struct ControlSignal
{
    double status;
    double channel;
    double key;
    double velocity;
    double value[256];
};
]])

local ControlSignal_t = ffi.typeof('struct ControlSignal')

function ControlSignal:new(o)
    o = o or {this = ControlSignal_t()}
    setmetatable(o, self)
    self.__index = self
    return o
end

function ControlSignal:clear()
    local this = self.this
    ffi.fill(this, ffi.sizeof(ControlSignal_t), 0)
end

-- Only controller values can actually be summed in any sense.
-- Other ControlSignal values are assigned, instead. 

function ControlSignal:sum(other)
    this.status = other.status
    this.channel = other.channel
    this.key = other.key
    this.velocity = other.velocity
    for i = 0, 255 do
        if other.value[i] ~= 0 then
            this.value[i] = other.value[i]
        end
    end
end

function ControlSignal:assign(other)
    local this = self.this
    local other = other.this
    ffi.copy(this, other, ffi.sizeof(ControlSignal_t))
end

function ControlSignal:isNote()
    local this = self.this
    if this.status == 144 or this.status == 128 then
        return true
    else
        return false
    end
end

function ControlSignal:isNoteOn()
    local this = self.this
    if this.status == 144 and this.velocity > 0 then
        return true
    else
        return false
    end
end

function ControlSignal:isNoteOff()
    local this = self.this
    if (this.status == 144 and this.value[1] == 0) or this.status == 128 then
        return true
    else
        return false
    end
end

function isMatchingNoteNoteOn(note)
    local this = self.this
    local other = note.this
    if this.channel ~= other.channel then
        return false
    end
    if not ((this.status == 144 and this.velocity ~= 0) and (other.status == 128 or (other.status == 144 and other.velocity == 0))) then
        return false
    end
    if this.key ~= other.key then
        return false
    end
    return true
end

-- An audio signal buffer represents the state of one channel of PCM audio during one tick.

AudioSignal = Signal:new()

function AudioSignal:new(o)
    o = o or {frames = 64, buffer = nil}
    setmetatable(o, self)
    self.__index = self
    if self.buffer == nil then
       self:resize(frames)
    end
    return o
end

function AudioSignal:clear()
    local buffer = self.buffer
    ffi.fill(buffer, ffi.sizeof(buffer), 0)
end

function AudioSignal:sum(other)
    local buffer = self.buffer
    local other = other.buffer
    for i = 0, frames - 1 do
        buffer[i] = buffer[i] + other[i]
    end
end

function AudioSignal:assign(other)
    local buffer = self.buffer
    local other = other.buffer
    ffi.copy(buffer, other, ffi.sizeof(buffer))
end

function AudioSignal:resize(frames)
   self.frames = frames
   self.buffer = sample_t(self.frames)
end

-- Denotes that a node has not been visited.

NODE_WHITE = 0

-- Denotes that a node has been visited, but may have an unvisited neighbor.

NODE_GREY = 1

-- Denotes that a node and all of its neighbors have been visited.

NODE_BLACK = 2

function Node:new(o)
    o = o or {status = NODE_PROCESSOR, color = NODE_WHITE}
    setmetatable(o, self)
    self.__index = self
    return o
end

-- Called by Som just before the signal flow graph starts running.
-- The Node must acquire any system resources it needs here.

function Node:open(signalFlowGraph)
end

-- Called by Som to indicate the beginning of 
-- processing for this Node instance. If the Node is a 
-- generator, it must go into the NODE_GENERATOR_ACTIVE state.

function Node:attack(signalFlowGraph)
end

-- Called by Som to indicate the end of processing for this Node. 
-- The Node may still take some time for a release envelope, reverb tail, etc. 
-- When a generator Node has finished releasing, 
-- it must go into the NODE_GENERATOR_PASSIVE state.
-- A Node may also release itself, e.g. if its note duration has elapsed.

function Node:release(signalFlowGraph)
    if self.status == NODE_GENERATOR_ACTIVE then
        self.status = NODE_GENERATOR_PASSIVE
    end
end

-- Runtime status of a Node that is always "on," 
-- usually for the purpose of consuming input signals, processing them, 
-- and producing output signals.
-- The tick function of a processor Node is called on every time step
-- of the signal flow graph.

NODE_PROCESSOR = 1

-- Runtime "on" status of a Node that may turn "on" and "off,"
-- for example an "instrument" that becomes active when it begins a note, 
-- and becomes inactive when the note is released.
-- The tick function of an active generator Node 
-- is called on every time step of the signal flow graph.

NODE_GENERATOR_ACTIVE = 2

-- Runtime "off" status of a node that may turn "on" and "off,"
-- for example an "instrument" that becomes active when it begins a note, 
-- and becomes inactive when the note is released.
-- The tick function of a passive generator node is never called.

NODE_GENERATOR_PASSIVE = 3

-- Called by Som on every tick of the signal flow graph
-- to obtain the runtime status of the Node. The overall state
-- of the signal flow graph is controlled by the individual 
-- states of its constituent Nodes. As long as one or more 
-- Node instances is in the NODE_GENERATOR_ACTIVE state, the
-- signal flow graph will continue to tick. When there are no more 
-- Nodes in the NODE_GENERATOR_ACTIVE state, the signal flow graph
-- will stop ticking and shut down. "Scores" and "input drivers"
-- must remain in the NODE_GENERATOR_ACTIVE state as long as they 
-- have any signals pending for the current or future time step.
-- "Instruments" must remain in the NODE_GENERATOR_ACTIVE state as 
-- long as they are producing signals. They must enter the 
-- NODE_GENERATOR_PASSIVE state after they have released and 
-- quit producing signals. "Effects" will always be in the 
-- NODE_PROCESSOR state, which has no effect on the overall 
-- state of the signal flow graph.

function Node:status()
    return self.status
end

-- Called by Som on every tick of the signal flow graph for every 
-- active Node instance. The Node must consume incoming control 
-- and audio signals, and produce outgoing control and audio signals,
-- for the current time step only.

function Node:tick(signalFlowGraph)
end

-- Called by Som just after the signal flow graph has stopped running.
-- The Node must release any system resources that it has acquired.

function Node:close(signalFlowGraph)
end

SignalFlowGraph = Node:new()

function SignalFlowGraph:new(o)
    o = o or {currentFrame = 0, currentTick = 0, currentSecond = 0, framesPerSecond = 48000, framesPerTick = 64, secondsPerFrame = 0, ticksPerSecond = 0, secondsPerTick = 0, ticklist = {}, adjacencies = {}, nodes = {}, play = false, needsCompilation = false}
    self:setFramesPerSecond()
    setmetatable(o, self)
    self.__index = self
    return o
end

function SignalFlowGraph:setFramesPerSecond(value)
    self.framesPerSecond = value
    self.secondsPerFrame = 1 / self.framesPerSecond
    self:setFramesPerTick(self.ticksPerFrame)
    self:resetTime()
end

function SignalFlowGraph:setFramesPerTick(value)
    self.framesPerTick = value
    self.secondsPerTick = self.secondsPerFrame * self.ticksPerFrame
    self.ticksPerSecond = 1 / self.secondsPerTick
    self:resetTime()
end

function SignalFlowGraph:resetTime()
    self.currentFrame = 0
    self.currentTick = 0
    self.currentSecond = 0
end

function SignalFlowGraph:addEdge(outnode, innode)
    if self.nodes[outnode] == nil then
        self.nodes[outnode] = outnode
    end
    if self.nodes[innode] == nil then
        self.nodes[innode] = outnode
    end
    if self.adjacencies[outnode] == nil then
        self.adjacencies[outnode] = {}
    end
    if self.adjacencies[outnode][innode] == nil then
        self.adjacencies[outnode][innode] = innode
    end
end

function SignalFlowGraph:clear()
    stop()
    resetTimes()
    self.nodes = {}
    self.adjacencies = {}
end

-- This function completely defines the topology of the SignalFlowGraph.

function SignalFlowGraph:connect(outnode, outport, inport, innode)
    -- To the user, a signal source has an out edge, 
    -- and a signal sink has an in edge;
    -- but to the SignalFlowGraph, a signal source has an in edge, 
    -- and a signal sink has an out edge.
    -- Hence, we reverse the direction of all user edges here.
    inport:addSource(outport)
    self:addEdge(innode, inport)
    self:addEdge(inport, outport)
    self:addEdge(innode, inport)
    self:addEdge(self, innode)
    self.needsCompilation = true
end

function SignalFlowGraph:depthFirstVisit(node, tickset, ticklist)
    node.color = NODE_GREY
    for successor, unused in ipairs(self.adjacencies[node]) do
        if successor.color == NODE_WHITE then
            self:depthFirstVisit(successor, ticklist, tickset)
        end
    end
    node.color = NODE_BLACK
    -- Sinks can receive signals from more than one source;
    -- sources can send signals to more than one sink.
    -- However, we want to tick each node only once in each time step.
    if tickset[node] == nil then
        tickset[node] = true
        ticklist[#ticklist + 1] = node
    end
end

function SignalFlowGraph:depthFirstSearch(node, tickset, ticklist)
    for node, unused in ipairs(self.nodes) do
        node.color = COLOR_WHITE
    end
    self:depthFirstVisit(node, tickset, ticklist)
    --[[
    for node, unused in ipairs(nodes) do
        if colors[node] == NODE_WHITE then
            print 'This graph is disconnected, aborting compilation.'
            return false
        end
    end
    ]]
    return true
end

-- Produces a non-redundant, least-dependent-first list of all Nodes in the 
-- SignalFlowGraph by sorting them topologically. The SignalFlowGraph itself
-- is the root of the graph, and must be the only Node with out-edges but 
-- no in-edges.

function SignalFlowGraph:compile()
    local tickset = {}
    self.ticklist = {}
    result = self:depthFirstSearch(self, tickset, self.ticklist)
    if result then
        self.needsCompilation = false
        return true
    else
        return false
    end
end

function SignalFlowGraph:perform()
    if not self:compile() then
        return false
    end
    self:resetTime()
    self:render()
    return true
end

function SignalFlowGraph:render()
    self:open()
    self:play()
    self:close()
end

function SignalFlowGraph:play()
    self.play = true
    while self.play do
        self:tickNodes()
        if self.status ~= NODE_GENERATOR_ACTIVE then 
            return 
        end
        -- Some Nodes, like Polyphonic, may insert new nodes
        -- and edges into the SignalFlowGraph.
        if self.needsCompilation then
            self:compile()
        end
    end
end

function SignalFlowGraph:stop()
    self.play = false
end

function SignalFlowGraph:open()
    for i = 1, #self.ticklist do
        self.ticklist[i]:open(self)
    end
end

function SignalFlowGraph:tick()
    -- Does nothing.
end

function SignalFlowGraph:tickNodes()
    local tickStatus = NODE_GENERATOR_PASSIVE
    for i = 1, #self.ticklist do
        local node = self.ticklist[i]
        local nodeStatus = node:status()
        if nodeStatus == NODE_GENERATOR_ACTIVE then
            node:tick(self)
            tickStatus = nodeStatus
        else if nodeStatus == NODE_PROCESSOR then
            node:tick(self)
        end end
    end
    self.status = tickStatus
end

function SignalFlowGraph:close()
    for i = 1, #self.ticklist do
        self.ticklist[i]:close(self)
    end
end

-- An Outlet is a Node that sends a signal.

Outlet = Node:new()

function Outlet:new(o)
   if not o then
        o = Node:new()
        self.signal = nil
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

ControlOutlet = Outlet:new()

function ControlOutlet:new()
   if not o then
        o = Outlet:new()
        self.signal = ControlSignal:new()
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

AudioOutlet = Outlet:new()

function AudioOutlet:new()
   if not o then
        o = Outlet:new()
        self.signal = AudioSignal:new()
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

-- An Inlet is a Node that receives a signal.

Inlet = Outlet:new()

function Inlet:new(o)
    if not o then
        o = Outlet:new()
        self.signal = nil
        self.sources = {}
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

function Inlet:addSource(source)
    for index, value in ipairs(self.sources) do
        if value == source then 
            return
        end
    end
    self.sources[#self.sources + 1] = source
end

-- An inlet ticks merely by summing its source 
-- signals into its own signal.

function Inlet:tick(signalFlowGraph)
    self.signal:clear()
    for i = 1, #self.sources do
        self.signal:sum(self.sources[i].signal)
    end
end

-- A ControlInlet is an Inlet that receives a control signal.

ControlInlet = Inlet:new()

function ControlInlet:new(o)
    if not o then
        o = ControlOutlet:new()
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

-- An AudioInlet is an Inlet that receives an audio signal.

AudioInlet = Inlet:new()

function AudioInlet:new(o)
    if not o then
        o = AudioOutlet:new()
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

-- A ScoreSource is a Node that contains a Silencio Score
-- that feeds pending control signals into a ControlOutlet
-- every time step.

ScoreSource = Node:new()

function ScoreSource:new(o)
    if not o then
        o = Node:new()
        self.score = Score:new()
        self.controlOutlet = ControlOutlet:new()
        self.currentIndex = nil
    end
    setmetatable(o, self)
    self.__index = self
    return o
end

function ScoreSource:open(signalFlowGraph)
    self.currentIndex = 1
end

function ScoreSource:tick(signalFlowGraph)
    local currentEvent = self.score[self.currentIndex]
    if currentEvent == nil then
        return
    end
end




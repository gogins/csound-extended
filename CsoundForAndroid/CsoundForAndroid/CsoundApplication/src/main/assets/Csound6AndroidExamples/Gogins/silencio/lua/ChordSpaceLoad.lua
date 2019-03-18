require "Silencio"
ChordSpace = require("ChordSpace")
local matrix = require("matrix")
local os = require("os")
local voicesToTest = 7

print([[

L O A D I N G  T E S T S   F O R   C H O R D S P A C E G R O U P

It is critical that PITV be performant for 7 voices in 5 octaves.
To achieve this speed, it is necessary to precompute the groups one time, save them,
and load them for later uses.
]])

for voices = 2, voicesToTest do
    local began = os.clock()
    chordSpaceGroup = ChordSpace.load(voices, 48, 1)
    chordSpaceGroup:list()
    print()
end

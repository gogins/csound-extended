Silencio = {}

function Silencio.help()
print [[
S I L E N C I O

Copyright (C) 2010 by Michael Gogins
This software is licensed under the terms
of the GNU Lesser General Public License.

Silencio is a simple system for doing algorithmic composition in Lua.
Silencio runs on Android smartphones and personal computers.
It will output scores as MIDI sequence files or as Csound score files.

There are also convenience functions to simplify algorithmic composition,
including rescaling scores, splitting and combining scores, and applying
matrix arithmetic operations to scores.

A score is a matrix in which the rows are events.

An event is a homogeneous vector with the following dimensions:

 1 Time in seconds from start of performance.
 2 Duration in seconds.
 3 MIDI status (only the most significant nybble, e.g. 144 for 'NOTE ON').
 4 MIDI channel (any real number >= 0, fractional part ties events).
 5 MIDI key number from 0 to 127, 60 is middle C (a real number).
 6 MIDI velocity from 0 to 127, 80 is mezzo-forte (a real number.
 7 Pan, 0 is the origin.
 8 Depth, 0 is the origin.
 9 Height, 0 is the origin.
10 Phase, in radians.
11 Homogeneity, normally always 1.

It is possible to embed a Csound orchestra in a Score object.
If Csound is present, the Score object will save this orchestra along with the
score in Csound .sco format, and shell out to render the piece using Csound.
This enables compositions to be edited and auditioned on a phone, then
rendered using Csound on a computer.

Pass the invoking script's arg table to Score:processArg()
and it will perform the following commands:

--csound        Render generated score using set Csound orchestra.
--dir           Sets directory in which to render files (must come first;
                default is cwd or, on Android, scripts). Script is copied
                to this directory.
--display       Display the generated score as a 3-D piano roll using OpenGL.
--fomus         Render generated score as Fomus music notation file.
--midi          Render generated score as MIDI file and play it (default).
--pianoteq      Play generated MIDI sequence file with Pianoteq.
--pianoteq-wav  Render score to soundfile using Pianoteq,
                post-process it, and play it.
--playmidi      Play generated MIDI file.
                post-process it, and play it.
--playwav       Play rendered normalized output soundfile.
--post          Post-process Csound output soundfile:
                normalize, CD, MP3, tag, and play it.

Thanks to Peter Billam for the Lua MIDI package
(http://www.pjb.com.au/comp/lua/MIDI.html).
]]
print(string.format("Platform: %s\n", platform))
print('Current directory: "' .. cwd .. '".\n')
if arg ~= nil then
    print('Invoking script: "' .. arg[0] .. '".\n')
end
end

function Silencio.clone(object)
    if object == nil then
        return nil
    end
    local lookup_table = {}
    local function _copy(object)
        if type(object) ~= "table" then
            return object
        elseif lookup_table[object] then
            return lookup_table[object]
        end
        local new_table = {}
        lookup_table[object] = new_table
        for index, value in pairs(object) do
            new_table[_copy(index)] = _copy(value)
        end
        return setmetatable(new_table, getmetatable(object))
    end
    return _copy(object)
end

function Silencio.split(str, pat)
    pat = pat or '%s+'
    local t = {}  -- NOTE: use {n = 0} in Lua-5.0
    local fpat = "(.-)" .. pat
    local last_end = 1
    local s, e, cap = str:find(fpat, 1)
    while s do
        if s ~= 1 or cap ~= "" then
            table.insert(t,cap)
        end
        last_end = e+1
        s, e, cap = str:find(fpat, last_end)
    end
    if last_end <= #str then
        cap = str:sub(last_end)
        table.insert(t, cap)
    end
    return t
end

function os.capture(cmd, raw)
    local f = assert(io.popen(cmd, 'r'))
    local s = assert(f:read('*a'))
    f:close()
    if raw then return s end
    s = string.gsub(s, '^%s+', '')
    s = string.gsub(s, '%s+$', '')
    s = string.gsub(s, '[\n\r]+', ' ')
    return s
end

do
    platform = 'Unknown'
    result, android = pcall(require, 'android')
    if result then
        platform = 'Android'
    else
        local osname = os.getenv('WINDIR')
        if osname then
            platform = 'Windows'
        else
            result, osname = pcall(os.capture, 'uname')
            if osname then
                platform = osname
            end
        end
    end
end

do
    if platform == 'Windows' then
        result, cwd = pcall(os.capture, 'cd')
    else
        result, cwd = pcall(os.capture, 'pwd')
    end
end

local MIDI = require("MIDI")

local haveFfi, ffi = pcall(require, 'ffi')
if haveFfi == true then
    print('ffi found.')
else
    print('ffi not found.')
end

local haveJit, jit = pcall(require, 'jit')
if haveJit == true then
    print('jit found.')
else
    print('jit not found.')
end

local haveScoreView, ScoreView = pcall(require, "ScoreView")
if haveScoreView == true then
    print('ScoreView found.')
else
    print('ScoreView not found.')
end

TIME        =  1
DURATION    =  2
STATUS      =  3
CHANNEL     =  4
INSTRUMENT  =  4
KEY         =  5
PITCH       =  5
VELOCITY    =  6
LOUDNESS    =  6
PAN         =  7
DEPTH       =  8
HEIGHT      =  9
PHASE       = 10
HOMOGENEITY = 11

dimensions = {}
dimensions[TIME        ] = "Time"
dimensions[DURATION    ] = "Duration"
dimensions[STATUS      ] = "Status"
dimensions[INSTRUMENT  ] = "Instrument"
dimensions[PITCH       ] = "Pitch"
dimensions[LOUDNESS    ] = "Loudness"
dimensions[PAN         ] = "Pan"
dimensions[DEPTH       ] = "Depth"
dimensions[HEIGHT      ] = "Height"
dimensions[PHASE       ] = "Phase"
dimensions[HOMOGENEITY ] = "Homogeneity"



local eventSortOrder = {TIME, DURATION, STATUS, CHANNEL, KEY, VELOCITY, PAN, DEPTH, HEIGHT, PHASE, HOMOGENEITY}

-- Unfortunately, MIDI.lua doesn't know how to use more than 1 meta length
-- for long duration notes.

TICKS_PER_BEAT = 96

Event = {}

function Event:new(o)
    o = o or {0,0,144,0,0,0,0,0,0,0,1,data=nil}
    setmetatable(o, self)
    self.__index = self
    return o
end

-- Translates this Event to a Csound score "i" statement.
-- Note that MIDI channels are zero-based, Csound instrument numbers are one-based.

function Event:csoundIStatement()
    local istatement = string.format("i %g %g %g %g %g %g %g %g %g %g %g", self[CHANNEL] + 1, self[TIME], self[DURATION], self[KEY], self[VELOCITY], self[PAN], self[DEPTH], self[HEIGHT], self[PHASE], self[STATUS], self[HOMOGENEITY])
    return istatement
end

function Event:midiScoreEvent()
    local tipe = 'note'
    return {tipe, self[TIME] * TICKS_PER_BEAT * 2, self[DURATION] * TICKS_PER_BEAT * 2, self[CHANNEL], self[KEY], self[VELOCITY]}
end

function Event:fomusNote()
    -- Time in Silencio is in absolute seconds.
    -- Default time in Fomus is in quarter-note beats.
    -- 'beat' in Fomus is type of note per durational unit, e.g. 1/4 for ordinary 4/4.
    -- We assume MM 120 and 4/4, which is 8 16th notes per second.
    -- But we pre-quantize to 64th notes at that tempo, so we set the beat to 64/2.
    local noteString = string.format("time %g part %g dur %g pitch %g;", self[TIME] * 32, self[CHANNEL] + 1, self[DURATION] * 32, self[KEY])
    return noteString
end

function Event:midiScoreEventString()
    local event = self:midiScoreEvent()
    local eventString = string.format("{%s, %d, %d, %g, %g, %g}", event[1], event[2], event[3], event[4], event[5], event[6])
    return eventString
end

-- Assigns the value of this to a ctype buffer of doubles,
-- in conventional Silencio order. This is for use with csoundScoreEvent.
-- This function currently assumes 11 fields in the specified order for
-- Csound 'i' score events only.

function Event:toBuffer(buffer)
    buffer[ 0] = self[CHANNEL] + 1
    buffer[ 1] = self[TIME]
    buffer[ 2] = self[DURATION]
    buffer[ 3] = self[KEY]
    buffer[ 4] = self[VELOCITY]
    buffer[ 5] = self[PHASE]
    buffer[ 6] = self[PAN]
    buffer[ 7] = self[HEIGHT]
    buffer[ 8] = self[DEPTH]
    buffer[ 9] = self[STATUS]
    buffer[10] = self[HOMOGENEITY]
end

function Event:clone()
    return Silencio.clone(self)
end

function Event:getOffTime()
    return self[TIME] + self[DURATION]
end

function Event:setOffTime(offTime)
    self[DURATION] = offTime - self[TIME]
end

function Event:__tostring()
    return string.format('t %9.3f d %9.3f s %6.2f c %7.3f k %7.3f v %7.3f x %7.3f y %7.3f z %7.3f p %7.3f h %7.2f', self[TIME], self[DURATION], self[STATUS], self[CHANNEL], self[KEY], self[VELOCITY], self[PHASE], self[PAN], self[DEPTH], self[HEIGHT], self[HOMOGENEITY])
end

Score = {}

function Score:new(o)
    o = o or {title = "MyScore", artist = '', orchestra = '', copyright = '', album = ''}
    setmetatable(o, self)
    self.__index = self
    if platform == 'Android' then
        self.directory = '/sdcard/sl4a/scripts/'
    else
        self.directory = ''
    end
    return o
end

function Score:setCsound(csound, api)
    self.csound = csound
    self.csoundApi = api
end

function Score:getTitle()
    return self.title
end

function Score:setTitle(value)
    self.title = value
end

function Score:getArtist()
    return self.artist
end

function Score:setArtist(value)
    self.artist = value
end

function Score:getCopyright()
    return self.copyright
end

function Score:setCopyright(value)
    self.copyright = value
end

function Score:getAlbum()
    return self.album
end

function Score:setAlbum(value)
    self.album = value
end

function Score:getLicense()
    return self.license
end

function Score:setLicense(value)
    self.license = value
end

-- Sets a directory in which all files will be rendered.

function Score:setDirectory(value)
    self.directory = value
    print(string.format('Changed output directory to: %s', self.directory))
end

function Score:getDirectory()
    return self.directory
end

function Score:getFomusFilename()
    return string.format('%s%s.fms', self.directory, self.title)
end

function Score:getMidiFilename()
    return string.format('%s%s.mid', self.directory, self.title)
end

function Score:getScoFilename()
    return string.format('%s%s.sco', self.directory, self.title)
end

function Score:getOrcFilename()
    return string.format('%s%s.orc', self.directory, self.title)
end

function Score:getOutputSoundfileName()
    return string.format('%s%s.wav', self.directory, self.title)
end

function Score:getCdAudioFilename()
    return string.format('%s%s.cd.wav', self.directory, self.title)
end

function Score:getNormalizedSoundfileName()
    return string.format('%s%s.norm.wav', self.directory, self.title)
end

function Score:getMp3SoundfileName()
    return string.format('%s%s.mp3', self.directory, self.title)
end

-- If the first parameter only is given, it is assumed to be an Event,
-- otherwise the parameters are assumed to be the fields for an Event.

function Score:append(time_, duration, status, channel, key, velocity, pan, depth, height, phase)
    if duration ~= nil then
        local event = Event:new{time_, duration, status, channel, key, velocity, pan, depth, height, phase, 1}
        table.insert(self, event)
    else
        table.insert(self, time_)
    end
end

function Score:temper(tonesPerOctave)
    for i, event in ipairs(self) do
        local octave = event[KEY] / 12.0
        local tone = math.floor((octave * tonesPerOctave) + 0.5)
        octave = tone / tonesPerOctave
        event[KEY] = octave * 12.0
    end
end

function Score:saveSco()
    print(string.format("Saving \"%s\" as Csound score file...", self:getScoFilename()))
    local file = io.open(self:getScoFilename(), "w")
    for i, event in ipairs(self) do
        file:write(event:csoundIStatement().."\n")
    end
    file:write('s 4.0\n')
    file:write('e 4.0\n')
    file:close()
end

-- Save the score as a format 0 MIDI sequence.
-- The optional patch changes are an array of
-- {'patch_change', dtime, channel, patch}.
function Score:renderMidi(patchChanges)
    print(string.format("Saving \"%s\" as MIDI sequence file...", self:getMidiFilename()))
    -- Time resolution is 96000 ticks per beat (i.e. per second),
    -- i.e. sample precision at 96 KHz.
    local track = {}
    if patchChanges then
        print ('Patch changes:', patchChanges)
        for i, patchChange in ipairs(patchChanges) do
            table.insert(track, i, patchChange)
        end
    end
    for i, event in ipairs(self) do
        local midiscoreevent = event:midiScoreEvent()
        local channel = midiscoreevent[4]
        table.insert(track, i, midiscoreevent)
    end
    local midiscore = {TICKS_PER_BEAT, track}
    local midisequence = MIDI.score2midi(midiscore)
    file = io.open(self:getMidiFilename(), "w")
    file:write(midisequence)
    file:close()
end

function Score:renderFomus(namesForChannels, header)
    print(string.format("Saving \"%s\" as Fomus music notation file...", self:getFomusFilename()))
    file = io.open(self:getFomusFilename(), "w")
    file:write(string.format("title = \"%s\"\n", self.title))
    if self.artist:len() > 1 then
        file:write(string.format("author = \"%s\"\n", self.artist))
    end
    file:write("beat = 1/64\n")
    file:write("timesig (4 4)\n")
    file:write("lily-papersize = 11x17\n")
    if namesForChannels then
        for part, name in ipairs(namesForChannels) do
            file:write(string.format("part <id = %s name = %s>\n", part, name))
        end
    end
    if header then
        file:write(header..'\n')
    end
    for i, event in ipairs(self) do
        file:write(event:fomusNote().."\n")
    end
    file:close()
    print("Running Fomus...")
    os.execute(string.format('fomus -i %s -o %s.ly', self:getFomusFilename(), self.title))
    print("Running Lilypond...")
    os.execute(string.format('lilypond -fpdf %s.ly', self.title))
end

function Score:playMidi(inBackground)
    local background = ''
    if inBackground then
        background = '&'
    end
    print(string.format('Playing \"%s\" on %s...', self:getMidiFilename(), platform))
    if platform == 'Linux' then
        assert(os.execute(string.format("timidity -idv %s -Od %s", self:getMidiFilename(), background)))
    end
    if platform == 'Android' then
        android.startActivity('android.intent.action.VIEW', 'file:///'..self:getMidiFilename(), 'audio/mid')
    end
    if platform == 'Windows' then
        local command = string.format("wmplayer.exe \"%s\\%s\"", cwd, self:getMidiFilename())
        print (command)
        assert(os.execute(command, background))
    end
end

function Score:playPianoteq(inBackground)
    local background = ''
    if inBackground then
        background = '&'
    end
    print(string.format('Playing \"%s\" on %s...', self:getMidiFilename(), platform))
    if platform == 'Linux' or platform == 'Windows' then
        assert(os.execute(string.format("Pianoteq --midi %s  %s", self:getMidiFilename(), background)))
    end
end

function Score:getOutputSoundfileName()
    return string.format('%s%s.wav', self.directory, self.title)
end

function Score:playWav(inBackground)
    print(string.format('Playing \"%s\" on %s...', self:getNormalizedSoundfileName(), platform))
    local background = ''
    if inBackground then
        background = '&'
    end
    if platform == 'Linux' then
        assert(os.execute(string.format("audacity %s %s", self:getNormalizedSoundfileName(), background)))
    end
    if platform == 'Android' then
        android.startActivity('android.intent.action.VIEW', 'file:///'..self:getNormalizedSoundfileName(), 'audio/x-wav')
    end
    if platform == 'Windows' then
        assert(os.execute(string.format('wmplayer.exe %s %s', self:getNormalizedSoundfileName(), background)))
    end
end

function Score:setOrchestra(orchestra)
    self.orchestra = orchestra
end

function Score:setPreCsoundCommands(commands)
    self.preCsoundCommands = commands
end

function Score:setPostCsoundCommands(commands)
    self.postCsoundCommands = commands
end

-- Sets MIDI bank and program setup to use
-- when rendering a MIDI sequence.
-- The format is a table of tables
-- each entry containing {'patch change', bank number, channel number, program number},
-- e.g. {{'patch_change', 0, 0, 1},{'patch_change', 0, 0, 8}, {'patch_change', 0, 2, 9}.

function Score:setMidiPatches(midiPatches)
    self.midiPatches = midiPatches
end

function Score:getMidiPatches()
    return self.midiPatches
end

-- Sets optional part names to use when rendering
-- Fomus music notation. The format is a table
-- of names.

function Score:setFomusParts(fomusParts)
    self.fomusParts = fomusParts
end

function Score:getFomusParts()
    return self.fomusParts
end

-- Sets an optional header of Fomus commands
-- to use when rendering Fomus music notation.

function Score:setFomusHeader(fomusHeader)
    self.fomusHeader = fomusHeader
end

function Score:getFomusHeader()
    return self.fomusHeader
end

function Score:saveOrc()
    print(string.format("Saving \"%s\" as Csound orchestra file...", self:getOrcFilename()))
    orcfile = io.open(self:getOrcFilename(), 'w')
    orcfile:write(self.orchestra)
    orcfile:close()
end

function Score:renderCsound()
    print(string.format("Rendering \"%s\" with Csound...", self:getOutputSoundfileName()))
    self:renderMidi(self.midiPatches)
    self:saveOrc()
    self:saveSco()
    os.execute(self.preCsoundCommands)
    local command = string.format('csound -m227 -W -f -R -K -r 48000 -k 375 --midi-key=4 --midi-velocity=5 -o %s %s %s', self:getOutputSoundfileName(), self:getOrcFilename(), self:getScoFilename())
    os.execute(command)
    os.execute(self.postCsoundCommands)
    self:postProcess()
end

function Score:renderPianoteq()
    print(string.format("Rendering \"%s\" with Pianoteq...", self:getOutputSoundfileName()))
    self:renderMidi(self.midiPatches)
    local command = string.format('Pianoteq --headless --midi %s --rate 48000 --wav %s', self:getMidiFilename(), self:getOutputSoundfileName())
    os.execute(command)
    self:postProcess()
end

function Score:findScale(dimension)
    local minimum = 0
    local maximum = 0
    for i, event in ipairs(self) do
        local value = event[dimension]
        if i == 1 then
            minimum = value
            maximum = value
        end
        if value < minimum then
            minimum = value
        end
        if value > maximum then
            maximum = value
        end
    end
    local range = maximum - minimum
    return minimum, range
end

function Score:getDuration()
    local duration = 0
    for i, event in ipairs(self) do
        local value = event:getOffTime()
        if value > duration then
            duration = value
        end
    end
    return duration
end

function Score:findScales()
    local minima = Event:new()
    local ranges = Event:new()
    for i = 1, HOMOGENEITY do
        local minimum, range = self:findScale(i)
        minima[i] = minimum
        ranges[i] = range
    end
    return {minima, ranges}
end

function Score:printStats()
    print(string.format('%11s: %s', 'Title', self:getTitle()))
    print(string.format('%11s: %s', 'Artist', self:getArtist()))
    print(string.format('%11s: %d', 'Events', #self))
    local scales = self:findScales()
    local minima = scales[1]
    local ranges = scales[2]
    for i = 1, #minima do
        print(string.format('%11s: min: %9.4f  range: %9.4f', dimensions[i], minima[i], ranges[i]))
    end
end

function Score:print()
    print()
    self:printStats()
    print()
    for i, event in ipairs(self) do
        print(i, event)
    end
    print()
end

function Score:setScale(dimension, minimum, range)
    local currentMinimum, currentRange = self:findScale(dimension)
    if currentRange == 0 then
        currentRange = 1
    end
    for i, event in ipairs(self) do
        event[dimension] = event[dimension] - currentMinimum
        if range ~= nil then
            event[dimension] = event[dimension] * range / currentRange
        end
        event[dimension] = event[dimension] + minimum
    end
end

function Score:tagFile(filename)
    print('Tagging: "' .. filename .. '"...')
    local timestamp = os.date('%Y-%m-%d')
    command = 'bwfmetaedit'
    command = command .. ' --OriginationDate=' .. timestamp
    command = command .. ' --ICRD=' .. timestamp
    if (self:getTitle()) then
      command = command .. ' --Description=' .. self:getTitle()
      command = command .. ' --INAM=' .. self:getTitle()
    end
    if (self:getCopyright():len() > 0) then
      command = command .. ' --ICOP=' .. self:getCopyright()
    end
    if (self:getArtist()) then
      command = command .. ' --Originator=' .. self:getArtist()
      command = command .. ' --IART=' .. self:getArtist()
    end
    if (self:getAlbum()) then
      command = command .. ' --IPRD=' .. self:getAlbum()
    end
    if (self:getLicense()) then
      command = command .. ' --ICMT=' .. self:getLicense()
    end
    command = command .. ' ' .. filename
    print('Command: "' .. command .. '".')
    assert(os.execute(command))
end

function Score:normalizeOutputSoundfile(levelDb)
    if not levelDb then
        levelDb = -6.0
    end
    local buffer = string.format('sox %s -V3 -b 32 -e floating-point %s gain -n %f',
		  self:getOutputSoundfileName(),
		  self:getNormalizedSoundfileName(),
		  levelDb);
    print('Normalizing output soundfile:"' .. command .. '"..')
    assert(os.execute(buffer))
    self:tagFile(self:getNormalizedSoundfileName())
end

function Score:translateToCdAudio(levelDb)
    if not levelDb then
        levelDb = -6.0
    end
    local command = string.format('sox %s -V3 -b 16 %s gain -n %f rate 44100',
		  self:getOutputSoundfileName(),
		  self:getCdAudioFilename(),
		  levelDb)
    print('Translating output soundfile to CD audio: "' .. command .. '".')
    assert(os.execute(command))
    self:tagFile(self:getCdAudioFilename())
end

function Score:translateToMp3()
    local command = string.format('lame --verbose --disptime 2 --nohist --preset cd --tt %s --ta %s --tl %s --tc %s %s %s &',
		  self:getTitle(),
		  self:getArtist(),
		  self:getAlbum(),
		  self:getCopyright(),
		  self:getCdAudioFilename(),
		  self:getMp3SoundfileName())
    print('Translating CD audio file to MP3: "' .. command .. '".')
    assert(os.execute(command))
end

-- Tag the output soundfile,
-- normalize it and tag the normalized file,
-- translate it to CD audio and tag the CD audio file,
-- and translate it to a tagged MP3 file.

function Score:postProcess()
    print('Post-processing: "' .. self.title .. '"...')
    self:tagFile(self:getOutputSoundfileName())
    self:normalizeOutputSoundfile()
    self:translateToCdAudio()
    self:translateToMp3()
end

-- Process rendering-related commands passed in the args table
-- (typically, command-line arguments from the invoking script).

function Score:processArg(args)
    print('In script: "' .. args[0] .. '"\n')
    if platform == 'Android' then
        local argz = Silencio.split(args[0], '/')
        local title = argz[#argz]
        self:setTitle(title)
    else
        if platform ~= 'Windows' then
            self:setTitle(args[0])
        end
    end
    if #args == 0 then
        args[1] = '--midi'
    end
    for i, argument in ipairs(args) do
        if argument == '--dir' then
            self:setDirectory(args[i + 1])
            local scriptPath = cwd
            if platform == 'Windows' then
                scriptPath = scriptPath .. '\\' .. args[0]
            end
            if platform == 'Linux' then
                scriptPath = scriptPath .. '/' .. args[0]
                print( 'scriptPath', scriptPath)
            end
            assert(os.execute(string.format('cd %s', args[i + 1])))
            if platform == 'Windows' then
                assert(os.execute(string.format('copy %s', scriptPath)))
            end
            if platform == 'Linux' then
                local command = string.format('cp %s %s', scriptPath, args[i + 1])
                print(command)
                assert(os.execute(command))
            end
        end
        if argument == '--midi' then
            self:renderMidi(self.midiPatches)
            self:playMidi()
        end
        if argument == '--playmidi' then
             self:playMidi()
        end
        if argument == '--pianoteq' then
            self:renderMidi(self.midiPatches)
            self:playPianoteq()
        end
        if argument == '--pianoteq-wav' then
            self:renderMidi(self.midiPatches)
            self:renderPianoteq()
            self:playWav()
        end
        if argument == '--fomus' then
            self:renderFomus(self.fomusParts, self.fomusHeader)
        end
        if argument == '--display' then
            self:display()
        end
        if argument == '--csound' then
            self:renderCsound()
            self:playWav()
        end
        if argument == '--post' then
            self:postProcess()
            self:playWav()
        end
        if argument == '--playwav' then
            self:playWav()
        end
    end
end

function Score:clone()
    return Silencio.clone(self)
end

local function eventComparator(a, b)
    for k, dimension in ipairs(eventSortOrder) do
        if a[dimension] < b[dimension] then
            return true
        else
            if a[dimension] > b[dimension] then
                return false
            end
        end
    end
    return false
end

function Score:sort()
    return table.sort(self, eventComparator)
end

function Score:setDuration(newDuration)
    local minimum, duration = self:findScale(TIME)
    local factor = newDuration / duration
    for i, event in ipairs(self) do
        event[TIME] = event[TIME] - minimum
        event[TIME] = event[TIME] * factor
        event[DURATION] = event[DURATION] * factor
    end
end

-- Joins notes (only) of the same pitch and channel that overlap in time.
-- If the score contains two notes of the same pitch and channel
-- and loudness and duration greater than 0 that overlap in time,
-- extends the earlier note and discards the later note.

function Score:tieOverlaps(tieExact)
    tieExact = tieExact or false
    self:sort()
    for laterI = #self, 1, -1 do
        local laterEvent = self[laterI]
        if laterEvent[STATUS] == 144 then
            if laterEvent[DURATION] <= 0 or laterEvent[VELOCITY] <= 0 then
                table.remove(self, laterI)
            else
                for earlierI = laterI - 1, 1, -1 do
                    local earlierEvent = self[earlierI]
                    local later = false
                    if tieExact then
                        later = (earlierEvent:getOffTime() >= laterEvent[TIME])
                    else
                        later = (earlierEvent:getOffTime() > laterEvent[TIME])
                    end
                    if earlierEvent[STATUS] == 144 and math.floor(earlierEvent[CHANNEL]) == math.floor(laterEvent[CHANNEL])
                        and math.floor(earlierEvent[KEY ] + 0.5) == math.floor(laterEvent[KEY] + 0.5)
                        and later then
                        earlierEvent:setOffTime(laterEvent:getOffTime())
                        table.remove(self, laterI)
                        break
                    end
                end
            end
        end
    end
end

-- Returns the sub-score containing events
-- starting at or later than the begin time,
-- and up to but not including the end time.
-- The events in the slice are references.

function Score:slice(begin, end_)
    self:sort()
    local s = Score:new()
    for index, event in ipairs(self) do
        local time_ = event[TIME]
        if time_ >= begin and time_ < end_ then
            table.insert(s, event)
        end
    end
    return s
end

-- Sends all events in this, at once, to a running
-- instance of Csound. This function is meant
-- to be called from the lua_exec opcode or
-- other Lua opcode, in Csound, to render a score
-- that has been generated as part of a Csound performance.
--
-- Note that in this mode, Csound will continue to play
-- after the final event has finished. Csound must be stopped
-- by some external means. A single f 0 statement of the
-- desired duration in the Csound score will do.

function Score:sendToCsound()
    self:sort()
    local buffer = {}
    for index, event in ipairs(self) do
        table.insert(buffer, event:csoundIStatement())
    end
    self.csoundApi.csoundReadScore(self.csound, table.concat(buffer, '\n'))
    --local buffer = ffi.new('double[11]')
    --for index, event in ipairs(self) do
    --    event:toBuffer(buffer)
    --    self.csoundApi.csoundScoreEvent(self.csound, 105, buffer, 11)
    --end
    self.csoundApi.csoundMessage(csound, 'Sent %.0f score events to Csound.\n', #self)
end

function Score:display()
    if ScoreView and ScoreView.display then
        ScoreView.display(self)
    else
        print('ScoreView.display(score) not available: check for OpenGL and glfw.')
    end
end

--[[
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

generator = function(cursor, depth)
cursor, events = generator(cursor, depth)

The algorithm is similar to the deterministic algorithm for computing the
attractor of a recurrent iterated function systems. Instead of using
affine transformation matrixes as the RIFS algorithm does, the current
algorithm uses generating functions; but if each generating function
applies a single affine transformation matrix to the cursor, the current
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
]]

function Silencio.recurrent(generators, transitions, depth, index, cursor, score)
    depth = depth - 1
    print(string.format('recurrent(depth: %d  index: %d  cursor: %s)', depth, index, cursor:__tostring()))
    if depth == 0 then
        return
    end
    local transitionsForThisIndex = transitions[index]
    for transitionIndex, liveNode in ipairs(transitionsForThisIndex) do
        if liveNode == 1 then
            local newCursor, events = generators[transitionIndex](cursor:clone(), depth)
            for k,event in ipairs(events) do
                score:append(event:clone())
            end
            Silencio.recurrent(generators, transitions, depth, transitionIndex, newCursor:clone(), score)
        end
    end
end

return Silencio

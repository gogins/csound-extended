-- Copyright (c) 2002, 2003 by Michael Gogins. All rights reserved.
-- Tutorial demonstrating a MusicModel composition based on a Lindenmayer system.

-- require "luaCsnd6"
require "luaCsoundAC"

filename = 'Lindenmayer.lua'
model = luaCsoundAC.MusicModel()
lindenmayer = luaCsoundAC.Lindenmayer()
lindenmayer:setAxiom("Ts=425 b")
lindenmayer:setAngle(2.0 * math.pi / 0.5)
lindenmayer:addRule("b", " b [ Ts*16 Ti-1 a N b ] Tt+.91 Tk-3.01 a N b Tt+.3 N Tt+1.5 Tk+2.5 b [ Ti+1 a b ] N")
lindenmayer:addRule("a", " N Tt+1.251 Tk+1 N [ Tk+2 b ] Tk+4.1 N Tk-3 Tt-1 [ Tt+1 Tk-4 [ a ] N ] N ")
lindenmayer:setIterationCount(5)
lindenmayer:generate()
random = luaCsoundAC.Random()
random:createDistribution("uniform_real")
random:setElement(7, 11, 1)
rescale = luaCsoundAC.Rescale()
rescale:setRescale( 0, true, true,  0,     240)
rescale:setRescale( 1, true, true,  3,       4)
rescale:setRescale( 3, true, true,  2,       8)
rescale:setRescale( 4, true, true,  36,      60)
rescale:setRescale( 5, true, true,  80,      10)
rescale:setRescale( 7, true, true,  -0.9875,    1.875)
scales =  {'Dm', 'Fm', 'AM', 'G7', 'Em', 'FM'}
pcsForScales = {}
for i,scale in ipairs(scales) do
    scalenumber = luaCsoundAC.Conversions_nameToM(scale)
    pcsForScales[scale] = scalenumber
    print(scale .. "="  .. scalenumber)
end
random:addChild(lindenmayer)
rescale:addChild(random)
model:addChild(rescale)
model:generate()
score = model:getScore()
print('Events in generated score: '..score:size())
model:setConformPitches(true)
model:cppsoundLoad('../csound-vst/CsoundAC.csd')
model:setCsoundCommand("-m195 --0dbfs=1 -RWZdfo " .. filename .. ".wav ")
duration = score:getDuration()		
print('Duration: '.. duration)
score:arrange(0, 7)
score:arrange(1, 5)
score:arrange(2, 13)
score:arrange(3, 10)
score:arrange(4, 14)
score:arrange(5, 7)
score:arrange(6, 15)
score:arrange(7, 19)
model:perform()


















































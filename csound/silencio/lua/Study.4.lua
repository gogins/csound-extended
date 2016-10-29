require "Silencio"

Silencio.help()

score = Score:new()
score:setArtist('Michael_Gogins')
score:setTitle('Study.4')
score:setCopyright('Copr_2010_Michael_Gogins')
score:setAlbum('Silencio')

function normalizeGeneratorTimes(g)
    sum = 0.0
    for i = 1, #g, 4 do
        sum = (sum + g[i + 2])
    end
    for i = 1, #g, 4 do
        g[i + 2] = g[i + 2] / sum
    end
end

function Koch(generator, t, d, c, k, v, level, score)
    local t1 = t
    local d1 = d
    local c1 = c
    local k1 = k
    local v1 = v
    local pan = 0
    if level > 0 then
        for i = 1, #generator, 4 do
            k1 = k + generator[i]
            v1 = v + generator[i + 1]
            d1 = d  * generator[i + 2]
            d2 = d1 * generator[i + 3]
            score:append(t1, d2, 144, c1, math.floor(k1), v1, 0, 0, 0, 0, 1)
            Koch(generator, t1, d1, c1, 12+k1, v1, level - 1, score)
            t1 = t1 + d1
        end
    end
end

g = {  4, -4,  4,  1,
      -5, -4,  2,  .875,
       4, -3,  4,  1,
       7, -3,  2,  1,
       4, -3,  2,  1,
       7, -2,  3,  1,
      -1, -3,  4,  .875,
       2, -3,  4,  1, 
    }
    
s=[[
-- Score is a Silencio.Score object to contain generated events.
-- Event is a Silencio.Event object representing the current position in score space
-- (maybe I should use an affine transformation to represent the current local transformation of coordinates).
-- Generators is a list of functions with this signature; each function may generate or transform
-- events or do anything else.
-- Depth is the current depth of recursion; recursion stops when depth == 0.
-- Index indicates the current generator in the list.
-- Generator must: decrement depth; generator or transform events, probably based on 
-- the current event; and then invoke each of generators. Generators may use depth and index
-- and event as they see fit, may call any or none of generators, rewrite generators, etc.

x = generator(score, event, generators, depth, index) {}
]]    

normalizeGeneratorTimes(g)

duration = 60 * 3.5

Koch(g, 1.00, duration, 1, 30, 74, 3, score)
Koch(g, 1.50, duration, 0, 42, 80, 3, score)

print("Generated score:")
for i, event in ipairs(score) do
    print(i, event:csoundIStatement())
    print(' ', event:midiScoreEventString())
end

scales = score:findScales()
print(string.format('minima: %s', scales[1]:csoundIStatement()))
print(string.format('ranges: %s', scales[2]:csoundIStatement()))

score:setMidiPatches({{'patch_change', 1, 0,16},{'patch_change', 1, 1,19}, {'patch_change', 1, 2,21} })
score:setFomusParts({'Cembalom', 'Gong', 'Fife'})
orchestra = [[
sr      =   96000
ksmps   =       1
nchnls  =       2
0dbfs   =       1.0

;#define ENABLE_PIANOTEQ #1#

giFluidsynth		    fluidEngine		        0, 0
giFluidSteinway		    fluidLoad		        "Piano Steinway Grand Model C (21,738KB).sf2",  giFluidsynth, 1
                        fluidProgramSelect	    giFluidsynth, 0, giFluidSteinway, 0, 1
giFluidGM		        fluidLoad		        "63.3mg The Sound Site Album Bank V1.0.SF2", giFluidsynth, 1
                        fluidProgramSelect	    giFluidsynth, 1, giFluidGM, 0, 59
giFluidMarimba		    fluidLoad		        "Marimba Moonman (414KB).SF2", giFluidsynth, 1
                        fluidProgramSelect	    giFluidsynth, 2, giFluidMarimba, 0, 0
giFluidOrgan		    fluidLoad		        "Organ Jeux V1.4 (3,674KB).SF2", giFluidsynth, 1
                        fluidProgramSelect	    giFluidsynth, 3, giFluidOrgan, 0, 40
                        
#ifdef ENABLE_PIANOTEQ
            
giPianoteq              vstinit                 "C:\\utah\\opt\\pianoteq-3.5\\Pianoteq35.dll", 0
                        vstinfo                 giPianoteq

#end

       JackoInit		"default", "csound"

       ; To use ALSA midi ports, use "jackd -Xseq"
       ; and use "jack_lsp -A -c" or aliases from JackInfo,
       ; probably together with information from the sequencer,
       ; to figure out the damn port names.

       ; JackoMidiInConnect   "alsa_pcm:in-131-0-Master", "midiin"
        JackoAudioInConnect 	"aeolus:out.L", "leftin"
        JackoAudioInConnect 	"aeolus:out.R", "rightin"
        JackoAudioInConnect 	"Pianoteq36:out_1", "leftin1"
        JackoAudioInConnect 	"Pianoteq36:out_2", "rightin1"
        JackoMidiOutConnect 	"midiout", "aeolus:Midi/in"
        JackoMidiOutConnect 	"midiout1", "aeolus:Midi/in"
       
        ; Note that Jack enables audio to be output to a regular
        ; Csound soundfile and, at the same time, to a sound 
        ; card in real time to the system client via Jack. 

        JackoAudioOutConnect "leftout", "system:playback_1"
        JackoAudioOutConnect "rightout", "system:playback_2"
        JackoInfo

        ; Turning freewheeling on seems automatically    
        ; to turn system playback off. This is good!

        JackoFreewheel	1
        JackoOn
                        
;                       ALL SIGNAL FLOW GRAPH CONNECTIONS ARE DEFINED BELOW THIS

connect                 "STKBeeThree",      "leftout",     "LeftReverberator",     	"input"
connect                 "STKBeeThree",      "rightout",    "RightReverberator",     	"input"
connect                 "FMBell",           "leftout",     "Reverberator",     	"leftin"
connect                 "FMBell",           "rightout",    "Reverberator",     	"rightin"
connect                 "DelayedPlucked",   "leftout",     "Reverberator",     	"leftin"
connect                 "DelayedPlucked",   "rightout",    "Reverberator",     	"rightin"
connect                 "FMModerate2",      "leftout",     "Reverberator",     	"leftin"
connect                 "FMModerate2",      "rightout",    "Reverberator",     	"rightin"
connect                 "Flute",            "leftout",     "Reverberator",     	"leftin"
connect                 "Flute",            "rightout",    "Reverberator",     	"rightin"
connect                 "FMModerate",       "leftout",     "Reverberator",     	"leftin"
connect                 "FMModerate",       "rightout",    "Reverberator",     	"rightin"
connect                 "TubularBell",      "leftout",     "Reverberator",     	"leftin"
connect                 "TubularBell",      "rightout",    "Reverberator",     	"rightin"
connect                 "Rhodes",           "leftout",     "Reverberator",     	"leftin"
connect                 "Rhodes",           "rightout",    "Reverberator",     	"rightin"
connect                 "FilteredSines",    "leftout",     "Reverberator",     	"leftin"
connect                 "FilteredSines",    "rightout",    "Reverberator",     	"rightin"
connect                 "LivingstonGuitar", "leftout",     "Reverberator",     	"leftin"
connect                 "LivingstonGuitar", "rightout",    "Reverberator",     	"rightin"
connect                 "Xing",             "leftout",     "Reverberator",     	"leftin"
connect                 "Xing",             "rightout",    "Reverberator",     	"rightin"
connect                 "FMModulatedChorus","leftout",     "Reverberator",     	"leftin"
connect                 "FMModulatedChorus","rightout",    "Reverberator",     	"rightin"
connect                 "HeavyMetal",       "leftout",     "Reverberator",     	"leftin"
connect                 "HeavyMetal",       "rightout",    "Reverberator",     	"rightin"
connect                 "ToneWheelOrgan",   "leftout",     "Reverberator",     	"leftin"
connect                 "ToneWheelOrgan",   "rightout",    "Reverberator",     	"rightin"
connect                 "Melody",           "leftout",     "Reverberator",     	"leftin"
connect                 "Melody",           "rightout",    "Reverberator",     	"rightin"
connect                 "Plucked",          "leftout",     "Reverberator",     	"leftin"
connect                 "Plucked",          "rightout",    "Reverberator",     	"rightin"
connect                 "Guitar",           "leftout",     "Reverberator",     	"leftin"
connect                 "Guitar",           "rightout",    "Reverberator",     	"rightin"
connect                 "Harpsichord",      "leftout",     "Reverberator",     	"leftin"
connect                 "Harpsichord",      "rightout",    "Reverberator",     	"rightin"
connect                 "STKPlucked",       "leftout",     "Reverberator",     	"leftin"
connect                 "STKPlucked",       "rightout",    "Reverberator",     	"rightin"
connect                 "Guitar2",          "leftout",     "Reverberator",     	"leftin"
connect                 "Guitar2",          "rightout",    "Reverberator",     	"rightin"
connect                 "STKBowed",         "leftout",     "Reverberator",     	"leftin"
connect                 "STKBowed",         "rightout",    "Reverberator",     	"rightin"

#ifdef ENABLE_PIANOTEQ
connect                 "PianoteqAudio",    "leftout",     "LeftReverberator",  "input"
connect                 "PianoteqAudio",    "rightout",    "RightReverberator", "input"
#endif

connect                 "FluidAudio",       "leftout",     "Reverberator",     	"leftin"
connect                 "FluidAudio",       "rightout",    "Reverberator",     	"rightin"
connect                 "JackAudio",        "leftout",     "LeftReverberator",  "input"
connect                 "JackAudio",        "rightout",    "RightReverberator", "input"
connect                 "LeftReverberator", "output",      "Compressor",        "leftin"
connect                 "RightReverberator","output",      "Compressor",        "rightin"
connect                 "Compressor",       "leftout",     "Soundfile",       	"leftin"
connect                 "Compressor",       "rightout",    "Soundfile",       	"rightin"

;                       ALL ALWAYSON CONTROL STATEMENTS GO BELOW THIS

#ifdef ENABLE_PIANOTEQ
alwayson			    "PianoteqAudio", 0, 100
#end

alwayson                "FluidAudio", 0, 32
alwayson		        "JackAudio"	  

alwayson                "LeftReverberator", 0.82, 0.3433, 13000, 0.128
alwayson                "RightReverberator", 0.80, 0.3333, 14000, 0.1285
alwayson                "Compressor"
alwayson                "Soundfile"

#include                "patches/CommonOpcodes"

;                       INSTRUMENTS THAT TAKE "i" STATEMENTS TO CREATE NOTES 
;                       GO BELOW THIS IN INSNO ORDER

#include                "patches/JackNote"
#include                "patches/JackNote1"
#include                "patches/Harpsichord"
;#include                "patches/Pianoteq.inc"
#include                "patches/STKBeeThree"
;#include                "patches/Plucked"
#include                "patches/Flute"
#include                "patches/Rhodes"
#include                "patches/Melody"
#include                "patches/FMModulatedChorus"
#include                "patches/FMModerate"
#include                "patches/TubularBell"
#include                "patches/FMBell"
#include                "patches/DelayedPlucked"
#include                "patches/FMModerate2"
;#include                "patches/STKBowed"
;                        Clicks, alas.
; #include                "patches/HeavyMetal"
#include                "patches/Xing"
#include                "patches/Guitar"
#include                "patches/Guitar2"
;#include                "patches/STKPlucked"
#include                "patches/FluidSteinwayNote"
#include			    "patches/LivingstonGuitar"

;                       INSTRUMENTS THAT COLLECT AUDIO FROM INSTRUMENTS ABOVE THIS 
;                       GO BELOW THIS 

;#include                "patches/PianoteqAudio.inc"
#include                "patches/FluidAudio"
#include                "patches/JackAudio"

;                       INSTRUMENTS THAT ARE MASTER EFFECTS AND OUTPUTS GO BELOW THIS 

#include                "patches/RightReverberator"
#include                "patches/LeftReverberator"
#include                "patches/Compressor"
#include                "patches/Soundfile"
]]
score:setOrchestra(orchestra)
score:setPreCsoundCommands([[
pkill -9 jackd
pkill -9 aeolus
pkill -9 Pianoteq
pkill -9 csound
sleep 2
/usr/bin/jackd -R -P50 -t2000 -u -dalsa -s -dhw:0 -r48000 -p128 -n3 &
sleep 2
Pianoteq &
aeolus -u -S /home/mkg/stops &
sleep 20    
]])
score:setPostCsoundCommands([[
pkill -9 jackd
pkill -9 aeolus
pkill -9 Pianoteq
pkill -9 csound
]])
arg[1] = '--dir'
arg[2] = '/utah/home/mkg/silencio/music'
arg[3] = '--display'
arg[4] = '--csound'
score:processArg(arg)

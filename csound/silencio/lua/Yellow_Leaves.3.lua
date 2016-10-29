package.path = package.path .. ';/media/3935-3164/sl4a/scripts/?.lua;/home/mkg/Downloads/iup/?.lua'
package.cpath = package.cpath .. ';/home/mkg/Downloads/iup/?.so;/home/mkg/Downloads/iup/lib?51.so;/home/mkg/Downloads/iup/libiup?.so;/home/mkg/Downloads/iup/libiup?51.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/lib?51.so;/usr/local/lib/lua/5.1/loadall.so'

print('package.path:', package.path)
print('package.cpath:', package.cpath)

require "Silencio"
require "matrix"

Silencio.help()

score = Score:new()
score:setArtist('Michael_Gogins')
score:setCopyright('Copr_2010_Michael_Gogins')
score:setAlbum('Silencio')

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
function recurrent(generators, transitions, depth, index, cursor, score)
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
            recurrent(generators, transitions, depth, transitionIndex, newCursor:clone(), score) 
        end
    end
end

g1 = function(cursor, depth)
    --cursor[TIME] = (cursor[TIME] / 2) - 1
    cursor[TIME] = (cursor[TIME] / 2) - 1.025
    --cursor[TIME] = (cursor[TIME] / 1.875) - 1
    cursor[KEY] = cursor[KEY] / 2 
    cursor[CHANNEL] = 0.0
    cursor[VELOCITY] = (cursor[VELOCITY] / 2) - 1
    return cursor, {cursor}
end

g2 = function(cursor, depth)
    cursor[TIME] = (cursor[TIME] / 2) - 1.075
    --cursor[TIME] = (cursor[TIME] / 2) - 1.075
    --cursor[TIME] = (cursor[TIME] / 1.875) - 1.075
    cursor[KEY] = (cursor[KEY] / 2) + 1.775
    cursor[CHANNEL] = 1.0                                     
    cursor[VELOCITY] = (cursor[VELOCITY] / 3)                      
    return cursor, {cursor}
end

g3 = function(cursor, depth)
    --cursor[TIME] = (cursor[TIME] / 2) + 1.05375
    cursor[TIME] = (cursor[TIME] / 2) + 1.075
    --cursor[TIME] = (cursor[TIME] / 1.875) + 1.05375
    cursor[KEY] = cursor[KEY] / 2
    cursor[CHANNEL] = 0.0
    cursor[VELOCITY] = (cursor[VELOCITY] / 2) + 2
    return cursor, {cursor}
end

g4 = function(cursor, depth)
    cursor[TIME] = (cursor[TIME] / 2) + 1
    --cursor[TIME] = (cursor[TIME] / 2) + 1
    --cursor[TIME] = (cursor[TIME] / 1.875) + 1
    cursor[KEY] = (cursor[KEY] / 2 ) + 2 - 0.09875
    cursor[CHANNEL] = 1.0
    cursor[VELOCITY] = (cursor[VELOCITY] / 3)
    return cursor, {cursor}
end

generators = {g1, g2, g3, g4}

transitions = {}
transitions[1] = {1, 1, 0, 1}
transitions[2] = {1, 1, 1, 1}
transitions[3] = {0, 1, 1, 1}
transitions[4] = {1, 0, 1, 1}

cursor = Event:new{1,1,144,0,1,1,0,0,0,0,1}
cursor[DURATION] = 0.25
recurrent(generators, transitions, 7, 1, cursor, score)
score:setScale(CHANNEL, 0.0, 2)
score:setScale(KEY, 22.0, 88.0)
score:setScale(VELOCITY, 50.0, 50.0)
score:setScale(DURATION, 2.0, 4.0)
score:setDuration(305.0)
score:setScale(DURATION, 2.0, 8.0)
score:temper(12.0)
score:tieOverlaps()
print("Generated score:")
score:print()
scales = score:findScales()
print('minima:', scales[1])
print('ranges:', scales[2])
print()

score:setMidiPatches({{'patch_change', 1, 0,16},{'patch_change', 1, 1,19}, {'patch_change', 1, 2,21} })
score:setFomusParts({'Cembalom', 'Gong', 'Fife'})
orchestra = [[
sr      =   96000
ksmps   =       1
nchnls  =       2
0dbfs   =       6.0

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
            
giPianoteq              vstinit                 "C:\\utah\\opt\\pianoteq-3.5\\Pianoteq.dll", 0
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

#include                "patches/Pianoteq.inc"
#include                "patches/JackNote1"
#include                "patches/JackNote"
#include                "patches/Harpsichord"
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
arg[2] = '/home/mkg/Dropbox/music/'
arg[3] = '--display'
--arg[4] = '--pianoteq-wav'
score:processArg(arg)

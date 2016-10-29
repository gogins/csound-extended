require "Silencio"

Silencio.help()

score = Score:new()
score:setArtist('Michael Gogins')
score:setTitle('SilencioTest')
c = 0.9739333
y = 0.55
n = 1000
dt = 0.125/2
d = 0.25
for i = 0, n - 1 do 
    y0 = y * c * 4.0 * (1.0 - y)
    y = y0
    score:append(i * dt, d, 144, i % 3, math.floor(36.5 + y * 60.0), 100, 0, 0, 0, 0, 1)
end
print("Generated score:")
for i, event in ipairs(score) do
    print(i, event:csoundIStatement())
    print(' ', event:midiScoreEventString())
end

scales = score:findScales()
print(string.format('minima: %s', scales[1]:csoundIStatement()))
print(string.format('ranges: %s', scales[2]:csoundIStatement()))

print("BEGAN MIDI rendering...")
score:saveMidi({{'patch_change', 0, 0, 1}})
parts = {'Cembalom', 'Gong', 'Fife'}
score:playMidi()
print("ENDED MIDI rendering.")
os.exit(0)
score:saveFomus(parts)

print("BEGAN Csound rendering...")
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
       JackoMidiOutConnect 	"midiout", "aeolus:Midi/in"

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

connect                 "LivingstonGuitar", "leftout",     "Reverberator",     	"leftin"
connect                 "LivingstonGuitar", "rightout",    "Reverberator",     	"rightin"
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

connect                 "PianoteqAudio",    "leftout",     "LeftReverberator",  "input"
connect                 "PianoteqAudio",    "rightout",    "RightReverberator", "input"
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

alwayson                "LeftReverberator", 0.72, 0.3433, 13000, 0.128
alwayson                "RightReverberator", 0.73, 0.3333, 14000, 0.1285
alwayson                "Compressor"
alwayson                "Soundfile"

#include                "patches/CommonOpcodes"

;                       INSTRUMENTS THAT TAKE "i" STATEMENTS TO CREATE NOTES 
;                       GO BELOW THIS IN INSNO ORDER

#include                "patches/JackNote"
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
#include                "patches/Harpsichord"
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

#include                "patches/PianoteqAudio.inc"
#include                "patches/FluidAudio"
#include                "patches/JackAudio"

;                       INSTRUMENTS THAT ARE MASTER EFFECTS AND OUTPUTS GO BELOW THIS 

#include                "patches/RightReverberator"
#include                "patches/LeftReverberator"
#include                "patches/Compressor"
#include                "patches/Soundfile"
]]
score:setOrchestra(orchestra)
print('Starting Jack...')
os.execute([[
/usr/bin/jackd -R -Z -P50 -t2000 -u -dalsa -s -dhw:0 -r48000 -p128 -n3&
sleep 2
aeolus -t -u -S /home/mkg/stops &
sleep 2
]])
score:renderCsound()
print('Stopping Jack...')
os.execute('pkill -9 aeolus')
os.execute('pkill -9 jackd')
score:playWav()
print("ENDED Csound rendering.")

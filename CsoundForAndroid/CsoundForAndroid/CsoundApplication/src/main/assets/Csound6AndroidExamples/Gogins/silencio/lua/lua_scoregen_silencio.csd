<CsoundSynthesizer>
<CsOptions>
-d -RWfo D:/Dropbox/music/lua_scoregen_silencio.wav -m3
</CsOptions>
<CsInstruments>

sr      = 44100
ksmps   = 100
nchnls  = 2

; Lua code to generate a score in the orchestra header.

lua_exec {{
local ffi = require("ffi")
local math = require("math")
local string = require("string")
local silencio = require("silencio")
local csoundApi = ffi.load('csound64.dll')
-- Declare the parts of the Csound API that we need.
-- You must declare MYFLT as double or float as the case may be.
ffi.cdef[[
    int csoundGetKsmps(void *);
    double csoundGetSr(void *);
    int csoundInputMessage(void *, const char *);
    int csoundScoreEvent(void *, char type, const double *, int);
    int csoundMessage(void *, const char *, ...);
]]
csoundApi.csoundMessage(csound, "package.path:      %s\\n", package.path)
csoundApi.csoundMessage(csound, "csound:            0x%08x\\n", csound)

score = Score:new()
score:setCsound(csound, csoundApi)
score:setTitle('lua_scoregen_silencio')
score:setArtist('Michael_Gogins')
score:setDirectory('D:/Dropbox/music/')

-- Compute a score using the logistic equation.

local c = .9849
local y = 0.5
local y1 = 0.5
local interval = 0.125
local duration = 0.75
local insno = 1
local scoretime = 0.5

for i = 1, 400 do
    scoretime = scoretime + interval
    y1 = c * y * (1 - y) * 4
    y = y1
    local key = math.floor(36 + y * 60)
    local velocity = 80
    score:append(scoretime, duration, 144.0, 0.0, key, velocity, 0.0, 0.0, 0.0, 0.0)
end
-- This note invokes postprocessing.
-- It must be the last note in the piece and come after all sounds have died away.
csoundApi.csoundInputMessage(csound, string.format("i 2 %9.4f 5", scoretime + 5)) 
score:sendToCsound()
}}

    lua_opdef "postprocess", {{
local ffi = require("ffi")
local csoundApi = ffi.load('csound64.dll.5.2')
-- Declare the parts of the Csound API that we need.
-- You must declare MYFLT as double or float as the case may be.
ffi.cdef[[
    int csoundMessage(void *, const char *, ...);
]]

function postprocess_init(csound, opcode, carguments)
    csoundApi.csoundMessage(csound, 'Post-processing...\\n')
    score:postProcess()
    return 0
end
}}

            instr       1
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ; Simple FM instrument.
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
khz         =           cpsmidinn(p4)
kgain       invalue     "kgain"
kamplitude  =           ampdb(p5) * kgain
kcarrier    invalue     "kcarrier"
kmodulator  invalue     "kmodulator"
            ; Intensity sidebands.
kindex      transeg     1, p3, -5, 0.01	
isine       ftgenonce   1, 0, 16384, 10, 1
asignal     foscili     kamplitude * kindex, khz, kcarrier, kmodulator, 20* kindex, isine
            outs        asignal, asignal
            endin
            
            instr 	2
S4          getcfg	4
iresult     strcmp     S4, "1"
            if iresult != 0 then
            prints     "Off-line renderin: post-processing will be performed.\n"
            lua_iopcall "postprocess"
            else
            prints     "Real-time rendering: no post-processing will be performed.\n"
            endif
            exitnow
            endin
            
</CsInstruments>

<CsScore>
f 0 [60*100]
e 4.0
</CsScore>

</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>0</x>
 <y>0</y>
 <width>151</width>
 <height>137</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="background">
  <r>50</r>
  <g>77</g>
  <b>24</b>
 </bgcolor>
 <bsbObject type="BSBLabel" version="2">
  <objectName/>
  <x>5</x>
  <y>6</y>
  <width>146</width>
  <height>50</height>
  <uuid>{ff229e77-bbc5-40fd-a416-6ece90f927eb}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>kgain</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>170</r>
   <g>170</g>
   <b>127</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>5</borderradius>
  <borderwidth>6</borderwidth>
 </bsbObject>
 <bsbObject type="BSBHSlider" version="2">
  <objectName>kgain</objectName>
  <x>9</x>
  <y>33</y>
  <width>138</width>
  <height>20</height>
  <uuid>{a5d72fc6-9c45-4d42-b9cc-195e9fd9748d}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.12318841</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
 <bsbObject type="BSBDisplay" version="2">
  <objectName>kgain</objectName>
  <x>68</x>
  <y>6</y>
  <width>79</width>
  <height>20</height>
  <uuid>{6e42a5d1-f37d-4ca5-abf2-9d39d383c809}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.123</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject type="BSBLabel" version="2">
  <objectName/>
  <x>5</x>
  <y>59</y>
  <width>146</width>
  <height>78</height>
  <uuid>{e958825f-6f31-4566-b6a3-8753db34d8d8}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>kcarrier(x) and kmodulator (y)</label>
  <alignment>left</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="background">
   <r>170</r>
   <g>170</g>
   <b>127</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>5</borderradius>
  <borderwidth>6</borderwidth>
 </bsbObject>
 <bsbObject type="BSBController" version="2">
  <objectName>kcarrier</objectName>
  <x>10</x>
  <y>76</y>
  <width>97</width>
  <height>56</height>
  <uuid>{75981d5e-a027-4302-8652-adc45bff59d9}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <objectName2>kmodulator</objectName2>
  <xMin>0.00000000</xMin>
  <xMax>5.00000000</xMax>
  <yMin>0.00000000</yMin>
  <yMax>5.00000000</yMax>
  <xValue>1.23711340</xValue>
  <yValue>0.00000000</yValue>
  <type>crosshair</type>
  <pointsize>1</pointsize>
  <fadeSpeed>0.00000000</fadeSpeed>
  <mouseControl act="press">jump</mouseControl>
  <color>
   <r>0</r>
   <g>234</g>
   <b>0</b>
  </color>
  <randomizable mode="both" group="0">false</randomizable>
  <bgcolor>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </bgcolor>
 </bsbObject>
 <bsbObject type="BSBDisplay" version="2">
  <objectName>kcarrier</objectName>
  <x>106</x>
  <y>76</y>
  <width>44</width>
  <height>20</height>
  <uuid>{546eedac-356c-4274-bace-f3d70f31be7f}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>1.237</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
 <bsbObject type="BSBDisplay" version="2">
  <objectName>kmodulator</objectName>
  <x>106</x>
  <y>112</y>
  <width>44</width>
  <height>20</height>
  <uuid>{25b06aca-60cd-4e38-be6e-3549e4115cdf}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>0</midicc>
  <label>0.000</label>
  <alignment>right</alignment>
  <font>Arial</font>
  <fontsize>10</fontsize>
  <precision>3</precision>
  <color>
   <r>0</r>
   <g>0</g>
   <b>0</b>
  </color>
  <bgcolor mode="nobackground">
   <r>255</r>
   <g>255</g>
   <b>255</b>
  </bgcolor>
  <bordermode>noborder</bordermode>
  <borderradius>1</borderradius>
  <borderwidth>1</borderwidth>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
